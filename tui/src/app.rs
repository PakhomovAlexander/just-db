use std::borrow::Borrow;

use color_eyre::{eyre, Result};
use crossterm::event::KeyEvent;
use db::embedded::Db;
use db::optimizer::types::{Tuple, Val};
use ratatui::prelude::Rect;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc;
use tracing::{debug, info};

use crate::{
    action::Action,
    components::{editor::Editor, fps::FpsCounter, status_bar::StatusBar, table::Table, Component},
    config::Config,
    layout::AppLayout,
    tui::{Event, Tui},
};

pub struct App {
    config: Config,
    tick_rate: f64,
    frame_rate: f64,
    components: Vec<Box<dyn Component>>,
    should_quit: bool,
    should_suspend: bool,
    mode: Mode,
    last_tick_key_events: Vec<KeyEvent>,
    action_tx: mpsc::UnboundedSender<Action>,
    action_rx: mpsc::UnboundedReceiver<Action>,
    current_text_in_editor: String,
    db: Db,
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Mode {
    Typing,
    #[default]
    Normal,
}

impl App {
    pub fn new(tick_rate: f64, frame_rate: f64) -> Result<Self> {
        let (action_tx, action_rx) = mpsc::unbounded_channel();

        Ok(Self {
            tick_rate,
            frame_rate,
            components: vec![
                Box::new(Editor::new()),
                Box::new(Table::new()),
                Box::new(FpsCounter::default()),
                Box::new(StatusBar::default()),
            ],
            should_quit: false,
            should_suspend: false,
            config: Config::new()?,
            mode: Mode::Normal,
            last_tick_key_events: Vec::new(),
            db: Db::new(),
            current_text_in_editor: String::new(),
            action_tx,
            action_rx,
        })
    }

    pub async fn run(&mut self) -> Result<()> {
        let mut tui = Tui::new()?
            // .mouse(true) // uncomment this line to enable mouse support
            .tick_rate(self.tick_rate)
            .frame_rate(self.frame_rate);
        tui.enter()?;

        for component in self.components.iter_mut() {
            component.register_action_handler(self.action_tx.clone())?;
        }
        for component in self.components.iter_mut() {
            component.register_config_handler(self.config.clone())?;
        }
        for component in self.components.iter_mut() {
            component.init(tui.size()?)?;
        }

        let action_tx = self.action_tx.clone();
        loop {
            self.handle_events(&mut tui).await?;
            self.handle_actions(&mut tui)?;
            if self.should_suspend {
                tui.suspend()?;
                action_tx.send(Action::Resume)?;
                action_tx.send(Action::ClearScreen)?;
                // tui.mouse(true);
                tui.enter()?;
            } else if self.should_quit {
                tui.stop()?;
                break;
            }
        }
        tui.exit()?;
        Ok(())
    }

    async fn handle_events(&mut self, tui: &mut Tui) -> Result<()> {
        let Some(event) = tui.next_event().await else {
            return Ok(());
        };
        let action_tx = self.action_tx.clone();
        match event {
            Event::Quit => action_tx.send(Action::Quit)?,
            Event::Tick => action_tx.send(Action::Tick)?,
            Event::Render => action_tx.send(Action::Render)?,
            Event::Resize(x, y) => action_tx.send(Action::Resize(x, y))?,
            Event::Key(key) => self.handle_key_event(key)?,
            _ => {}
        }
        for component in self.components.iter_mut() {
            if let Some(action) = component.handle_events(Some(event.clone()))? {
                action_tx.send(action)?;
            }
        }
        Ok(())
    }

    fn handle_key_event(&mut self, key: KeyEvent) -> Result<()> {
        let action_tx = self.action_tx.clone();
        let Some(keymap) = self.config.keybindings.get(&self.mode) else {
            eyre::bail!("No such mode in keymap");
        };

        if self.mode == Mode::Typing {
            if let Some(Action::ToNormalMode) = keymap.get(&vec![key]) {
                self.mode = Mode::Normal;
            };
            action_tx.send(Action::Typed(key))?;
            return Ok(());
        }

        match keymap.get(&vec![key]) {
            Some(action) => {
                info!("Got action: {action:?}");
                if *action == Action::D {
                    let a = Action::QueryResultReceived(vec![
                        Tuple::new(vec![
                            ("col1", Val::Int(1)),
                            ("col2", Val::String("Hii1".to_string())),
                            ("col3", Val::Int(111)),
                        ]),
                        Tuple::new(vec![
                            ("col1", Val::Int(2)),
                            ("col2", Val::String("Hii2".to_string())),
                            ("col3", Val::Int(222)),
                        ]),
                        Tuple::new(vec![
                            ("col1", Val::Int(3)),
                            ("col2", Val::String("Hii3".to_string())),
                            ("col3", Val::Int(333)),
                        ]),
                        Tuple::new(vec![
                            ("col1", Val::Int(4)),
                            ("col2", Val::String("Hii4".to_string())),
                            ("col3", Val::Int(444)),
                        ]),
                    ]);
                    action_tx.send(a.clone())?;
                } else {
                    action_tx.send(action.clone())?;
                }
            }
            _ => {
                // If the key was not handled as a single key action,
                // then consider it for multi-key combinations.
                self.last_tick_key_events.push(key);

                // Check for multi-key combinations
                if let Some(action) = keymap.get(&self.last_tick_key_events) {
                    info!("Got action: {action:?}");
                    action_tx.send(action.clone())?;
                }
            }
        }
        Ok(())
    }

    fn handle_actions(&mut self, tui: &mut Tui) -> Result<()> {
        while let Ok(action) = self.action_rx.try_recv() {
            if action != Action::Tick && action != Action::Render {
                debug!("{action:?}");
            }
            match &action {
                Action::Tick => {
                    self.last_tick_key_events.drain(..);
                }
                Action::Quit => self.should_quit = true,
                Action::Suspend => self.should_suspend = true,
                Action::Resume => self.should_suspend = false,
                Action::ClearScreen => tui.terminal.clear()?,
                Action::Resize(w, h) => self.handle_resize(tui, *w, *h)?,
                Action::Render => self.render(tui)?,
                Action::ToEditingMode => self.mode = Mode::Typing,
                Action::ToNormalMode => self.mode = Mode::Normal,
                Action::TextUpdated(text) => {
                    self.current_text_in_editor = text.clone();
                }
                Action::ExecuteQueryRequested => {
                    let query = self.current_text_in_editor.clone();
                    let execute_query = Action::ExecuteQuery(query.to_string());
                    self.action_tx.send(execute_query)?;
                }
                Action::ExecuteQuery(query) => {
                    let tuples = self.db.run_query(&query.clone());
                    let cnt = tuples.len();
                    let query_result_received = Action::QueryResultReceived(tuples);
                    self.action_tx.send(query_result_received)?;
                    self.action_tx.send(Action::UpdateStatusBar(
                        format!("rows get: {}, query: {}", cnt, &query.clone()).to_string(),
                    ))?;
                }
                _ => {}
            }
            for component in self.components.iter_mut() {
                if let Some(action) = component.update(action.clone())? {
                    self.action_tx.send(action)?
                };
            }
        }
        Ok(())
    }

    fn handle_resize(&mut self, tui: &mut Tui, w: u16, h: u16) -> Result<()> {
        tui.resize(Rect::new(0, 0, w, h))?;
        self.render(tui)?;
        Ok(())
    }

    fn render(&mut self, tui: &mut Tui) -> Result<()> {
        tui.draw(|frame| {
            for component in self.components.iter_mut() {
                let l = AppLayout::from(frame.borrow());

                let area = l.get(component.name()).unwrap();
                if let Err(err) = component.draw(frame, area) {
                    let _ = self
                        .action_tx
                        .send(Action::Error(format!("Failed to draw: {:?}", err)));
                }
            }
        })?;
        Ok(())
    }
}
