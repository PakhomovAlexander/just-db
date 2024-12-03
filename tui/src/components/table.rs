use color_eyre::Result;
use ratatui::{prelude::*, widgets::*};
use tokio::sync::mpsc::UnboundedSender;

use super::Component;
use crate::{action::Action, config::Config};

#[derive(Default)]
pub struct Table {
    command_tx: Option<UnboundedSender<Action>>,
    config: Config,
}

impl Table {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Component for Table {
    fn name(&self) -> &str {
        "table"
    }
    fn register_action_handler(&mut self, tx: UnboundedSender<Action>) -> Result<()> {
        self.command_tx = Some(tx);
        Ok(())
    }

    fn register_config_handler(&mut self, config: Config) -> Result<()> {
        self.config = config;
        Ok(())
    }

    fn update(&mut self, action: Action) -> Result<Option<Action>> {
        match action {
            Action::Tick => {
                // add any logic here that should run on every tick
            }
            Action::Render => {
                // add any logic here that should run on every render
            }
            _ => {}
        }
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, area: Rect) -> Result<()> {
        let text = format!("AM AM A TABLE!!!! TRUST ME!!");

        let block = Block::default().borders(Borders::ALL);

        frame.render_widget(Paragraph::new(text).block(block), area);
        Ok(())
    }
}
