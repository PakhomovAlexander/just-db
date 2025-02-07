use color_eyre::Result;
use edtui::{EditorEventHandler, EditorMode, EditorState};
use ratatui::prelude::*;
use tokio::sync::mpsc::UnboundedSender;

use super::Component;
use crate::{action::Action, config::Config};

#[derive(Default)]
pub struct Editor {
    command_tx: Option<UnboundedSender<Action>>,
    config: Config,
    editor_state: EditorState,
    editor_event_handler: EditorEventHandler,
}

impl Editor {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Component for Editor {
    fn name(&self) -> &str {
        "editor"
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
            Action::ToEditingMode => {
                self.editor_state.mode = EditorMode::Insert;
            }
            Action::Typed(event) => {
                self.editor_event_handler
                    .on_key_event(event, &mut self.editor_state);

                let mut text = String::new();
                for line in &self.editor_state.lines.flatten(&Some('\n')) {
                    text.push(*line);
                }
                // maybe flatten?

                self.command_tx
                    .as_ref()
                    .unwrap()
                    .send(Action::TextUpdated(text))?;
            }
            Action::ClearEditor => {
                self.editor_state.lines.clear();
                self.command_tx
                    .as_ref()
                    .unwrap()
                    .send(Action::TextUpdated(String::new()))?;
            }
            _ => {}
        }
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, area: Rect) -> Result<()> {
        use edtui::{EditorTheme, EditorView};

        let editor = EditorView::new(&mut self.editor_state)
            .theme(EditorTheme::default())
            .wrap(true);

        frame.render_widget(editor, area);
        Ok(())
    }
}
