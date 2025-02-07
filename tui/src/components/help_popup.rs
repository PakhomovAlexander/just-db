use color_eyre::Result;
use ratatui::{
    layout::Rect,
    style::{Style, Stylize},
    text::Span,
    widgets::{Block, Clear, Paragraph},
    Frame,
};

use super::Component;

use crate::action::Action;

#[derive(Debug, Clone, PartialEq)]
pub struct HelpPopup {
    enabled: bool,
}

impl Default for HelpPopup {
    fn default() -> Self {
        Self::new()
    }
}

impl HelpPopup {
    pub fn new() -> Self {
        Self { enabled: false }
    }

    fn toggle(&mut self) -> Result<()> {
        self.enabled = !self.enabled;
        Ok(())
    }
}

impl Component for HelpPopup {
    fn name(&self) -> &str {
        "help_popup"
    }

    fn update(&mut self, action: Action) -> Result<Option<Action>> {
        if let Action::Help = action {
            let _ = self.toggle();
        };
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, area: Rect) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }

        frame.render_widget(Clear, frame.area());

        let message = "Help:
  <q>: Quit,
  <i>: Switch to insert mode,
  <d>: Clear editor,
  <h>: Show/Suppress Help,
  <enter>: Execute query from editor";

        let paragraph = Paragraph::new(message).block(Block::bordered());
        frame.render_widget(paragraph, area);
        Ok(())
    }
}
