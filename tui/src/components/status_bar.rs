
use color_eyre::Result;
use ratatui::{
    layout::Rect,
    style::{Style, Stylize},
    text::Span,
    widgets::Paragraph,
    Frame,
};

use super::Component;

use crate::action::Action;

#[derive(Debug, Clone, PartialEq)]
pub struct StatusBar {
    last_message: String,
}

impl Default for StatusBar {
    fn default() -> Self {
        Self::new()
    }
}

impl StatusBar {
    pub fn new() -> Self {
        Self {
            last_message: String::new(),
        }
    }

    fn update_status(&mut self, status: String) -> Result<()> {
        self.last_message = status;
        Ok(())
    }
}

impl Component for StatusBar {
    fn name(&self) -> &str {
        "status_bar"
    }

    fn update(&mut self, action: Action) -> Result<Option<Action>> {
        if let Action::UpdateStatusBar(msg) = action {
            self.update_status(msg)?
        };
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, area: Rect) -> Result<()> {
        let message = format!("Status: {}", self.last_message);
        let span = Span::styled(message, Style::new().dim());
        let paragraph = Paragraph::new(span);
        frame.render_widget(paragraph, area);
        Ok(())
    }
}
