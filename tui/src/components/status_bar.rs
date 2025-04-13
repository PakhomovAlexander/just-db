use std::fmt::Debug;

use color_eyre::{owo_colors::OwoColorize, Result};
use db::parser::errors::ParseError;
use miette::Report;
use ratatui::{
    layout::Rect,
    style::{Color, Style, Stylize},
    text::{Line, Span, Text, ToText},
    widgets::{Block, Clear, Paragraph},
    Frame,
};

use super::Component;

use crate::action::Action;

#[derive(Debug, Clone, PartialEq)]
pub struct StatusBar {
    last_message: String,
    p_err: Option<ParseError>,
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
            p_err: None,
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
        if let Action::UpdateStatusBar(msg) = action.clone() {
            self.update_status(msg)?
        };
        if let Action::Error(err) = action {
            self.p_err = Some(err);
            //self.update_status(message)?
        };
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, area: Rect) -> Result<()> {
        if self.p_err.is_some() {
            let p_err = self.p_err.clone().unwrap();
            let report = Report::new(p_err);
            let message = format!("{:?}", report);

            frame.render_widget(Clear, area);
            frame.render_widget(
                // color::white is the workaround for the report formatting
                Paragraph::new(message).style(Style::default().fg(Color::White)),
                area,
            );
        }

        Ok(())
    }
}
