use std::collections::HashMap;

use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    Frame,
};

pub struct AppLayout {
    areas: HashMap<String, Rect>,
}

impl AppLayout {
    fn new(frame: &Frame) -> Self {
        let outer_layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints(vec![
                Constraint::Percentage(50),
                Constraint::Percentage(40),
                Constraint::Percentage(10),
            ])
            .split(frame.size());

        let inner_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints(vec![Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(outer_layout[2]);

        let mut areas = HashMap::new();
        areas.insert("table".to_string(), outer_layout[0]);
        areas.insert("editor".to_string(), outer_layout[1]);
        areas.insert("status_bar".to_string(), inner_layout[0]);
        areas.insert("fps_counter".to_string(), inner_layout[1]);

        Self { areas }
    }

    pub fn get(&self, name: &str) -> Option<Rect> {
        self.areas.get(name).copied()
    }
}

impl From<&Frame<'_>> for AppLayout {
    fn from(frame: &Frame) -> Self {
        Self::new(frame)
    }
}
