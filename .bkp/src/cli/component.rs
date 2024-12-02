use color_eyre::eyre::Result;
use ratatui::crossterm::event::{KeyEvent, MouseEvent};
use ratatui::layout::Rect;

use crate::{action::Action, event::Event, terminal::Frame};

pub trait Component {
    fn init(&mut self) -> Result<()> {
        Ok(())
    }
    fn handle_events(&mut self, event: Option<Event>) -> Action {
        match event {
            Some(Event::Quit) => Action::Quit,
            Some(Event::Tick) => Action::Tick,
            Some(Event::Key(key_event)) => self.handle_key_events(key_event),
            Some(Event::Mouse(mouse_event)) => self.handle_mouse_events(mouse_event),
            Some(Event::Resize(x, y)) => Action::Resize(x, y),
            Some(_) => Action::Noop,
            None => Action::Noop,
        }
    }
    fn handle_key_events(&mut self, key: KeyEvent) -> Action {
        Action::Noop
    }
    fn handle_mouse_events(&mut self, mouse: MouseEvent) -> Action {
        Action::Noop
    }
    fn update(&mut self, action: Action) -> Action {
        Action::Noop
    }
    fn render(&mut self, f: &mut Frame, rect: Rect);
}
