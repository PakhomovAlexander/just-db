use color_eyre::Result;
use db::optimizer::types::{Tuple, Val};
use ratatui::{prelude::*, widgets::*};
use tokio::sync::mpsc::UnboundedSender;

use super::Component;
use crate::{action::Action, config::Config};

#[derive(Default)]
pub struct Table {
    command_tx: Option<UnboundedSender<Action>>,
    config: Config,
    current_data: Option<Vec<Tuple>>,
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
        if let Action::QueryResultReceived(data) = action {
            self.current_data = Some(data);
        }
        Ok(None)
    }

    fn draw(&mut self, frame: &mut Frame, area: Rect) -> Result<()> {
        let table = InnerTable::new(self.current_data.clone());

        frame.render_widget(table.build_widget(), area);
        Ok(())
    }
}

struct InnerTable<'a> {
    rows: Vec<Row<'a>>,
    widths: Vec<Constraint>,
    column_spacing: u16,
    style: Style,
    header: Option<Row<'a>>,
}

impl<'a> InnerTable<'a> {
    fn new(data: Option<Vec<Tuple>>) -> Self {
        let header: Vec<String> = match &data {
            Some(data) => data[0].keys(),
            None => Vec::new(),
        };

        let rows = match &data {
            Some(data) => {
                let mut rows = Vec::new();
                for tuple in data.iter() {
                    rows.push(tuple_to_row(tuple.clone(), header.clone()));
                }
                rows
            }
            None => Vec::new(),
        };

        Self {
            rows,
            widths: Vec::new(),
            column_spacing: 1,
            style: Style::default(),
            header: Some(Row::new(header)),
        }
    }

    fn build_widget(self) -> ratatui::widgets::Table<'a> {
        use ratatui::widgets::Table;

        Table::default()
            .rows(self.rows)
            .header(self.header.unwrap())
            .widths(self.widths)
            .column_spacing(self.column_spacing)
            .style(self.style)
    }
}

fn tuple_to_row<'a>(tuple: Tuple, header: Vec<String>) -> Row<'a> {
    let cells: Vec<String> = header
        .iter()
        .map(|cell| val_to_sting(tuple.get(cell).clone()))
        .collect();

    Row::new(cells)
}

// TODO: implement display for Val
fn val_to_sting(v: Val) -> String {
    match v {
        Val::Int(i) => i.to_string(),
        Val::String(s) => s,
        Val::Bool(b) => b.to_string(),
        Val::Null => "NULL".to_string(),
    }
}
