mod analyzer;
mod catalog;
mod optimizer;
mod parser;

use std::rc::Rc;

use analyzer::Analyzer;
use catalog::{Catalog, DataType, TableSchemaBuilder};
use color_eyre::Result;
use crossterm::event::KeyModifiers;
use itertools::Itertools;
use optimizer::{Optimizer, StorageEngine, Tuple, Val};
use parser::{lexer::Lexer, Parser};
use ratatui::{
    crossterm::event::{self, Event, KeyCode, KeyEventKind},
    layout::{Constraint, Layout, Margin, Rect},
    style::{self, Color, Modifier, Style, Stylize},
    text::Text,
    widgets::{
        Block, BorderType, Cell, HighlightSpacing, Paragraph, Row, Scrollbar, ScrollbarOrientation,
        ScrollbarState, Table, TableState,
    },
    DefaultTerminal, Frame,
};
use style::palette::tailwind;
use tui_textarea::TextArea;
use unicode_width::UnicodeWidthStr;

const PALETTES: [tailwind::Palette; 4] = [
    tailwind::BLUE,
    tailwind::EMERALD,
    tailwind::INDIGO,
    tailwind::RED,
];

const INFO_TEXT: [&str; 2] = [
    "(Esc) quit | (↑) move up | (↓) move down | (←) move left | (→) move right",
    "(Shift + →) next color | (Shift + ←) previous color",
];

const ITEM_HEIGHT: usize = 4;

fn main() -> Result<()> {
    color_eyre::install()?;
    let terminal = ratatui::init();
    let app_result = App::new().run(terminal);
    ratatui::restore();
    app_result
}

struct Db {
    catalog_rc: Rc<Catalog>,
    storage_rc: Rc<StorageEngine>,
    analyzer: Analyzer,
    optimizer: Optimizer,
}

impl Db {
    fn new() -> Self {
        let mut catalog = Catalog::mem();
        let mut storage = StorageEngine::mem();

        let ts = TableSchemaBuilder::public()
            .table("table1")
            .col("name", DataType::Int)
            .col("address", DataType::Int)
            .col("email", DataType::Int)
            .build();
        let _ = catalog.register_table(&ts);

        storage.insert(
            "table1",
            vec![
                Tuple::new(vec![
                    ("name", Val::Int(1)),
                    ("address", Val::Int(2)),
                    ("email", Val::Int(10)),
                ]),
                Tuple::new(vec![
                    ("name", Val::Int(111)),
                    ("address", Val::Int(2222)),
                    ("email", Val::Int(10000)),
                ]),
                Tuple::new(vec![
                    ("name", Val::Int(13211)),
                    ("address", Val::Int(3212)),
                    ("email", Val::Int(10)),
                ]),
                Tuple::new(vec![
                    ("name", Val::Int(1)),
                    ("address", Val::Int(2)),
                    ("email", Val::Int(10)),
                ]),
            ],
        );

        let catalog_rc = Rc::new(catalog);
        let storage_rc = Rc::new(storage);

        let optimizer = Optimizer::new(Rc::clone(&catalog_rc), Rc::clone(&storage_rc));

        Self {
            catalog_rc,
            storage_rc,
            analyzer: Analyzer::new(),
            optimizer,
        }
    }

    fn run_query(&self, query: &str) -> Vec<Data> {
        let lexer = Lexer::new(query);
        let mut parser = Parser::new(lexer);
        let analyzer = Analyzer::new();

        let l_plan = analyzer.analyze(&parser.parse());

        let mut p_plan = self.optimizer.optimize(l_plan);

        p_plan
            .execute_all()
            .into_iter()
            .map(|tuple| {
                let name = Self::val_to_string(tuple.get("name"));
                let address = Self::val_to_string(tuple.get("address"));
                let email = Self::val_to_string(tuple.get("email"));

                Data {
                    name,
                    address,
                    email,
                }
            })
            .collect()
    }

    fn val_to_string(val: &Val) -> String {
        match val {
            Val::Int(i) => i.to_string(),
            Val::String(s) => s.clone(),
            _ => unimplemented!(),
        }
    }
}

struct TableColors {
    buffer_bg: Color,
    header_bg: Color,
    header_fg: Color,
    row_fg: Color,
    selected_row_style_fg: Color,
    selected_column_style_fg: Color,
    selected_cell_style_fg: Color,
    normal_row_color: Color,
    alt_row_color: Color,
    footer_border_color: Color,
}

impl TableColors {
    const fn new(color: &tailwind::Palette) -> Self {
        Self {
            buffer_bg: tailwind::SLATE.c950,
            header_bg: color.c900,
            header_fg: tailwind::SLATE.c200,
            row_fg: tailwind::SLATE.c200,
            selected_row_style_fg: color.c400,
            selected_column_style_fg: color.c400,
            selected_cell_style_fg: color.c600,
            normal_row_color: tailwind::SLATE.c950,
            alt_row_color: tailwind::SLATE.c900,
            footer_border_color: color.c400,
        }
    }
}

struct Data {
    name: String,
    address: String,
    email: String,
}

impl Data {
    const fn ref_array(&self) -> [&String; 3] {
        [&self.name, &self.address, &self.email]
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn address(&self) -> &str {
        &self.address
    }

    fn email(&self) -> &str {
        &self.email
    }
}

struct App<'a> {
    db: Db,
    app_state: AppState,
    state: TableState,
    textarea: TextArea<'a>,
    items: Vec<Data>,
    longest_item_lens: (u16, u16, u16), // order is (name, address, email)
    scroll_state: ScrollbarState,
    colors: TableColors,
    color_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum AppState {
    InputMode,
    NormalMode,
}

impl App<'_> {
    fn new() -> Self {
        let db = Db::new();

        // let data_vec = generate_fake_names();
        let data_vec = db.run_query("select name, address, email from table1");

        // Create an empty `TextArea` instance which manages the editor state
        let textarea = TextArea::default();
        Self {
            db,
            app_state: AppState::NormalMode,
            state: TableState::default().with_selected(0),
            textarea,
            longest_item_lens: constraint_len_calculator(&data_vec),
            scroll_state: ScrollbarState::new((data_vec.len() - 1) * ITEM_HEIGHT),
            colors: TableColors::new(&PALETTES[0]),
            color_index: 0,
            items: data_vec,
        }
    }

    pub fn next_row(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i >= self.items.len() - 1 {
                    0
                } else {
                    i + 1
                }
            }
            None => 0,
        };
        self.state.select(Some(i));
        self.scroll_state = self.scroll_state.position(i * ITEM_HEIGHT);
    }

    pub fn previous_row(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i == 0 {
                    self.items.len() - 1
                } else {
                    i - 1
                }
            }
            None => 0,
        };
        self.state.select(Some(i));
        self.scroll_state = self.scroll_state.position(i * ITEM_HEIGHT);
    }

    pub fn next_column(&mut self) {
        self.state.select_next_column();
    }

    pub fn previous_column(&mut self) {
        self.state.select_previous_column();
    }

    pub fn next_color(&mut self) {
        self.color_index = (self.color_index + 1) % PALETTES.len();
    }

    pub fn previous_color(&mut self) {
        let count = PALETTES.len();
        self.color_index = (self.color_index + count - 1) % count;
    }

    pub fn set_colors(&mut self) {
        self.colors = TableColors::new(&PALETTES[self.color_index]);
    }

    pub fn to_input_mode(&mut self) {
        self.app_state = AppState::InputMode;
    }

    pub fn to_normal_mode(&mut self) {
        self.app_state = AppState::NormalMode;
    }

    fn run(mut self, mut terminal: DefaultTerminal) -> Result<()> {
        loop {
            terminal.draw(|frame| self.draw(frame))?;

            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    if self.app_state == AppState::InputMode {
                        self.handle_input_mode(key)?;
                    } else {
                        self.handle_normal_mode(key)?;
                    }
                }
            }
        }
    }

    fn draw(&mut self, frame: &mut Frame) {
        let vertical = &Layout::vertical([Constraint::Min(5), Constraint::Length(4)]);
        let rects = vertical.split(frame.area());

        self.set_colors();

        self.render_table(frame, rects[0]);
        self.render_scrollbar(frame, rects[0]);
        self.render_input_box(frame, rects[1]);
        //self.render_footer(frame, rects[1]);
    }

    fn render_table(&mut self, frame: &mut Frame, area: Rect) {
        let header_style = Style::default()
            .fg(self.colors.header_fg)
            .bg(self.colors.header_bg);
        let selected_row_style = Style::default()
            .add_modifier(Modifier::REVERSED)
            .fg(self.colors.selected_row_style_fg);
        let selected_col_style = Style::default().fg(self.colors.selected_column_style_fg);
        let selected_cell_style = Style::default()
            .add_modifier(Modifier::REVERSED)
            .fg(self.colors.selected_cell_style_fg);

        let header = ["Name", "Address", "Email"]
            .into_iter()
            .map(Cell::from)
            .collect::<Row>()
            .style(header_style)
            .height(1);
        let rows = self.items.iter().enumerate().map(|(i, data)| {
            let color = match i % 2 {
                0 => self.colors.normal_row_color,
                _ => self.colors.alt_row_color,
            };
            let item = data.ref_array();
            item.into_iter()
                .map(|content| Cell::from(Text::from(format!("\n{content}\n"))))
                .collect::<Row>()
                .style(Style::new().fg(self.colors.row_fg).bg(color))
                .height(4)
        });
        let bar = " █ ";
        let t = Table::new(
            rows,
            [
                // + 1 is for padding.
                Constraint::Length(self.longest_item_lens.0 + 1),
                Constraint::Min(self.longest_item_lens.1 + 1),
                Constraint::Min(self.longest_item_lens.2),
            ],
        )
        .header(header)
        .row_highlight_style(selected_row_style)
        .column_highlight_style(selected_col_style)
        .cell_highlight_style(selected_cell_style)
        .highlight_symbol(Text::from(vec![
            "".into(),
            bar.into(),
            bar.into(),
            "".into(),
        ]))
        .bg(self.colors.buffer_bg)
        .highlight_spacing(HighlightSpacing::Always);
        frame.render_stateful_widget(t, area, &mut self.state);
    }

    fn render_scrollbar(&mut self, frame: &mut Frame, area: Rect) {
        frame.render_stateful_widget(
            Scrollbar::default()
                .orientation(ScrollbarOrientation::VerticalRight)
                .begin_symbol(None)
                .end_symbol(None),
            area.inner(Margin {
                vertical: 1,
                horizontal: 1,
            }),
            &mut self.scroll_state,
        );
    }

    fn render_footer(&self, frame: &mut Frame, area: Rect) {
        let info_footer = Paragraph::new(Text::from_iter(INFO_TEXT))
            .style(
                Style::new()
                    .fg(self.colors.row_fg)
                    .bg(self.colors.buffer_bg),
            )
            .centered()
            .block(
                Block::bordered()
                    .border_type(BorderType::Double)
                    .border_style(Style::new().fg(self.colors.footer_border_color)),
            );
        frame.render_widget(info_footer, area);
    }

    fn render_input_box(&self, frame: &mut Frame, area: Rect) {
        frame.render_widget(&self.textarea, area);
    }

    fn handle_normal_mode(&mut self, key: event::KeyEvent) -> Result<()> {
        let shift_pressed = key.modifiers.contains(KeyModifiers::SHIFT);
        match key.code {
            KeyCode::Char('q') | KeyCode::Esc => return Ok(()),
            KeyCode::Char('j') | KeyCode::Down => self.next_row(),
            KeyCode::Char('k') | KeyCode::Up => self.previous_row(),
            KeyCode::Char('i') => self.to_input_mode(),
            KeyCode::Char('l') | KeyCode::Right if shift_pressed => self.next_color(),
            KeyCode::Char('h') | KeyCode::Left if shift_pressed => {
                self.previous_color();
            }
            KeyCode::Char('l') | KeyCode::Right => self.next_column(),
            KeyCode::Char('h') | KeyCode::Left => self.previous_column(),
            _ => {}
        }

        Ok(())
    }

    fn handle_input_mode(&mut self, key: event::KeyEvent) -> Result<()> {
        match key.code {
            KeyCode::Esc => return Ok(()),
            KeyCode::Enter => self.enter_pressed(),
            _ => {
                // `TextArea::input` can directly handle key events from backends and update the editor state
                self.textarea.input(key);
            }
        }

        Ok(())
    }

    fn enter_pressed(&mut self) {
        // get str from the editor
        // run query agains database
        // display the result
        todo!();
    }
}

fn generate_fake_names() -> Vec<Data> {
    use fakeit::{address, contact, name};

    (0..20)
        .map(|_| {
            let name = name::full();
            let address = format!(
                "{}\n{}, {} {}",
                address::street(),
                address::city(),
                address::state(),
                address::zip()
            );
            let email = contact::email();

            Data {
                name,
                address,
                email,
            }
        })
        .sorted_by(|a, b| a.name.cmp(&b.name))
        .collect()
}

fn constraint_len_calculator(items: &[Data]) -> (u16, u16, u16) {
    let name_len = items
        .iter()
        .map(Data::name)
        .map(UnicodeWidthStr::width)
        .max()
        .unwrap_or(0);
    let address_len = items
        .iter()
        .map(Data::address)
        .flat_map(str::lines)
        .map(UnicodeWidthStr::width)
        .max()
        .unwrap_or(0);
    let email_len = items
        .iter()
        .map(Data::email)
        .map(UnicodeWidthStr::width)
        .max()
        .unwrap_or(0);

    #[allow(clippy::cast_possible_truncation)]
    (name_len as u16, address_len as u16, email_len as u16)
}

#[cfg(test)]
mod tests {
    use crate::Data;

    #[test]
    fn constraint_len_calculator() {
        let test_data = vec![
            Data {
                name: "Emirhan Tala".to_string(),
                address: "Cambridgelaan 6XX\n3584 XX Utrecht".to_string(),
                email: "tala.emirhan@gmail.com".to_string(),
            },
            Data {
                name: "thistextis26characterslong".to_string(),
                address: "this line is 31 characters long\nbottom line is 33 characters long"
                    .to_string(),
                email: "thisemailis40caharacterslong@ratatui.com".to_string(),
            },
        ];
        let (longest_name_len, longest_address_len, longest_email_len) =
            crate::constraint_len_calculator(&test_data);

        assert_eq!(26, longest_name_len);
        assert_eq!(33, longest_address_len);
        assert_eq!(40, longest_email_len);
    }
}
