use cli::App;
use color_eyre::Result;

mod analyzer;
mod catalog;
mod cli;
mod optimizer;
mod parser;

fn main() -> Result<()> {
    color_eyre::install()?;
    let terminal = ratatui::init();
    let app_result = App::new().run(terminal);
    ratatui::restore();
    app_result
}
