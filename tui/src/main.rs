use clap::Parser;
use cli::Cli;
use color_eyre::Result;

use crate::app::App;

mod action;
mod app;
mod cli;
mod components;
mod config;
mod errors;
mod layout;
mod logging;
mod tui;

#[tokio::main]
async fn main() -> Result<()> {
    crate::errors::init()?;
    crate::logging::init()?;

    // let _ = miette::set_hook(Box::new(|_| {
    //     Box::new(
    //         miette::MietteHandlerOpts::new()
    //             .terminal_links(true)
    //             .unicode(false)
    //             .context_lines(10)
    //             .tab_width(2)
    //             .break_words(false)
    //             .build(),
    //     )
    // }));

    let args = Cli::parse();
    let mut app = App::new(args.tick_rate, args.frame_rate)?;
    app.run().await?;
    Ok(())
}
