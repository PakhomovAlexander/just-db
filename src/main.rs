mod analyzer;
mod parser;

fn main() {
    print!("{}", 1);
    parser::lexer::Lexer::new("SELECT * FROM table;").for_each(|token| {
        println!("{:?}", token);
    });
}
