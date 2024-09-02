fn main() {
    println!("{}", parse())
}

fn parse() -> String {
    return "parsed".to_string();
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let parsed = parse();
        assert_eq!("parsed".to_string(), parsed)
    }
}
