#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Select,
    From,
    Where,
    Insert,
    Into,
    Values,
    Update,
    Set,
    Delete,
    Create,
    Table,
    Primary,
    Key,
    Foreign,
    References,
    Drop,
    Alter,
    Add,
    Column,
    Constraint,
    Index,
    Join,
    Inner,
    Left,
    Right,
    Full,
    Outer,
    On,
    Group,
    By,
    Order,
    Asc,
    Desc,
    Union,
    All,
    Distinct,
    Limit,
    Offset,
    Having,
    As,
    And,
    Or,
    Not,
    Null,
    Is,
    In,
    Between,
    Like,
    Exists,
    Any,
    Case,
    When,
    Then,
    Else,
    End,
    Default,

    Int,
    Integer,
    SmallInt,
    TinyInt,
    BigInt,
    Float,
    Real,
    Double,
    Decimal,
    Numeric,
    VarChar,
    Char,
    Text,
    Date,
    DateTime,
    Time,
    Timestamp,
    Boolean,

    Asterisk,
    Comma,
    Semicolon,
    OpenParen,
    CloseParen,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,
    Plus,
    Minus,
    Slash,
    Percent,
    Concat,
    SingleQuote,
    DoubleQuote,

    Identifier {
        first_name: &'a str,
        second_name: Option<&'a str>,
        third_name: Option<&'a str>,
    },

    StringLiteral(String),
    NumericLiteral(String),
    BooleanLiteral(bool),

    SingleLineComment(String),
    MultiLineComment(String),
}

impl<'a> Token<'a> {
    pub fn identifier(first_name: &'a str) -> Token<'a> {
        Token::Identifier {
            first_name,
            second_name: None,
            third_name: None,
        }
    }
}
