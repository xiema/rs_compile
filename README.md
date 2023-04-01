# Rscompile

General compiler tools written in Rust for educational purposes.

# Components:
- [x] Tokenizer
- [x] Parser
- [ ] Analyzer
- [ ] Generator

# Tokenizer

Splits an input string into tokens, with each token type defined by its own regular expression. Characters or character sequences that should be ignored (e.g. whitespaces) are also defined by a regular expression.


    let seq = "1, 2, 3\n101,102, 103"

    // separate into number and comma tokens, ignoring whitespace
    let tokenizer = Tokenizer::new(
        // Tokens
        vec![
            // number tokens
            TokenPattern::Single("[[:digit:]]+"),
            // comma tokens
            TokenPattern::Single(","),
        ],
        // Ignore
        TokenPattern::Single([[:space:]]),
        // Optional Preprocessor
        None
    );

    let tokens = tokenizer.tokenize(seq).unwrap();
    // 1  ,  2  ,  3  101  ,  102  ,  103


An optional preprocessor function &str -> String can be passed that is called on the input string prior to the tokenization.


    let tokenizer = Tokenizer::new(
        ...
        // Optional Preprocessor
        // Trim whitespace
        Some(|s| String::from(s.trim()))
    );


Outputs `Vec<Token>`. The `Token` struct contains the original token string slice and a `TokenTypeId` representing the Token type. The `TokenTypeId` corresponds to the index of the _TOKEN_ pattern given to the `Tokenizer` constructor. The `Token` struct also contains text position information for error messages.

`TokenPattern`s may be `Single` or `Surround`:
1. `Single`: captures the whole pattern at once
2. `Surround`: finds the begin and end pattern, and capturing them and everything in between, with optional escape characters

<!-- list end -->

    let tokenizer = Tokenizer::new(
        // Tokens
        vec![
            // Numbers with optional decimal point
            TokenPattern::Single("[[:digit:]]+(\.[[:digit:]]+)?"),

            // Double-quoted string w/ forward-slash escape char
            TokenPattern::Surround(
                // Begin pattern, End pattern, Escape pattern
                "\"", "\"", "\\\\"),
            ...
        
        
Patterns are tried consecutively in the order given during construction until the first match. The Ignore pattern is always tried last.

`Token` types are uniquely identified by `TokenTypeId`s, which are then used by the `Parser` in parsing the token sequence. An _EOF_ Token with a unique `TokenTypeId = -1` is automatically added to the end of the output `Token` sequence.

# Parser

Table-driven, deterministic `Parser` that creates a parse tree from a sequence of `Token`s. The grammar rules are configured prior to parsing by adding Non-Terminals, Terminals, and Production Rules.


Generate the grammar:

    let gram_gen = GrammarGenerator::new();

    // Create Non-Terminals
    gram_gen.new_nonterm("Expression");
    gram_gen.new_nonterm("Term");

    // Create Terminals (with associated TokenTypeId)
    gram_gen.new_term("NumberLiteral", 0 as TokenTypeId);
    gram_gen.new_term("Identifier", 1 as TokenTypeId);
    gram_gen.new_term("Operator", 2 as TokenTypeId);
    gram_gen.new_term("EOF", -1 as TokenTypeId)

    // Create Production Rules
    // Expression -> Term Operator Term
    gram_gen.make_prod("Expression", vec["Term", "Operator", "Term"]);
    // Term -> NumberLiteral
    gram_gen.make_prod("Term", vec["NumberLiteral"]);
    // Term -> Identifier
    gram_gen.make_prod("Term", vec["Identifier"]);

    let grammar = gram_gen.generate();


Create a parser and parse the `Token` sequence:

    let parser = ParserLR::new(&grammar);
    let nodes = parser.parse(&tokens).unwrap();


The output is a vector of `Node`s of the resulting parse tree. Each `Node` has an `ElementType` (_Grammar Element_), as defined in the grammar.

Can handle _LL(k)_ grammars and _LR(1)_ grammars.

`GrammarGenerator` can automatically convert a _Non-LL_ `Grammar` into an _LL_ `Grammar` using left-recursion elimination and left-factoring.

Note: The parser needs all input tokens to be defined in the grammar, so if using the token sequence created by Tokenizer, make sure to define the `EOF` token in the Grammar definition.

# GrammarReader

Instead of manually configuring a `Tokenizer` and `GrammarGenerator`, `GrammarReader` can be used to automatically create `Grammar`s from a grammar description in a text file using a metagrammar.

Example grammar file content:

    Program -> StatementList $$
    StatementList -> StatementList Statement ";" | Statement ";"
    Statement -> Assignment | FunctionCall
    Assignment -> Identifier "=" Expression
    Expression -> Expression Operator Term | Term
    Term -> Number | Identifier
    FunctionCall -> Identifier "\\(" FunctionArgs "\\)"
    FunctionArgs -> FunctionArgs "," Identifier | Identifier

    Identifier ~= "[[:word:]]+"
    Number ~= "[[:digit:]]+"
    Operator ~= "[-+/*]"

Metagrammar notes:
- Literal symbols have to be enclosed in double quotes
- Currently, special Regex characters in literal symbols also need to be "double-escaped" so that correct Regexes can be made from them
- Non-literal symbols become either Non-terminals or Terminals
- Each Non-terminal needs to have at least 1 production
- Terminals need to define a regex pattern for recognizing the token they consume. The pattern should be enclosed in double quotes and have special Regex chars doubly escaped (like Literal symbols).
- Comments can be inserted using `//`

Special symbols of the metagrammar:
- `->` - production symbol
- `~=` - pattern symbol (for terminals)
- `|` - production OR
- `$$` - END-OF-FILE token of the resulting grammar

Approximation of the metagrammar used by GrammarReader, defined by itself:

    Grammar -> RuleList $$
    RuleList -> RuleList Rule | RuleList Comment | Rule | Comment
    Rule -> ProductionRule | PatternRule
    ProductionRule -> Symbol "->" RHSList EndRule
    PatternRule -> Symbol "~=" LiteralSymbol EndRule
    RHSList -> RHS "|" RHSList | RHS
    RHS -> RHS Symbol | Symbol
    Symbol -> Identifier | LiteralSymbol | "$$"

    ProductionSymbol ~= "->"
    PatternSymbol ~= "~="
    EndRule ~= "\n+[[:space:]]*"
    Identifier ~= "[[:alnum:]]+"

    // Comment ~= any text after '//' and before the first line break
    // LiteralSymbol ~= any text between double quotes, with '\' as an escape char

# Internals

## ParserLL

A top-down parser for _LL(k)_ grammars (_Left-to-right, leftmost derivation_). Compatible with _LL(k)_ grammars of any _k_ as long as the grammar is unambiguous and the lookahead does not need to go through a recursion. For example, the following grammar is incompatible:

    S -> Ay | Bz
    A -> xA | eps
    B -> xB | eps

The parser cannot tell whether to reduce the prefix sequence of `x`s to `A`s or `B`s until it reaches the `y` or `z` at the end of the whole sequence. `ParserLL` can deal with arbitrary _k_, but it needs to pre-compute the whole transition table during construction, and currently automatically computes the required lookahead from the grammar rules. In the example above, this lookahead value cannot be computed so it is incompatible with `ParserLL`, even though the grammar is obviously unambiguous.

**Example LL(2) grammar (with common prefix)**

    Program -> Expression EOF
    Expression -> Term Operator Expression
    Expression -> Term
    Term -> [0-9]+
    Operator -> [-+/*]

**Example LL(1) grammar (eliminated common prefixes from above)**

    Program -> Expression EOF
    Expression -> Term Expression_Tail
    Expression_Tail -> Operator Term Expression_Tail
    Expression_Tail -> eps
    Term -> [0-9]+
    Operator -> [-+/*]

## ParserLR

A bottom-up _LALR(1)_ (_Look Ahead, left-to-right, rightmost derivation_) Parser that can parse unambiguous _LR(1)_ grammars. Uses a parse table to select the proper action based on the current state and the next token. Internally, the possible actions are `Shift`, `Reduce` and `ShiftReduce`.

Currently only support 1 token of lookahead without recursion. The following grammar is unambiguous but incompatible:

    S -> Ay | Bz
    A -> Ax | x
    B -> Bx | x

Similar to the example for `ParserLL`, in this example, `ParserLR` cannot tell whether to reduce the prefix sequence of `x`s to `A`s or `B`s until it reaches the `y` or `z` at the end of the whole sequence. `ParserLR` cannot parse this grammar even though the grammar is obviously unambiguous. Remobing the recursion however will make the grammar compatible:

    S -> Ay | Bz
    A -> x
    B -> x

No recursion does not guarantee compatibility, however. For example, the following grammar is incompatible:

    S -> Awy | Bwz
    A -> x
    B -> x

This grammar is requires more than 1 lookahead token in order to decide whether or not the prefix `x` should be reduced to an `A` or a `B`.

**Example LR(1) grammar**

    Program -> Expression EOF
    Expression -> Expression Operator Term
    Expression -> Term
    Term -> [0-9]+
    Operator -> [-+/*]


# To Do:
- Configurable disambiguation
- More descriptive errors
- Implied EOF token
- Convert Parse tree to Abstract Syntax Tree
- Move Kleene Conversion into GrammarGenerator