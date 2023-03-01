# RISC-V Assembler

A RISC-V Assembler written in Rust for educational purposes.

# Tokenizer

Tokenizes a sequence of characters (String) using 3 states recognized using regular expressions:
1. TOKEN - for recognizing valid tokens
2. IGNORE - for recognizing "whitespace" that must be discarded
3. BLANK - for recognizing skips in the output token sequence (such as for new lines)

IGNORE chars are discarded from the output token sequence, while BLANKS are replaced by a configurable blank char.

General rules are:
1. TOKENS can appear consecutively, or with IGNORE chars and at most one BLANK in between them. IGNORE chars are discarded.
2. BLANKS cannot be the first char, and cannot appear consecutively, but can have IGNORE chars before and after it.

The only valid transitions are:
1. START -> TOKEN, IGNORE, BLANK
2. TOKEN -> TOKEN, IGNORE, BLANK
3. IGNORE -> TOKEN, BLANK
4. BLANK -> TOKEN, IGNORE

Regex for each category is customizable and defined at creation of the tokenizer. The regex must be able to handle all chars in the input to tokenize successfully.

For well-defined behavior, the ff guidelines should be followed:
1. Regex for TOKENS should not contain any chars from IGNORE or BLANK
2. Regex for IGNORE should not contain any chars from BLANK
3. Regex for BLANK can contain IGNORE chars but must begin with a unique BLANK char
4. To ensure no consecutive BLANKS are recognized, the regex can be defined to group consecutive BLANK chars into one

To do:
> Recognize surrounds (parentheses, quotes, etc.)
> Ignore comments


# Parser/Grammar

Parser creates an abstract syntax tree from a sequence of tokens (created by Tokenizer). The grammar rules can be customized prior to parsing by creating a custom Grammar instance. Currently only accepts LL grammars.

Grammar can be configured by creating NodeDefs of Non-Terminals and Terminals through new_nonterm or new_term, and then creating Productions using make_prod.