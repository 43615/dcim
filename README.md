```
╭─────────────────────────╮
│   ╷           •         │
│   │                     │
│ ╭─┤  ╭─╴  •  ╶┤   ┌─┬─╮ │
│ │ │  │        │   │ │ │ │
│ ╰─┘  ╰─╴  •  ╶┴╴  ╵   ╵ │
╰─────────────────────────╯
```
### *dc improved - Expanded rewrite of a classic RPN calculator / esoteric programming language*

This readme only mentions changes compared to GNU dc. If you're unfamiliar with its core principles, read its man page or [the Wikipedia article](https://en.wikipedia.org/wiki/dc_(computer_program)).

[**Complete documentation in the wiki**](https://github.com/43615/dcim/wiki)

## Building
(Assuming complete and up-to-date build environment)

### In general
```
cargo install dcim
```

### Windows
`gmp-mpfr-sys` requires some extra setup. Follow the instructions [here](https://crates.io/crates/gmp-mpfr-sys), under "Building on Windows". After building it in MinGW once, new dc:im versions can be built normally until I update it to a new version of `gmp-mpfr-sys`.

Note: Numbers with huge mantissae (W≥2³⁰) cause crashes for some arcane internal reason I can't control. If you want to calculate something to a billion digits, use WSL.

### Android (Termux)
- Pray to a deity of your choosing
- Sacrifice this envar and 3 lambs at 13:37 on a Sunday while looking north-northeast:
```
export RUSTFLAGS=" -C link-arg=$(clang -print-libgcc-file-name)"
```

## Most important changes compared to GNU dc
- Expanded shell argument syntax, run `dcim -h` to learn more.
- Default (interactive/shell) mode now has a prompt indicator.
- Error messages are much more helpful and differentiated, and are always prefixed with `! `.
- When type/semantic errors occur, all used objects are returned to the stack.
- Numbers are binary floats with user-changeable precision (`W`). The parameter `K` only applies to output.
- Strings have full Unicode support (stored in UTF-8).
- Several new arithmetic and string manipulation commands.
- Niladic (non-popping) printing commands print brackets around strings for clarity.
- Commands that need integers round numbers towards zero (discarding the fractional part).
- Number input/output bases are unlimited upwards, bases above 36 use a custom "any-base" notation.
- The amount of registers provided is now unlimited.
- Arbitrary registers can be selected with a "register pointer" command.
- Nonexistent register array objects are initialized with empty strings.
- A library of various named constants and unit conversion factors.

## Using dc:im in your code
- Create a state storage struct (`State::default()`).
  - *Non-trivial defaults can be customized with corresponding `.custom_*` methods.*
- Create a set of IO streams (`IOTriple`).
  - *The macro `stdio!()` creates a set that uses standard input, output and error.* 
- Execute commands with `exec`.
- Example:
```rust
use dcim::*;
let mut state = State::default()  //create state storage
    .custom_w(1_000_000_000);   //precision to 1 billion bits
let mut io = stdio!();    //will print to console
exec(&mut state, &mut io, true, "[pi]\"p").unwrap();  //calculate π, print
```