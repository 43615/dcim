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

# Building/installing
(Assuming complete and up-to-date Rust environment)

## In general
```shell
cargo install dcim
```
Platform-specific quirks:
- Due to the way Unix-likes handle terminal controls, navigation inputs (arrows, home, end...) are broken. Might fix it by adding a proper line editor at some point.
- On Windows, numbers with huge mantissae (W ⪆ 2³⁰) cause crashes for some arcane reason I can't control (within GNU MPFR). If you want to calculate something to a billion digits, use WSL. I will switch to a pure-Rust arbitrary-precision float library when a satisfactory one becomes available.

## Windows
### Prebuilt x86-64 exe
[Download here.](https://43615.xyz/sharing/AJsJSuDz8) Hosted on my NAS, not guaranteed to be available or up-to-date. I strongly recommend building it yourself if you can spare the trouble.

### Building
`gmp-mpfr-sys` requires a speical environment to link to C libraries, follow the instructions [here](https://docs.rs/gmp-mpfr-sys/latest/gmp_mpfr_sys/#building-on-windows). After building it in MinGW once, the artifacts should remain and new dc:im versions can be built natively until I update it to a new version of `gmp-mpfr-sys`.

## Android (Termux)
```shell
MAKEFLAGS="-i" cargo install dcim
```
The current version of GNU MPFR fails one inconsequential test (no idea why), use `-i` to pretend it doesn't happen.

If something else goes wrong, use clang:
```shell
export RUSTFLAGS=" -C link-arg=$(clang -print-libgcc-file-name)"
```

# Most important changes compared to GNU dc
- New commands for easier stack/register manipulation and macro execution.
- Numbers are binary floats with arbitrary precision (parameter `W`). The parameter `K` only applies to output.
- Strings have full Unicode support (stored in UTF-8).
- Improved and new arithmetic commands.
- Full suite of string manipulation commands, including a regex engine.
- The amount of available registers is unlimited.
- Number input/output bases are unlimited upwards, bases above 36 use a custom "any-base" notation.
- Expanded command line argument syntax, run `dcim -h` to learn more.
- Error messages are much more helpful and differentiated, and are always prefixed with `! `.
- Commands for multithreaded macro execution.
- A library of various named constants and unit conversion factors.

# Using dc:im as a library
[Crate documentation (autogenerated)](https://docs.rs/dcim/latest/dcim/)
- The main point of this is to connect the logic to a different set of IO streams.
- You could also use it to calculate something for your program, manually extracting the results from the `State` afterwards.