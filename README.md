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

This readme only lists changes compared to GNU dc. If you're unfamiliar with it, read the [Wikipedia article about dc](https://en.wikipedia.org/wiki/dc_(computer_program)) or see the [***full reference manual***](../../wiki).
## Building
(Assuming complete and up-to-date environment)
### In general
```
cargo install --git https://github.com/43615/dcim
```
### Windows
`gmp-mpfr-sys` requires some extra setup. Follow the instructions [here](https://crates.io/crates/gmp-mpfr-sys), under "Building on Windows". After building it in MinGW once, new dc:im versions can be built normally until I update it to a new version of `gmp-mpfr-sys`.
## General changes and notes
- Default (interactive) mode now has a prompt indicator.
- The file and expression modes now accept and execute any number of arguments. If the last expression or filename is `?`, it will enter interactive mode after finishing.
- File mode removes all #comments before executing.
- Error messages are (hopefully) more helpful and always prefixed with `!`.
- When errors occur, all used objects are returned to the stack.
- `P` is now like `n`, but with a newline. The conversion feature is moved to `A`.
- "Diagnostic" printing commands (all except `n` and `P`) now print brackets around strings for clarity.
- Commands that need integers always implicitly round their arguments. When rounding, the fractional part is discarded (rounding towards zero).
- The amount of registers provided is now fixed to 65536, meaning that any character on Unicode's Basic Multilingual Plane (0000-FFFF) can be used as a register name. This is the only arbitrary limit imposed on the user.
- When saving or loading uninitialized array objects, all previously nonexistent objects are initialized with the number 0. This fixes undefined behaviour like with `123d:ala`.
- Strings have full Unicode support (stored as UTF-8). The new string manipulation features index strings by characters.
- The `!` pseudocommand for executing OS commands is replaced with `\`, which pops and runs a string.
## Number input changes
- For normal input, the input base is now in range 2-36 (inclusive).
- Capital A-F are no longer used for number input in the normal way, base-11+ numbers now need to be escaped with `'`. This change frees up A-F to be used as commands and allows for bases over 16.
  - Example: `'_123orletters.ANYCASE789` (ends on any character that doesn't continue the number, space recommended).
- Digits too high for the input base (such as `'G` when base is 16) are not parsed, but always cause an error message instead.
- Exponential/scientific notation shorthand: `<a>@<b>` is equivalent to `<a>I{<b>}^*`, but is easier to input and might avoid rounding artifacts. '@' replaces the traditional 'e'/'E' to work with all input bases. The exponent must be a decimal integer and is applied to the current input base. If no mantissa is provided, 1 is assumed (`1@123`=`@123`).
## Precision and number output changes
- Numbers are output the normal way if output base is in range 2-36 (inclusive).
- For consistency, negative numbers are printed with `_` instead of `-`.
- The precision parameter has been split into output precision (`k`/`K`) and working/mantissa precision (`w`/`W`).
- Output precision now applies correctly regardless of output base.
- If output precision is -1 (new default), numbers are printed with enough precision to be exact (reproducible by inputting what's printed).
- If it's ≥0, up to that many digits are printed after the point. Sufficiently small or large numbers are displayed in scientific notation like `_1.23456@_789`. Like with SCI input, the exponent is always in decimal.
- Working precision (default: 256 bits) determines the mantissa size of all newly created numbers. 256 bits can store about 75 decimal digits accurately. For comparison: an IEEE 754 `double` has a 54-bit mantissa.
  - Tip: The amount of bits you need for a certain level of precision can be estimated using `<prec> <base> 2G*`. Always add a little more.
- Scale is limited to 2^±2³⁰ or ≈10^±323'228'496.
- Floating-point rounding artifacts (wrong digits at the end of a number) are guaranteed unless the number is a binary fraction or the input base is a power of 2. This is an unavoidable problem with conversion between incompatible bases.
- Attention: W applies to the whole number, so large integers may be represented incorrectly. For example, with W=4, 17 is stored as 16.
- W is limited to an unsigned 32-bit integer (4'294'967'295 bits). Actually going that high is definitely not recommended, but I'm not stopping you.
- `X` and `Z` don't make sense for binary floats. They are used for different commands.
## Any-base input and output
- The input and output bases are now unlimited. If they are over 36, an "any-base" format is used.
- Any-base numbers consist of parentheses containing individual digit values (in decimal) separated by spaces, with one optional negative sign anywhere and one optional fractional separator `.` between two digits, as well as an optional exponential part like with normal input (decimal integer).
  - Example: `(-123 456.789@-4321)`
- Because the number is processed separately from commands, both `-` and `_` may be used as negative signs.
- "Empty" digits (like two spaces in a row) default to 0.
- Due to a limitation of the current base conversion algorithm, any-base output is limited in the amount of digits that can be printed and may create inaccurate digits at the end of fractional values. It is only guaranteed to be correct for integers of non-extreme size.
## New feature: Parameter stack
- `{` switches to a new "parameter context" with defaults `_1k 10i 10o` while keeping the previous one.
- `}` restores the previous context or resets the parameters to default if no previous context exists.
- Working precision is unaffected to avoid accidental changing of existing numbers' precision.
## New mathematical commands and changes
- `^` no longer rounds the exponent.
- `V`: bth root of a.
- `g`: natural log of a.
- `G`: bth log of a, effectively a shorthand for `<a>g <b>g /`.
- `°`: shorthand for converting an angle from degrees to radians, intuitive syntax. Not found on standard QWERTY, sorry.
- `uyt`, `UYT`: sin, cos, tan, asin, acos, atan. Arguments in radians.
  - Mnemonic/reason for placement: On QWERTY, this is one of two three-key runs not used by GNU dc. T matches "tangent", sin and cos are next to it to create the traditional three-key layout found on many calculators (albeit reversed).
- All modulo operators (`%`, `~`, `|`) now round their arguments.
- `N` pushes a random integer 0≤n<a.
## New stack/register manipulation commands
- `C` deletes the top a objects.
- `D` duplicates the top a objects.
- `Z`\<reg\> now pushes the depth of a register.
- `F`\<reg\> now prints an entire register including array contents with indices.
- `R` rotates the top abs(a) objects: upward if a is positive, downward if negative.
  - Example: `1 2 3 4 3R` results in 1 4 2 3.
- To enable copying of register arrays, a buffer for one complete register object has been added. Refer to the [memory model diagram](../../wiki#memory-model-diagram).
  - `j`\<reg\> and `J`\<reg\> are like `l` and `L`, but copy and pop to the buffer instead of the main stack.
  - `h`\<reg\> and `H`\<reg\> are like `s` and `S`, but overwrite and push reading from the buffer.
  - Example: `jaHa` duplicates the top RegObj of reg 97, `JaHb` moves the top RegObj of reg 97 to reg 98.
  - Mnemonics: J looks like a flipped L, H is next to it on QWERTY.
## New/overloaded string manipulation commands
- `+` concatenates two strings.
- `-` removes abs(b) characters from string a: from the back if b is positive, from the front if negative.
  - Example: `[abcde]_2-` results in "cde".
- `*` repeats string a abs(b) times, reversing it if b is negative.
- `/` removes characters from string a such that abs(b) remain: from the back if b is positive, from the front if negative.
  - Example: `[vwxyz]_2/` results in "yz".
- `%` isolates the bth character.
- `~` splits a string at character b, pushing the left side first.
- `^` finds b in a (index of first match, -1 if not found).
- `|` searches a for b and replaces with c.
- `g` can also return the length of a string.
- `a` now converts a number to a character or a character to its numerical value.
- `A` either converts a number to a UTF-8 string (like dc `P`, but without printing) or reverses the conversion.
- `"`, which is also used for the [library of constants](#library-of-constants-and-conversion-factors), converts a number to its string representation (like printing, but without printing). Useful for prettifying output (tabulation etc.).
## Direct register number selection
- `,` writes a number to a single-use direct register selector and marks it as valid.
- This selector can only be written to and expires (becomes invalid) at the next call of any register command.
- When it's valid, register commands access the register specified by the selector and don't process the next character as a register name.
- Example: `[test] sa 97,l p` (assuming input base is 10).
- If no register name is provided and the selector is marked as invalid, an error message is displayed.
- Advantages/use cases:
  - Enables scripts to select registers automatically without having to convert the number to a string first (`123,s` is shorter than `123a[s]r+x`).
  - Register numbers can be input in any base because the number is parsed like any other.
  - All register numbers in the allowed range can be used, not just valid/inputtable Unicode characters.
## Macro changes
- `X` pops a string and a number and executes the string that many times.
  - Examples: `[+]z2-X` sums the entire stack, `[Sa]z1-X` saves the entire stack to register 97, `[Lb]ZbX` loads the entirety of reg 98.
- `q` now always exits regardless of where it's called from.
  - If the [DRS](#Direct-register-number-selection) is set, its value is used as the exit code.
- `Q` may behave slightly differently, honestly CBA to even try comparing with GNU dc's source C.
- `&` pops a string and executes the file with that name as a macro script if it's accessible (like file mode). This enables easy usage of existing helper scripts while in interactive mode and splitting of scripts into multiple modular files. Because the script is executed in the same instance of dcim, it may overwrite register contents.
- `$` pops a string and pushes the environment variable with that name if it exists.
- `\` pops a string and executes it as one or more OS commands (separated by `;`). Very basic syntax support (worst shell ever): `cmd arg1 arg2...` for normal commands, `var=val` for setting environment variables.
## Library of constants and conversion factors
[List of all available constants](../../wiki/List-of-constants-and-unit-conversion-factors)
- `"` pushes the constant or conversion factor with name a.
  - Exception: Some names cause dc:im to exit in different ways.
- All constants and units are stored in amounts of their respective international standard units.
- Unit conversion doesn't happen automatically; multiplying with the created factor converts the selected unit to the standard unit, dividing by it does the opposite.
- The string may also be two names separated by a space. This is a shorthand for converting from one unit to another.
- Units may have a signed integer at the beginning, which works as a scale prefix (milli = -3, giga = 9 etc.).
- Units may also have an unsigned integer at the end, which is a shorthand for powers (area, volume).
- There are NO checks for whether the conversion makes sense, the provided values are just numbers.
- Examples:
  - `[phi]"` pushes the golden ratio.
  - `90[deg]"*` converts 90° to radians.
  - `80[lb]"/` converts 80 kilograms to pounds.
  - `3[6in nmi]"*` converts 3 million inches to nautical miles.
  - `5[in3 -2m3]"*` converts 5 cubic inches to cubic centimeters.
  - `69[w2 21tsp]"*` converts 69 square weeks to zetateaspoons. Nonsense is possible, be careful.
