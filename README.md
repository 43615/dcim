# dcim
### *dc improved: Feature-added rewrite of a 50+ year old RPN calculator/stack machine/programming language*
This readme is currently incomplete.

Features present in GNU dc are not listed unless different. [Familiarize yourself first.](https://linux.die.net/man/1/dc)

This is my first real Rust project, please expect low quality and report anything weird. Any suggestions are appreciated.

Currently missing planned features: manual rounding, different modes like file input, more conversion factors
## General changes and notes
- Default (interactive) mode now has a prompt indicator.
- Error messages are prefixed with `!`.
- When numbers are rounded, the fractional part is discarded (all rounding is towards zero).
- The amount of registers provided is now fixed to 65536, meaning that any character on Unicode's Basic Multilingual Plane can be used as a register name.
- The default value when saving or loading uninitialized array objects is the number 0.
- The `!` command for executing OS commands is deliberately not implemented.
## Number input changes
- Both the input and output bases are now in the range 2-36 (inclusive).
- Capital A-F are no longer used for number input in the normal way, higher-base numbers need to be escaped with `'`. This change frees A-F up as commands and allows for bases over 16.
- Example: `'_123orletters.ANYCASE789` (ends on any character that doesn't continue the number, space recommended).
- Digits too high for the input base (such as `'G` when base is 16) no longer default to the highest possible digit, but cause an error message instead.
## Precision and number output changes
- The precision parameter has been split into output precision (`k`/`K`) and working/mantissa precision (`m`/`M`).
- Output precision now applies correctly regardless of output base.
- If output precision is negative (default: -1), numbers are printed with enough precision to be exact.
- Working precision (default: 256) determines the mantissa size of all newly created numbers. 256 bits can store about 75 decimal digits accurately. For comparison: an IEEE 754 `double` has a 53-bit mantissa. Scale is unlimited within reason.
  - Tip: The amount of bits you need for a certain level of precision can be estimated using `<prec> <base> 2G*`. Always add a little more.
- Attention: M applies to the whole number, so large integers may be represented incorrectly.
## New: Parameter stack
- `{` switches to a new "parameter context" with defaults `_1k 10i 10o` while keeping the previous one.
- `}` restores the old context or resets the parameters to default if no older context exists.
- Working precision is unaffected.
## New mathematical commands and changes
- `^` no longer rounds the exponent.
- `V`: bth root of a.
- `g`: natural log of a.
- `G`: bth log of a.
- `uyt`, `UYT`: sin, cos, tan, asin, acos, atan. Arguments in radians.
- All modulo operators (`%`, `~`, `|`) now round their inputs.
- `N` pushes a random integer in range \[0;a).
## New stack/register manipulation commands
- `C` deletes the top a elements.
- `D` duplicates the top a elements.
- `Z`\<reg\> now pushes the depth of a register.
- `F`\<reg\> now prints an entire register.
- `R` rotates the top abs(a) elements: upward if a is positive, downward if negative.
  - For example: `1 2 3 4 3R` results in 1 4 2 3
## New/overloaded string manipulation commands
- `+` concatenates two strings.
- `-` removes abs(b) characters from string a: from the back if b is positive, from the front if negative.
- `*` repeats string a abs(b) times, flipping it if b is negative.
- `/` removes characters from string a such that abs(b) remain: from the back if b is positive, from the front if negative.
- `a` now uses the least significant 32 bits.
- `A` converts a number to a string in 32-bit blocks corresponding to characters, similarly to `P`(which parses bytes as UTF-8).
## Macro changes
- `q` now always exits regardless of where it's called from.
- `Q` may behave slightly differently, TODO: test.
## New: Library of constants and conversion factors
TODO: List all factors
- `"` pushes the constant or conversion factor with name a.
  - For example: `90[deg]"*` converts 90° to radians, `10 6^[in]"*[nmi]"/` converts 1 million inches to nautical miles.
- All constants and units are stored in amounts of their respective international standard units.
