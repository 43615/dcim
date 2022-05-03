# dcim
### *dc improved: Feature-added rewrite of a 50+ year old RPN calculator/stack machine/programming language*

Features present in GNU dc are not listed here unless different. [Familiarize yourself first](https://linux.die.net/man/1/dc) or see the [full reference manual](../../wiki/Home).

Planned upcoming features/changes:
- More conversion factors
## General changes and notes
- Default (interactive) mode now has a prompt indicator.
- The file and expression modes now accept and execute any number of arguments. If the last expression or filename is `?`, it will enter interactive mode after finishing.
- File mode removes all #comments before executing.
- Error messages are (hopefully) more helpful and always prefixed with `!`.
- Commands that need integers always explicitly round their arguments. When rounding, the fractional part is discarded (rounding towards zero).
- The amount of registers provided is now fixed to 65536, meaning that any character on Unicode's Basic Multilingual Plane can be used as a register name.
- The default value when saving or loading uninitialized array objects is the number 0. This fixes the issue with `123d:ala`.
- The `!` command for executing OS commands is deliberately not implemented.
## Number input changes
- Both the input and output bases are now in the range 2-36 (inclusive).
- Capital A-F are no longer used for number input in the normal way, base-11+ numbers now need to be escaped with `'`. This change frees up A-F to be used as commands and allows for bases over 16.
  - Example: `'_123orletters.ANYCASE789` (ends on any character that doesn't continue the number, space recommended).
- Digits too high for the input base (such as `'G` when base is 16) are not parsed, but always cause an error message instead.
- Exponential/scientific notation shorthand: `<a>@<b>` is equivalent to `<a>I{<b>}^*`, but is easier to input and might avoid rounding errors. '@' replaces the traditional 'e'/'E' to work with all input bases. The exponent must be a decimal integer and is applied to the input base. If no mantissa is provided, 1 is assumed (`1@123`=`@123`).
## Precision and number output changes
- The precision parameter has been split into output precision (`k`/`K`) and working/mantissa precision (`w`/`W`).
- Output precision now applies correctly regardless of output base.
- If output precision is negative (new default: -1), numbers are printed with enough precision to be exact (reproducible by inputting what's printed).
- If it's not, up to K digits are printed after the point. Sufficiently small or large numbers are displayed in scientific notation like `-1.23456 @-789`. Like with SCI input, the exponent is always in decimal.
- Working precision (default: 256 bits) determines the mantissa size of all newly created numbers. 256 bits can store about 75 decimal digits accurately. For comparison: an IEEE 754 `double` has a 53-bit mantissa. Scale is unlimited within reason (signed 32-bit, up to ≈10^(±646'456'993)).
  - Tip: The amount of bits you need for a certain level of precision can be estimated using `<prec> <base> 2G*`. Always add a little more.
- Floating-point rounding artifacts (wrong digits at the end of a number) are guaranteed unless the number is a binary fraction or the output base is a power of 2. This is an unavoidable problem, GNU dc just hid it from view by storing numbers in the base they were created in.
- Attention: W applies to the whole number, so large integers may be represented incorrectly. The default corresponds to a generous "signed 257".
- W is limited to an unsigned 32-bit integer (4'294'967'295 bits). Actually going that high is definitely not recommended, but I'm not stopping you.
- `X` doesn't make sense for binary floats, so it's used for "[auto-macros](#macro-changes)".
## New feature: Parameter stack
- `{` switches to a new "parameter context" with defaults `_1k 10i 10o` while keeping the previous one.
- `}` restores the previous context or resets the parameters to default if no previous context exists.
- Working precision is unaffected.
## New mathematical commands and changes
- `^` no longer rounds the exponent.
- `V`: bth root of a.
- `g`: natural log of a.
- `G`: bth log of a, effectively a shorthand for `<a>g <b>g /`.
- `uyt`, `UYT`: sin, cos, tan, asin, acos, atan. Arguments in radians.
- All modulo operators (`%`, `~`, `|`) now round their arguments.
- `N` pushes a random integer in range \[0;a).
## New stack/register manipulation commands
- `C` deletes the top a elements.
- `D` duplicates the top a elements.
- `Z`\<reg\> now pushes the depth of a register.
- `F`\<reg\> now prints an entire register including array contents.
- `R` rotates the top abs(a) elements: upward if a is positive, downward if negative.
  - Example: `1 2 3 4 3R` results in 1 4 2 3.
- To enable copying of register arrays, a buffer for one complete register object has been added. Refer to the memory diagram below.
  - `j`\<reg\> and `J`\<reg\> are like `l` and `L`, but copy and pop to the buffer instead of the main stack.
  - `h`\<reg\> and `H`\<reg\> are like `s` and `S`, but overwrite and push reading from the buffer.
  - Example: `jaHa` duplicates the top object of reg 97, `JaHb` moves the top object of reg 97 to reg 98.
  - Yes, I'm running out of letters.
## New/overloaded string manipulation commands
- `+` concatenates two strings.
- `-` removes abs(b) characters from string a: from the back if b is positive, from the front if negative.
  - Example: `[abcde]_2-` results in "cde".
- `*` repeats string a abs(b) times, reversing it if b is negative.
- `/` removes characters from string a such that abs(b) remain: from the back if b is positive, from the front if negative.
  - Example: `[vwxyz]_2/` results in "yz".
- `a` now converts the least significant 32 bits to a character or a character to its numerical value.
- `P` now parses the number as a UTF-8 byte sequence.
- `A` is like `P`, but pushes the string to the stack instead of printing it.
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
- `X` pops a string and a number and executes the string n times.
  - Examples: `[+]z2-X` sums the entire stack, `[Sa]z1-X` saves the entire stack to register 97, `[Lb]ZbX` loads the entirety of reg 98.
- `q` now always exits regardless of where it's called from.
- `Q` may behave slightly differently, TODO: test.
## New feature: Library of constants and conversion factors
[List of all available constants](../../wiki/List-of-constants-and-unit-conversion-factors)
- `"` pushes the constant or conversion factor with name a.
- The name may also be two names separated by a space. This is a shorthand for converting from one unit to another.
- All constants and units are stored in amounts of their respective international standard units.
- Unit conversion doesn't happen automatically; multiplying with the created factor converts the selected unit to the standard unit, dividing by it does the opposite.
- Examples:
  - `[phi]"` pushes the golden ratio.
  - `90[deg]"*` converts 90° to radians.
  - `80[lb]"/` converts 80 kilograms to pounds.
  - `@6[in nmi]"*` converts 1 million inches to nautical miles.
# Memory model diagram
dc's manpage doesn't do a great job at explaining it, so here's a diagram:
```
Basic object "Obj":
+--------+
| String |
+--------+
| Number |
+--------+
|  Type  |
+--------+
Boolean "Type" (true iff string) solely determines how an object is treated. The thereby "activated" field must not be empty, the other one is never used.

Main stack:
+-----+-----+-----+----
| Obj | Obj | Obj | ... theoretically unlimited
+-----+-----+-----+----

Register object "RegObj":
+-----+
| Obj | principal object (s/l)
+-----+-----+-----+----
| Obj | Obj | Obj | ... array of objects (:/;), theoretically unlimited
+-----+-----+-----+----
Each RegObj has its own array.
Note that arrays are continuous (all lower indices are always valid to read from).
Writing to or reading from an uninitialized array element initializes all previously nonexistent Objs with the number 0.

Register:
+--------+--------+--------+----
| RegObj | RegObj | RegObj | ... theoretically unlimited
+--------+--------+--------+----
RegObj buffer:
+--------+
| RegObj | initialized with number 0 and empty array
+--------+
s and l overwrite and copy the top RegObj's principal Obj, S and L push and pop the whole RegObj (which wastes the array).
There is also a buffer for one RegObj, which is written to by `j`/`J` and read from by `h`/`H` (preserving the array).

Array of all registers:
+----------+----------+----------+-----+----------+
| Register | Register | Register | ... | Register | size fixed to 65536
+----------+----------+----------+-----+----------+
```
