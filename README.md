# dc:im
### *dc improved: Feature-added rewrite of a 50+ year old RPN calculator/stack machine/programming language*

Features present in GNU dc are not listed here unless different. [Familiarize yourself first](https://linux.die.net/man/1/dc) or see the [***full reference manual***](../../wiki).

## General changes and notes
- Default (interactive) mode now has a prompt indicator.
- The file and expression modes now accept and execute any number of arguments. If the last expression or filename is `?`, it will enter interactive mode after finishing.
- File mode removes all #comments before executing.
- Error messages are (hopefully) more helpful and always prefixed with `!`.
- `P` is now like `n`, but with a newline. The conversion feature is moved to `A`.
- "Diagnostic" printing commands (all except `n` and `P`) now print brackets around strings for clarity.
- Commands that need integers always implicitly round their arguments. When rounding, the fractional part is discarded (rounding towards zero).
- The amount of registers provided is now fixed to 65536, meaning that any character on Unicode's Basic Multilingual Plane (0000-FFFF) can be used as a register name. This is the only arbitrary limit imposed on the user.
- When saving or loading uninitialized array objects, all previously nonexistent objects are initialized with the number 0. This fixes undefined behaviour like with `123d:ala`.
- Strings have full Unicode support (stored as UTF-8). The new string manipulation features index all strings by characters.
- The `!` command for executing OS commands is replaced with `\`, which pops and runs a string.
## Number input changes
- For normal input, the input base is now in range 2-36 (inclusive).
- Capital A-F are no longer used for number input in the normal way, base-11+ numbers now need to be escaped with `'`. This change frees up A-F to be used as commands and allows for bases over 16.
  - Example: `'_123orletters.ANYCASE789` (ends on any character that doesn't continue the number, space recommended).
- Digits too high for the input base (such as `'G` when base is 16) are not parsed, but always cause an error message instead.
- Exponential/scientific notation shorthand: `<a>@<b>` is equivalent to `<a>I{<b>}^*`, but is easier to input and might avoid rounding errors. '@' replaces the traditional 'e'/'E' to work with all input bases. The exponent must be a decimal integer and is applied to the input base. If no mantissa is provided, 1 is assumed (`1@123`=`@123`).
## Precision and number output changes
- Numbers are output the normal way if output base is in range 2-36 (inclusive).
- The precision parameter has been split into output precision (`k`/`K`) and working/mantissa precision (`w`/`W`).
- Output precision now applies correctly regardless of output base.
- If output precision is -1 (new default), numbers are printed with enough precision to be exact (reproducible by inputting what's printed).
- If it's not, up to K digits are printed after the point. Sufficiently small or large numbers are displayed in scientific notation like `-1.23456 @-789`. Like with SCI input, the exponent is always in decimal.
- Working precision (default: 256 bits) determines the mantissa size of all newly created numbers. 256 bits can store about 75 decimal digits accurately. For comparison: an IEEE 754 `double` has a 53-bit mantissa.
  - Tip: The amount of bits you need for a certain level of precision can be estimated using `<prec> <base> 2G*`. Always add a little more.
- Scale is limited to 2^±2³⁰ or ≈10^±323'228'496.
- Floating-point rounding artifacts (wrong digits at the end of a number) are guaranteed unless the number is a binary fraction or the input base is a power of 2. This is an unavoidable problem, GNU dc just hid it from view by storing numbers in the base they were created in.
- Attention: W applies to the whole number, so large integers may be represented incorrectly. For example, with W=4, 17 is stored as 16.
- W is limited to an unsigned 32-bit integer (4'294'967'295 bits). Actually going that high is definitely not recommended, but I'm not stopping you.
- `X` and `Z` don't make sense for binary floats. They are used for different commands.
## Any-base input and output
- The input and output bases are now unlimited. If they are over 36, an "any-base" format is used.
- Any-base numbers consist of parentheses containing individual digit values (in decimal) separated by spaces, with one optional negative sign anywhere and one optional fractional separator `.` between two digits, as well as an optional exponential part like with normal input (decimal integer).
  - Example: `(-123 456.789@-321)`
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
  - Mnemonics: J looks like a flipped L, H is next to it on QWERTY.
## New/overloaded string manipulation commands
- `+` concatenates two strings.
- `-` removes abs(b) characters from string a: from the back if b is positive, from the front if negative.
  - Example: `[abcde]_2-` results in "cde".
- `*` repeats string a abs(b) times, reversing it if b is negative.
- `/` removes characters from string a such that abs(b) remain: from the back if b is positive, from the front if negative.
  - Example: `[vwxyz]_2/` results in "yz".
- `~` splits a string at character b, pushing the left side first.
- `g` can also return the length of a string.
- `a` now converts the least significant 32 bits to a character or a character to its numerical value.
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
- `X` pops a string and a number and executes the string n times.
  - Examples: `[+]z2-X` sums the entire stack, `[Sa]z1-X` saves the entire stack to register 97, `[Lb]ZbX` loads the entirety of reg 98.
- `q` now always exits regardless of where it's called from.
  - If the [DRS](#Direct-register-number-selection) is set, its value is used as the exit code.
- `Q` may behave slightly differently, TODO: test.
- `&` pops a string and executes the file with that name as a macro script if it's accessible (like file mode). This enables easy usage of existing macros in interactive mode and splitting of scripts into multiple modular files. Because the script is executed in the same instance of dcim, it may overwrite register contents.
- `$` pops a string and pushes the environment variable with that name if it exists.
- `\` pops a string and executes it as one or more OS commands (separated by `;`). Features of your shell like aliases are not available. `[clear]\` is a useful thing to remember.
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
  - `5[m3 in3]"*` converts 5 cubic meters to cubic inches.
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
Boolean "Type" (true iff string) solely determines how an object is treated. The thereby "deactivated" field is never used.

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
| RegObj | RegObj | RegObj | ... S/L, theoretically unlimited
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
