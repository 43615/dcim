use rug::{Integer, integer::Order, Complete, Float, float::{Round, Constant, Special}, ops::Pow, rand::RandState};
use std::io::{stdin, stdout, Write};
use std::time::{SystemTime, Duration};
use std::cmp::Ordering;

const HELPMSG: &str = "
╭─────────────────────────╮
│   ╷           •         │
│   │                     │
│ ╭─┤  ╭─╴  •  ╶┤   ┌─┬─╮ │
│ │ │  │        │   │ │ │ │
│ ╰─┘  ╰─╴  •  ╶┴╴  ╵   ╵ │
╰─────────────────────────╯

dc improved - Feature-added rewrite of an RPN calculator/stack machine language from 1970-72
Most basic GNU dc features are unaltered, full documentation at https://github.com/43615/dcim

Options and syntax:

<nothing> | --inter | -i
	Interactive mode, standard prompt loop.

(--expr | -e) expr1 expr2 expr3 ... [?]
	Expression mode, executes expressions in order. If the last argument is '?', enters interactive mode after expressions are done.

[(--file | -f)] file1 file2 file3 ... [?]
	File mode, executes contents of files in order. '?' behaves the same as with -e.

--help | -h
	Print this help message.
";

//environment parameter defaults and storage, initialized in main
fn kdef() -> Integer { Integer::from(-1) }
fn idef() -> Integer { Integer::from(10) }
fn odef() -> Integer { Integer::from(10) }
static mut ENVSTK: Vec<(Integer, Integer, Integer)> = Vec::new();	//stores (k,i,o) tuples, used by '{' and '}'
static mut WPREC: u32 = 256;	//working precision (rug Float mantissa length)

//basic object on a dc stack, need to differentiate between numbers and strings
#[derive(Clone)]
struct Obj {
	t: bool,	//type, true iff string
	n: Float,	//number
	s: String,	//string
}

//register object, may have a dynamic array
#[derive(Clone)]
struct RegObj {
	o: Obj,			//principal object
	a: Vec<Obj>,	//associated array
}

//shorthand constructors
impl Obj {
	fn n(number: Float) -> Self {	//number variant
		Obj {
			t: false,
			n: number,
			s: String::new()
		}
	}
	fn s(string: String) -> Self {	//string variant
		Obj {
			t: true,
			n: flt_def(),
			s: string
		}
	}
}

static mut MSTK: Vec<Obj> = Vec::new();	//main stack

const REG_DEF: Vec<RegObj> = Vec::new();
static mut REGS: [Vec<RegObj>; 65536] = [REG_DEF; 65536];	//array of registers
static mut RO_BUF: Vec<RegObj> = Vec::new();	//vec because constant constructors are impossible, initialized in main

static mut DRS: Option<usize> = None;	//direct register selector

static mut RNG: Vec<RandState> = Vec::new();	//same problem as with RO_BUF

const INT_ORD_DEF: (Integer, Ordering) = (Integer::ZERO, Ordering::Equal);	//default tuple for to_integer_round().unwrap_or()
fn flt_def() -> Float {Float::new(1)}	//default Float value for unused Obj.n

fn main() {
	let mut args: Vec<String> = std::env::args().collect();
	args.remove(0);	//remove name of executable

	//init everything that doesn't have a const constructor
	unsafe {
		ENVSTK.push((kdef(), idef(), odef()));	//initialize env params
		RO_BUF.push(RegObj{	//and RegObj buffer
			a: Vec::new(),
			o: Obj::n(Float::with_val(WPREC, 0))
		});
		//initialize RNG with system time (* PID for a bit less predictability)
		RNG.push(RandState::new());
		RNG[0].seed(&(Integer::from(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or(Duration::MAX).as_nanos()) * std::process::id()));
	}

	if args.is_empty() {
		interactive_mode();	//default to interactive
	}
	else {
		let mode = &args[0];	//first arg, usually mode
		match mode.as_str() {
			"--inter"|"-i" => {
				//ability to force interactive mode just in case
				interactive_mode();
			},
			"--expr"|"-e" => {
				args.remove(0);
				expression_mode(args);
			},
			"--file"|"-f" => {
				args.remove(0);
				file_mode(args);
			},
			"--help"|"-h" => {
				println!("{}", HELPMSG);
			},
			_ => {
				if mode.starts_with('-'){
					eprintln!("! Invalid option \"{}\", use -h for option syntax help", mode);
				}
				else {	//assume filenames
					file_mode(args);
				}
			},
		}
	}
}

//interactive/shell mode, the default
fn interactive_mode() {
	//prompt loop
	loop {
		//prompt for user input
		print!("> ");
		stdout().flush().unwrap();
		let mut input = String::new();
		match stdin().read_line(&mut input) {
			Ok(_) => {},
			Err(error) => {
				eprintln!("! Unable to read standard input: {}", error);
				break;
			}
		}
		if input.is_empty() {
			print!("\r");
			std::process::exit(0);	//stop on end of pipe input
		}
		input = input.trim_end_matches(|c: char| c=='\n'||c=='\r').to_string();	//remove trailing LF

		unsafe {
			exec(input);
		}
	}
}

fn expression_mode(exprs: Vec<String>) {
	if !exprs.is_empty() {
		for i in 0..exprs.len() {
			if i==exprs.len()-1&&exprs[i]=="?" {
				interactive_mode();	//if last expression is "?", enter prompt loop
			}
			else {
				unsafe {
					exec(exprs[i].clone());
				}
			}
		}
	}
}

fn file_mode(files: Vec<String>) {
	if files.is_empty() {
		eprintln!("! No file name provided");
	}
	else {
		for i in 0..files.len() {
			if i==files.len()-1&&files[i]=="?"{
				interactive_mode();	//if last filename is "?", enter prompt loop
			}
			else {
				match std::fs::read_to_string(files[i].clone()) {
					Ok(script) => {
						let mut script_nc = String::new();	//script with comments removed
						for line in script.split('\n') {
							script_nc.push_str(line.split_once('#').unwrap_or((line,"")).0);	//remove comment on every line
							script_nc.push('\n');
						}
						unsafe {
							exec(script_nc);
						}
								
					},
					Err(error) => {
						eprintln!("! Unable to read file \"{}\": {}", files[i], error);
					},
				}
			}
		}
	}
}

//checks if n arguments are sufficient for a command (defines adicity)
//not used by niladics
fn check_n(op: char, n: usize) -> bool {
	if match op {
		//triadic
		'|' => n>=3,

		//dyadic
		'+'|'-'|'*'|'/'|'^'|'V'|'G'|'%'|'~'|'@'|':'|'='|'<'|'>'|'X' => n>=2,

		//monadic unless specified
		_ => n>=1,
	}
	{ true }
	else {
		eprintln!("! Insufficient arguments for command '{}'", op);
		false
	}
}

//checks if a command can be used on provided argument types
//a-c: types (.t) of the operands that would be used (in canonical order), use false if not required
fn check_t(op: char, a: bool, b: bool, c: bool) -> bool {
	if match op {
		//concat, find
		'+'|'^' => a==b,

		//replace
		'|' => a==b && b==c,

		//string manipulation, store into array
		'-'|'*'|'/'|'%'|'~'|':' => !b,

		//read file by name, get env variable, execute os command
		'&'|'$'|'\\' => a,

		//convert both ways, constant lookup or print to string, execute macros, log or string length
		'a'|'A'|'"'|'x'|'g' => true,

		//auto-macro
		'X' => a&&!b,

		//all other ops can only have numbers
		_ => !a&&!b&&!c,
	}
	{ true }
	else {
		eprintln!("! Invalid argument type(s) for command '{}'", op);
		false
	}
}

//scientific notation to Some(Float), for brevity
fn sci_to_flt(prec: u32, man: i128, exp: i128) -> Option<Float> {
	Some(Float::with_val(prec, man)*Float::with_val(prec, exp).exp10())
}

//library of constants and unit conversion factors
//unless specified, unit factors are based on the most prevalent international standard units for their respective quantities
//ex: "in" (inch) returns 0.0254, thus executing 20[in]"* converts 20 inches to meters (0.508)
fn constants(prec: u32, key: String) -> Option<Float> {
	match key.as_str() {
		/*----------------------------
			MATHEMATICAL CONSTANTS
		----------------------------*/
		"e" => {Some(Float::with_val(prec, 1).exp())}
		"pi" => {Some(Float::with_val(prec, Constant::Pi))}
		"gamma" => {Some(Float::with_val(prec, Constant::Euler))}
		"phi" => {Some((Float::with_val(prec, 5).sqrt()+1)/2)}
		"deg"|"°" => {Some(Float::with_val(prec, Constant::Pi)/180)}
		"gon"|"grad" => {Some(Float::with_val(prec, Constant::Pi)/200)}
		/*------------------------
			PHYSICAL CONSTANTS
		------------------------*/
		"c" => {Some(Float::with_val(prec, 299792458))}
		"hbar" => {Some(sci_to_flt(prec, 662607015, -42).unwrap()/(2*Float::with_val(prec, Constant::Pi)))}
		"G" => {sci_to_flt(prec, 6674, -3)}
		"qe" => {sci_to_flt(prec, 1602176634, -28)}
		"NA" => {sci_to_flt(prec, 602214076, 31)}
		"kB" => {sci_to_flt(prec, 1380649, -29)}
		"u" => {sci_to_flt(prec, 1660539066, -36)}
		"lp" => {sci_to_flt(prec, 16162, -39)}
		"tp" => {sci_to_flt(prec, 5391, -47)}
		"mp" => {sci_to_flt(prec, 21764, -12)}
		"Tp" => {sci_to_flt(prec, 14167, 28)}
		/*------------------
			LENGTH UNITS
		------------------*/
		"in" => {sci_to_flt(prec, 254, -4)}
		"ft" => {Some(constants(prec, "in".to_string()).unwrap()*12)}
		"yd" => {Some(constants(prec, "ft".to_string()).unwrap()*3)}
		"m" => {Some(Float::with_val(prec, 1))}
		"fur" => {Some(constants(prec, "ft".to_string()).unwrap()*660)}
		"mi" => {Some(constants(prec, "ft".to_string()).unwrap()*5280)}
		"nmi" => {Some(Float::with_val(prec, 1852))}
		"AU" => {Some(Float::with_val(prec, 149597870700i64))}
		"ly" => {Some(Float::with_val(prec, 9460730472580800i64))}
		"pc" => {Some(Float::with_val(prec, 96939420213600000i64)/Float::with_val(prec, Constant::Pi))}
		/*-------------------------------
			   AREA & VOLUME UNITS
			with no length equivalent
		-------------------------------*/
		"ac"|"acre" => {sci_to_flt(prec, 40468564224, -7)}
		"l" => {Some(Float::with_val(prec, 10).pow(-3))}
		"ifloz" => {sci_to_flt(prec, 284130625, -13)}
		"ipt" => {Some(constants(prec, "ifloz".to_string()).unwrap()*20)}
		"iqt" => {Some(constants(prec, "ifloz".to_string()).unwrap()*40)}
		"igal" => {Some(constants(prec, "ifloz".to_string()).unwrap()*160)}
		"ibu"|"ibsh" => {Some(constants(prec, "ifloz".to_string()).unwrap()*1280)}
		"ufldr" => {sci_to_flt(prec, 36966911953125, -19)}
		"tsp" => {Some(constants(prec, "ufldr".to_string()).unwrap()/3*4)}
		"tbsp" => {Some(constants(prec, "ufldr".to_string()).unwrap()*4)}
		"ufloz" => {Some(constants(prec, "ufldr".to_string()).unwrap()*8)}
		"upt" => {Some(constants(prec, "ufloz".to_string()).unwrap()*16)}
		"uqt" => {Some(constants(prec, "ufloz".to_string()).unwrap()*32)}
		"ugal" => {Some(constants(prec, "ufloz".to_string()).unwrap()*128)}
		"bbl" => {Some(constants(prec, "ugal".to_string()).unwrap()*42)}
		"udpt" => {sci_to_flt(prec, 5506104713575, -16)}
		"udqt" => {Some(constants(prec, "udpt".to_string()).unwrap()*2)}
		"udgal" => {Some(constants(prec, "udpt".to_string()).unwrap()*8)}
		"ubu"|"ubsh" => {Some(constants(prec, "udpt".to_string()).unwrap()*64)}
		"dbbl" => {sci_to_flt(prec, 115627123584, -12)}
		/*----------------
			MASS UNITS
		----------------*/
		"ct" => {sci_to_flt(prec, 2, -4)}
		"oz" => {sci_to_flt(prec, 28349523125, -12)}
		"lb" => {Some(constants(prec, "oz".to_string()).unwrap()*16)}
		"kg" => {Some(Float::with_val(prec, 1))}
		"st" => {Some(constants(prec, "lb".to_string()).unwrap()*14)}
		"t" => {Some(constants(prec, "lb".to_string()).unwrap()*2240)}
		/*----------------
			TIME UNITS
		----------------*/
		"s" => {Some(Float::with_val(prec, 1))}
		"min" => {Some(Float::with_val(prec, 60))}
		"h" => {Some(constants(prec, "min".to_string()).unwrap()*60)}
		"d" => {Some(constants(prec, "h".to_string()).unwrap()*24)}
		"w" => {Some(constants(prec, "d".to_string()).unwrap()*7)}
		/*-----------------
			OTHER UNITS
		-----------------*/
		"J" => {Some(Float::with_val(prec, 1))}
		"cal" => {sci_to_flt(prec, 4184, -3)}
		"Pa" => {Some(Float::with_val(prec, 1))}
		"atm" => {Some(Float::with_val(prec, 101325))}
		"psi" => {sci_to_flt(prec, 6894757293168, -9)}
		"torr" => {Some(constants(prec, "atm".to_string()).unwrap()/760)}
		/*------------------------------
			SPECIAL VALUES/FUNCTIONS
		------------------------------*/
		"inf" => {Some(Float::with_val(prec, Special::Infinity))}
		"ninf" => {Some(Float::with_val(prec, Special::NegInfinity))}
		"nan" => {Some(Float::with_val(prec, Special::Nan))}
		"time" => {Some(Float::with_val(prec, SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or(Duration::ZERO).as_secs()))}
		"timens" => {Some(Float::with_val(prec, SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or(Duration::ZERO).as_nanos()))}
		"pid" => {Some(Float::with_val(prec, std::process::id()))}
		"abort" => {std::process::abort();}
		"crash" => {constants(prec, "crash".to_string())}	//stack overflow through recursion
		"panic" => {std::panic::panic_any(
			unsafe {if let Some(ptr) = MSTK.last() {
				if ptr.t {&ptr.s} else {"Manual panic"}}
			else {"Manual panic"}});}
		"author" => {Some(Float::with_val(prec, 43615))}	//why not
		_ => {
			eprintln!("! Constant/conversion factor \"{}\" doesn't exist", key);
			None
		}
	}
}

//slightly more efficient string reverser, at least on my machine
fn rev_str(mut instr: String) -> String {
	let mut outstr = String::new();
	while !instr.is_empty() {
		outstr.push(instr.pop().unwrap());
	}
	outstr
}

//custom number printing function
//if output base is over 36, prints in custom "any-base" notation
//otherwise, applies precision like dc and converts from exponential notation if not too small
fn flt_to_str(mut num: Float, obase: Integer, oprec: Integer) -> String {
	if !num.is_normal() {
		if num.is_zero() {
			return String::from(if obase>36 {"(0)"} else {"0"});	//causes issues, always "0" regardless of parameters
		}
		let mut ret = String::from(if num.is_sign_negative() {"-"} else {""});
		if num.is_infinite() {
			ret += "∞";
			return ret;
		}
		if num.is_nan() {
			ret += "NaN";
			return ret;
		}
	}

	if obase>36 {	//any-base printing (original base conversion algorithm out of necessity, limited precision possible)
		let mut outstr = String::from(if num<0 {"(-"} else {"("});	//apply negative sign
		num = num.abs();
		let mut scale: usize = 0;	//amount to shift fractional separator in output
		while !num.is_integer()&&(oprec<0||scale<oprec) {	//turn into integer scaled by power of obase, apply output precision if enabled
			let temp = num.clone() * &obase;	//preview scale-up
			if temp.is_infinite() {	//possible with high precision due to Float's exponent limitation
				num /= &obase;	//prevent overflow in later "extra precision" part
				break;	//disregard further precision
			}
			num = temp;	//if ok, commit to scale-up
			scale +=1;
		}
		num *= &obase;	//get extra precision for last digit
		let mut int = num.to_integer().unwrap();	//convert to Integer
		int /= &obase;	//undo extra precision
		let mut dig = Integer::from(1);	//current digit value
		while dig<=int {
			dig *= &obase;	//get highest required digit value
		}
		dig /= &obase;	//correct off-by-one error
		loop {	//separate into digits
			let (quot, rem) = int.clone().div_rem_euc(dig.clone());	//separate into amount of current digit and remainder
			outstr.push_str(&quot.to_string());	//print amount of current digit
			outstr.push(' ');
			int = rem;	//switch to remainder
			if dig==1 {break;}	//stop when all digits done
			dig /= &obase;	//switch to next lower digit
		}
		if scale>0 {
			if let Some((idx, _)) = outstr.rmatch_indices(' ').nth(scale) {	//find location for fractional separator
				unsafe { outstr.as_bytes_mut()[idx] = b'.'; }	//and insert it
			}
			else {
				outstr.insert_str(if outstr.starts_with("(-") {2} else {1}, "0.");	//number has no integer part, add "0."
			}
		}
		outstr.pop();
		outstr.push(')');
		outstr
	}
	else {	//normal printing
		let mut outstr = num.to_string_radix(
			obase.to_i32().unwrap(),
			if oprec<0 {
				None
			}
			else {
				(oprec + Integer::from(
					num.to_integer_round(Round::Zero).unwrap().0	//integer part of num
					.to_string_radix(obase.to_i32().unwrap())	//...to string
					.trim_start_matches('-').len())).to_usize() 	//...length without negative sign, print exactly if too large
			}
		);
		if obase <= 10 {	//unify exponent symbol without searching the whole string
			let im = outstr.len()-1;	//max index
			unsafe {
				let bytes = outstr.as_bytes_mut();
				for ir in 0..=im {	//right offset
					if ir>10 {break;}	//exponents cannot have more digits, longest is @-323228496
					if bytes[im-ir]==b'e' {
						bytes[im-ir] = b'@';	//replace
						break;
					}
				}
			}
		}
		if outstr.starts_with('-') {
			outstr = outstr.replacen('-', "_", 1);	//replace negative sign
		}
		if outstr[if outstr.len()>11 {outstr.len()-11} else {0}..].contains('@') {	//efficiently check if in exponential notation
			let (mut mpart, epart) = outstr.rsplit_once('@').unwrap();
			mpart = mpart.trim_end_matches('0').trim_end_matches('.');	//remove trailing zeros from mantissa
			let eint = epart.parse::<i32>().unwrap();	//isolate exponential part
			if eint<0 && eint>-10 {
				outstr = "0.".to_string() + &"0".repeat(eint.abs() as usize -1) + &mpart.replacen('.', "", 1);	//convert from exponential notation if not too small
				if num<0 {
					let (ipart, fpart) = outstr.split_once('_').unwrap();
					outstr = "_".to_string() + ipart + fpart;	//move negative sign to front
				}
			}
			else {
				outstr = mpart.to_string() + "@" + &epart.replacen('-', "_", 1);	//reassemble, replace negative sign in exponent
			}
		}
		else {	//if in normal notation
			if let Some((ipart, fpart)) = outstr.split_once('.') {
				outstr = ipart.to_string() + "." + fpart.trim_end_matches('0');	//trim trailing zeros
			}
		}
		outstr.trim_end_matches('.').to_string()	//remove fractional separator
	}
}

//CORE EXECUTION ENGINE
//unsafe for accessing static mut objects across different runs
unsafe fn exec(input: String) {
	let mut cmdstk: Vec<String> = Vec::new();	//stack of reversed command strings to execute, enables pseudorecursive macro calls
	let mut inv = false;	//invert next comparison
	if !input.is_empty() {	//loop expects contents, do nothing if none provided
		cmdstk.push(rev_str(input));	//all command strings are reversed since pop() is O(1)
	}
	while !cmdstk.is_empty() {	//last().unwrap() is guaranteed to not panic within
	
		let mut cmd = cmdstk.last_mut().unwrap().pop().unwrap();	//isolate first character as command

		//defines behavior of all commands
		match cmd {
			/*------------------
				OBJECT INPUT
			------------------*/
			//standard number input, force with single quote to use letters
			'0'..='9'|'.'|'_'|'\''|'@' => {
				if ENVSTK.last().unwrap().1>36 {
					eprintln!("! Any-base input must be used for input bases over 36");
				}
				else {
					let mut numstr = String::new();	//gets filled with number to be parsed later
					let mut frac = false;	//'.' has already occurred
					let mut neg = false;	//'_' has already occurred
					let mut alpha = false;	//letters are used
					if cmd == '\'' {
						alpha = true;
						cmd = cmdstk.last_mut().unwrap().pop().unwrap_or('\0');
					}
					//keep adding to numstr until number is finished
					'STDNUM_FINISHED: loop {
						//numbers, periods and exponential notation
						if cmd.is_ascii_digit()||cmd == '.'||cmd == '@' {
							if cmd == '.' { if frac { break 'STDNUM_FINISHED; } else { frac = true; } } //break on encountering second '.'
							if cmd == '@' { neg = false; }	//allow for second negative sign in exponent
							numstr.push(cmd);						
						}
						//'_' needs to be replaced with '-'
						else if cmd == '_' {
							if neg { break 'STDNUM_FINISHED; } else { neg = true; } //break on encountering second '_'
							numstr.push('-');
						}
						//parse letters if number is prefixed with quote
						else if cmd.is_ascii_alphabetic() {
							if alpha {
								numstr.push(cmd);							
							}
							else {
								break 'STDNUM_FINISHED;
							}
						}
						else {
							break 'STDNUM_FINISHED;
						}
						cmd = cmdstk.last_mut().unwrap().pop().unwrap_or('\0');
					}
					cmdstk.last_mut().unwrap().push(cmd);	//restore first char that isn't part of the number
					if numstr.starts_with('@') { numstr.insert(0, '1') }	//add implied 1 before exponential marker
					if numstr.starts_with('.')||numstr.starts_with("-.") { numstr = numstr.replace('.', "0."); }	//add implied zero before fractional separator
					if numstr.ends_with('.')||numstr.ends_with('-')||numstr.is_empty() { numstr.push('0'); }	//add implied zero at end
					match Float::parse_radix(numstr.clone(), ENVSTK.last().unwrap().1.to_i32().unwrap()) {		
						Ok(res) => {
							MSTK.push(Obj::n(Float::with_val(WPREC, res)));
						},
						Err(error) => {
							eprintln!("! Unable to parse number \"{}\": {}", numstr, error);
						},
					}
				}
			},

			//any-base number input
			'(' => {
				let mut num = Integer::from(0);	//resulting number
				if cmdstk.last().unwrap().is_empty() {
					MSTK.push(Obj::n(Float::with_val(WPREC, num)));	//default to 0 if on end of input
				}
				else {
					let ibase = ENVSTK.last().unwrap().1.clone();
					let mut dig = String::new();	//digit being parsed
					let mut neg = false;	//number negative?
					let mut frac = false;	//fractional separator has occurred
					let mut scale = Integer::from(1);	//scale to divide by, for non-integers
					let mut exp = false;	//exponential symbol has occurred
					'CANCEL_ABNUM: loop {
						cmd = if let Some(c) = cmdstk.last_mut().unwrap().pop() {c} else {')'};	//get next character, finish number if not possible
						match cmd {
							'0'..='9' => {
								dig.push(cmd);	//add numerals to digit
							},
							'-'|'_' => {
								if neg {
									eprintln!("! Unable to parse any-base number: more than one negative sign");
									if let Some(idx) = cmdstk.last().unwrap().rfind(')') {
										cmdstk.last_mut().unwrap().truncate(idx);	//remove rest of erroneous number
									}
									else {
										cmdstk.last_mut().unwrap().clear();
									}
									break;
								}
								neg = true;
							},
							'.' => {
								if frac {
									eprintln!("! Unable to parse any-base number: more than one fractional separator");
									if let Some(idx) = cmdstk.last().unwrap().rfind(')') {
										cmdstk.last_mut().unwrap().truncate(idx);	//remove rest of erroneous number
									}
									else {
										cmdstk.last_mut().unwrap().clear();
									}
									break;
								}
								frac = true;
								cmdstk.last_mut().unwrap().push(' ');	//end digit in next iteration
							},
							'@' => {
								exp = true;
								cmdstk.last_mut().unwrap().push(' ');	//end digit in next iteration, exponent handled by finalizer
							},
							' '|')' => {	//if digit or whole number is finished
								let digint = if dig.clone().is_empty() {Integer::ZERO} else {Integer::parse(dig.clone()).unwrap().complete()};	//parse digit, default to 0
								if digint >= ibase {
									eprintln!("! Unable to parse any-base number: digit '{}' is too high for base {}", digint, ibase);
									if cmd==')' {break;}
									else {
										if let Some(idx) = cmdstk.last().unwrap().rfind(')') {
											cmdstk.last_mut().unwrap().truncate(idx);	//remove rest of erroneous number
										}
										else {
											cmdstk.last_mut().unwrap().clear();
										}
										break;
									}
								}
								num *= ibase.clone();	//add digit to number: multiply old contents by radix...
								num += digint;	//... and add new digit
								dig.clear();
								if frac {
									scale *= ibase.clone();	//if fractional part has started, make scale keep up
								}
								let escale =	//power applied to input base for exponential notation
								if exp {	//if exponential part has begun
									let mut epart = String::new();
									let mut eneg = false;
									while !cmdstk.last().unwrap().is_empty() {
										cmd = cmdstk.last_mut().unwrap().pop().unwrap();
										match cmd {
											'0'..='9' => {
												epart.push(cmd);
											},
											'-'|'_' => {
												if eneg {
													eprintln!("! Unable to parse any-base number: more than one negative sign in exponent");
													if let Some(idx) = cmdstk.last().unwrap().rfind(')') {
														cmdstk.last_mut().unwrap().truncate(idx);	//remove rest of erroneous number
													}
													else {
														cmdstk.last_mut().unwrap().clear();
													}
													break 'CANCEL_ABNUM;
												}
												epart.insert(0, '-');
												eneg = true;
											},
											')' => {
												break;
											},
											_ => {
												eprintln!("! Unable to parse any-base number: invalid character '{}' in exponent", cmd);
												if let Some(idx) = cmdstk.last().unwrap().rfind(')') {
													cmdstk.last_mut().unwrap().truncate(idx);	//remove rest of erroneous number
												}
												else {
													cmdstk.last_mut().unwrap().clear();
												}
												break 'CANCEL_ABNUM;
											},
										}
									}
									Integer::parse(epart).unwrap().complete()
								}
								else {
									Integer::from(0)
								};
								if cmd==')' {	//if number finished, push to stack
									if scale>1 {
										scale /= ibase.clone();	//correct off-by-one error
									}
									MSTK.push(Obj::n(Float::with_val(WPREC, num * if neg {-1} else {1}) / scale
										* Float::with_val(WPREC, ibase).pow(escale)));
									break;
								}
							},
							_ => {
								eprintln!("! Invalid character in any-base number: '{}'", cmd);
								if let Some(idx) = cmdstk.last().unwrap().rfind(')') {
									cmdstk.last_mut().unwrap().truncate(idx);	//remove rest of erroneous number
								}
								else {
									cmdstk.last_mut().unwrap().clear();
								}
								break;
							},
						}
					}
				}
			},

			//string input
			'[' => {
				let mut res = String::new();	//result string
				let mut nest: usize = 1;	//nesting level
				cmd = cmdstk.last_mut().unwrap().pop().unwrap_or('\0');	//overwrite opening bracket, null if nothing left
				loop {
					res.push(cmd);
					if cmd == '[' { nest+=1; }
					if cmd == ']' { nest-=1; }
					if nest==0 {	//string finished
						res.pop();	//remove closing bracket
						MSTK.push(Obj::s(res));
						break;
					}
					if cmdstk.last().unwrap().is_empty() {	//only reached on improper string
						eprintln!("! Unable to parse string \"[{}\": missing closing bracket", res);
						break;
					}
					else {cmd = cmdstk.last_mut().unwrap().pop().unwrap();}
				}
			},
			/*--------------
				PRINTING
			--------------*/
			//print top with newline
			'p' => {
				if !MSTK.is_empty() {
					if MSTK.last().unwrap().t {
						println!("[{}]", MSTK.last().unwrap().s.clone());
					}
					else {
						println!("{}", flt_to_str(MSTK.last().unwrap().n.clone(), ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone()));
					}
				}
			},

			//print full stack top to bottom
			'f' => {
				if !MSTK.is_empty() {
					for i in (0..MSTK.len()).rev() {
						if MSTK[i].t {
							println!("[{}]", MSTK[i].s.clone());
						}
						else {
							println!("{}", flt_to_str(MSTK[i].n.clone(), ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone()));
						}
					}
				}
			},

			//pop and print without newline
			'n' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if a.t {
						print!("{}", a.s);
						stdout().flush().unwrap();
					}
					else {
						print!("{}", flt_to_str(a.n, ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone()));
						stdout().flush().unwrap();
					}
				}
			},

			//pop and print with newline
			'P' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if a.t {
						println!("{}", a.s);
					}
					else {
						println!("{}", flt_to_str(a.n, ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone()));
					}
				}
			},

			//print register
			'F' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if REGS.len()>ri {
						if !REGS[ri].is_empty(){
							for i in (0..REGS[ri].len()).rev() {
								if REGS[ri][i].o.t {
									println!("[{}]", REGS[ri][i].o.s.clone());
								}
								else {
									println!("{}", flt_to_str(REGS[ri][i].o.n.clone(), ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone()));
								}
								if !REGS[ri][i].a.is_empty() {
									let maxwidth = (REGS[ri][i].a.len()-1).to_string().len();	//length of longest index number
									for ai in 0..REGS[ri][i].a.len() {
										if REGS[ri][i].a[ai].t {
											println!("\t{:>maxwidth$}: [{}]", ai, REGS[ri][i].a[ai].s);
										}
										else {
											println!("\t{:>maxwidth$}: {}", ai, flt_to_str(REGS[ri][i].a[ai].n.clone(), ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone()));
										}
									}
								}
							}
						}
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},
			/*----------------
				ARITHMETIC
			----------------*/
			//add or concatenate strings
			'+' => {
				if check_n(cmd, MSTK.len()) {
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						//concat strings
						if a.t {
							MSTK.push(Obj::s(a.s + &b.s));
						}
						//add numbers
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.n + b.n)));
						}
					}
				}
			},

			//subtract or remove chars from string
			'-' => {
				if check_n(cmd, MSTK.len()) {
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						//remove b chars from string a
						if a.t {
							let mut newstr = a.s.chars().collect::<Vec<char>>();
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;	//extract b, keep for checking if negative
							if let Some(mut num) = &int.abs_ref().complete().to_usize() {
								if num>newstr.len() { num = newstr.len(); }	//account for too large b
								if int<0 { newstr.reverse(); }	//if negative, reverse to remove from front
								newstr.truncate(newstr.len()-num);
								if int<0 { newstr.reverse(); }	//undo reversal
								MSTK.push(Obj::s(newstr.iter().collect::<String>()));
							}
							else {
								eprintln!("! Cannot possibly remove {} characters from a string", int);
							}
						}
						//subtract numbers
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.n - b.n)));
						}
					}
				}
			},

			//multiply or repeat/invert string
			'*' => {
				if check_n(cmd, MSTK.len()) {
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						//repeat string a b times
						if a.t {
							let mut newstr = a.s;
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;	//extract b, keep for checking if negative
							if let Some(num) = &int.abs_ref().complete().to_usize() {
								newstr = newstr.repeat(*num);
								if int<0 { newstr = rev_str(newstr); }	//if b is negative, invert string
								MSTK.push(Obj::s(newstr));
							}
							else {
								eprintln!("! Cannot possibly repeat a string {} times", int);
							}
						}
						//multiply numbers
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.n * b.n)));
						}
					}
				}
			},
			
			//divide or shorten string to length
			'/' => {
				if check_n(cmd, MSTK.len()) {
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						//shorten string a to length b
						if a.t {
							let mut newstr = a.s.chars().collect::<Vec<char>>();
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;	//extract b, keep for checking if negative
							if let Some(num) = &int.abs_ref().complete().to_usize() {
								if int<0 { newstr.reverse(); }	//if negative, reverse to remove from front
								newstr.truncate(*num);
								if int<0 { newstr.reverse(); }	//undo reversal
								MSTK.push(Obj::s(newstr.iter().collect::<String>()));
							}
							else {
								eprintln!("! Cannot possibly shorten a string to {} characters", int);
							}
						}
						//divide numbers
						else {
							if b.n==0 {
								eprintln!("! Arithmetic error: Attempted division by zero");
							}
							else {
								MSTK.push(Obj::n(Float::with_val(WPREC, a.n / b.n)));
							}
						}
					}
				}
			},

			//modulo or isolate char
			'%' => {
				if check_n(cmd, MSTK.len()) {
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.t {
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							if let Some(idx) = int.to_usize() {
								if let Some(chr) = a.s.chars().nth(idx) {
									MSTK.push(Obj::s(chr.to_string()));
								}
								else {
									eprintln!("! String \"{}\" is too short for index {}", a.s, idx);
								}
							}
							else {
								eprintln!("! Cannot possibly isolate character at index {}", int);
							}
						}
						else {
							let ia = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							let ib = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							if ib==0 {
								eprintln!("! Arithmetic error: Attempted reduction mod 0");
							}
							else {
								MSTK.push(Obj::n(Float::with_val(WPREC, ia % ib)));
							}
						}
					}
				}
			},

			//euclidean division or split string
			'~' => {
				if check_n(cmd, MSTK.len()) {
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.t {
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							if let Some(mut idx) = &int.to_usize() {
								let cvec = a.s.chars().collect::<Vec<char>>();
								if idx>cvec.len() { idx=cvec.len(); }	//if too large, split at max index to preserve signature
								MSTK.push(Obj::s(cvec[0..idx].iter().collect::<String>()));
								MSTK.push(Obj::s(cvec[idx..].iter().collect::<String>()));
							}
							else {
								eprintln!("! Cannot possibly split a string at character {}", int);
							}
						}
						else {
							let ia = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							let ib = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							if ib==0 {
								eprintln!("! Arithmetic error: Attempted reduction mod 0");
							}
							else {
								let (quot, rem)=ia.div_rem_euc(ib);
								MSTK.push(Obj::n(Float::with_val(WPREC, quot)));
								MSTK.push(Obj::n(Float::with_val(WPREC, rem)));
							}
						}
					}
				}
			},

			//exponentiation or find in string
			'^' => {
				if check_n(cmd, MSTK.len()) {
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.t {
							if let Some(bidx) = a.s.find(&b.s) {	//find byte index
								let cidx = a.s.char_indices().position(|x| x.0==bidx).unwrap();	//corresp. char index
								MSTK.push(Obj::n(Float::with_val(WPREC, cidx)));
							}
							else {
								MSTK.push(Obj::n(Float::with_val(WPREC, -1)));	//not found, silent error
							}
						}
						else {
							if a.n<0&&b.n.clone().abs()<1{
								eprintln!("! Arithmetic error: Roots of negative numbers are not allowed");
							}
							else {
								MSTK.push(Obj::n(Float::with_val(WPREC, a.n.pow(b.n))));
							}
						}
					}
				}
			},

			//modular exponentiation or find/replace in string
			'|' => {
				if check_n(cmd, MSTK.len()) {
					let c = MSTK.pop().unwrap();
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, c.t) {
						if a.t {
							MSTK.push(Obj::s(a.s.replace(&b.s, &c.s)));
						}
						else {
							let ia = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							let ib = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							let ic = c.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							if ic==0 {
								eprintln!("! Arithmetic error: Attempted reduction mod 0");
							}
							else {
								if let Ok(res) = ia.clone().pow_mod(&ib, &ic) {
									MSTK.push(Obj::n(Float::with_val(WPREC, res)));
								}
								else {
									eprintln!("! Arithmetic error: {} doesn't have an inverse mod {}", ia, ic);
								}
							}
						}
					}
				}
			},

			//square root
			'v' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.n<0 {
							eprintln!("! Arithmetic error: Roots of negative numbers are not allowed");
						}
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.n.sqrt())));
						}
					}
				}
			},

			//bth root
			'V' => {
				if check_n(cmd, MSTK.len()){
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.n<0&&b.n.clone().abs()>1{
							eprintln!("! Arithmetic error: Roots of negative numbers are not allowed");
						}
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.n.pow(b.n.recip()))));
						}
					}
				}
			},

			//length of string or natural logarithm
			'g' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.s.chars().count())));
						}
						else {
							if a.n<=0 {
								eprintln!("! Arithmetic error: Logarithms of zero and negative numbers are not allowed");
							}
							else {
								MSTK.push(Obj::n(Float::with_val(WPREC, a.n.ln())));
							}
						}
					}
				}
			},

			//base b logarithm
			'G' => {
				if check_n(cmd, MSTK.len()){
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.n<=0 {
							eprintln!("! Arithmetic error: Logarithms of zero and negative numbers are not allowed");
						}
						else if b.n==1||b.n<=0{
							eprintln!("! Arithmetic error: Logarithm base must be positive and not equal to 1");
						}
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.n.ln()/b.n.ln())));
						}
					}
				}
			},

			//sine
			'u' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj::n(Float::with_val(WPREC, a.n.sin())));
					}
				}
			},

			//cosine
			'y' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj::n(Float::with_val(WPREC, a.n.cos())));
					}
				}
			},

			//tangent
			't' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj::n(Float::with_val(WPREC, a.n.tan())));
					}
				}
			},

			//arc-sine
			'U' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.n.clone().abs()>1 {
							eprintln!("! Arithmetic error: Arc-sine of value outside [-1,1]");
						}
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.n.asin())));
						}
					}
				}
			},

			//arc-cosine
			'Y' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.n.clone().abs()>1 {
							eprintln!("! Arithmetic error: Arc-cosine of value outside [-1,1]");
						}
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, a.n.acos())));
						}
					}
				}
			},

			//arc-tangent
			'T' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj::n(Float::with_val(WPREC, a.n.atan())));
					}
				}
			},

			//random integer [0;a)
			'N' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if int<=0 {
							eprintln!("! Upper bound for random value must be above 0");
						}
						else {
							MSTK.push(Obj::n(Float::with_val(WPREC, int.random_below(&mut RNG[0]))));
						}
					}
				}
			},

			//constant/conversion factor lookup or convert number to string
			'"' => {
				if check_n(cmd, MSTK.len()) {
					let mut a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {	//constant lookup
							match a.s.matches(' ').count() {
								0 => {	//normal lookup
									let mut scale = String::new();
									while a.s.starts_with(|c: char| c.is_ascii_digit()||c=='-') {
										scale.push(a.s.remove(0));	//extract scale prefix
									}
									if scale.is_empty()||scale.ends_with('-') {scale.push('0');}

									let mut power = String::new();
									while a.s.ends_with(|c: char| c.is_ascii_digit()) {
										power.insert(0, a.s.pop().unwrap());	//extract power suffix
									}
									if power.is_empty() {power.push('1');}

									if let Some(res) = constants(WPREC, a.s) {
										MSTK.push(Obj::n(Float::with_val(WPREC, (res*Float::with_val(WPREC, Integer::parse(scale).unwrap().complete()).exp10())
												.pow(Integer::parse(power).unwrap().complete()))));
									}
								},
								1 => {	//conversion shorthand, everything is like the 0 case but twice
									let (from, to) = a.s.split_once(' ').unwrap();
									let mut sfrom = String::from(from);	//convert from this
									let mut sto = String::from(to);	//to this

									let mut kfrom = String::new();
									while sfrom.starts_with(|c: char| c.is_ascii_digit()||c=='-') {
										kfrom.push(sfrom.remove(0));	//extract scale prefix
									}
									if kfrom.is_empty()||kfrom.ends_with('-') {kfrom.push('0');}
									let mut pfrom = String::new();
									while sfrom.ends_with(|c: char| c.is_ascii_digit()) {
										pfrom.insert(0, sfrom.pop().unwrap());	//extract power suffix
									}
									if pfrom.is_empty() {pfrom.push('1');}

									let mut kto = String::new();
									while sto.starts_with(|c: char| c.is_ascii_digit()||c=='-') {
										kto.push(sto.remove(0));	//extract scale prefix
									}
									if kto.is_empty()||kto.ends_with('-') {kto.push('0');}
									let mut pto = String::new();
									while sto.ends_with(|c: char| c.is_ascii_digit()) {
										pto.insert(0, sto.pop().unwrap());	//extract power suffix
									}
									if pto.is_empty() {pto.push('1');}

									if let Some(nfrom) = constants(WPREC, sfrom.to_string()) {
										if let Some(nto) = constants(WPREC, sto.to_string()) {
											MSTK.push(Obj::n(Float::with_val(WPREC,
													(nfrom*Float::with_val(WPREC, Integer::parse(kfrom).unwrap().complete()).exp10())
														.pow(Integer::parse(pfrom).unwrap().complete())/
													(nto*Float::with_val(WPREC, Integer::parse(kto).unwrap().complete()).exp10())
														.pow(Integer::parse(pto).unwrap().complete()))));
										}
									}
								},
								_ => {
									eprintln!("! Too many spaces in constant lookup/unit conversion string \"{}\"", a.s);
								},
							}
						}
						else {	//"print" number to string
							MSTK.push(Obj::s(flt_to_str(a.n, ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone())));
						}
					}
				}
			},

			//deg -> rad shorthand
			'°' => {
				if check_n(cmd, MSTK.len()){
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false){
						MSTK.push(Obj::n(a.n * Float::with_val(WPREC, Constant::Pi) / 180))
					}
				}
			},
			/*------------------------
				STACK MANIPULATION
			------------------------*/
			//clear stack
			'c' => {
				MSTK.clear();
			},

			//remove top a objects from stack
			'C' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(mut num) = int.to_usize() {
							if num>MSTK.len() { num = MSTK.len(); }	//limit clear count
							MSTK.truncate(MSTK.len()-num);
						}
						else {
							eprintln!("! Cannot possibly remove {} objects from the main stack", int);
						}
					}
				}
			},

			//duplicate top of stack
			'd' => {
				if MSTK.is_empty() {
					eprintln!("! Nothing to duplicate");
				}
				else {
					MSTK.extend_from_within(MSTK.len()-1..);
				}
			},

			//duplicate top a objects
			'D' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(num) = int.to_usize() {
							if num<=MSTK.len() {
								MSTK.extend_from_within(MSTK.len()-num..);
							}
							else {
								eprintln!("! Not enough objects to duplicate");
							}
						}
						else {
							eprintln!("! Cannot possibly duplicate {} objects", int);
						}
					}
				}
			},

			//swap top 2 objects
			'r' => {
				if MSTK.len()>=2 {
					MSTK.swap(MSTK.len()-2, MSTK.len()-1);
				}
				else {
					eprintln!("! Not enough objects to rotate")
				}
			},

			//rotate top a objects
			'R' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let mut int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if int==0 { int = Integer::from(1); }	//replace 0 with effective no-op
						if let Some(num) = int.clone().abs().to_usize() {
							if num<=MSTK.len() {
								let sl = MSTK.as_mut_slice();
								if int<0 {
									sl[MSTK.len()-num..].rotate_left(1);	//if negative, rotate left/down
								}
								else {
									sl[MSTK.len()-num..].rotate_right(1);	//right/up otherwise
								}
								MSTK = sl.to_vec();
							}
							else {
								eprintln!("! Not enough objects to rotate");
							}
						}
						else {
							eprintln!("! Cannot possibly rotate {} objects", int.abs());
						}
					}
				}
			},

			//push stack depth
			'z' => {
				MSTK.push(Obj::n(Float::with_val(WPREC, MSTK.len())));
			},
			/*----------------------------
				ENVIRONMENT PARAMETERS
			----------------------------*/
			//set output precision
			'k' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if int>=-1 {
							ENVSTK.last_mut().unwrap().0 = int;
						}
						else {
							eprintln!("! Output precision must be at least -1");
						}
					}
				}
			},

			//set input base
			'i' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if int>=2 {
							ENVSTK.last_mut().unwrap().1 = int;
						}
						else {
							eprintln!("! Input base must be at least 2");
						}
					}
				}
			},

			//set output base
			'o' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if int>=2 {
							ENVSTK.last_mut().unwrap().2 = int;
						}
						else {
							eprintln!("! Output base must be at least 2");
						}
					}
				}
			},

			//set working precision
			'w' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if (1..=u32::MAX).contains(&int) {
							WPREC = int.to_u32().unwrap();
						}
						else {
							eprintln!("! Working precision must be between {} and {} (inclusive)", 1, u32::MAX);
						}
					}
				}
			},

			//push output precision
			'K' => {
				MSTK.push(Obj::n(Float::with_val(WPREC, ENVSTK.last().unwrap().0.clone())));
			},

			//push input base
			'I' => {
				MSTK.push(Obj::n(Float::with_val(WPREC, ENVSTK.last().unwrap().1.clone())));
			},

			//push output base
			'O' => {
				MSTK.push(Obj::n(Float::with_val(WPREC, ENVSTK.last().unwrap().2.clone())));
			},

			//push working precision
			'W' => {
				MSTK.push(Obj::n(Float::with_val(WPREC, WPREC)));
			},

			//create new k,i,o context
			'{' => {
				ENVSTK.push((kdef(), idef(), odef()));
			},

			//revert to previous context
			'}' => {
				ENVSTK.pop();
				if ENVSTK.is_empty() {
					ENVSTK.push((kdef(), idef(), odef()));	//ensure 1 entry always remains
				}
			},
			/*--------------------------
				REGISTERS AND MACROS
			--------------------------*/
			//save to top of register
			's' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if check_n(cmd, MSTK.len()) {
						let a = MSTK.pop().unwrap();
						if REGS.len()>ri {
							if REGS[ri].is_empty() {
								REGS[ri].push(RegObj {
									o: a,
									a: Vec::new()
								});
							}
							else {
								REGS[ri].last_mut().unwrap().o = a;
							}
						}
						else {
							eprintln!("! Register {} is not available", ri);
						}
					}
				}
			},

			//push to top of register
			'S' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if check_n(cmd, MSTK.len()) {
						let a=RegObj {
							o: MSTK.pop().unwrap(),
							a: Vec::new()
						};
						if REGS.len()>ri {
							REGS[ri].push(a);
						}
						else {
							eprintln!("! Register {} is not available", ri);
						}
					}
				}
			},

			//load from top of register
			'l' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if REGS.len()>ri {
						if REGS[ri].is_empty() {
							eprintln!("! Register {} is empty", ri);
						}
						else {
							MSTK.push(REGS[ri].last().unwrap().o.clone());
						}
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//pop from top of register
			'L' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if REGS.len()>ri {
						if REGS[ri].is_empty() {
							eprintln!("! Register {} is empty", ri);
						}
						else {
							MSTK.push(REGS[ri].pop().unwrap().o);
						}
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//save to top-of-register's array
			':' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if check_n(cmd, MSTK.len()) {
						let b = MSTK.pop().unwrap();
						let a = MSTK.pop().unwrap();
						if check_t(cmd, a.t, b.t, false) {
							if REGS.len()>ri {
								if REGS[ri].is_empty() {
									REGS[ri].push(RegObj {
										o: Obj::n(Float::with_val(WPREC, 0)),	//create default register object if empty
										a: Vec::new()
									});
								}
								let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
								if let Some(rai) = int.to_usize() {
									if rai>=REGS[ri].last().unwrap().a.len() {
										REGS[ri].last_mut().unwrap().a.resize(rai+1, Obj::n(Float::with_val(WPREC, 0)));	//extend if required, initialize with default objects
									}
									REGS[ri].last_mut().unwrap().a[rai] = a;
								}
								else {
									eprintln!("! Cannot possibly save to array index {}", int);
								}
							}
							else {
								eprintln!("! Register {} is not available", ri);
							}
						}
					}
				}
			},

			//load from top-of-register's array
			';' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if check_n(cmd, MSTK.len()) {
						let a = MSTK.pop().unwrap();
						if check_t(cmd, a.t, false, false) {
							if REGS.len()>ri {
								if REGS[ri].is_empty() {
									REGS[ri].push(RegObj {
										o: Obj::n(Float::with_val(WPREC, 0)),	//create default register object if empty
										a: Vec::new()
									});
								}
								let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
								if let Some(rai) = int.to_usize() {
									if rai>=REGS[ri].last().unwrap().a.len() {
										REGS[ri].last_mut().unwrap().a.resize(rai+1, Obj::n(Float::with_val(WPREC, 0)));	//extend if required, initialize with default objects
									}
									MSTK.push(REGS[ri].last().unwrap().a[rai].clone());
								}
								else {
									eprintln!("! Cannot possibly load from array index {}", int);
								}
							}
							else {
								eprintln!("! Register {} is not available", ri);
							}
						}
					}
				}
			},

			//load top-of-reg into buffer
			'j' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if REGS.len()>ri {
						if REGS[ri].is_empty() {
							eprintln!("! Register {} is empty", ri);
						}
						else {
							RO_BUF[0] = REGS[ri].last().unwrap().clone();
						}
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//pop top-of-reg into buffer
			'J' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if REGS.len()>ri {
						if REGS[ri].is_empty() {
							eprintln!("! Register {} is empty", ri);
						}
						else {
							RO_BUF[0] = REGS[ri].pop().unwrap();
						}
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//save buffer to top-of-reg
			'h' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if REGS.len()>ri {
						REGS[ri].pop();
						REGS[ri].push(RO_BUF[0].clone());
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//push buffer to register
			'H' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if REGS.len()>ri {
						REGS[ri].push(RO_BUF[0].clone());
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//push register depth
			'Z' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if REGS.len()>ri {
						MSTK.push(Obj::n(Float::with_val(WPREC, REGS[ri].len())));
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//specify manual register index
			',' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(ri) = int.to_usize() {
							if REGS.len()>ri {
								DRS = Some(ri);
							}
							else {
								eprintln!("! Register {} is not available", ri);
							}
						}
						else {
							eprintln!("! Register {} cannot possibly exist", int);
						}
					}
				}
			},
			/*------------
				MACROS
			------------*/

			//convert least significant 32 bits to one-char string or first char of string to number
			'a' => {
				if check_n(cmd, MSTK.len()) {
					let mut a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {
							if a.s.is_empty() {
								eprintln!("! Cannot convert empty string to number");
							}
							else {
								MSTK.push(Obj::n(Float::with_val(WPREC, a.s.remove(0) as u32)));
							}
						}
						else {
							if let Some(ia) = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0.to_u32() {
								if let Some(res) = char::from_u32(ia) {
									MSTK.push(Obj::s(res.to_string()));
								}
								else {
									eprintln!("! Unable to convert number {} to character: not a valid Unicode value", ia);
								}
							}
							else {
								eprintln!("! Unable to convert number {} to character: valid range is 0 to {}", a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0, u32::MAX);
							}
						}
					}
				}
			},

			//convert number to UTF-8 string or back
			'A' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {
							MSTK.push(Obj::n(Float::with_val(WPREC, Integer::from_digits(a.s.as_bytes(), Order::Msf))));
						}
						else {
							if let Ok(res) = String::from_utf8(a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0.to_digits::<u8>(Order::Msf)) {
								MSTK.push(Obj::s(res));
							}
							else {
								eprintln!("! Unable to convert number {} to string: not valid UTF-8", a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0);
							}
						}
					}
				}
			},

			//execute string as macro
			'x' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if a.t {
						if cmdstk.last().unwrap().is_empty() {
							cmdstk.pop();	//optimize tail call
						}
						cmdstk.push(rev_str(a.s));
					}
					else {
						MSTK.push(a);
					}
				}
			},

			//invert next conditional
			'!' => {
				inv = !inv;
			},

			//conditionally execute macro
			'<'|'='|'>' => {
				if cmdstk.last().unwrap().is_empty()&&DRS.is_none() {
					eprintln!("! No register number provided");
				}
				else {
					let ri = DRS.unwrap_or_else(|| cmdstk.last_mut().unwrap().pop().unwrap() as usize);
					DRS = None;
					if check_n(cmd, MSTK.len()) {
						let a = MSTK.pop().unwrap();	//deliberately reverse order
						let b = MSTK.pop().unwrap();
						if check_t(cmd, a.t, b.t, false) {
							let mut mac = String::new();
							if REGS.len()>ri {
								if REGS[ri].is_empty() {
									eprintln!("! Register {} is empty", ri);
								}
								else {
									mac = REGS[ri].last().unwrap().clone().o.s;	//get macro if possible
								}
							}
							else {
								eprintln!("! Register {} is not available", ri);
							}
							if inv != match cmd {	//like xor
								'<' => { a.n < b.n },
								'=' => { a.n == b.n },
								'>' => { a.n > b.n },
								_ => {false},}
							{
								if cmdstk.last().unwrap().is_empty() {
									cmdstk.pop();	//optimize tail call
								}
								cmdstk.push(rev_str(mac));
							}
						}
					}
				}
				inv = false;	//always reset inversion
			},

			//auto-macro
			'X' => {
				if check_n(cmd, MSTK.len()) {
					let b = MSTK.pop().unwrap();
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(reps) = int.to_usize() {
							if cmdstk.last().unwrap().is_empty() {
								cmdstk.pop();	//optimize tail call
							}
							cmdstk.resize(cmdstk.len()+reps, rev_str(a.s));
						}
						else {
							eprintln!("! Invalid macro repeat count: {}", int);
						}
					}
				}
			},

			//quit dcim
			'q' => {
				std::process::exit(DRS.unwrap_or(0) as i32);
			},

			//quit a macro calls
			'Q' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(mut num) = int.to_usize() {
							if num>cmdstk.len() {num=cmdstk.len();}
							cmdstk.truncate(cmdstk.len()-num);
							if cmdstk.is_empty() {
								cmdstk.push(String::new());	//guarantee at least one object
							}
						}
						else {
							eprintln!("! Cannot possibly quit {} levels", int);
						}
					}
				}
			},

			//prompt and execute
			'?' => {
				let mut prompt_in = String::new();
				stdin().read_line(&mut prompt_in).expect("Unable to read input");
				prompt_in = prompt_in.trim_end_matches(char::is_whitespace).to_string();	//trim trailing LF
				if cmdstk.last().unwrap().is_empty() {
					cmdstk.pop();	//optimize tail call
				}
				cmdstk.push(rev_str(prompt_in));
			},

			//execute file as script
			'&' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						match std::fs::read_to_string(a.s.clone()) {
							Ok(script) => {
								let mut script_nc = String::new();	//script with comments removed
								for line in script.split('\n') {
									script_nc.push_str(line.split_once('#').unwrap_or((line,"")).0);	//remove comment on every line
									script_nc.push('\n');
								}
								cmdstk.push(rev_str(script_nc));
							},
							Err(error) => {
								eprintln!("! Unable to read file \"{}\": {}", a.s, error);
							},
						}
					}
				}
			},

			//get environment variable
			'$' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						match std::env::var(&a.s) {
							Ok(val) => {
								MSTK.push(Obj::s(val));
							},
							Err(err) => {
								eprintln!("! Unable to get value of \"{}\": {}", a.s, err);
							},
						}
					}
				}
			},

			//execute os command(s)
			'\\' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						for oscmd in a.s.split(';') {
							if let Some((var, val)) = oscmd.split_once('=') {	//set variable
								std::env::set_var(var, val);
							}
							else {	//normal command
								let mut args: Vec<&str> = oscmd.trim().split(' ').collect();
								match std::process::Command::new(args.remove(0)).args(args).spawn() {
									Ok(mut child) => {
										if let Ok(stat) = child.wait() {
											if let Some(code) = stat.code() {
												if code!=0 {eprintln!("! OS command \"{}\" exited with code {}", oscmd, code);}
											}
										}
									},
									Err(error) => {
										eprintln!("! Unable to execute OS command \"{}\": {}", oscmd, error);
									},
								}
							}
						}
					}
				}
			},

			//stop on beginning of #comment
			'#' => {
				cmdstk.last_mut().unwrap().clear();
			},

			//notify on invalid command, keep going
			_ => {
				if !cmd.is_whitespace()&&cmd!='\0' { eprintln!("! Invalid command: {} (U+{:04X})", cmd, cmd as u32); }
			},
		}
		while let Some(ptr) = cmdstk.last() {
			if ptr.is_empty() {
				cmdstk.pop();
			}
			else{break;}
		}
	}
}
