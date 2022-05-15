use rug::{Integer, integer::Order, Complete, Float, float::{Round, Constant}, ops::Pow, rand::RandState};
use std::io::{stdin, stdout, Write};
use std::time::{SystemTime, Duration};
use std::cmp::Ordering;

const HELPMSG: &str = "
╔════════════════════════╗
║                        ║
║    █        █          ║
║    █                   ║
║  ███  ███   █   ████   ║
║  █ █  █     █   █ █ █  ║
║  ███  ███  ███  █ █ █  ║
║                        ║
╚════════════════════════╝
dc improved - Feature-added rewrite of an RPN calculator/stack machine language from 1970-72
Documentation at https://github.com/43615/dcim

Options and syntax:

<nothing> | --interactive | -i | i
	Interactive mode, standard prompt loop.

(--expression | -e | e) expr1 expr2 expr3 ... [?]
	Expression mode, executes expressions in order. If the last argument is \"?\", enters interactive mode after expressions are done.

(--file | -f | f) file1 file2 file3 ... [?]
	File mode, executes contents of files in order. \"?\" behaves the same as with -e.

--help | -h | h
	Print this help message.
";

//environment parameters with default values:
static mut KDEF: Integer = Integer::new();	//output precision
static mut IDEF: Integer = Integer::new();	//input base
static mut ODEF: Integer = Integer::new();	//output base
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

static mut MSTK: Vec<Obj> = Vec::new();	//main stack

static mut REGS: Vec<Vec<RegObj>> = Vec::new();	//array of registers
const REGS_SIZE: usize = 65536;	//amount of available registers
static mut RO_BUF: Vec<RegObj> = Vec::new();

static mut DRS: usize = 0;	//direct register selector
static mut DRS_EN: bool = false;	//DRS valid?

static mut RNG: Vec<RandState> = Vec::new();

const INT_ORD_DEF: (Integer, Ordering) = (Integer::ZERO, Ordering::Equal);	//default tuple for to_integer_round()
fn flt_def() -> Float {Float::new(1)}	//default Float value for unused Obj.n

fn main() {
	let mut args: Vec<String> = std::env::args().collect();
	args.remove(0);

	unsafe {
		KDEF = Integer::from(-1);
		IDEF = Integer::from(10);
		ODEF = Integer::from(10);
		ENVSTK.push((KDEF.clone(), IDEF.clone(), ODEF.clone()));	//initialize env params
		REGS.resize(REGS_SIZE, Vec::new());	//initialize registers
		RO_BUF.push(RegObj{
			a: Vec::new(),
			o: Obj {
				t: false,
				n: Float::with_val(WPREC, 0),
				s: String::new()
			}
		});
		//initialize RNG with system time (* PID for a bit less predictability)
		RNG.push(RandState::new());
		RNG[0].seed(&(Integer::from(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or(Duration::MAX).as_nanos()) * std::process::id()));
	}

	if args.is_empty() {
		interactive_mode();	//default to interactive
	}
	else {
		let mode = args.remove(0);
		match mode.as_str() {
			"--interactive"|"-i"|"i" => {
				//ability to force interactive mode just in case
				interactive_mode();
			},
			"--expression"|"-e"|"e" => {
				expression_mode(args);
			},
			"--file"|"-f"|"f" => {
				file_mode(args);
			},
			"--help"|"-h"|"h" => {
				println!("{}", HELPMSG);
			},
			_ => {
				eprintln!("! Invalid operating mode \"{}\"", mode);
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
			}
		}
		if input.is_empty() {
			print!("\r\r");
			std::process::exit(0);	//stop on end of pipe input
		}
		input = input.trim_end_matches('\n').to_string();	//remove trailing LF

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

//checks if n operands are sufficient for operator op (defines adicity)
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
		eprintln!("! Insufficient arguments for command \"{}\"", op);
		false
	}
}

//checks if an operator can be used on provided operand types
//a-c: types (.t) of the operands that would be used (in canonical order), use false if not required
fn check_t(op: char, a: bool, b: bool, c: bool) -> bool {
	if match op {
		//'+' can also concatenate strings
		'+' => (!a&&!b)||(a&&b),

		//string manipulation, store into array
		'-'|'*'|'/'|'~'|':' => (!a&&!b)||(a&&!b),

		//read file by name, get env variable, execute os command
		'&'|'$'|'\\' => a,

		//convert both ways, constant lookup by string name or convert number to string, execute macros, get log or string length
		'a'|'A'|'"'|'x'|'g' => !a||a,

		//auto-macro
		'X' => a&&!b,

		//all other ops can only have numbers
		_ => !a&&!b&&!c,
	}
	{ true }
	else {
		eprintln!("! Invalid argument type(s) for command \"{}\"", op);
		false
	}
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
		"hbar" => {Some(Float::with_val(prec, 662607015)*Float::with_val(prec, 10).pow(-42)/(2*Float::with_val(prec, Constant::Pi)))}
		"G" => {Some(Float::with_val(prec, 6674)*Float::with_val(prec, 10).pow(-3))}
		"qe" => {Some(Float::with_val(prec, 1602176634)*Float::with_val(prec, 10).pow(-28))}
		"NA" => {Some(Float::with_val(prec, 602214076)*Float::with_val(prec, 10).pow(31))}
		"kB" => {Some(Float::with_val(prec, 1380649)*Float::with_val(prec, 10).pow(-29))}
		"u" => {Some(Float::with_val(prec, 1660539066)*Float::with_val(prec, 10).pow(-36))}
		"lp" => {Some(Float::with_val(prec, 16162)*Float::with_val(prec, 10).pow(-39))}
		"tp" => {Some(Float::with_val(prec, 5391)*Float::with_val(prec, 10).pow(-47))}
		"mp" => {Some(Float::with_val(prec, 21764)*Float::with_val(prec, 10).pow(-12))}
		"Tp" => {Some(Float::with_val(prec, 14167)*Float::with_val(prec, 10).pow(28))}
		/*------------------
			LENGTH UNITS
		------------------*/
		"in" => {Some(Float::with_val(prec, 254)*Float::with_val(prec, 10).pow(-4))}
		"ft" => {Some(Float::with_val(prec, 3048)*Float::with_val(prec, 10).pow(-4))}
		"yd" => {Some(Float::with_val(prec, 9144)*Float::with_val(prec, 10).pow(-4))}
		"m" => {Some(Float::with_val(prec, 1))}
		"mi" => {Some(Float::with_val(prec, 1609344)*Float::with_val(prec, 10).pow(-3))}
		"nmi" => {Some(Float::with_val(prec, 1852))}
		"AU" => {Some(Float::with_val(prec, 149597870700i64))}
		"ly" => {Some(Float::with_val(prec, 9460730472580800i64))}
		"pc" => {Some(Float::with_val(prec, 96939420213600000i64)/Float::with_val(prec, Constant::Pi))}
		/*----------------
			MASS UNITS
		----------------*/
		"ct" => {Some(Float::with_val(prec, 2)*Float::with_val(prec, 10).pow(-4))}
		"oz" => {Some(Float::with_val(prec, 28349523125i64)*Float::with_val(prec, 10).pow(-12))}
		"lb" => {Some(Float::with_val(prec, 45359237)*Float::with_val(prec, 10).pow(-8))}
		"kg" => {Some(Float::with_val(prec, 1))}
		"st" => {Some(Float::with_val(prec, 635029318)*Float::with_val(prec, 10).pow(-8))}
		/*----------------
			TIME UNITS
		----------------*/
		"s" => {Some(Float::with_val(prec, 1))}
		"min" => {Some(Float::with_val(prec, 60))}
		"h" => {Some(Float::with_val(prec, 3600))}
		"d" => {Some(Float::with_val(prec, 86400))}
		"w" => {Some(Float::with_val(prec, 604800))}
		_ => {
			eprintln!("! Constant/conversion factor \"{}\" not found", key);
			None
		}
	}
}

//custom number printing function
//if output base is over 36, prints in custom "any-base" notation
//otherwise, applies precision to fractional part and converts from exponential notation
fn flt_to_str(mut num: Float, obase: Integer, oprec: Integer) -> String {
	if num.is_zero() {
		return String::from(if obase>36 {"(0)"} else {"0"});	//causes issues, always "0" regardless of parameters
	}
	if num.is_infinite() {
		return String::from("Infinity");
	}
	if num.is_nan() {
		return String::from("Not a number");
	}

	if obase>36 {	//any-base printing
		let mut outstr = String::from(if num<0 {"(-"} else {"("});	//get rid of negative sign
		let mut scale = 0usize;	//amount to shift fractional separator in output
		while !num.is_integer()&&if oprec<0 {true} else {scale<oprec} {	//apply output precision if enabled
			num *= obase.clone();	//make integer
			scale +=1;
		}
		num *= obase.clone();	//get extra precision for last digit
		let mut int = num.clone().abs().to_integer().unwrap();	//get absolute value
		int /= obase.clone();	//undo extra precision
		let mut dig = Integer::from(1);	//current digit value
		while dig<=int {
			dig *= obase.clone();	//get highest required digit value
		}
		dig /= obase.clone();	//correct off-by-one error
		loop {
			let (quot, rem) = int.clone().div_rem_euc(dig.clone());	//separate into amount of current digit and remainder
			outstr.push_str(&quot.to_string());	//print amount of current digit
			outstr.push(' ');
			int = rem;	//switch to remainder
			if dig==1 {break;}	//stop when all digits done
			dig /= obase.clone();	//switch to next lower digit
		}
		if scale>0 { unsafe {
			if let Some((idx, _)) = outstr.rmatch_indices(' ').nth(scale){	//find location for fractional separator
				outstr.as_bytes_mut()[idx] = '.' as u8;	//and apply it
			}
			else {
				outstr.insert_str(if outstr.starts_with("(-") {2} else {1}, "0.");	//number has no integer part, add "0."
			}
		}}
		outstr.pop();
		outstr.push(')');
		outstr
	}
	else {	//normal printing
		let mut outstr = num.to_string_radix(
			obase.to_i32().unwrap(),
			if oprec>=0 {
				if let Some(p) = (oprec + Integer::from(
						num.clone().to_integer_round(Round::Zero).unwrap().0	//integer part of num
						.to_string_radix(obase.to_i32().unwrap())	//...to string
						.trim_start_matches('-').len())).to_usize() 	//...length without negative sign
				{Some(p)}	//desired length of integer+fractional part
				else {None}	//if too big for usize, print exactly
			}
			else {None}
		);
		if obase <= 10 {	//unify exponent symbol without searching the whole string
			let idxm = outstr.len()-1;
			for idxr in 0..=idxm {
				if idxr>10 {break;}	//exponents cannot have more digits, longest is @-323228496
				if outstr.as_bytes()[idxm-idxr]=='e' as u8 {
					unsafe {outstr.as_bytes_mut()[idxm-idxr] = '@' as u8;}
					break;
				}
			}
		}
		if outstr[if outstr.len()>11 {outstr.len()-11} else {0}..].contains('@') {	//efficiently check if in exponential notation
			let (mut mpart, epart) = outstr.rsplit_once('@').unwrap();
			mpart = mpart.trim_end_matches('0').trim_end_matches('.');	//remove trailing zeros from mantissa
			let eint = epart.parse::<i32>().unwrap();	//isolate exponential part
			if eint<0 && eint>-10 {
				outstr = "0.".to_string() + &"0".repeat(eint.abs() as usize -1) + &mpart.replacen('.', "", 1);	//convert from exponential notation if not too small
				if num<0 {
					let (ipart, fpart) = outstr.split_once('-').unwrap();
					outstr = "-".to_string() + ipart + fpart;	//move negative sign to front
				}
			}
			else {
				outstr = mpart.to_string() + " @" + epart;	//reassemble, add space for clarity
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
		cmdstk.push(input.chars().rev().collect());	//all command strings are reversed since pop() is O(1)
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
						cmd = if let Some(c) = cmdstk.last_mut().unwrap().pop() {c} else {' '};
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
						cmd = if let Some(c) = cmdstk.last_mut().unwrap().pop() {c} else {' '};
					}
					cmdstk.last_mut().unwrap().push(cmd);	//restore first char that isn't part of the number
					if numstr.starts_with('@') { numstr.insert(0, '1') }	//add implied 1 before exponential marker
					if numstr.starts_with('.')||numstr.starts_with("-.") { numstr = numstr.replace('.', "0."); }	//add implied zero before fractional separator
					if numstr.ends_with('.')||numstr.ends_with('-')||numstr.is_empty() { numstr.push('0'); }	//add implied zero at end
					match Float::parse_radix(numstr.clone(), ENVSTK.last().unwrap().1.to_i32().unwrap()) {		
						Ok(res) => {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, res),
								s: String::new()
							});
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
					MSTK.push(Obj {
						t: false,
						n: Float::with_val(WPREC, num),	//default to 0 if on end of input
						s: String::new()
					});
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
									eprintln!("! Unable to parse any-base number: digit {} is too high for base {}", digint, ibase);
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
												eprintln!("! Unable to parse any-base number: invalid character \"{}\" in exponent", cmd);
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
									MSTK.push(Obj {
										t: false,
										n: Float::with_val(WPREC, num * if neg {-1} else {1}) / scale
										* Float::with_val(WPREC, ibase).pow(escale),	//apply neg, scale and escale
										s: String::new()
									});
									break;
								}
							},
							_ => {
								eprintln!("! Invalid character in any-base number: \"{}\"", cmd);
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
				let mut err = false;
				if !cmdstk.last().unwrap().is_empty() {
					cmd = cmdstk.last_mut().unwrap().pop().unwrap();
					while nest>0 {
						res.push(cmd);
						if cmd == '[' { nest+=1; }
						if cmd == ']' { nest-=1; }
						if cmdstk.last().unwrap().is_empty() {
							if nest>0 {
								//only reached on improper string
								eprintln!("! Unable to parse string \"{}\": missing closing bracket", res);
								err = true;
								nest = 0;
							}
						}
						else {cmd = cmdstk.last_mut().unwrap().pop().unwrap();}
					}
					cmdstk.last_mut().unwrap().push(cmd);	//restore first char that isn't part of the string
					if !err {
						res.pop();
						MSTK.push(Obj {
							t: true,
							n: flt_def(),
							s: res.to_string()
						});
					}
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
				if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
					eprintln!("! No register number provided");
				}
				else {
					let ri = if DRS_EN {
						DRS_EN = false;
						DRS
					}
					else {
						cmdstk.last_mut().unwrap().pop().unwrap() as usize
					};
					if REGS_SIZE>ri {
						if !REGS[ri].is_empty(){
							for i in (0..REGS[ri].len()).rev() {
								if REGS[ri][i].o.t {
									println!("[{}]", REGS[ri][i].o.s.clone());
								}
								else {
									println!("{}", flt_to_str(REGS[ri][i].o.n.clone(), ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone()));
								}
								if !REGS[ri][i].a.is_empty() {
									let tablen = REGS[ri][i].a.len().to_string().len();	//length of longest index number
									for ai in 0..REGS[ri][i].a.len() {
										if REGS[ri][i].a[ai].t {
											println!("\t{}{}: [{}]", " ".repeat(tablen-ai.to_string().len()), ai, REGS[ri][i].a[ai].s);
										}
										else {
											println!("\t{}{}: {}", " ".repeat(tablen-ai.to_string().len()), ai, flt_to_str(REGS[ri][i].a[ai].n.clone(), ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone()));
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
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						//concat strings
						if a.t {
							MSTK.push(Obj {
								t: true,
								n: flt_def(),
								s: a.s + &b.s
							});
						}
						//add numbers
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n + b.n),
								s: String::new()
							});
						}
					}
				}
			},

			//subtract or remove chars from string
			'-' => {
				if check_n(cmd, MSTK.len()) {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						//remove b chars from string a
						if a.t {
							let mut newstr = a.s.clone();
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;	//extract b, keep for checking if negative
							if let Some(mut num) = int.clone().abs().to_usize() {
								if num>newstr.len() { num = newstr.len(); }	//account for too large b
								if int<0 { newstr = newstr.chars().rev().collect(); }	//if negative, reverse to remove from front
								if newstr.is_char_boundary(newstr.len()-num) {
									newstr.truncate(newstr.len()-num);
									if int<0 { newstr = newstr.chars().rev().collect(); }	//undo reversal
									MSTK.push(Obj {
										t: true,
										n: flt_def(),
										s: newstr
									});
								}
								else {
									eprintln!("! Cannot remove {} characters from string \"{}\": not on a character boundary", num, a.s);
								}
							}
							else {
								eprintln!("! Cannot possibly remove {} characters from a string", int);
							}
						}
						//subtract numbers
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n - b.n),
								s: String::new()
							});
						}
					}
				}
			},

			//multiply or repeat/invert string
			'*' => {
				if check_n(cmd, MSTK.len()) {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						//repeat string a b times
						if a.t {
							let mut newstr = a.s;
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;	//extract b, keep for checking if negative
							if let Some(mut num) = int.clone().abs().to_usize() {
								if num*newstr.len()>usize::MAX { num = usize::MAX/newstr.len(); }	//account for too large b
								newstr = newstr.repeat(num);
								if int<0 { newstr = newstr.chars().rev().collect(); }	//if b is negative, invert string
							}
							else {
								eprintln!("! Cannot possibly repeat a string {} times", int);
							}
							MSTK.push(Obj {
								t: true,
								n: flt_def(),
								s: newstr
							});
						}
						//multiply numbers
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n * b.n),
								s: String::new()
							});
						}
					}
				}
			},
			
			//divide or shorten string to length
			'/' => {
				if check_n(cmd, MSTK.len()) {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						//shorten string a to length b
						if a.t {
							let mut newstr = a.s.clone();
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;	//extract b, keep for checking if negative
							if let Some(num) = int.clone().abs().to_usize() {
								if int<0 { newstr = newstr.chars().rev().collect(); }	//if negative, reverse to remove from front
								if newstr.is_char_boundary(num) {
									newstr.truncate(num);
									if int<0 { newstr = newstr.chars().rev().collect(); }	//undo reversal
									MSTK.push(Obj {
										t: true,
										n: flt_def(),
										s: newstr
									});
								}
								else {
									eprintln!("! Cannot truncate string \"{}\" to length {}: not on a character boundary", a.s, num);
								}
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
								MSTK.push(Obj {
									t: false,
									n: Float::with_val(WPREC, a.n / b.n),
									s: String::new()
								});
							}
						}
					}
				}
			},

			//modulo, integers only
			'%' => {
				if check_n(cmd, MSTK.len()) {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						let ia = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						let ib = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if ib==0 {
							eprintln!("! Arithmetic error: Attempted modulo by zero");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, ia % ib),
								s: String::new()
							});
						}
					}
				}
			},

			//euclidean division or split string
			'~' => {
				if check_n(cmd, MSTK.len()) {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.t {
							let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							if let Some(idx) = int.clone().to_usize() {
								if a.s.is_char_boundary(idx) {
									let mut lstr = a.s.clone();
									let rstr = lstr.split_off(idx);
									MSTK.push(Obj {
										t: true,
										n: flt_def(),
										s: lstr
									});
									MSTK.push(Obj {
										t: true,
										n: flt_def(),
										s: rstr
									});
								}
								else {
									eprintln!("! Cannot split string \"{}\" at index {}: out of range or not on a character boundary", a.s, idx);
								}
							}
							else {
								eprintln!("! Cannot possibly split a string at index {}", int);
							}
						}
						else {
							let ia = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							let ib = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
							if ib==0 {
								eprintln!("! Arithmetic error: Attempted modulo by zero");
							}
							else {
								let (quot, rem)=ia.div_rem_euc(ib);
								MSTK.push(Obj {
									t: false,
									n: Float::with_val(WPREC, quot),
									s: String::new()
								});
								MSTK.push(Obj {
									t: false,
									n: Float::with_val(WPREC, rem),
									s: String::new()
								});
							}
						}
					}
				}
			},

			//exponentiation
			'^' => {
				if check_n(cmd, MSTK.len()) {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.n<0&&b.n.clone().abs()<1{
							eprintln!("! Arithmetic error: Roots of negative numbers are not allowed");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n.pow(b.n)),
								s: String::new()
							});
						}
					}
				}
			},

			//modular exponentiation, integers only
			'|' => {
				if check_n(cmd, MSTK.len()) {
					let c=MSTK.pop().unwrap();
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, c.t) {
						let ia = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						let ib = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						let ic = c.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if ic==0 {
							eprintln!("! Arithmetic error: Attempted modulo by zero");
						}
						else {
							if let Ok(res) = ia.clone().pow_mod(&ib, &ic) {
								MSTK.push(Obj {
									t: false,
									n: Float::with_val(WPREC, res),
									s: String::new()
								});
							}
							else {
								eprintln!("! Arithmetic error: {} doesn't have an inverse mod {}", ia, ic);
							}
						}
					}
				}
			},

			//square root
			'v' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.n<0 {
							eprintln!("! Arithmetic error: Roots of negative numbers are not allowed");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n.sqrt()),
								s: String::new()
							});
						}
					}
				}
			},

			//bth root
			'V' => {
				if check_n(cmd, MSTK.len()){
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.n<0&&b.n.clone().abs()>1{
							eprintln!("! Arithmetic error: Roots of negative numbers are not allowed");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n.pow(b.n.recip())),
								s: String::new()
							});
						}
					}
				}
			},

			//natural logarithm
			'g' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.s.len()),
								s: String::new()
							});
						}
						else {
							if a.n<=0 {
								eprintln!("! Arithmetic error: Logarithms of zero and negative numbers are not allowed");
							}
							else {
								MSTK.push(Obj {
									t: false,
									n: Float::with_val(WPREC, a.n.ln()),
									s: String::new()
								});
							}
						}
					}
				}
			},

			//base b logarithm
			'G' => {
				if check_n(cmd, MSTK.len()){
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if a.n<=0 {
							eprintln!("! Arithmetic error: Logarithms of zero and negative numbers are not allowed");
						}
						else if b.n==1||b.n<=0{
							eprintln!("! Arithmetic error: Logarithm base must be positive and not equal to 1");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n.ln()/b.n.ln()),
								s: String::new()
							});
						}
					}
				}
			},

			//sine
			'u' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj {
							t: false,
							n: Float::with_val(WPREC, a.n.sin()),
							s: String::new()
						});
					}
				}
			},

			//cosine
			'y' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj {
							t: false,
							n: Float::with_val(WPREC, a.n.cos()),
							s: String::new()
						});
					}
				}
			},

			//tangent
			't' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj {
							t: false,
							n: Float::with_val(WPREC, a.n.tan()),
							s: String::new()
						});
					}
				}
			},

			//arc-sine
			'U' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.n.clone().abs()>1 {
							eprintln!("! Arithmetic error: Arc-sine of value outside [-1,1]");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n.asin()),
								s: String::new()
							});
						}
					}
				}
			},

			//arc-cosine
			'Y' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.n.clone().abs()>1 {
							eprintln!("! Arithmetic error: Arc-cosine of value outside [-1,1]");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, a.n.acos()),
								s: String::new()
							});
						}
					}
				}
			},

			//arc-tangent
			'T' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj {
							t: false,
							n: Float::with_val(WPREC, a.n.atan()),
							s: String::new()
						});
					}
				}
			},

			//random integer [0;a)
			'N' => {
				if check_n(cmd, MSTK.len()){
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if int<=0 {
							eprintln!("! Upper bound for random value must be above 0");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, int.random_below(&mut RNG[0])),
								s: String::new()
							});
						}
					}
				}
			},

			//constant/conversion factor lookup or convert number to string
			'"' => {
				if check_n(cmd, MSTK.len()) {
					let mut a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {
							match a.s.matches(' ').count() {
								0 => {
									let mut power = String::new();
									while a.s.ends_with(|c: char| c.is_ascii_digit()) {
										power.insert(0, a.s.pop().unwrap());
									}
									if power.is_empty() {power.push('1');}
									if let Some(res) = constants(WPREC, a.s) {
										MSTK.push(Obj {
											t: false,
											n: Float::with_val(WPREC, res.pow(Integer::parse(power).unwrap().complete())),
											s: String::new()
										});
									}
								},
								1 => {
									let (from, to) = a.s.split_once(' ').unwrap();
									let mut sfrom = String::from(from);
									let mut sto = String::from(to);

									let mut pfrom = String::new();
									while sfrom.ends_with(|c: char| c.is_ascii_digit()) {
										pfrom.insert(0, sfrom.pop().unwrap());
									}
									if pfrom.is_empty() {pfrom.push('1');}

									let mut pto = String::new();
									while sto.ends_with(|c: char| c.is_ascii_digit()) {
										pto.insert(0, sto.pop().unwrap());
									}
									if pto.is_empty() {pto.push('1');}

									if let Some(nfrom) = constants(WPREC, sfrom.to_string()) {
										if let Some(nto) = constants(WPREC, sto.to_string()) {
											MSTK.push(Obj {
												t: false,
												n: Float::with_val(WPREC,
													nfrom.pow(Integer::parse(pfrom).unwrap().complete())/
													nto.pow(Integer::parse(pto).unwrap().complete())),
												s: String::new()
											});
										}
									}
								},
								_ => {
									eprintln!("! Invalid unit conversion string: \"{}\"", a.s);
								},
							}
						}
						else {
							MSTK.push(Obj {
								t: true,
								n: flt_def(),
								s: flt_to_str(a.n, ENVSTK.last().unwrap().2.clone(), ENVSTK.last().unwrap().0.clone())
							});
						}
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

			//remove top a elements from stack
			'C' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(mut num) = int.to_usize() {
							if num>MSTK.len() { num=MSTK.len(); }	//limit clear count
							MSTK.truncate(MSTK.len()-num);
						}
						else {
							eprintln!("! Cannot possibly remove {} elements from the main stack", int);
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

			//duplicate top a elements
			'D' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(num) = int.to_usize() {
							if num<=MSTK.len() {
								MSTK.extend_from_within(MSTK.len()-num..);
							}
							else {
								eprintln!("! Not enough elements to duplicate");
							}
						}
						else {
							eprintln!("! Cannot possibly duplicate {} elements", int);
						}
					}
				}
			},

			//swap top 2 elements
			'r' => {
				if MSTK.len()>=2 {
					MSTK.swap(MSTK.len()-2, MSTK.len()-1);
				}
				else {
					eprintln!("! Not enough elements to rotate")
				}
			},

			//rotate top a elements
			'R' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
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
								eprintln!("! Not enough elements to rotate");
							}
						}
						else {
							eprintln!("! Cannot possibly rotate {} elements", int.abs());
						}
					}
				}
			},

			//push stack depth
			'z' => {
				MSTK.push(Obj {
					t: false,
					n: Float::with_val(WPREC, MSTK.len()),
					s: String::new()
				});
			},
			/*----------------------------
				ENVIRONMENT PARAMETERS
			----------------------------*/
			//set output precision
			'k' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
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
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if int>=2 {
							ENVSTK.last_mut().unwrap().1 = int;
						}
						else {
							eprintln!("! Inpt base must be at least 2");
						}
					}
				}
			},

			//set output base
			'o' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
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
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if int>=1 && int<=u32::MAX {
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
				MSTK.push(Obj {
					t: false,
					n: Float::with_val(WPREC, ENVSTK.last().unwrap().0.clone()),
					s: String::new()
				});
			},

			//push input base
			'I' => {
				MSTK.push(Obj {
					t: false,
					n: Float::with_val(WPREC, ENVSTK.last().unwrap().1.clone()),
					s: String::new()
				});
			},

			//push output base
			'O' => {
				MSTK.push(Obj {
					t: false,
					n: Float::with_val(WPREC, ENVSTK.last().unwrap().2.clone()),
					s: String::new()
				});
			},

			//push working precision
			'W' => {
				MSTK.push(Obj {
					t: false,
					n: Float::with_val(WPREC, WPREC),
					s: String::new()
				});
			},

			//create new k,i,o context
			'{' => {
				ENVSTK.push((KDEF.clone(), IDEF.clone(), ODEF.clone()));
			},

			//revert to previous context
			'}' => {
				ENVSTK.pop();
				if ENVSTK.is_empty() {
					ENVSTK.push((KDEF.clone(), IDEF.clone(), ODEF.clone()));	//ensure 1 entry always remains
				}
			},
			/*--------------------------
				REGISTERS AND MACROS
			--------------------------*/
			//save to top of register
			's' => {
				if MSTK.len()>=1 {
					let a=MSTK.pop().unwrap();					
					if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
						eprintln!("! No register number provided");
					}
					else {
						let ri = if DRS_EN {
							DRS_EN = false;
							DRS
						}
						else {
							cmdstk.last_mut().unwrap().pop().unwrap() as usize
						};
						if REGS_SIZE>ri {
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
				else {
					eprintln!("! Nothing to save to register");
					if !DRS_EN&&!cmdstk.last().unwrap().is_empty() {
						cmdstk.last_mut().unwrap().pop();	//remove register name
					}
					DRS_EN = false;	//invalidate DRS
				}
			},

			//push to top of register
			'S' => {
				if MSTK.len()>=1 {
					let a=RegObj {
						o: MSTK.pop().unwrap(),
						a: Vec::new()
					};
					if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
						eprintln!("! No register number provided");
					}
					else {
						let ri = if DRS_EN {
							DRS_EN = false;
							DRS
						}
						else {
							cmdstk.last_mut().unwrap().pop().unwrap() as usize
						};
						if REGS_SIZE>ri {
							REGS[ri].push(a);
						}
						else {
							eprintln!("! Register {} is not available", ri);
						}
					}
				}
				else {
					eprintln!("! Nothing to push to register");
					if !DRS_EN&&!cmdstk.last().unwrap().is_empty() {
						cmdstk.last_mut().unwrap().pop();	//remove register name
					}
					DRS_EN = false;	//invalidate DRS
				}
			},

			//load from top of register
			'l' => {
				if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
					eprintln!("! No register number provided");
				}
				else {
					let ri = if DRS_EN {
						DRS_EN = false;
						DRS
					}
					else {
						cmdstk.last_mut().unwrap().pop().unwrap() as usize
					};
					if REGS_SIZE>ri {
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
				if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
					eprintln!("! No register number provided");
				}
				else {
					let ri = if DRS_EN {
						DRS_EN = false;
						DRS
					}
					else {
						cmdstk.last_mut().unwrap().pop().unwrap() as usize
					};
					if REGS_SIZE>ri {
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
				if MSTK.len()>=2 {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
							eprintln!("! No register number provided");
						}
						else {
							let ri = if DRS_EN {
								DRS_EN = false;
								DRS
							}
							else {
								cmdstk.last_mut().unwrap().pop().unwrap() as usize
							};
							if REGS_SIZE>ri {
								if REGS[ri].is_empty() {
									REGS[ri].push(RegObj {
										o: Obj {
											t: false,
											n: Float::with_val(WPREC, 0),	//create default register object if empty
											s: String::new()
										},
										a: Vec::new()
									});
								}
								let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
								if let Some(rai) = int.to_usize() {
									if rai>=REGS[ri].last().unwrap().a.len() {
										REGS[ri].last_mut().unwrap().a.resize(rai+1, Obj {
											t: false,
											n: Float::with_val(WPREC, 0),	//extend if required, initialize with default objects
											s: String::new()
										});
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
				else {
					eprintln!("! Saving to an array requires an object and an index");
					if !DRS_EN&&!cmdstk.last().unwrap().is_empty() {
						cmdstk.last_mut().unwrap().pop();	//remove register name
					}
					DRS_EN = false;	//invalidate DRS
				}
			},

			//load from top-of-register's array
			';' => {
				if MSTK.len()>=1 {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
							eprintln!("! No register number provided");
						}
						else {
							let ri = if DRS_EN {
								DRS_EN = false;
								DRS
							}
							else {
								cmdstk.last_mut().unwrap().pop().unwrap() as usize
							};
							if REGS_SIZE>ri {
								if REGS[ri].is_empty() {
									REGS[ri].push(RegObj {
										o: Obj {
											t: false,
											n: Float::with_val(WPREC, 0),	//create default register object if empty
											s: String::new()
										},
										a: Vec::new()
									});
								}
								let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
								if let Some(rai) = int.to_usize() {
									if rai>=REGS[ri].last().unwrap().a.len() {
										REGS[ri].last_mut().unwrap().a.resize(rai+1, Obj {
											t: false,
											n: Float::with_val(WPREC, 0),	//extend if required, initialize with default objects
											s: String::new()
										});
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
				else {
					eprintln!("! Loading from an array requires an index");
					if !DRS_EN&&!cmdstk.last().unwrap().is_empty() {
						cmdstk.last_mut().unwrap().pop();	//remove register name
					}
					DRS_EN = false;	//invalidate DRS
				}
			},

			//load top-of-reg into buffer
			'j' => {
				if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
					eprintln!("! No register number provided");
				}
				else {
					let ri = if DRS_EN {
						DRS_EN = false;
						DRS
					}
					else {
						cmdstk.last_mut().unwrap().pop().unwrap() as usize
					};
					if REGS_SIZE>ri {
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
				if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
					eprintln!("! No register number provided");
				}
				else {
					let ri = if DRS_EN {
						DRS_EN = false;
						DRS
					}
					else {
						cmdstk.last_mut().unwrap().pop().unwrap() as usize
					};
					if REGS_SIZE>ri {
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
				if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
					eprintln!("! No register number provided");
				}
				else {
					let ri = if DRS_EN {
						DRS_EN = false;
						DRS
					}
					else {
						cmdstk.last_mut().unwrap().pop().unwrap() as usize
					};
					if REGS_SIZE>ri {
						if !REGS[ri].is_empty() {
							REGS[ri].pop();	//remove old top, effectively overwrite
						}
						REGS[ri].push(RO_BUF[0].clone());
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//push buffer to register
			'H' => {
				if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
					eprintln!("! No register number provided");
				}
				else {
					let ri = if DRS_EN {
						DRS_EN = false;
						DRS
					}
					else {
						cmdstk.last_mut().unwrap().pop().unwrap() as usize
					};
					if REGS_SIZE>ri {
						REGS[ri].push(RO_BUF[0].clone());
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//push register depth
			'Z' => {
				if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
					eprintln!("! No register number provided");
				}
				else {
					let ri = if DRS_EN {
						DRS_EN = false;
						DRS
					}
					else {
						cmdstk.last_mut().unwrap().pop().unwrap() as usize
					};
					if REGS_SIZE>ri {
						MSTK.push(Obj {
							t: false,
							n: Float::with_val(WPREC, REGS[ri].len()),
							s: String::new()
						});
					}
					else {
						eprintln!("! Register {} is not available", ri);
					}
				}
			},

			//specify manual register index
			',' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(ri) = int.to_usize() {
							if REGS_SIZE>ri {
								DRS = ri;
								DRS_EN = true;
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
					let mut a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {
							if a.s.is_empty() {
								eprintln!("! Cannot convert empty string to number");
							}
							else {
								MSTK.push(Obj {
									t: false,
									n: Float::with_val(WPREC, a.s.remove(0) as u32),
									s: String::new()
								});
							}
						}
						else {
							MSTK.push(Obj {
								t: true,
								n: flt_def(),
								s: String::from(char::from_u32_unchecked(a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0.to_u32_wrapping()))
							});
						}
					}
				}
			},

			//convert number to UTF-8 string or back
			'A' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, Integer::from_digits(a.s.as_bytes(), Order::Msf)),
								s: String::new()
							});
						}
						else {
							if let Ok(res) = String::from_utf8(a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0.to_digits::<u8>(Order::Msf)) {
								MSTK.push(Obj {
									t: true,
									n: flt_def(),
									s: res
								});
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
					let a=MSTK.pop().unwrap();
					if a.t {
						if cmdstk.last().unwrap().is_empty() {
							cmdstk.pop();	//optimize tail call
						}
						cmdstk.push(a.s.chars().rev().collect());
					}
					else {
						MSTK.push(a);
					}
				}
			},

			//invert next conditional
			'!' => {
				inv = true;
			},

			//conditionally execute macro
			'<'|'='|'>' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();	//deliberately reverse order
					let b=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						let mut mac = String::new();
						if cmdstk.last().unwrap().is_empty()&&!DRS_EN {
							eprintln!("! No register name provided");
						}
						else {
							let ri = if DRS_EN {
								DRS_EN = false;
								DRS
							}
							else {
								cmdstk.last_mut().unwrap().pop().unwrap() as usize
							};
							if REGS_SIZE>ri {
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
						}
						if !mac.is_empty() {
							if inv != match cmd {
								'<' => { a.n < b.n },
								'=' => { a.n == b.n },
								'>' => { a.n > b.n },
								_ => {false},
							}
							{
								if cmdstk.last().unwrap().is_empty() {
									cmdstk.pop();	//optimize tail call
								}
								cmdstk.push(mac.chars().rev().collect());
							}
						}
					}
					else {
						if !DRS_EN&&!cmdstk.last().unwrap().is_empty() {
							cmdstk.last_mut().unwrap().pop();	//remove register name
						}
						DRS_EN = false;	//invalidate DRS
					}
				}
				else {
					if !DRS_EN&&!cmdstk.last().unwrap().is_empty() {
						cmdstk.last_mut().unwrap().pop();	//remove register name
					}
					DRS_EN = false;	//invalidate DRS
				}
				inv = false;
			},

			//auto-macro
			'X' => {
				if check_n(cmd, MSTK.len()) {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						let int = b.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(reps) = int.to_usize() {
							if cmdstk.last().unwrap().is_empty() {
								cmdstk.pop();	//optimize tail call
							}
							cmdstk.resize(cmdstk.len()+reps, a.s.chars().rev().collect());
						}
						else {
							eprintln!("! Invalid macro repeat count: {}", int);
						}
					}
				}
			},

			//quit dcim
			'q' => {
				std::process::exit(0);
			},

			//quit a macro calls
			'Q' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap_or(INT_ORD_DEF).0;
						if let Some(mut num) = int.to_usize() {
							if num>cmdstk.len() {num=cmdstk.len();}
							cmdstk.truncate(cmdstk.len()-num);
							if cmdstk.is_empty() {
								cmdstk.push(String::new());	//guarantee at least one element
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
				cmdstk.push(prompt_in.chars().rev().collect());
			},

			//execute file as script
			'&' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						match std::fs::read_to_string(a.s.clone()) {
							Ok(script) => {
								let mut script_nc = String::new();	//script with comments removed
								for line in script.split('\n') {
									script_nc.push_str(line.split_once('#').unwrap_or((line,"")).0);	//remove comment on every line
									script_nc.push('\n');
								}
								cmdstk.push(script_nc.chars().rev().collect());
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
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						for (var, val) in std::env::vars() {
							if var==a.s {
								MSTK.push(Obj {
									t: true,
									n: flt_def(),
									s: val
								});
							}
						}
					}
				}
			},

			//execute os command(s)
			'\\' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						for oscmd in a.s.split(';') {
							let mut args: Vec<&str> = oscmd.trim().split(' ').collect();
							match std::process::Command::new(args.remove(0)).args(args).spawn() {
								Ok(mut child) => {
									let _ = child.wait();
								},
								Err(error) => {
									eprintln!("! Unable to execute command \"{}\": {}", oscmd, error);
								},
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
				if !cmd.is_whitespace()&&cmd!=']' { eprintln!("! Invalid command: {}", cmd); }
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
