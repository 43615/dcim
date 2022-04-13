use rug::{Integer, integer::Order, Float, float::{Round, Constant}, ops::Pow, rand::RandState};
use std::io::{stdin, stdout, Write};
use std::time::{SystemTime, Duration};

//environment parameters with default values:
const KDEF: i32 = -1;	//output precision
const IDEF: i32 = 10;	//input base
const ODEF: i32 = 10;	//output base
static mut ENVSTK: Vec<(i32, i32, i32)> = Vec::new();	//stores (k,i,o) tuples, used by '{' and '}'
static mut WPREC: u32 = 256;	//wokring precision (rug Float mantissa length)

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

static mut QLEVEL: usize = 0;	//for exiting macros

fn main() {
	unsafe{
		ENVSTK.push((KDEF, IDEF, ODEF));	//initialize env params
		REGS.resize(REGS_SIZE, Vec::new());	//initialize registers
	}

	//initialize RNG with system time (* PID for a bit less predictability)
	let mut rng = RandState::new();
	rng.seed(&(Integer::from(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or(Duration::MAX).as_nanos()) * std::process::id()));

	interactive_mode(&mut rng);
}

//interactive/shell mode, the default
fn interactive_mode(mut rng: &mut RandState) {
	//prompt loop
	loop {
		//prompt for user input
		print!("> ");
		stdout().flush().unwrap();
		let mut input = String::new();
		stdin().read_line(&mut input).expect("Unable to read input");

		input = input.trim_end_matches(char::is_whitespace).to_owned();		//trim trailing LF

		unsafe {
			exec(input, &mut rng);
		}
	}
}

//checks if n operands are sufficient for operator op (defines adicity)
fn check_n(op: char, n: usize) -> bool {
	if match op {
		//triadic
		'|' => n>=3,

		//dyadic
		'+'|'-'|'*'|'/'|'^'|'V'|'G'|'%'|'~'|':'|'='|'<'|'>' => n>=2,

		//monadic unless specified
		_ => n>=1,
	}
	{ true }
	else {
		eprintln!("! Insufficient operands for operator \"{}\"", op);
		false
	}
}

//checks if an operator can be used on provided operand types
//a-c: types (.t) of the operands that would be used (in canonical order), use false if not required
fn check_t(op: char, a: bool, b: bool, c: bool) -> bool {
	if match op {
		//'+' can also concatenate strings
		'+' => (!a&&!b)||(a&&b),

		//these can operate on strings with a number
		'-'|'*'|'/' => (!a&&!b)||(a&&!b),

		//constant/conversion factor lookup by string name
		'"' => a,

		//arrays can store numbers and strings
		':' => (!a&&!b)||(a&&!b),

		//strings can be executed
		'x' => !a||a,

		//all other ops can only have numbers
		_ => !a&&!b&&!c,
	}
	{ true }
	else {
		eprintln!("! Invalid operand types for operator \"{}\"", op);
		false
	}
}

//library of constants and unit conversion factors
//unless specified, unit factors are based on the most prevalent international standard units for their respective quantities
//ex: "in" (inch) returns 0.0254, thus executing 20[in]"* converts 20 inches to meters (0.508)
fn constants(prec: u32, key: String) -> Option<Float> {
	let ten = Float::with_val(prec, 10);
	let pi = Float::with_val(prec, Constant::Pi);
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
		"hbar" => {Some(Float::with_val(prec, 6.62607015)*ten.pow(-34)/(2*pi))}
		"G" => {Some(Float::with_val(prec, 6.674))}
		"qe" => {Some(Float::with_val(prec, 1.602176634)*ten.pow(-19))}
		"NA" => {Some(Float::with_val(prec, 6.02214076)*ten.pow(23))}
		"kB" => {Some(Float::with_val(prec, 1.380649)*ten.pow(-23))}
		"u" => {Some(Float::with_val(prec, 1.660539066)*ten.pow(-27))}
		"lp" => {Some(Float::with_val(prec, 1.6162)*ten.pow(-35))}
		"tp" => {Some(Float::with_val(prec, 5.391)*ten.pow(-44))}
		"mp" => {Some(Float::with_val(prec, 2.1764)*ten.pow(-8))}
		"Tp" => {Some(Float::with_val(prec, 1.4167)*ten.pow(32))}
		/*------------------
			LENGTH UNITS
		------------------*/
		"in" => {Some(Float::with_val(prec, 0.0254))}
		"ft" => {Some(Float::with_val(prec, 0.3048))}
		"yd" => {Some(Float::with_val(prec, 0.9144))}
		"mi" => {Some(Float::with_val(prec, 1609.344))}
		"nmi" => {Some(Float::with_val(prec, 1852))}
		"AU" => {Some(Float::with_val(prec, 149597870700i64))}
		"ly" => {Some(Float::with_val(prec, 9460730472580800i64))}
		"pc" => {Some(Float::with_val(prec, 96939420213600000i64)/pi)}
		/*----------------
			MASS UNITS
		----------------*/
		"ct" => {Some(Float::with_val(prec, 0.0002))}
		"oz" => {Some(Float::with_val(prec, 0.028349523125))}
		"lb" => {Some(Float::with_val(prec, 0.45359237))}
		"st" => {Some(Float::with_val(prec, 6.35029318))}
		/*----------------
			TIME UNITS
		----------------*/
		"min" => {Some(Float::with_val(prec, 60))}
		"h" => {Some(Float::with_val(prec, 3600))}
		"d" => {Some(Float::with_val(prec, 86400))}
		"w" => {Some(Float::with_val(prec, 604800))}
		_ => {
			eprintln!("! Constant/factor \"{}\" not found", key);
			None
		}
	}
}

//custom printing function
fn flt_to_str(num: Float, obase: i32, oprec: i32) -> String {
	let ipart = num.clone().to_integer_round(Round::Zero).unwrap().0.to_string_radix(obase);
	let ilen = if ipart.starts_with('-') { ipart.len()-1 } else { ipart.len() };
	if num.is_zero() {
		"0".to_string()	//zero would get trimmed to ""
	}
	else {
		num.to_string_radix(
			obase,
			if oprec>=0 { Some(oprec as usize + ilen) } else { None }
			).trim_end_matches('0').trim_end_matches('.').to_string()
	}
}

//core execution engine
//unsafe for accessing static mut objects across different runs regardless of where it's called from
//single-threaded so idgaf
unsafe fn exec(mut cmds: String, mut rng: &mut RandState) {
	
	'STOP_EXEC: while !cmds.is_empty() {
		if QLEVEL>0 { break 'STOP_EXEC; }	//exit prematurely
		let mut cmd = cmds.remove(0);	//isolate first character as command

		//defines behavior of all commands
		match cmd {
			/*------------------
				OBJECT INPUT
			------------------*/
			//number input, force with single quote to use letters
			'0'..='9'|'.'|'_'|'\'' => {
				let mut numstr = String::new();	//gets filled with number to be parsed later
				let mut frac = false;	//'.' has already occurred
				let mut neg = false;	//'_' has already occurred
				let mut alpha = false;	//letters are used
				if cmd == '\'' {
					alpha = true;
					cmd = if cmds.is_empty() {' '} else {cmds.remove(0)};
				}
				//keep adding to numstr until number is finished
				'STDNUM_FINISHED: loop {
					//numbers and periods
					if cmd.is_ascii_digit()||cmd == '.' {
						if cmd =='.' { if frac { break 'STDNUM_FINISHED; } else { frac = true; } } //break on encountering second '.'
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
					cmd = if cmds.is_empty() {' '} else {cmds.remove(0)};
				}
				cmds.insert(0, cmd);	//restore first char that isn't part of the number
				if numstr.starts_with(".")||numstr.starts_with("-.") { numstr = numstr.replace(".", "0."); }	//add implied zero before fractional separator
				if numstr.ends_with(".")||numstr.ends_with("-")||numstr.is_empty() { numstr = "0".to_string(); }	//assume 0 if no digits provided
				if let Ok(res) = Float::parse_radix(numstr.clone(), ENVSTK.last().unwrap().1) {		
					MSTK.push(Obj {
						t: false,
						n: Float::with_val(WPREC, res),
						s: String::new()
					});
				}
				else {
					eprintln!("! Unable to parse number \"{}\"", numstr);
				}
			},

			//string input
			'[' => {
				let mut res = String::new();	//result string
				let mut nest: usize = 1;	//nesting level
				let mut err = false;
				if !cmds.is_empty() {
					cmd = cmds.remove(0);
					while nest>0 {
						res.push(cmd);
						if cmd == '[' { nest+=1; }
						if cmd == ']' { nest-=1; }
						if cmds.is_empty() {
							if nest>0 {
								//only reached on improper string
								eprintln!("! Unable to parse string \"{}\": missing closing bracket", res);
								err = true;
								nest = 0;
							}
						}
						else { cmd = cmds.remove(0); }
					}
					cmds.insert(0, cmd);	//restore first char that isn't part of the string
					if !err {
						MSTK.push(Obj {
							t: true,
							n: Float::new(WPREC),
							s: res.trim_end_matches(']').to_string()
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
						println!("{}", MSTK.last().unwrap().s.clone());
					}
					else {
						println!("{}", flt_to_str(MSTK.last().unwrap().n.clone(), ENVSTK.last().unwrap().2, ENVSTK.last().unwrap().0));						
					}
				}
			},

			//print full stack top to bottom
			'f' => {
				if !MSTK.is_empty() {
					for i in (0..MSTK.len()).rev() {
						if MSTK[i].t {
							println!("{}", MSTK[i].s.clone());
						}
						else {
							println!("{}", flt_to_str(MSTK[i].n.clone(), ENVSTK.last().unwrap().2, ENVSTK.last().unwrap().0));
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
						print!("{}", flt_to_str(a.n, ENVSTK.last().unwrap().2, ENVSTK.last().unwrap().0));
						stdout().flush().unwrap();
					}
				}
			},

			//pop, convert if number, print without newline
			'P' => {
				if check_n(cmd, MSTK.len()) {
					let a = MSTK.pop().unwrap();
					if a.t {
						print!("{}", a.s);
						stdout().flush().unwrap();
					}
					else {
						if let Ok(res) = String::from_utf8(a.n.to_integer_round(Round::Zero).unwrap().0.to_digits::<u8>(Order::Msf)) {
							print!("{}", res);
							stdout().flush().unwrap();
						}
						else {
							eprintln!("! Unable to convert number {} to string: not a valid UTF-8 sequence", a.n.to_integer_round(Round::Zero).unwrap().0);
						}
					}
				}
			},

			//print register
			'F' => {
				if cmds.is_empty() {
					eprintln!("! No register name provided");
				}
				else {
					let rn = cmds.remove(0);
					let ri = rn as usize;
					if REGS_SIZE>ri {
						if !REGS[ri].is_empty(){
							for i in (0..REGS[ri].len()).rev() {
								if REGS[ri][i].o.t {
									println!("{}", REGS[ri][i].o.s.clone());
								}
								else {
									println!("{}", flt_to_str(REGS[ri][i].o.n.clone(), ENVSTK.last().unwrap().2, ENVSTK.last().unwrap().0));
								}
							}
						}
					}
					else {
						eprintln!("! Register '{}'({}) is not available", rn, ri);
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
								n: Float::new(WPREC),
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
							let mut newstr = a.s;
							let int = b.n.to_integer_round(Round::Zero).unwrap().0;	//extract b, keep for checking if negative
							if let Some(mut num) = int.clone().abs().to_usize() {
								if num>newstr.len() { num = newstr.len(); }	//account for too large b
								if int<0 {
									//if b is negative, remove from front
									newstr = newstr.chars().rev().collect();
									newstr.truncate(newstr.len()-num);
									newstr = newstr.chars().rev().collect();
								}
								else {
									newstr.truncate(newstr.len()-num);
								}
							}
							else {
								eprintln!("! Cannot possibly remove {} characters from a string", int);
							}
							MSTK.push(Obj {
								t: true,
								n: Float::new(WPREC),
								s: newstr
							});
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
							let int = b.n.to_integer_round(Round::Zero).unwrap().0;	//extract b, keep for checking if negative
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
								n: Float::new(WPREC),
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
							let mut newstr = a.s;
							let int = b.n.to_integer_round(Round::Zero).unwrap().0;	//extract b, keep for checking if negative
							if let Some(num) = int.clone().abs().to_usize() {								
								if int<0 {
									//if b is negative, remove from front
									newstr = newstr.chars().rev().collect();
									newstr.truncate(num);
									newstr = newstr.chars().rev().collect();
								}
								else {
									newstr.truncate(num);
								}
							}
							else {
								eprintln!("! Cannot possibly shorten a string to {} characters", int);
							}
							MSTK.push(Obj {
								t: true,
								n: Float::new(WPREC),
								s: newstr
							});
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
						let ia = a.n.to_integer_round(Round::Zero).unwrap().0;
						let ib = b.n.to_integer_round(Round::Zero).unwrap().0;
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

			//euclidean division
			'~' => {
				if check_n(cmd, MSTK.len()) {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						let ia = a.n.to_integer_round(Round::Zero).unwrap().0;
						let ib = b.n.to_integer_round(Round::Zero).unwrap().0;
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
						let ia = a.n.to_integer_round(Round::Zero).unwrap().0;
						let ib = b.n.to_integer_round(Round::Zero).unwrap().0;
						let ic = c.n.to_integer_round(Round::Zero).unwrap().0;
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
						let int = a.n.to_integer_round(Round::Zero).unwrap().0;
						if int<=0 {
							eprintln!("! Upper bound for random value must be above 0");
						}
						else {
							MSTK.push(Obj {
								t: false,
								n: Float::with_val(WPREC, int.random_below(rng)),
								s: String::new()
							});
						}
					}
				}
			},

			//constant/conversion factor lookup
			'"' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						MSTK.push(Obj {
							t: false,
							n: Float::with_val(WPREC,
								if let Some(num) = constants(WPREC, a.s) { num }
								else { Float::with_val(WPREC, 1) }),
							s: String::new()
						});
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
						let int = a.n.to_integer_round(Round::Zero).unwrap().0;
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
						let int = a.n.to_integer_round(Round::Zero).unwrap().0;
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
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					MSTK.push(b);
					MSTK.push(a);
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
						let mut int = a.n.to_integer_round(Round::Zero).unwrap().0;
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
						let int = a.n.to_integer_round(Round::Zero).unwrap().0;
						if let Some(num) = int.to_i32() {
							ENVSTK.last_mut().unwrap().0 = num;
						}
						else {
							eprintln!("! Output precision must be between {} and {} (inclusive)", i32::MIN, i32::MAX);
						}
					}
				}
			},

			//set input base
			'i' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap().0;
						if int>=2 && int<=36 {
							ENVSTK.last_mut().unwrap().1 = int.to_i32().unwrap();
						}
						else {
							eprintln!("! Input base must be between {} and {} (inclusive)", 2, 36);
						}
					}
				}
			},

			//set output base
			'o' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap().0;
						if int>=2 && int<=36 {
							ENVSTK.last_mut().unwrap().2 = int.to_i32().unwrap();
						}
						else {
							eprintln!("! Output base must be between {} and {} (inclusive)", 2, 36);
						}
					}
				}
			},

			//set working precision
			'w' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap().0;
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
					n: Float::with_val(WPREC, ENVSTK.last().unwrap().0),
					s: String::new()
				});
			},

			//push input base
			'I' => {
				MSTK.push(Obj {
					t: false,
					n: Float::with_val(WPREC, ENVSTK.last().unwrap().1),
					s: String::new()
				});
			},

			//push output base
			'O' => {
				MSTK.push(Obj {
					t: false,
					n: Float::with_val(WPREC, ENVSTK.last().unwrap().2),
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
				ENVSTK.push((KDEF, IDEF, ODEF));
			},

			//revert to previous context
			'}' => {
				ENVSTK.pop();
				if ENVSTK.is_empty() {
					ENVSTK.push((KDEF, IDEF, ODEF));	//ensure 1 entry always remains
				}
			},
			/*--------------------------
				REGISTERS AND MACROS
			--------------------------*/
			//save to top of register
			's' => {
				if check_n(cmd, MSTK.len()) {
					let a=RegObj {
						o: MSTK.pop().unwrap(),
						a: Vec::new()
					};
					if cmds.is_empty() {
						eprintln!("! No register name provided");
					}
					else {
						let rn = cmds.remove(0);
						let ri = rn as usize;
						if REGS_SIZE>ri {
							if !REGS[ri].is_empty() {
								REGS[ri].pop();	//remove old top, effectively overwrite
							}
							REGS[ri].push(a);
						}
						else {
							eprintln!("! Register '{}'({}) is not available", rn, ri);
						}
					}
				}
				else {
					eprintln!("! Nothing to save to register");
				}
			},

			//push to top of register
			'S' => {
				if check_n(cmd, MSTK.len()) {
					let a=RegObj {
						o: MSTK.pop().unwrap(),
						a: Vec::new()
					};
					if cmds.is_empty() {
						eprintln!("! No register name provided");
					}
					else {
						let rn = cmds.remove(0);
						let ri = rn as usize;
						if REGS_SIZE>ri {
							REGS[ri].push(a);
						}
						else {
							eprintln!("! Register '{}'({}) is not available", rn, ri);
						}
					}
				}
				else {
					eprintln!("! Nothing to push to register");
				}
			},

			//load from top of register
			'l' => {
				if cmds.is_empty() {
					eprintln!("! No register name provided");
				}
				else {
					let rn = cmds.remove(0);
					let ri = rn as usize;
					if REGS_SIZE>ri {
						if REGS[ri].is_empty() {
							eprintln!("! Register '{}'({}) is empty", rn, ri);
						}
						else {
							MSTK.push(REGS[ri].last().unwrap().o.clone());
						}
					}
					else {
						eprintln!("! Register '{}'({}) is not available", rn, ri);
					}
				}
			},

			//pop from top of register
			'L' => {
				if cmds.is_empty() {
					eprintln!("! No register name provided");
				}
				else {
					let rn = cmds.remove(0);
					let ri = rn as usize;
					if REGS_SIZE>ri {
						if REGS[ri].is_empty() {
							eprintln!("! Register '{}'({}) is empty", rn, ri);
						}
						else {
							MSTK.push(REGS[ri].pop().unwrap().o);
						}
					}
					else {
						eprintln!("! Register '{}'({}) is not available", rn, ri);
					}
				}
			},

			//save to top-of-register's array
			':' => {
				if MSTK.len()>=2 {
					let b=MSTK.pop().unwrap();
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						if cmds.is_empty() {
							eprintln!("! No register name provided");
						}
						else {
							let rn = cmds.remove(0);
							let ri = rn as usize;
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
								let int = b.n.to_integer_round(Round::Zero).unwrap().0;
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
								eprintln!("! Register '{}'({}) is not available", rn, ri);
							}
						}
					}
				}
				else {
					eprintln!("! Saving to an array requires an object and an index");
				}
			},

			//load from top-of-register's array
			';' => {
				if MSTK.len()>=1 {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if cmds.is_empty() {
							eprintln!("! No register name provided");
						}
						else {
							let rn = cmds.remove(0);
							let ri = rn as usize;
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
								let int = a.n.to_integer_round(Round::Zero).unwrap().0;
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
								eprintln!("! Register '{}'({}) is not available", rn, ri);
							}
						}
					}
				}
			},

			//push register depth
			'Z' => {
				if cmds.is_empty() {
					eprintln!("! No register name provided");
				}
				else {
					let rn = cmds.remove(0);
					let ri = rn as usize;
					if REGS_SIZE>ri {
						MSTK.push(Obj {
							t: false,
							n: Float::with_val(WPREC, REGS[ri].len()),
							s: String::new()
						});
					}
					else {
						eprintln!("! Register '{}'({}) is not available", rn, ri);
					}
				}
			},

			//convert least significant 32 bits to one-char string or isolate first char of string
			'a' => {
				if check_n(cmd, MSTK.len()) {
					let mut a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						if a.t {
							MSTK.push(Obj {
								t: true,
								n: Float::new(WPREC),
								s: String::from(a.s.remove(0))
							});
						}
						else {
							MSTK.push(Obj {
								t: true,
								n: Float::new(WPREC),
								s: String::from(char::from_u32_unchecked(a.n.to_integer_round(Round::Zero).unwrap().0.to_u32_wrapping()))
							});
						}
					}
				}
			},

			//convert to string in 32-bit blocks
			'A' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let mut newstr = String::new();
						let chars = a.n.to_integer_round(Round::Zero).unwrap().0.to_digits::<u32>(Order::Msf);
						for i in 0..chars.len() {
							newstr.push(char::from_u32_unchecked(chars[i]));
						}
						MSTK.push(Obj {
							t: true,
							n: Float::new(WPREC),
							s: newstr
						})
					}
				}
			},

			//execute string as macro
			'x' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if a.t {
						exec(a.s, &mut rng);
					}
					else {
						MSTK.push(a);
					}
				}
			},

			//conditionally execute macro
			'!'|'<'|'='|'>' => {
				//handle inversion
				let inv = cmd=='!';
				if inv {
					if cmds.is_empty() {
						eprintln!("! Missing comparison operator after '!'");
						cmd = ' ';
					}
					else {
						cmd = cmds.remove(0);
					}
				}
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();	//deliberately reverse order
					let b=MSTK.pop().unwrap();
					if check_t(cmd, a.t, b.t, false) {
						let mut mac = String::new();
						if cmds.is_empty() {
							eprintln!("! No register name provided");
						}
						else {
							let rn = cmds.remove(0);
							let ri = rn as usize;
							if REGS_SIZE>ri {
								if REGS[ri].is_empty() {
									eprintln!("! Register '{}'({}) is empty", rn, ri);
								}
								else {
									mac = REGS[ri].last().unwrap().o.clone().s;	//get macro if possible
								}
							}
							else {
								eprintln!("! Register '{}'({}) is not available", rn, ri);
							}
						}
						if !mac.is_empty() {
							if inv != match cmd {
								'<' => { a.n < b.n },
								'=' => { a.n == b.n },
								'>' => { a.n > b.n },
								_ => {
									eprintln!("! Invalid comparison operator '{}'", cmd);
									true	//only possible with inv, which xors it to false
								},
							}
							{
								exec(mac, &mut rng);
							}
						}
					}
				}
			},

			//quit dcim
			'q' => {
				std::process::exit(0);
			},

			//set macro quit level
			'Q' => {
				if check_n(cmd, MSTK.len()) {
					let a=MSTK.pop().unwrap();
					if check_t(cmd, a.t, false, false) {
						let int = a.n.to_integer_round(Round::Zero).unwrap().0;
						if let Some(num) = int.to_usize() {
							QLEVEL = num;
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
				prompt_in = prompt_in.trim_end_matches(char::is_whitespace).to_owned();		//trim trailing LF
				exec(prompt_in, &mut rng);
			},

			//stop on comment
			'#' => {
				break 'STOP_EXEC;
			}

			//notify on invalid command, keep going
			_ => {
				if !cmd.is_whitespace()&&cmd!=']' { eprintln!("! Invalid command: {}", cmd); }
			},
		}
	}
	if QLEVEL>0 { QLEVEL-=1; }	//decrement when about to quit
}