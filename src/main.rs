use std::fmt;
use std::io::{stdout, Write};
use std::time::{SystemTime, Duration};
use std::collections::{HashSet, HashMap};
use rug::{Integer, integer::Order, Complete, Float, float::{Round, Constant, Special}, ops::Pow, rand::RandState, Assign};
use rand::{RngCore, rngs::OsRng};
use read_input::prelude::*;
#[macro_use]
extern crate lazy_static;

lazy_static! { static ref HELPMSG: &'static str = {
r##"╭─────────────────────────╮
│   ╷           •         │
│   │                     │
│ ╭─┤  ╭─╴  •  ╶┤   ┌─┬─╮ │
│ │ │  │        │   │ │ │ │
│ ╰─┘  ╰─╴  •  ╶┴╴  ╵   ╵ │
╰─────────────────────────╯

dc improved - Expanded rewrite of a classic RPN calculator / esoteric programming language
Core principles of GNU dc are preserved, full documentation at https://github.com/43615/dcim/wiki

Command line options:
(order/position of --flags doesn't matter)

<nothing>
	Defaults to "-i".

--inter|-i [PROMPT]
	Interactive mode, standard prompt-eval loop. A custom prompt may be provided, default is "> ".

--expr|-e [--inter|-i] EXPR1 [EXPR2] [EXPR3] ...
	Expression mode, executes expressions in order. If combined with -i, enters interactive mode after expressions are finished.

[--file|-f] [--inter|-i] FILE1 [FILE2] [FILE3] ...
	File mode, executes contents of files in order. May also be combined with -i.
	For each line in the file(s), comments (following the first #) are removed before execution.
	-f is optional: If at least one option is provided without any --flags, file mode is implied.

--help|-h
	Ignores all other options and prints this help message."##};}

///basic object: either number or string
#[derive(Clone)]
enum Obj {
	N(Float),
	S(String)
}
///unused Obj
const DUMMY: Obj = Obj::S(String::new());

///register object, may have a dynamic array
#[derive(Clone)]
struct RegObj {
	o: Obj,			//principal object
	a: Vec<Obj>,	//associated array
}

///all existing command argument signatures
#[derive(Clone, Copy)]
#[repr(u8)]
enum CmdSig {
	Nil,
	Ax,
	An,
	As,
	AxBx,
	AxBn,
	AnBn,
	AsBn,
	AxBxCx
}
impl CmdSig {
	///aka argument count
	fn adicity(&self) -> u8 {
		match self {
			Self::Nil => 0,
			Self::Ax|Self::An|Self::As => 1,
			Self::AxBx|Self::AxBn|Self::AnBn|Self::AsBn => 2,
			Self::AxBxCx => 3
		}
	}

	///english plural ending
	fn plural(&self) -> &str {
		if self.adicity()==1 {""} else {"s"}
	}

	///correction messages
	fn correct(&self) -> &str {
		match self {
			Self::Nil|Self::Ax => "",
			Self::An => "must be a number",
			Self::As => "must be a string",
			Self::AxBx => "must be two numbers or two strings",
			Self::AxBn => "2nd must be a number",
			Self::AnBn => "must be two numbers",
			Self::AsBn => "1st must be a string, 2nd must be a number",
			Self::AxBxCx => "must be three numbers or three strings",
		}
	}
}

///stack for (K,I,O) tuples, with methods for checked editing
struct ParamStk(Vec<(Integer, Integer, Integer)>);
impl ParamStk {
	///switch to new param context with defaults (-1,10,10)
	fn create(&mut self) {
		self.0.push((Integer::from(-1), Integer::from(10), Integer::from(10)));
	}
	///revert to previous context, reset to defaults if nonexistent
	fn destroy(&mut self) {
		self.0.pop();
		if self.0.is_empty() {self.create()}
	}

	///checked edit of current output precision
	fn set_k(&mut self, n: Integer) -> Result<(), ParamError> {
		if n>=-1 {
			self.0.last_mut().unwrap().0 = n;
			Ok(())
		}
		else {Err(ParamError::K)}
	}
	///checked edit of current input base
	fn set_i(&mut self, n: Integer) -> Result<(), ParamError> {
		if n>=2 {
			self.0.last_mut().unwrap().1 = n;
			Ok(())
		}
		else {Err(ParamError::I)}
	}
	///checked edit of current output base
	fn set_o(&mut self, n: Integer) -> Result<(), ParamError> {
		if n>=2 {
			self.0.last_mut().unwrap().2 = n;
			Ok(())
		}
		else {Err(ParamError::O)}
	}
	
	///current output precision
	fn k(&self) -> Integer {self.0.last().unwrap().0.clone()}
	///current input base
	fn i(&self) -> Integer {self.0.last().unwrap().1.clone()}
	///current output base
	fn o(&self) -> Integer {self.0.last().unwrap().2.clone()}
}
///printable errors of ParamStk setters
#[repr(u8)]
enum ParamError {
	K, I, O
}
impl fmt::Display for ParamError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::K => "! k: Output precision must be at least -1",
			Self::I => "! i: Input base must be at least 2",
			Self::O => "! o: Output base must be at least 2",
		})
	}
}

/*---------------------------
		STATE STORAGE
	oops, all static mut!
---------------------------*/
///main stack
static mut MSTK: Vec<Obj> = Vec::new();

const REG_DEF: Vec<RegObj> = Vec::new();	//required for init of REGS
///array of "low" registers, index limited to u16
static mut REGS: [Vec<RegObj>; 65536] = [REG_DEF; 65536];
///RegObj buffer: needs to be a vec because `new` is const, only \[0\] is used
static mut RO_BUF: Vec<RegObj> = Vec::new();
///non-contiguous storage of arbitrarily-named registers
static mut HIGH_REGS: RegMap = RegMap(Vec::new());

///u16 or bigint
enum RegIdx {
	Low(u16),
	High(Integer)
}
impl fmt::Display for RegIdx {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self {
			Self::Low(u) => Integer::from(*u),
			Self::High(i) => i.into()
		})
	}
}
///direct register selector
static mut DRS: Option<RegIdx> = None;

///random number generator: needs to be a vec because `new` is const, only \[0\] is used
static mut RNG: Vec<RandState> = Vec::new();

///environment parameters K,I,O
static mut PARAMS: ParamStk = ParamStk(Vec::new());
///working precision W (Float mantissa length)
static mut WPREC: u32 = 256;

///arbitrary register storage, wrapper to allow static mut initialization
struct RegMap(Vec<HashMap<Integer, Vec<RegObj>>>);

///discard fractional part if finite, default to 0 otherwise
fn int(n: Float) -> Integer {
	if let Some((i, _)) = n.to_integer_round(Round::Zero) {i} else {Integer::ZERO}
}

fn main() {
	//init everything that doesn't have a const constructor
	unsafe {
		PARAMS.create();	//initialize env params
		RO_BUF.push(RegObj{	//and RegObj buffer
			a: Vec::new(),
			o: DUMMY
		});
		HIGH_REGS.0.push(HashMap::new());
		//init RNG, seed with 1024 bits of OS randomness
		RNG.push(RandState::new());
		let mut seed = [0u8; 128];
		OsRng.fill_bytes(&mut seed);
		RNG[0].seed(&Integer::from_digits(&seed, Order::Msf));
	}

	//parse options
	let (mut i, mut e, mut f, mut h) = (false, false, false, false);
	let mut names: Vec<String> = Vec::new();
	let args: Vec<String> = std::env::args().skip(1).collect();	//get args, skip name of binary
	if args.is_empty() {i=true};	//default to interactive
	for arg in args {
		if let Some(flag) = arg.strip_prefix("--") {	//long option
			match flag {
				"inter" => {i=true;}
				"expr" => {e=true;}
				"file" => {f=true;}
				"help" => {h=true;}
				_ => {
					eprintln!("! Unrecognized option: --{flag}, use -h for help");
					std::process::exit(0);
				}
			}
			continue;
		}
		if arg.starts_with('-') {	//short option, multiple at once possible
			for flag in arg.chars() {
				match flag {
					'-' => {}	//allow -f-i or similar
					'i' => {i=true;}
					'e' => {e=true;}
					'f' => {f=true;}
					'h' => {h=true;}
					_ => {
						eprintln!("! Unrecognized option: -{flag}, use -h for help");
						std::process::exit(0);
					}
				}
			}
			continue;
		}
		names.push(arg);	//branchless ftw
	}
	
	if h {	//always exits
		println!("{}", *HELPMSG);
		std::process::exit(0);
	}
	match (i, e, f) {
		(false, false, false) => {file_mode(names, false);}	//no flags: assume filenames
		(true, false, false) => {interactive_mode(names.first().cloned());}	//normal interactive
		(_, true, false) => {expression_mode(names, i);}	//expr mode, pass i on
		(_, false, true) => {file_mode(names, i);}	//file mode, pass i on
		(_, true, true) => {eprintln!("! Invalid options: both -e and -f present");}	//invalid combination
	}
}

fn interactive_mode(prompt: Option<String>) {
	let inputter = input::<String>().repeat_msg(prompt.unwrap_or_else(|| "> ".into()));
	loop {
		unsafe{exec(inputter.get());}
	}
}

fn expression_mode(exprs: Vec<String>, inter: bool) {
	if exprs.is_empty() {
		eprintln!("! No expression provided");
	}
	else {
		for expr in exprs {
			unsafe{exec(expr);}
		}
	}
	if inter {
		interactive_mode(None);
	}
}

fn file_mode(files: Vec<String>, inter: bool) {
	if files.is_empty() {
		eprintln!("! No file name provided");
	}
	else {
		for file in files {
			match std::fs::read_to_string(&file) {
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
					eprintln!("! Unable to read file \"{file}\": {error}");
				},
			}
		}
	}
	if inter {
		interactive_mode(None);
	}
}

///Float generator for library of constants (known value, variable precision)
struct FltGen(Box<dyn Fn(u32) -> Float>);
unsafe impl Sync for FltGen {}
impl FltGen {
	///simple value
	fn val<T: 'static + Copy>(val: T) -> Self
	where Float: Assign<T> {
		Self(Box::new(move |prec: u32| Float::with_val(prec, val)))
	}

	///scientific notation
	fn sci<T: 'static + Copy, U: 'static + Copy>(man :T, exp: U) -> Self
	where Float: Assign<T> + Assign<U> {
		Self(Box::new(move |prec: u32| Float::with_val(prec, man)*Float::with_val(prec, exp).exp10()))
	}

	///recursive (based on other unit)
	fn rec(que: &'static str, fun: &'static dyn Fn(Float) -> Float) -> Self {
		Self(Box::new(move |prec: u32| fun(CONSTANTS.get(que).unwrap().0(prec))))
	}
}

lazy_static! {
	///all commands that use a register
	static ref USES_REG: HashSet<char> = {
		let mut s = HashSet::new();
		for c in "FsSlL:;bBZ<=>".chars() {s.insert(c);}
		s
	};
	///command signatures and type error messages
	static ref CMD_SIGS: HashMap<char, CmdSig> = {
		let mut m = HashMap::new();
		use CmdSig::*;

		for c in ['+','^'] {m.insert(c, AxBx);}

		m.insert('|', AxBxCx);

		for c in "-*/%~:".chars() {m.insert(c, AxBn);}

		for c in "&$\\".chars() {m.insert(c, As);}

		for c in "nPaA\"xgsS,".chars() {m.insert(c, Ax);}

		m.insert('X', AsBn);

		for c in "v°uytUYTNCDRkiow;Q".chars() {m.insert(c, An);}

		for c in "VG<=>".chars() {m.insert(c, AnBn);}

		m
	};
	///library of constants/conversion factors
	static ref CONSTANTS: HashMap<&'static str, FltGen> = {
		let mut m = HashMap::new();
		/*----------------------------
			MATHEMATICAL CONSTANTS
		----------------------------*/
		m.insert("e", FltGen(Box::new(|prec| Float::with_val(prec, 1).exp())));
		m.insert("pi", FltGen::val(Constant::Pi));
		m.insert("gamma", FltGen::val(Constant::Euler));
		m.insert("phi", FltGen(Box::new(|prec| (Float::with_val(prec, 5).sqrt()+1)/2)));
		for q in ["deg","°"] {m.insert(q, FltGen(Box::new(|prec| Float::with_val(prec, Constant::Pi)/180)));}
		for q in ["gon","grad"] {m.insert(q, FltGen(Box::new(|prec| Float::with_val(prec, Constant::Pi)/200)));}
		/*------------------------
			PHYSICAL CONSTANTS
		------------------------*/
		m.insert("c", FltGen::val(299792458));
		m.insert("hbar", FltGen(Box::new(|prec| FltGen::sci(662607015, -42).0(prec) / FltGen::rec("pi", &|n| n*2).0(prec))));
		m.insert("G", FltGen::sci(6674, -3));
		m.insert("qe", FltGen::sci(1602176634, -28));
		m.insert("NA", FltGen::sci(602214076, 31));
		m.insert("kB", FltGen::sci(1380649, -29));
		m.insert("u", FltGen::sci(1660539066, -36));
		m.insert("lp", FltGen::sci(16162, -39));
		m.insert("tp", FltGen::sci(5391, -47));
		m.insert("mp", FltGen::sci(21764, -12));
		m.insert("Tp", FltGen::sci(14167, 28));
		/*------------------
			LENGTH UNITS
		------------------*/
		m.insert("in", FltGen::sci(254, -4));
		m.insert("ft", FltGen::rec("in", &|n| n*12));
		m.insert("yd", FltGen::rec("ft", &|n| n*3));
		m.insert("m", FltGen::val(1));
		m.insert("fur", FltGen::rec("ft", &|n| n*660));
		m.insert("mi", FltGen::rec("ft", &|n| n*5280));
		m.insert("nmi", FltGen::val(1852));
		m.insert("AU", FltGen::val(149597870700i64));
		m.insert("ly", FltGen::val(9460730472580800i64));
		m.insert("pc", FltGen(Box::new(|prec| Float::with_val(prec, 96939420213600000i64)/Float::with_val(prec, Constant::Pi))));
		/*-------------------------------
			AREA & VOLUME UNITS
			with no length equivalent
		-------------------------------*/
		for q in ["ac","acre"] {m.insert(q, FltGen::sci(40468564224i64, -7));}
		m.insert("l", FltGen::sci(1, -3));
		m.insert("ifloz", FltGen::sci(284130625, -13));
		m.insert("ipt", FltGen::rec("ifloz", &|n| n*20));
		m.insert("iqt", FltGen::rec("ifloz", &|n| n*40));
		m.insert("igal", FltGen::rec("ifloz", &|n| n*160));
		for q in ["ibu","ibsh"] {m.insert(q, FltGen::rec("ifloz", &|n| n*1280));}
		m.insert("ufldr", FltGen::sci(36966911953125i64, -19));
		m.insert("tsp", FltGen::rec("ufldr", &|n| n/3*4));
		m.insert("tbsp", FltGen::rec("ufldr", &|n| n*4));
		m.insert("ufloz", FltGen::rec("ufldr", &|n| n*8));
		m.insert("upt", FltGen::rec("ufloz", &|n| n*16));
		m.insert("uqt", FltGen::rec("ufloz", &|n| n*32));
		m.insert("ugal", FltGen::rec("ufloz", &|n| n*128));
		m.insert("bbl", FltGen::rec("ugal", &|n| n*42));
		m.insert("udpt", FltGen::sci(5506104713575i64, -16));
		m.insert("udqt", FltGen::rec("udpt", &|n| n*2));
		m.insert("udgal", FltGen::rec("udpt", &|n| n*8));
		for q in ["ubu","ubsh"] {m.insert(q, FltGen::rec("udpt", &|n| n*64));}
		m.insert("dbbl", FltGen::sci(115627123584i64, -12));
		/*----------------
			MASS UNITS
		----------------*/
		m.insert("ct", FltGen::sci(2, -4));
		m.insert("oz", FltGen::sci(28349523125i64, -12));
		m.insert("lb", FltGen::rec("oz", &|n| n*16));
		m.insert("kg", FltGen::val(1));
		m.insert("st", FltGen::rec("lb", &|n| n*14));
		m.insert("t", FltGen::rec("lb", &|n| n*2240));
		/*----------------
			TIME UNITS
		----------------*/
		m.insert("s", FltGen::val(1));
		m.insert("min", FltGen::val(60));
		m.insert("h", FltGen::rec("min", &|n| n*60));
		m.insert("d", FltGen::rec("h", &|n| n*24));
		m.insert("w", FltGen::rec("d", &|n| n*7));
		m.insert("mo", FltGen::rec("d", &|n| n*30));
		m.insert("a", FltGen::rec("d", &|n| n*365));
		m.insert("aj", FltGen::rec("d", &|n| n*36525/100));
		m.insert("ag", FltGen::rec("d", &|n| n*3652425/10000));
		/*-----------------
			OTHER UNITS
		-----------------*/
		m.insert("J", FltGen::val(1));
		m.insert("cal", FltGen::sci(4184, -3));
		m.insert("Pa", FltGen::val(1));
		m.insert("atm", FltGen::val(101325));
		m.insert("psi", FltGen::sci(6894757293168i64, -9));
		m.insert("torr", FltGen::rec("atm", &|n| n/760));
		/*------------------------------
			SPECIAL VALUES/FUNCTIONS
		------------------------------*/
		m.insert("inf", FltGen::val(Special::Infinity));
		m.insert("ninf", FltGen::val(Special::NegInfinity));
		m.insert("nan", FltGen::val(Special::Nan));
		m.insert("pid", FltGen::val(std::process::id()));
		m.insert("author", FltGen::val(43615));	//yay numerical nicknames!
		m
	};
}

///calculate value with given precision, apply scale prefix and power suffix
fn get_constant(prec: u32, query: &str) -> Option<Float> {
	let mut q = query.to_string();
	let mut scale = String::new();
	while q.starts_with(|c: char| c.is_ascii_digit()||c=='-') {
		scale.push(q.remove(0));	//extract scale prefix
	}
	if scale.is_empty()||scale.ends_with('-') {scale.push('0');}
	let s = Float::with_val(prec, Integer::parse(scale).unwrap().complete()).exp10();

	let mut power = String::new();
	while q.ends_with(|c: char| c.is_ascii_digit()) {
		power.insert(0, q.pop().unwrap());	//extract power suffix
	}
	if power.is_empty() {power.push('1');}
	let p = Float::with_val(prec, Integer::parse(power).unwrap().complete());

	if let Some(n) = CONSTANTS.get(q.as_str()).map(|c| c.0(prec)) {
		Some((s*n).pow(p))
	}
	else {
		match q.as_str() {	//non-constants and terminators are here to avoid execution by lazy_static::initialize
			"time" => {Some(Float::with_val(prec, SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or(Duration::ZERO).as_secs()))}
			"timens" => {Some(Float::with_val(prec, SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or(Duration::ZERO).as_nanos()))}
			"abort" => {std::process::abort();}
			"crash" => {get_constant(prec, "crash")}	//stack go brrrrr
			"panic" => {
				std::panic::panic_any(
					unsafe {
						if let Some(Obj::S(s)) = MSTK.last() {s} else {"Expected, [panic]\" was executed"}	//if top-of-stack is a string, display it
					}
				);
			}
			_ => None
		}
	}
}

///custom number printing function:
///if output base is over 36, prints in custom "any-base" notation,
///otherwise, applies precision like dc and converts from exponential notation if not too small
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
		if outstr[outstr.len().saturating_sub(11)..].contains('@') {	//efficiently check if in exponential notation
			let (mut mpart, epart) = outstr.rsplit_once('@').unwrap();
			mpart = mpart.trim_end_matches('0').trim_end_matches('.');	//remove trailing zeros from mantissa
			let eint = epart.parse::<i32>().unwrap();	//isolate exponential part
			if eint<0 && eint>-10 {
				outstr = "0.".to_string() + &"0".repeat(eint.unsigned_abs() as usize -1) + &mpart.replacen('.', "", 1);	//convert from exponential notation if not too small
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

///CORE EXECUTION ENGINE:
///unsafe for accessing static mut objects across different runs
unsafe fn exec(commands: String) {
	let mut cmdstk: Vec<String> = Vec::new();	//stack of reversed command strings to execute, enables pseudorecursive macro calls
	let mut inv = false;	//invert next comparison
	if !commands.is_empty() {	//loop expects contents, do nothing if none provided
		cmdstk.push(commands.chars().rev().collect());	//all command strings are reversed since pop() is O(1)
	}
	while !cmdstk.is_empty() {	//last().unwrap() is guaranteed to not panic within
	
		let mut cmd = cmdstk.last_mut().unwrap().pop().unwrap_or('\0');	//get next command

		let (reg, rnum): (&mut Vec<RegObj>, Integer) = if USES_REG.contains(&cmd) {	//get register reference, before the other checks since it's syntactically significant ("sq" etc)
			if let Some(d) = DRS.take() {	//use DRS
				match d {
					RegIdx::Low(u) => (&mut REGS[u as usize], Integer::from(u)),
					RegIdx::High(i) => {
						if !HIGH_REGS.0[0].contains_key(&i) {
							HIGH_REGS.0[0].insert(i.clone(), REG_DEF);	//touch reg
						}
						(HIGH_REGS.0[0].get_mut(&i).unwrap(), i)
					}
				}
			}
			else if let Some(c) = cmdstk.last_mut().unwrap().pop() {	//use next char
				if let Ok(u) = u16::try_from(c as u32) {	//within u16
					(&mut REGS[u as usize], u.into())
				}
				else {	//outside u16
					let i = Integer::from(c as u32);
					if !HIGH_REGS.0[0].contains_key(&i) {
						HIGH_REGS.0[0].insert(i.clone(), REG_DEF);	//touch reg
					}
					(HIGH_REGS.0[0].get_mut(&i).unwrap(), i)
				}
			}
			else {
				eprintln!("! Command '{cmd}' needs a register number");
				continue;
			}
		}
		else {(&mut REGS[0], Integer::ZERO)};	//not used

		let sig = CMD_SIGS.get(&cmd).unwrap_or(&CmdSig::Nil);
		let adi = sig.adicity();

		if MSTK.len() < adi as usize {	//check stack depth
			eprintln!("! Command '{cmd}' needs {} argument{}", adi, sig.plural());
			continue;
		}

		let (c, b, a) = match adi {	//pop required amount from stack
			1 => (DUMMY, DUMMY, MSTK.pop().unwrap()),
			2 => (DUMMY, MSTK.pop().unwrap(), MSTK.pop().unwrap()),
			3 => (MSTK.pop().unwrap(), MSTK.pop().unwrap(), MSTK.pop().unwrap()),
			_ => (DUMMY, DUMMY, DUMMY)
		};

		let (mut na, mut nb, mut nc) = (Float::new(1), Float::new(1), Float::new(1));	//number slots
		let (mut sa, mut sb, mut sc) = (String::new(), String::new(), String::new());	//string slots
		let mut svari = false;	//string variant of overloaded command is used

		if !(	//check and destructure Objs
			match sig {
				CmdSig::Nil => true,
				CmdSig::Ax => match &a {
					Obj::N(x) => {
						na = x.clone();
						true
					},
					Obj::S(x) => {
						sa = x.clone();
						svari = true;
						true
					}
				},
				CmdSig::An => match &a {
					Obj::N(x) => {
						na = x.clone();
						true
					},
					_ => false
				},
				CmdSig::As => match &a {
					Obj::S(x) => {
						sa = x.clone();
						true
					},
					_ => false
				},
				CmdSig::AxBx => match (&a, &b) {
					(Obj::N(x), Obj::N(y)) => {
						na = x.clone();
						nb = y.clone();
						true
					},
					(Obj::S(x), Obj::S(y)) => {
						sa = x.clone();
						sb = y.clone();
						svari = true;
						true
					},
					_ => false
				},
				CmdSig::AxBn => match (&a, &b) {
					(Obj::N(x), Obj::N(y)) => {
						na = x.clone();
						nb = y.clone();
						true
					},
					(Obj::S(x), Obj::N(y)) => {
						sa = x.clone();
						nb = y.clone();
						svari = true;
						true
					},
					_ => false
				},
				CmdSig::AnBn => match (&a, &b) {
					(Obj::N(x), Obj::N(y)) => {
						na = x.clone();
						nb = y.clone();
						true
					},
					_ => false
				},
				CmdSig::AsBn => match (&a, &b) {
					(Obj::S(x), Obj::N(y)) => {
						sa = x.clone();
						nb = y.clone();
						true
					},
					_ => false
				},
				CmdSig::AxBxCx => match (&a, &b, &c) {
					(Obj::N(x), Obj::N(y), Obj::N(z)) => {
						na = x.clone();
						nb = y.clone();
						nc = z.clone();
						true
					},
					(Obj::S(x), Obj::S(y), Obj::S(z)) => {
						sa = x.clone();
						sb = y.clone();
						sc = z.clone();
						svari = true;
						true
					},
					_ => false
				},
			}
		)
		{
			eprintln!("! Wrong argument type{} for command '{cmd}': {}", sig.plural(), sig.correct());
			match adi {	//push Objs back
				1 => {MSTK.push(a);},
				2 => {MSTK.push(a); MSTK.push(b);},
				3 => {MSTK.push(a); MSTK.push(b); MSTK.push(c);},
				_ => {}
			}
			continue;
		}

		match cmd {
			/*------------------
				OBJECT INPUT
			------------------*/
			//standard number input, force with single quote to use letters
			'0'..='9'|'.'|'_'|'\''|'@' => {
				if PARAMS.i()>36 {
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
					match Float::parse_radix(numstr.clone(), PARAMS.i().to_i32().unwrap()) {		
						Ok(res) => {
							MSTK.push(Obj::N(Float::with_val(WPREC, res)));
						},
						Err(error) => {
							eprintln!("! Unable to parse number \"{numstr}\": {error}");
						},
					}
				}
			},

			//any-base number input
			'(' => {
				let mut num = Integer::from(0);	//resulting number
				if cmdstk.last().unwrap().is_empty() {
					MSTK.push(Obj::N(Float::with_val(WPREC, num)));	//default to 0 if on end of input
				}
				else {
					let ibase = PARAMS.i();
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
									eprintln!("! Unable to parse any-base number: digit '{digint}' is too high for base {ibase}");
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
												eprintln!("! Unable to parse any-base number: invalid character '{cmd}' in exponent");
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
									MSTK.push(Obj::N(Float::with_val(WPREC, num * if neg {-1} else {1}) / scale
										* Float::with_val(WPREC, ibase).pow(escale)));
									break;
								}
							},
							_ => {
								eprintln!("! Invalid character in any-base number: '{cmd}'");
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
						MSTK.push(Obj::S(res));
						break;
					}
					if cmdstk.last().unwrap().is_empty() {	//only reached on improper string
						eprintln!("! Unable to parse string \"[{res}\": missing closing bracket");
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
					match MSTK.last().unwrap() {
						Obj::N(n) => {println!("{}", flt_to_str(n.clone(), PARAMS.o(), PARAMS.k()));},
						Obj::S(s) => {println!("[{s}]");},
					}
				}
			},

			//print full stack top to bottom
			'f' => {
				if !MSTK.is_empty() {
					for i in (0..MSTK.len()).rev() {
						match &MSTK[i] {
							Obj::N(n) => {println!("{}", flt_to_str(n.clone(), PARAMS.o(), PARAMS.k()));},
							Obj::S(s) => {println!("[{s}]");},
						}
					}
				}
			},

			//pop and print without newline
			'n' => {
				if !svari {
					print!("{}", flt_to_str(na, PARAMS.o(), PARAMS.k()));
					stdout().flush().unwrap();
				}
				else {
					print!("{sa}");
					stdout().flush().unwrap();
				}
			},

			//pop and print with newline
			'P' => {
					if !svari {println!("{}", flt_to_str(na, PARAMS.o(), PARAMS.k()));}
					else {println!("{sa}");}
			},

			//print register
			'F' => {
				if !reg.is_empty(){
					for i in (0..reg.len()).rev() {
						match &reg[i].o {
							Obj::N(n) => {println!("{}", flt_to_str(n.clone(), PARAMS.o(), PARAMS.k()));},
							Obj::S(s) => {println!("[{s}]");},
						}
						if !reg[i].a.is_empty() {
							let width = (reg[i].a.len()-1).to_string().len();	//length of longest index number
							for ai in 0..reg[i].a.len() {
								match &reg[i].a[ai] {
									Obj::N(n) => {println!("\t{ai:>width$}: {}", flt_to_str(n.clone(), PARAMS.o(), PARAMS.k()));},
									Obj::S(s) => {println!("\t{ai:>width$}: [{s}]");},
								}
							}
						}
					}
				}
			},
			/*----------------
				ARITHMETIC
				+str manip
			----------------*/
			//add or concatenate strings
			'+' => {
				if !svari {MSTK.push(Obj::N(Float::with_val(WPREC, na + nb)));}
				else {MSTK.push(Obj::S(sa + &sb));}
			},

			//subtract or remove chars from string
			'-' => {
				if !svari {MSTK.push(Obj::N(Float::with_val(WPREC, na - nb)));}
				else {
					let ib = int(nb);
					if let Some(n) = ib.clone().abs().to_usize() {
						MSTK.push(Obj::S(
							if ib<0 {	//remove from front
								sa.chars().skip(n).collect()
							}
							else {	//remove from back
								sa.chars().take(sa.chars().count().saturating_sub(n)).collect()
							}
						));
					}
					else {
						eprintln!("! -: Cannot possibly remove {ib} characters from a string");
						MSTK.push(a);
						MSTK.push(b);
					}
				}
			},

			//multiply or repeat/invert string
			'*' => {
				if !svari {MSTK.push(Obj::N(Float::with_val(WPREC, na * nb)));}
				else {
					let ib = int(nb);
					if let Some(n) = ib.clone().abs().to_usize() {
						MSTK.push(Obj::S(
							if ib<0 {	//repeat and reverse
								sa.chars().rev().collect::<String>().repeat(n)
							}
							else {	//repeat
								sa.repeat(n)
							}
						));
					}
					else {
						eprintln!("! *: Cannot possibly repeat a string {ib} times");
						MSTK.push(a);
						MSTK.push(b);
					}
				}
			},
			
			//divide or shorten string to length
			'/' => {
				if !svari {
					if nb.is_zero() {
						eprintln!("! /: Division by zero");
						MSTK.push(a);
						MSTK.push(b);
					}
					else {
						MSTK.push(Obj::N(Float::with_val(WPREC, na / nb)));
					}
				}
				else {
					let ib = int(nb);
					if let Some(n) = ib.clone().abs().to_usize() {
						MSTK.push(Obj::S(
							if ib<0 {	//discard from front
								sa.chars().skip(sa.chars().count().saturating_sub(n)).collect()
							}
							else {	//discard from back
								sa.chars().take(n).collect()
							}
						));
					}
					else {
						eprintln!("! /: Cannot possibly shorten a string to {ib} characters");
						MSTK.push(a);
						MSTK.push(b);
					}
				}
			},

			//modulo or isolate char
			'%' => {
				if !svari {
					let ia = int(na);
					let ib = int(nb);
					if ib==0 {
						eprintln!("! %: Reduction mod 0");
						MSTK.push(a);
						MSTK.push(b);
					}
					else {
						MSTK.push(Obj::N(Float::with_val(WPREC, ia % ib)));
					}
				}
				else {
					let ib = int(nb);
					if let Some(n) = ib.to_usize() {
						if let Some(c) = sa.chars().nth(n) {
							MSTK.push(Obj::S(c.into()))
						}
						else {
							eprintln!("! %: String is too short for index {n}");
							MSTK.push(a);
							MSTK.push(b);
						}
					}
					else {
						eprintln!("! %: Cannot possibly extract character at index {ib}");
						MSTK.push(a);
						MSTK.push(b);
					}
				}
			},

			//euclidean division or split string
			'~' => {
				if !svari {
					let ia = int(na);
					let ib = int(nb);
					if ib==0 {
						eprintln!("! ~: Reduction mod 0");
						MSTK.push(a);
						MSTK.push(b);
					}
					else {
						let (quot, rem)=ia.div_rem_euc(ib);
						MSTK.push(Obj::N(Float::with_val(WPREC, quot)));
						MSTK.push(Obj::N(Float::with_val(WPREC, rem)));
					}
				}
				else {
					let ib = int(nb);
					if let Some(n) = ib.to_usize() {
						MSTK.push(Obj::S(sa.chars().take(n).collect()));
						MSTK.push(Obj::S(sa.chars().skip(n).collect()));
					}
					else {
						eprintln!("! ~: Cannot possibly split a string at character {ib}");
						MSTK.push(a);
						MSTK.push(b);
					}
				}
			},

			//exponentiation or find in string
			'^' => {
				if !svari {
					if na<0 && nb.clone().abs()<1{
						eprintln!("! ^: Root of negative number");
						MSTK.push(a);
						MSTK.push(b);
					}
					else {
						MSTK.push(Obj::N(Float::with_val(WPREC, na.pow(nb))));
					}
				}
				else if let Some(bidx) = sa.find(&sb) {	//find byte index
					let cidx = sa.char_indices().position(|x| x.0==bidx).unwrap();	//corresp. char index
					MSTK.push(Obj::N(Float::with_val(WPREC, cidx)));
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, -1)));	//not found, silent error
				}
			},

			//modular exponentiation or find/replace in string
			'|' => {
				if !svari {
					let ia = int(na);
					let ib = int(nb);
					let ic = int(nc);
					if ic==0 {
						eprintln!("! |: Reduction mod 0");
						MSTK.push(a);
						MSTK.push(b);
						MSTK.push(c);
					}
					else if let Ok(res) = ia.clone().pow_mod(&ib, &ic) {
						MSTK.push(Obj::N(Float::with_val(WPREC, res)));
					}
					else {
						eprintln!("! |: {ia} doesn't have an inverse mod {ic}");
						MSTK.push(a);
						MSTK.push(b);
						MSTK.push(c);
					}
				}
				else {
					MSTK.push(Obj::S(sa.replace(&sb, &sc)));
				}
			},

			//square root
			'v' => {
				if na<0 {
					eprintln!("! v: Root of negative number");
					MSTK.push(a);
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, na.sqrt())));
				}
			},

			//bth root
			'V' => {
				if na<0 && nb.clone().abs()>1{
					eprintln!("! V: Root of negative number");
					MSTK.push(a);
					MSTK.push(b);
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, na.pow(nb.recip()))));
				}
			},

			//length of string or natural logarithm
			'g' => {
				if !svari {
					if na<=0 {
						eprintln!("! g: Logarithm of non-positive number");
						MSTK.push(a);
					}
					else {
						MSTK.push(Obj::N(Float::with_val(WPREC, na.ln())));
					}
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, sa.chars().count())));
				}
			},

			//base b logarithm
			'G' => {
				if na<=0 {
					eprintln!("! G: Logarithm of non-positive number");
					MSTK.push(a);
					MSTK.push(b);
				}
				else if nb==1||nb<=0{
					eprintln!("! G: Logarithm with base ≤0 or =1");
					MSTK.push(a);
					MSTK.push(b);
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, na.ln()/nb.ln())));
				}
			},

			//sine
			'u' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, na.sin())));
			},

			//cosine
			'y' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, na.cos())));
			},

			//tangent
			't' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, na.tan())));
			},

			//arc-sine
			'U' => {
				if na.clone().abs()>1 {
					eprintln!("! U: Arc-sine of value outside [-1,1]");
					MSTK.push(a);
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, na.asin())));
				}
			},

			//arc-cosine
			'Y' => {
				if na.clone().abs()>1 {
					eprintln!("! Y: Arc-cosine of value outside [-1,1]");
					MSTK.push(a);
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, na.acos())));
				}
			},

			//arc-tangent
			'T' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, na.atan())));
			},

			//random integer [0;a)
			'N' => {
				let int = int(na);
				if int<=0 {
					eprintln!("! N: Upper bound for random value must be above 0");
					MSTK.push(a);
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, int.random_below(&mut RNG[0]))));
				}
			},

			//constant/conversion factor lookup or convert number to string
			'"' => {
				if !svari {
					MSTK.push(Obj::S(flt_to_str(na.clone(), PARAMS.o(), PARAMS.k())));
				}
				else {
					match sa.matches(' ').count() {
						0 => {	//normal lookup
							if let Some(res) = get_constant(WPREC, &sa) {
								MSTK.push(Obj::N(res));
							}
							else {
								eprintln!("! \": Constant/conversion factor not found");
								MSTK.push(a);
							}
						},
						1 => {	//conversion shorthand, left divided by right
							let (sl, sr) = sa.split_once(' ').unwrap();
							if let (Some(nl), Some(nr)) = (get_constant(WPREC, sl), get_constant(WPREC, sr)) {
								MSTK.push(Obj::N(nl/nr));
							}
							else {
								eprintln!("! \": Constant/conversion factor not found");
								MSTK.push(a);
							}
						},
						_ => {
							eprintln!("! \": Too many spaces in constant/conversion query");
							MSTK.push(a);
						},
					}
				}
			},

			//deg -> rad shorthand
			'°' => {
				MSTK.push(Obj::N(na * Float::with_val(WPREC, Constant::Pi) / 180));
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
				let int = int(na);
				if let Some(mut num) = int.to_usize() {
					if num>MSTK.len() { num = MSTK.len(); }	//limit clear count
					MSTK.truncate(MSTK.len()-num);
				}
				else {
					eprintln!("! C: Cannot possibly remove {int} objects from the main stack");
					MSTK.push(a);
				}
			},

			//duplicate top of stack
			'd' => {
				if MSTK.is_empty() {
					eprintln!("! d: Nothing to duplicate");
				}
				else {
					MSTK.extend_from_within(MSTK.len()-1..);
				}
			},

			//duplicate top a objects
			'D' => {
				let int = int(na);
				if let Some(num) = int.to_usize() {
					if num<=MSTK.len() {
						MSTK.extend_from_within(MSTK.len()-num..);
					}
					else {
						eprintln!("! D: Not enough objects to duplicate");
						MSTK.push(a);
					}
				}
				else {
					eprintln!("! D: Cannot possibly duplicate {int} objects");
					MSTK.push(a);
				}
			},

			//swap top 2 objects
			'r' => {
				if MSTK.len()>=2 {
					MSTK.swap(MSTK.len()-2, MSTK.len()-1);
				}
				else {
					eprintln!("! r: Not enough objects to swap");
				}
			},

			//rotate top a objects
			'R' => {
				let mut int = int(na);
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
						eprintln!("! R: Not enough objects to rotate");
						MSTK.push(a);
					}
				}
				else {
					eprintln!("! R: Cannot possibly rotate {} objects", int.abs());
					MSTK.push(a);
				}
			},

			//push stack depth
			'z' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, MSTK.len())));
			},
			/*----------------------------
				ENVIRONMENT PARAMETERS
			----------------------------*/
			//set output precision
			'k' => {
				if let Err(e) = PARAMS.set_k(int(na)) {
					eprintln!("{e}");
					MSTK.push(a);
				}
			},

			//set input base
			'i' => {
				if let Err(e) = PARAMS.set_i(int(na)) {
					eprintln!("{e}");
					MSTK.push(a);
				}
			},

			//set output base
			'o' => {
				if let Err(e) = PARAMS.set_o(int(na)) {
					eprintln!("{e}");
					MSTK.push(a);
				}
			},

			//set working precision
			'w' => {
				let i = int(na);
				if let (Some(u), false) = (i.to_u32(), i==0u8) {
					WPREC = u;
				}
				else {
					eprintln!("! w: Working precision must be in range 1 ≤ W ≤ {}", u32::MAX);
					MSTK.push(a);
				}
			},

			//push output precision
			'K' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, PARAMS.k())));
			},

			//push input base
			'I' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, PARAMS.i())));
			},

			//push output base
			'O' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, PARAMS.o())));
			},

			//push working precision
			'W' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, WPREC)));
			},

			//create new k,i,o context
			'{' => {
				PARAMS.create();
			},

			//revert to previous context
			'}' => {
				PARAMS.destroy();
			},
			/*---------------
				REGISTERS
			---------------*/
			//save to top of register
			's' => {
				if reg.is_empty() {
					reg.push(RegObj {
						o: a,
						a: Vec::new()
					});
				}
				else {
					reg.last_mut().unwrap().o = a;
				}
			},

			//push to top of register
			'S' => {
				reg.push(RegObj{o: a, a: Vec::new()});
			},

			//load from top of register
			'l' => {
				if reg.is_empty() {
					eprintln!("! l: Register № {rnum} is empty");
				}
				else {
					MSTK.push(reg.last().unwrap().o.clone());
				}
			},

			//pop from top of register
			'L' => {
				if reg.is_empty() {
					eprintln!("! L: Register № {rnum} is empty");
				}
				else {
					MSTK.push(reg.pop().unwrap().o);
				}
			},

			//save to top-of-register's array
			':' => {
				if reg.is_empty() {
					reg.push(RegObj {
						o: DUMMY,	//create default register object if empty
						a: Vec::new()
					});
				}
				let int = int(nb);
				if let Some(rai) = int.to_usize() {
					if rai>=reg.last().unwrap().a.len() {
						reg.last_mut().unwrap().a.resize(rai+1, DUMMY);	//extend if required, initialize with default objects
					}
					reg.last_mut().unwrap().a[rai] = a;
				}
				else {
					eprintln!("! :: Cannot possibly save to array index {int}");
					MSTK.push(a);
					MSTK.push(b);
				}
			},

			//load from top-of-register's array
			';' => {
				if reg.is_empty() {
					reg.push(RegObj {
						o: DUMMY,	//create default register object if empty
						a: Vec::new()
					});
				}
				let int = int(na);
				if let Some(rai) = int.to_usize() {
					if rai>=reg.last().unwrap().a.len() {
						reg.last_mut().unwrap().a.resize(rai+1, DUMMY);	//extend if required, initialize with default objects
					}
					MSTK.push(reg.last().unwrap().a[rai].clone());
				}
				else {
					eprintln!("! ;: Cannot possibly load from array index {int}");
					MSTK.push(a);
				}
			},

			//pop top-of-reg into buffer
			'b' => {
				if reg.is_empty() {
					eprintln!("! b: Register № {rnum} is empty");
				}
				else {
					RO_BUF[0] = reg.pop().unwrap();
				}
			},

			//push buffer to register
			'B' => {
				reg.push(RO_BUF[0].clone());
			},

			//push register depth
			'Z' => {
				MSTK.push(Obj::N(Float::with_val(WPREC, reg.len())));
			},

			//specify direct register selector
			',' => {
				let int = if !svari {
					int(na)	//from number
				}
				else {
					Integer::from_digits(sa.as_bytes(), Order::Msf)	//from string bytes
				};
				DRS = Some(
					if let Some(u) = int.to_u16() {
						RegIdx::Low(u)
					}
					else {
						RegIdx::High(int)
					}
				);
			},
			/*------------
				MACROS
			------------*/
			//convert least significant 32 bits to one-char string or first char of string to number
			'a' => {
				if !svari {
					let ia = int(na).to_u32_wrapping();
					if let Some(res) = char::from_u32(ia) {
						MSTK.push(Obj::S(res.to_string()));
					}
					else {
						eprintln!("! a: Unable to convert number {ia} to character: not a valid Unicode value");
						MSTK.push(a);
					}
				}
				else if sa.is_empty() {
					eprintln!("! a: Cannot convert empty string to number");
					MSTK.push(a);
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, sa.chars().next().unwrap() as u32)));
				}
			},

			//convert number to UTF-8 string or back
			'A' => {
				if !svari {
					if let Ok(res) = String::from_utf8(int(na.clone()).to_digits::<u8>(Order::Msf)) {
						MSTK.push(Obj::S(res));
					}
					else {
						eprintln!("! A: Unable to convert number {} to string: not a valid UTF-8 sequence", int(na));
						MSTK.push(a);
					}
				}
				else {
					MSTK.push(Obj::N(Float::with_val(WPREC, Integer::from_digits(sa.as_bytes(), Order::Msf))));
				}
			},

			//execute string as macro
			'x' => {
				if svari {
						if cmdstk.last().unwrap().is_empty() {
							cmdstk.pop();	//optimize tail call
						}
						cmdstk.push(sa.chars().rev().collect());
				} else {
					MSTK.push(a);
				}
			},

			//invert next conditional
			'!' => {
				inv = !inv;
			},

			//conditionally execute macro
			'<'|'='|'>' => {
				if reg.is_empty() {
					eprintln!("! <=>: Register № {rnum} is empty");
				}
				else if let Obj::S(mac) = &reg.last().unwrap().o {
					if inv != match cmd {
					'<' => { nb < na },	//reverse order, GNU dc convention
					'=' => { nb == na },
					'>' => { nb > na },
					_ => {false},}
					{
						if cmdstk.last().unwrap().is_empty() {
							cmdstk.pop();	//optimize tail call
						}
						cmdstk.push(mac.chars().rev().collect());
					}
				}
				else {
					eprintln!("! <=>: Top of register № {rnum} is not a string");
				}
				inv = false;	//always reset inversion
			},

			//auto-macro
			'X' => {
				let int = int(nb);
				if let Some(reps) = int.to_usize() {
					if cmdstk.last().unwrap().is_empty() {
						cmdstk.pop();	//optimize tail call
					}
					cmdstk.resize(cmdstk.len()+reps, sa.chars().rev().collect());
				}
				else {
					eprintln!("! X: Cannot possibly repeat a macro {int} times");
					MSTK.push(a);
					MSTK.push(b);
				}
			},

			//quit dcim
			'q' => {
				std::process::exit(
					if let Some(RegIdx::Low(u)) = DRS {u as i32}
					else {0}
				);
			},

			//quit a macro calls
			'Q' => {
				let int = int(na);
				if let Some(mut num) = int.to_usize() {
					if num>cmdstk.len() {num=cmdstk.len();}
					cmdstk.truncate(cmdstk.len()-num);
					if cmdstk.is_empty() {
						cmdstk.push(String::new());	//guarantee at least one object
					}
				}
				else {
					eprintln!("! Q: Cannot possibly quit {int} levels");
					MSTK.push(a);
				}
			},

			//prompt and execute
			'?' => {
				let prompt_in = input::<String>().get();
				if cmdstk.last().unwrap().is_empty() {
					cmdstk.pop();	//optimize tail call
				}
				cmdstk.push(prompt_in.chars().rev().collect());
			},
			/*----------
				MISC
			----------*/
			//execute file as script
			'&' => {
				match std::fs::read_to_string(sa.clone()) {
					Ok(script) => {
						let mut script_nc = String::new();	//script with comments removed
						for line in script.split('\n') {
							script_nc.push_str(line.split_once('#').unwrap_or((line,"")).0);	//remove comment on every line
							script_nc.push('\n');
						}
						cmdstk.push(script_nc.chars().rev().collect());
					},
					Err(err) => {
						eprintln!("! &: Unable to read file \"{sa}\": {err}");
						MSTK.push(a);
					},
				}
			},

			//get environment variable
			'$' => {
				match std::env::var(sa.clone()) {
					Ok(val) => {
						MSTK.push(Obj::S(val));
					},
					Err(err) => {
						eprintln!("! $: Unable to get value of ${sa}: {err}");
						MSTK.push(a);
					},
				}
			},

			//execute os command(s)
			'\\' => {
				for oscmd in sa.clone().split(';') {
					if let Some((var, val)) = oscmd.split_once('=') {	//set variable
						std::env::set_var(var, val);
					}
					else {	//normal command
						let mut args: Vec<&str> = oscmd.trim().split(' ').collect();
						match std::process::Command::new(args.remove(0)).args(args).spawn() {
							Ok(mut child) => {
								if let Ok(stat) = child.wait() {
									if let Some(code) = stat.code() {
										if code!=0 {eprintln!("! \\: OS command \"{oscmd}\" exited with code {code}");}
									}
								}
							},
							Err(err) => {
								eprintln!("! \\: Unable to execute OS command \"{oscmd}\": {err}");
								MSTK.push(a.clone());
							},
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
				if !cmd.is_whitespace()&&cmd!='\0' { eprintln!("! Invalid command: {cmd} (U+{:04X})", cmd as u32); }
			},
		}
		while let Some(ptr) = cmdstk.last() {	//clean up empty command strings
			if ptr.is_empty() {
				cmdstk.pop();
			}
			else{break;}
		}
	}
}
