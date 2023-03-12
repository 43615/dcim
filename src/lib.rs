use std::io::{Write, BufRead};
use std::time::{SystemTime, Duration};
use std::collections::{VecDeque, HashSet, HashMap};
use rug::{Integer, integer::Order, Complete, Float, float::{Round, Constant, Special}, ops::Pow, rand::RandState, Assign};
use rand::{RngCore, rngs::OsRng};
#[macro_use]
extern crate lazy_static;

///basic object: either number or string
#[derive(Clone)]
enum Obj {
	Num(Float),
	Str(String)
}
use Obj::*;
///unused/default Obj
const DUMMY: Obj = Str(String::new());

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
use CmdSig::*;
impl CmdSig {
	#[inline(always)]
	///english plural ending
	fn plural(&self) -> &str {
		if matches!(self, Ax|An|As) {""} else {"s"}
	}

	#[inline(always)]
	///correction messages
	fn correct(&self) -> &str {
		match self {
			Nil|Ax => "",
			An => "must be a number",
			As => "must be a string",
			AxBx => "must be two numbers or two strings",
			AxBn => "2nd must be a number",
			AnBn => "must be two numbers",
			AsBn => "1st must be a string, 2nd must be a number",
			AxBxCx => "must be three numbers or three strings",
		}
	}
}

///stack for (K,I,O) tuples, with methods for checked editing
struct ParamStk(Vec<(Integer, Integer, Integer)>);
impl ParamStk {
	#[inline(always)]
	///switch to new param context with defaults (-1,10,10)
	fn create(&mut self) {
		self.0.push((Integer::from(-1), Integer::from(10), Integer::from(10)));
	}
	#[inline(always)]
	///revert to previous context, reset to defaults if nonexistent
	fn destroy(&mut self) {
		self.0.pop();
		if self.0.is_empty() {self.create()}
	}

	#[inline(always)]
	///checked edit of current output precision
	fn set_k(&mut self, n: Integer) -> Result<(), &'static str> {
		if n>=-1 {
			self.0.last_mut().unwrap().0 = n;
			Ok(())
		}
		else {Err("! k: Output precision must be at least -1")}
	}
	#[inline(always)]
	///checked edit of current input base
	fn set_i(&mut self, n: Integer) -> Result<(), &'static str> {
		if n>=2 {
			self.0.last_mut().unwrap().1 = n;
			Ok(())
		}
		else {Err("! i: Input base must be at least 2")}
	}
	#[inline(always)]
	///checked edit of current output base
	fn set_o(&mut self, n: Integer) -> Result<(), &'static str> {
		if n>=2 {
			self.0.last_mut().unwrap().2 = n;
			Ok(())
		}
		else {Err("! o: Output base must be at least 2")}
	}
	#[inline(always)]
	///current output precision
	fn k(&self) -> Integer {self.0.last().unwrap().0.clone()}
	#[inline(always)]
	///current input base
	fn i(&self) -> Integer {self.0.last().unwrap().1.clone()}
	#[inline(always)]
	///current output base
	fn o(&self) -> Integer {self.0.last().unwrap().2.clone()}
}

///default register, const required for array init
const REG_DEF: Vec<RegObj> = Vec::new();

///Bundled state storage for one instance of dc:im
pub struct State<'a> {
	///main stack
	mstk: Vec<Obj>,
	///hashmap of arbitrarily-numbered registers
	regs: HashMap<Integer, Vec<RegObj>>,
	///RegObj buffer
	ro_buf: RegObj,
	///manual register pointer
	rptr: Option<Integer>,
	///random number generator
	rng: RandState<'a>,
	///parameters K,I,O
	par: ParamStk,
	///working precision W
	w: u32
}
impl Default for State<'_> {
	///Arbitrary initial values one might want to change:
	///- RNG: Mersenne twister seeded with 1024 bits of OS randomness
	///- Parameter stack: one entry, (K, I, O) = (-1, 10, 10)
	///- Working precision: 256 bits
	fn default() -> Self {
		Self {
			mstk: Vec::new(),
			regs: HashMap::new(),
			ro_buf: RegObj {o: DUMMY, a: Vec::new()},
			rptr: None,
			rng: {
				//seed RNG with 1024 bits of OS randomness
				let mut r = RandState::new();
				let mut seed = [0_u8; 128];
				OsRng.fill_bytes(&mut seed);
				r.seed(&Integer::from_digits(&seed, Order::Msf));
				r
			},
			par: {
				let mut p = ParamStk(Vec::new());
				p.create();
				p
			},
			w: 256
		}
	}
}
impl<'a> State<'a> {
	///replace RNG with custom one
	pub fn custom_rng(mut self, r: RandState<'a>) -> Self {
		self.rng = r;
		self
	}
	///custom initial (K, I, O) entry, cleaner than exec
	pub fn custom_params(mut self, k: Integer, i: Integer, o: Integer) -> Self {
		self.par = ParamStk(vec![(k, i, o)]);
		self
	}
	///custom initial W, cleaner than exec
	pub fn custom_w(mut self, w: u32) -> Self {
		self.w = w;
		self
	}
}

#[inline(always)]
///standard rounding function: discard fractional part if finite, default to 0 otherwise
fn round(n: &Float) -> Integer {
	if let Some((i, _)) = n.to_integer_round(Round::Zero) {i} else {Integer::ZERO}
}

#[inline(always)]
///parse any-base number
fn parse_abnum(src: String, base: Integer, prec: u32) -> Result<Float, &'static str> {
	let (mut mstr, estr) = match src.split(['@', 'e', 'E']).collect::<Vec<&str>>()[..] {	//split at exponential symbol
		[m] => (m, "0"),
		[m, e] => (m, e),
		_ => {return Err("more than one exponential part");}	//x@y@z not allowed
	};

	let mneg = if let Some(s) = mstr.strip_prefix(['-', '_']) {	//get negative sign out of the way
		mstr = s;
		true
	}
	else {false};

	let exp = if let Ok(i) = Integer::parse(estr.replace('_', "-")) {
		Float::with_val(prec, &base).pow(i.complete())	//final exponent
	}
	else {return Err("invalid exponential part");};

	let mut man = Integer::from(0);	//resulting mantissa
	let mut scale = Integer::from(1);	//scale to divide by
	let mut frac = false;	//. has occurred

	for mut dig in mstr.split_inclusive([' ', '.']) {	//split into digits, scan from the left:

		man *= &base;	//multiply existing mantissa by base
		if frac {scale *= &base;}	//counter scale-up if fractional part has started

		if let Some(d) = dig.strip_suffix('.') {	//digit followed by .
			if frac {return Err("more than one '.'");}
			frac = true;
			dig = d;
		}

		let di = if let Ok(i) = Integer::parse(String::from('0')+dig) {i.complete()}
		else {return Err("invalid character in digit");};

		if di >= base {return Err("digit too high for input base");}

		man += di;	//add new digit
	}

	if mneg {man *= -1;}

	Ok(
		Float::with_val(prec, man) / scale * exp
	)
}

///Float generator for library of constants (known value, variable precision)
struct FltGen(Box<dyn Fn(u32) -> Float>);
unsafe impl Sync for FltGen {}
impl FltGen {
	#[inline(always)]
	///simple value
	fn val<T: 'static + Copy>(val: T) -> Self
		where Float: Assign<T> {
		Self(Box::new(move |prec: u32| Float::with_val(prec, val)))
	}

	#[inline(always)]
	///scientific notation
	fn sci<T: 'static + Copy, U: 'static + Copy>(man :T, exp: U) -> Self
		where Float: Assign<T> + Assign<U> {
		Self(Box::new(move |prec: u32| Float::with_val(prec, man)*Float::with_val(prec, exp).exp10()))
	}

	#[inline(always)]
	///recursive (apply function to existing unit)
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
	///command signatures and adicities
	static ref CMD_SIGS: HashMap<char, (CmdSig, u8)> = {
		let mut m = HashMap::new();

		for c in ['+','^'] {m.insert(c, (AxBx, 2));}

		m.insert('|', (AxBxCx, 3));

		for c in "-*/%~:".chars() {m.insert(c, (AxBn, 2));}

		for c in "&$\\".chars() {m.insert(c, (As, 1));}

		for c in "nPaA\"xgsS,".chars() {m.insert(c, (Ax, 1));}

		m.insert('X', (AsBn, 2));

		for c in "v°uytUYTNCDRkiow;Q".chars() {m.insert(c, (An, 1));}

		for c in "VG<=>".chars() {m.insert(c, (AnBn, 2));}

		m
	};
	///library of constants/conversion factors
	static ref CONSTANTS: HashMap<&'static str, FltGen> = {
		let mut m = HashMap::new();
		/*----------------------------
			MATHEMATICAL CONSTANTS
		----------------------------*/
		m.insert("e", FltGen(Box::new(|prec| Float::with_val(prec, 1_u8).exp())));
		m.insert("pi", FltGen::val(Constant::Pi));
		m.insert("gamma", FltGen::val(Constant::Euler));
		m.insert("phi", FltGen(Box::new(|prec| (Float::with_val(prec, 5_u8).sqrt()+1_u8)/2_u8)));
		for q in ["deg","°"] {m.insert(q, FltGen::rec("pi", &|n| n/180_u8));}
		for q in ["gon","grad"] {m.insert(q, FltGen::rec("pi", &|n| n/200_u8));}
		/*------------------------
			PHYSICAL CONSTANTS
		------------------------*/
		m.insert("c", FltGen::val(299792458_u32));
		m.insert("hbar", FltGen(Box::new(|prec| FltGen::sci(662607015_u32, -42_i8).0(prec) / FltGen::rec("pi", &|n| n*2_u8).0(prec))));
		m.insert("G", FltGen::sci(6674_u16, -3_i8));
		m.insert("qe", FltGen::sci(1602176634_u32, -28_i8));
		m.insert("NA", FltGen::sci(602214076_u32, 31_u8));
		m.insert("kB", FltGen::sci(1380649_u32, -29_i8));
		m.insert("u", FltGen::sci(1660539066_u32, -36_i8));
		m.insert("lp", FltGen::sci(16162_u16, -39_i8));
		m.insert("tp", FltGen::sci(5391_u16, -47_i8));
		m.insert("mp", FltGen::sci(21764_u16, -12_i8));
		m.insert("Tp", FltGen::sci(14167_u16, 28_u8));
		/*------------------
			LENGTH UNITS
		------------------*/
		m.insert("in", FltGen::sci(254_u8, -4_i8));
		m.insert("ft", FltGen::rec("in", &|n| n*12_u8));
		m.insert("yd", FltGen::rec("ft", &|n| n*3_u8));
		m.insert("m", FltGen::val(1_u8));
		m.insert("fur", FltGen::rec("ft", &|n| n*660_u16));
		m.insert("mi", FltGen::rec("ft", &|n| n*5280_u16));
		m.insert("nmi", FltGen::val(1852_u16));
		m.insert("AU", FltGen::val(149597870700_u64));
		m.insert("ly", FltGen::val(9460730472580800_u64));
		m.insert("pc", FltGen(Box::new(|prec| Float::with_val(prec, 96939420213600000_u64)/Float::with_val(prec, Constant::Pi))));
		/*-------------------------------
			AREA & VOLUME UNITS
			with no length equivalent
		-------------------------------*/
		for q in ["ac","acre"] {m.insert(q, FltGen::sci(40468564224_u64, -7_i8));}
		m.insert("l", FltGen::sci(1_u8, -3_i8));
		m.insert("ifloz", FltGen::sci(284130625_u32, -13_i8));
		m.insert("ipt", FltGen::rec("ifloz", &|n| n*20_u8));
		m.insert("iqt", FltGen::rec("ifloz", &|n| n*40_u8));
		m.insert("igal", FltGen::rec("ifloz", &|n| n*160_u8));
		for q in ["ibu","ibsh"] {m.insert(q, FltGen::rec("ifloz", &|n| n*1280_u16));}
		m.insert("ufldr", FltGen::sci(36966911953125_u64, -19_i8));
		m.insert("tsp", FltGen::rec("ufldr", &|n| n/3_u8*4_u8));
		m.insert("tbsp", FltGen::rec("ufldr", &|n| n*4_u8));
		m.insert("ufloz", FltGen::rec("ufldr", &|n| n*8_u8));
		m.insert("upt", FltGen::rec("ufloz", &|n| n*16_u8));
		m.insert("uqt", FltGen::rec("ufloz", &|n| n*32_u8));
		m.insert("ugal", FltGen::rec("ufloz", &|n| n*128_u8));
		m.insert("bbl", FltGen::rec("ugal", &|n| n*42_u8));
		m.insert("udpt", FltGen::sci(5506104713575_u64, -16_i8));
		m.insert("udqt", FltGen::rec("udpt", &|n| n*2_u8));
		m.insert("udgal", FltGen::rec("udpt", &|n| n*8_u8));
		for q in ["ubu","ubsh"] {m.insert(q, FltGen::rec("udpt", &|n| n*64_u8));}
		m.insert("dbbl", FltGen::sci(115627123584_i64, -12_i8));
		/*----------------
			MASS UNITS
		----------------*/
		m.insert("ct", FltGen::sci(2_u8, -4_i8));
		m.insert("oz", FltGen::sci(28349523125_u64, -12_i8));
		m.insert("lb", FltGen::rec("oz", &|n| n*16_u8));
		m.insert("kg", FltGen::val(1_u8));
		m.insert("st", FltGen::rec("lb", &|n| n*14_u8));
		m.insert("t", FltGen::rec("lb", &|n| n*2240_u16));
		/*----------------
			TIME UNITS
		----------------*/
		m.insert("s", FltGen::val(1_u8));
		m.insert("min", FltGen::val(60_u8));
		m.insert("h", FltGen::rec("min", &|n| n*60_u8));
		m.insert("d", FltGen::rec("h", &|n| n*24_u8));
		m.insert("w", FltGen::rec("d", &|n| n*7_u8));
		m.insert("mo", FltGen::rec("d", &|n| n*30_u8));
		m.insert("a", FltGen::rec("d", &|n| n*365_u16));
		m.insert("aj", FltGen::rec("d", &|n| n*36525_u16/100_u8));
		m.insert("ag", FltGen::rec("d", &|n| n*3652425_u32/10000_u16));
		/*-----------------
			OTHER UNITS
		-----------------*/
		m.insert("J", FltGen::val(1_u8));
		m.insert("cal", FltGen::sci(4184_u16, -3_i8));
		m.insert("Pa", FltGen::val(1_u8));
		m.insert("atm", FltGen::val(101325_u32));
		m.insert("psi", FltGen::sci(6894757293168_u64, -9_i8));
		m.insert("torr", FltGen::rec("atm", &|n| n/760_u16));
		/*------------------------------
			SPECIAL VALUES/FUNCTIONS
		------------------------------*/
		m.insert("inf", FltGen::val(Special::Infinity));
		m.insert("ninf", FltGen::val(Special::NegInfinity));
		m.insert("nan", FltGen::val(Special::Nan));
		m.insert("pid", FltGen::val(std::process::id()));
		m.insert("author", FltGen::val(43615_u16));	//yay numerical nicknames!
		m
	};
}

#[inline(always)]
///calculate value with given precision, apply scale prefix and power suffix
///
///`safe` toggle disables terminating queries
fn get_constant(prec: u32, query: &str, safe: bool) -> Option<Float> {
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
			"abort" if !safe => {std::process::abort();}
			"crash" if !safe => {get_constant(prec, "crash", false)}	//stack go brrrrr
			"panic" if !safe => {std::panic::panic_any("Expected, [panic]\" was executed");}
			_ => None
		}
	}
}

#[inline(always)]
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

///Bundle of generic IO streams, for brevity.
pub struct IOTriple<'a> {
	pub input: &'a mut dyn BufRead,
	pub output: &'a mut dyn Write,
	pub error: &'a mut dyn Write
}
#[macro_export]
///Default IO triple using stdin, stdout, stderr
macro_rules! stdio {
	() => {
		::dcim::IOTriple {
			input: &mut ::std::io::BufReader::new(::std::io::stdin()),
			output: &mut ::std::io::stdout(),
			error: &mut ::std::io::stderr()
		}
	}
}

///Executes commands on given state, uses provided input/output/error streams.
///
///The `safe` toggle disables commands that interact with the OS, as well as terminating pseudoconstants.
///Be aware that this toggle does not prevent infinite loops (`[lax]salax`).
///
///Usage of the provided IO streams:
///- input: Read by the command `?` one line at a time.
///- output: Normal printing by the commands `pfnPF`.
///- error: All dc:im error messages, syntactic or semantic.
///
///If all commands run to completion, `Ok(None)` is returned.
///
///`Ok(Some(Integer))` indicates an early exit request (`q` command). The current `State` should be discarded.
///If the register pointer is set, its value is returned as the "exit code" (0 otherwise).
///
///Terminates with `Err` only if a write/read on an IO stream fails.
pub fn exec(st: &mut State, io: &mut IOTriple, safe: bool, cmds: &str) -> std::io::Result<Option<Integer>> {
	let mut cmdstk: Vec<VecDeque<char>> = vec!(cmds.chars().collect());	//stack of command strings to execute, enables pseudorecursive macro calls
	let mut inv = false;	//invert next comparison

	let mut dummy_reg = REG_DEF;	//required for let syntax, never accessed

	//default contents of string/number slots
	let sx_dummy = String::new();
	let nx_dummy = Float::new(1);

	while !cmdstk.is_empty() {	//last().unwrap() is guaranteed to not panic within

		let mut cmd = cmdstk.last_mut().unwrap().pop_front().unwrap_or('\0');	//get next command

		let (reg, rnum): (&mut Vec<RegObj>, Integer) = if USES_REG.contains(&cmd) {	//get register reference, before the other checks since it's syntactically significant ("sq" etc)
			let i = if let Some(i) = st.rptr.take() {i}	//take index from reg ptr
			else if let Some(c) = cmdstk.last_mut().unwrap().pop_front() {Integer::from(c as u32)}	//steal next command char as index
			else {
				writeln!(io.error, "! Command '{cmd}' needs a register number")?;
				continue;
			};
			(
				if let Some(r) = st.regs.get_mut(&i) {r}	//reg already exists?
				else {
					st.regs.insert(i.clone(), REG_DEF);	//else touch reg
					st.regs.get_mut(&i).unwrap()
				},
				i
			)
		}
		else {(&mut dummy_reg, Integer::ZERO)};	//no register needed

		let (sig, adi) = CMD_SIGS.get(&cmd).unwrap_or(&(Nil, 0));	//get correct command signature

		if st.mstk.len() < *adi as usize {	//check stack depth
			writeln!(io.error, "! Command '{cmd}' needs {} argument{}", adi, sig.plural())?;
			continue;
		}

		let (c, b, a) = match adi {	//pop required amount from stack
			1 => (DUMMY, DUMMY, st.mstk.pop().unwrap()),
			2 => (DUMMY, st.mstk.pop().unwrap(), st.mstk.pop().unwrap()),
			3 => (st.mstk.pop().unwrap(), st.mstk.pop().unwrap(), st.mstk.pop().unwrap()),
			_ => (DUMMY, DUMMY, DUMMY)
		};

		let (mut na, mut nb, mut nc) = (&nx_dummy, &nx_dummy, &nx_dummy);	//create number slots
		let (mut sa, mut sb, mut sc) = (&sx_dummy, &sx_dummy, &sx_dummy);	//create string slots
		let mut strv = false;	//use string variant of overloaded command (not stridsvagn :/)

		if
		match sig {	//check and destructure Objs
			Nil => false,
			Ax => match &a {
				Num(x) => {
					na = x;
					false
				},
				Str(x) => {
					sa = x;
					strv = true;
					false
				}
			},
			An => match &a {
				Num(x) => {
					na = x;
					false
				},
				_ => true
			},
			As => match &a {
				Str(x) => {
					sa = x;
					false
				},
				_ => true
			},
			AxBx => match (&a, &b) {
				(Num(x), Num(y)) => {
					(na, nb) = (x, y);
					false
				},
				(Str(x), Str(y)) => {
					(sa, sb) = (x, y);
					strv = true;
					false
				},
				_ => true
			},
			AxBn => match (&a, &b) {
				(Num(x), Num(y)) => {
					(na, nb) = (x, y);
					false
				},
				(Str(x), Num(y)) => {
					(sa, nb) = (x, y);
					strv = true;
					false
				},
				_ => true
			},
			AnBn => match (&a, &b) {
				(Num(x), Num(y)) => {
					(na, nb) = (x, y);
					false
				},
				_ => true
			},
			AsBn => match (&a, &b) {
				(Str(x), Num(y)) => {
					(sa, nb) = (x, y);
					false
				},
				_ => true
			},
			AxBxCx => match (&a, &b, &c) {
				(Num(x), Num(y), Num(z)) => {
					(na, nb, nc) = (x, y, z);
					false
				},
				(Str(x), Str(y), Str(z)) => {
					(sa, sb, sc) = (x, y, z);
					strv = true;
					false
				},
				_ => true
			},
		}
		{
			writeln!(io.error, "! Wrong argument type{} for command '{cmd}': {}", sig.plural(), sig.correct())?;
			match adi {	//push Objs back
				1 => {st.mstk.push(a);},
				2 => {st.mstk.push(a); st.mstk.push(b);},
				3 => {st.mstk.push(a); st.mstk.push(b); st.mstk.push(c);},
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
				if st.par.i()>36 {
					writeln!(io.error, "! Any-base input must be used for input bases over 36")?;
				}
				else {
					let mut numstr = String::new();	//gets filled with number to be parsed later
					let mut frac = false;	//'.' has already occurred
					let mut neg = false;	//'_' has already occurred
					let mut alpha = false;	//letters are used
					if cmd == '\'' {
						alpha = true;
						cmd = cmdstk.last_mut().unwrap().pop_front().unwrap_or('\0');
					}
					//keep adding to numstr until number is finished
					loop {
						//numbers, periods and exponential notation
						if cmd.is_ascii_digit()||cmd == '.'||cmd == '@' {
							if cmd == '.' { if frac { break; } else { frac = true; } } //break on encountering second '.'
							if cmd == '@' { neg = false; }	//allow for second negative sign in exponent
							numstr.push(cmd);
						}
						//'_' needs to be replaced with '-'
						else if cmd == '_' {
							if neg { break; } else { neg = true; } //break on encountering second '_'
							numstr.push('-');
						}
						//parse letters if number is prefixed with quote
						else if cmd.is_ascii_alphabetic() {
							if alpha {
								numstr.push(cmd);
							}
							else {
								break;
							}
						}
						else {
							break;
						}
						cmd = cmdstk.last_mut().unwrap().pop_front().unwrap_or('\0');
					}
					cmdstk.last_mut().unwrap().push_front(cmd);	//restore first char that isn't part of the number
					if numstr.starts_with('@') { numstr.insert(0, '1') }	//add implied 1 before exponential marker
					if numstr.starts_with('.')||numstr.starts_with("-.") { numstr = numstr.replace('.', "0."); }	//add implied zero before fractional separator
					if numstr.ends_with('.')||numstr.ends_with('-')||numstr.is_empty() { numstr.push('0'); }	//add implied zero at end
					match Float::parse_radix(numstr.clone(), st.par.i().to_i32().unwrap()) {
						Ok(res) => {
							st.mstk.push(Num(Float::with_val(st.w, res)));
						},
						Err(err) => {
							writeln!(io.error, "! Unable to parse number \"{numstr}\": {err}")?;
						},
					}
				}
			},

			//any-base number input
			'(' => {
				let mut to_parse = String::new();
				cmd = cmdstk.last_mut().unwrap().pop_front().unwrap_or(')');	//overwrite opening parenthesis, close if nothing left
				while cmd != ')' {
					to_parse.push(cmd);
					cmd = cmdstk.last_mut().unwrap().pop_front().unwrap_or(')');
				}
				match parse_abnum(to_parse, st.par.i(), st.w) {
					Ok(n) => {st.mstk.push(Num(n));}
					Err(e) => {writeln!(io.error, "! Unable to parse any-base number: {e}")?;}
				}
			},

			//string input
			'[' => {
				let mut res = String::new();	//result string
				let mut nest: usize = 1;	//nesting level
				cmd = cmdstk.last_mut().unwrap().pop_front().unwrap_or('\0');	//overwrite opening bracket, null if nothing left
				loop {
					res.push(cmd);
					if cmd == '[' { nest+=1; }
					if cmd == ']' { nest-=1; }
					if nest==0 {	//string finished
						res.pop();	//remove closing bracket
						st.mstk.push(Str(res));
						break;
					}
					if cmdstk.last().unwrap().is_empty() {	//only reached on improper string
						writeln!(io.error, "! Unable to parse string \"[{res}\": missing closing bracket")?;
						break;
					}
					else {cmd = cmdstk.last_mut().unwrap().pop_front().unwrap();}
				}
			},
			/*--------------
				PRINTING
			--------------*/
			//print top with newline
			'p' => {
				if !st.mstk.is_empty() {
					match st.mstk.last().unwrap() {
						Num(n) => {writeln!(io.output, "{}", flt_to_str(n.clone(), st.par.o(), st.par.k()))?;},
						Str(s) => {writeln!(io.output, "[{s}]")?;},
					}
				}
			},

			//print full stack top to bottom
			'f' => {
				if !st.mstk.is_empty() {
					for i in (0..st.mstk.len()).rev() {
						match &st.mstk[i] {
							Num(n) => {writeln!(io.output, "{}", flt_to_str(n.clone(), st.par.o(), st.par.k()))?;},
							Str(s) => {writeln!(io.output, "[{s}]")?;},
						}
					}
				}
			},

			//pop and print without newline
			'n' => {
				if !strv {
					write!(io.output, "{}", flt_to_str(na.clone(), st.par.o(), st.par.k()))?;
					io.output.flush().unwrap();
				}
				else {
					write!(io.output, "{sa}")?;
					io.output.flush().unwrap()
				}
			},

			//pop and print with newline
			'P' => {
				if !strv {writeln!(io.output, "{}", flt_to_str(na.clone(), st.par.o(), st.par.k()))?;}
				else {writeln!(io.output, "{sa}")?;}
			},

			//print register
			'F' => {
				if !reg.is_empty(){
					for i in (0..reg.len()).rev() {
						match &reg[i].o {
							Num(n) => {writeln!(io.output, "{}", flt_to_str(n.clone(), st.par.o(), st.par.k()))?;},
							Str(s) => {writeln!(io.output, "[{s}]")?;},
						}
						if !reg[i].a.is_empty() {
							let width = (reg[i].a.len()-1).to_string().len();	//length of longest index number
							for ai in 0..reg[i].a.len() {
								match &reg[i].a[ai] {
									Num(n) => {writeln!(io.output, "\t{ai:>width$}: {}", flt_to_str(n.clone(), st.par.o(), st.par.k()))?;},
									Str(s) => {writeln!(io.output, "\t{ai:>width$}: [{s}]")?;},
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
				if !strv {st.mstk.push(Num(Float::with_val(st.w, na + nb)));}
				else {st.mstk.push(Str(sa.clone() + sb));}
			},

			//subtract or remove chars from string
			'-' => {
				if !strv {st.mstk.push(Num(Float::with_val(st.w, na - nb)));}
				else {
					let ib = round(nb);
					if let Some(n) = ib.clone().abs().to_usize() {
						st.mstk.push(Str(
							if ib<0 {	//remove from front
								sa.chars().skip(n).collect()
							}
							else {	//remove from back
								sa.chars().take(sa.chars().count().saturating_sub(n)).collect()
							}
						));
					}
					else {
						writeln!(io.error, "! -: Cannot possibly remove {ib} characters from a string")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
				}
			},

			//multiply or repeat/invert string
			'*' => {
				if !strv {st.mstk.push(Num(Float::with_val(st.w, na * nb)));}
				else {
					let ib = round(nb);
					if let Some(n) = ib.clone().abs().to_usize() {
						st.mstk.push(Str(
							if ib<0 {	//repeat and reverse
								sa.chars().rev().collect::<String>().repeat(n)
							}
							else {	//repeat
								sa.repeat(n)
							}
						));
					}
					else {
						writeln!(io.error, "! *: Cannot possibly repeat a string {ib} times")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
				}
			},

			//divide or shorten string to length
			'/' => {
				if !strv {
					if nb.is_zero() {
						writeln!(io.error, "! /: Division by zero")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
					else {
						st.mstk.push(Num(Float::with_val(st.w, na / nb)));
					}
				}
				else {
					let ib = round(nb);
					if let Some(n) = ib.clone().abs().to_usize() {
						st.mstk.push(Str(
							if ib<0 {	//discard from front
								sa.chars().skip(sa.chars().count().saturating_sub(n)).collect()
							}
							else {	//discard from back
								sa.chars().take(n).collect()
							}
						));
					}
					else {
						writeln!(io.error, "! /: Cannot possibly shorten a string to {ib} characters")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
				}
			},

			//modulo or isolate char
			'%' => {
				if !strv {
					let ia = round(na);
					let ib = round(nb);
					if ib==0 {
						writeln!(io.error, "! %: Reduction mod 0")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
					else {
						st.mstk.push(Num(Float::with_val(st.w, ia % ib)));
					}
				}
				else {
					let ib = round(nb);
					if let Some(n) = ib.to_usize() {
						if let Some(c) = sa.chars().nth(n) {
							st.mstk.push(Str(c.into()))
						}
						else {
							writeln!(io.error, "! %: String is too short for index {n}")?;
							st.mstk.push(a);
							st.mstk.push(b);
						}
					}
					else {
						writeln!(io.error, "! %: Cannot possibly extract character at index {ib}")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
				}
			},

			//euclidean division or split string
			'~' => {
				if !strv {
					let ia = round(na);
					let ib = round(nb);
					if ib==0 {
						writeln!(io.error, "! ~: Reduction mod 0")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
					else {
						let (quot, rem)=ia.div_rem_euc(ib);
						st.mstk.push(Num(Float::with_val(st.w, quot)));
						st.mstk.push(Num(Float::with_val(st.w, rem)));
					}
				}
				else {
					let ib = round(nb);
					if let Some(n) = ib.to_usize() {
						st.mstk.push(Str(sa.chars().take(n).collect()));
						st.mstk.push(Str(sa.chars().skip(n).collect()));
					}
					else {
						writeln!(io.error, "! ~: Cannot possibly split a string at character {ib}")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
				}
			},

			//exponentiation or find in string
			'^' => {
				if !strv {
					if *na<0 && nb.clone().abs()<1{
						writeln!(io.error, "! ^: Root of negative number")?;
						st.mstk.push(a);
						st.mstk.push(b);
					}
					else {
						st.mstk.push(Num(Float::with_val(st.w, na.pow(nb))));
					}
				}
				else if let Some(bidx) = sa.find(sb) {	//find byte index
					let cidx = sa.char_indices().position(|x| x.0==bidx).unwrap();	//corresp. char index
					st.mstk.push(Num(Float::with_val(st.w, cidx)));
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, -1)));	//not found, silent error
				}
			},

			//modular exponentiation or find/replace in string
			'|' => {
				if !strv {
					let ia = round(na);
					let ib = round(nb);
					let ic = round(nc);
					if ic==0 {
						writeln!(io.error, "! |: Reduction mod 0")?;
						st.mstk.push(a);
						st.mstk.push(b);
						st.mstk.push(c);
					}
					else if let Ok(res) = ia.clone().pow_mod(&ib, &ic) {
						st.mstk.push(Num(Float::with_val(st.w, res)));
					}
					else {
						writeln!(io.error, "! |: {ia} doesn't have an inverse mod {ic}")?;
						st.mstk.push(a);
						st.mstk.push(b);
						st.mstk.push(c);
					}
				}
				else {
					st.mstk.push(Str(sa.replace(sb, sc)));
				}
			},

			//square root
			'v' => {
				if *na<0 {
					writeln!(io.error, "! v: Root of negative number")?;
					st.mstk.push(a);
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, na.clone().sqrt())));
				}
			},

			//bth root
			'V' => {
				if *na<0 && nb.clone().abs()>1{
					writeln!(io.error, "! V: Root of negative number")?;
					st.mstk.push(a);
					st.mstk.push(b);
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, na.pow(nb.clone().recip()))));
				}
			},

			//length of string or natural logarithm
			'g' => {
				if !strv {
					if *na<=0 {
						writeln!(io.error, "! g: Logarithm of non-positive number")?;
						st.mstk.push(a);
					}
					else {
						st.mstk.push(Num(Float::with_val(st.w, na.clone().ln())));
					}
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, sa.chars().count())));
				}
			},

			//base b logarithm
			'G' => {
				if *na<=0 {
					writeln!(io.error, "! G: Logarithm of non-positive number")?;
					st.mstk.push(a);
					st.mstk.push(b);
				}
				else if *nb==1||*nb<=0{
					writeln!(io.error, "! G: Logarithm with base ≤0 or =1")?;
					st.mstk.push(a);
					st.mstk.push(b);
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, na.clone().ln()/nb.clone().ln())));
				}
			},

			//sine
			'u' => {
				st.mstk.push(Num(Float::with_val(st.w, na.clone().sin())));
			},

			//cosine
			'y' => {
				st.mstk.push(Num(Float::with_val(st.w, na.clone().cos())));
			},

			//tangent
			't' => {
				st.mstk.push(Num(Float::with_val(st.w, na.clone().tan())));
			},

			//arc-sine
			'U' => {
				if na.clone().abs()>1 {
					writeln!(io.error, "! U: Arc-sine of value outside [-1,1]")?;
					st.mstk.push(a);
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, na.clone().asin())));
				}
			},

			//arc-cosine
			'Y' => {
				if na.clone().abs()>1 {
					writeln!(io.error, "! Y: Arc-cosine of value outside [-1,1]")?;
					st.mstk.push(a);
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, na.clone().acos())));
				}
			},

			//arc-tangent
			'T' => {
				st.mstk.push(Num(Float::with_val(st.w, na.clone().atan())));
			},

			//random integer [0;a)
			'N' => {
				let int = round(na);
				if int<=0 {
					writeln!(io.error, "! N: Upper bound for random value must be above 0")?;
					st.mstk.push(a);
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, int.random_below(&mut st.rng))));
				}
			},

			//constant/conversion factor lookup or convert number to string
			'"' => {
				if !strv {
					st.mstk.push(Str(flt_to_str(na.clone(), st.par.o(), st.par.k())));
				}
				else {
					match sa.matches(' ').count() {
						0 => {	//normal lookup
							if let Some(res) = get_constant(st.w, sa, safe) {
								st.mstk.push(Num(res));
							}
							else {
								writeln!(io.error, "! \": Constant/conversion factor not found")?;
								st.mstk.push(a);
							}
						},
						1 => {	//conversion shorthand, left divided by right
							let (sl, sr) = sa.split_once(' ').unwrap();
							if let (Some(nl), Some(nr)) = (get_constant(st.w, sl, safe), get_constant(st.w, sr, safe)) {
								st.mstk.push(Num(nl/nr));
							}
							else {
								writeln!(io.error, "! \": Constant/conversion factor not found")?;
								st.mstk.push(a);
							}
						},
						_ => {
							writeln!(io.error, "! \": Too many spaces in constant/conversion query")?;
							st.mstk.push(a);
						},
					}
				}
			},

			//deg -> rad shorthand
			'°' => {
				st.mstk.push(Num(na * Float::with_val(st.w, Constant::Pi) / 180));
			},
			/*------------------------
				STACK MANIPULATION
			------------------------*/
			//clear stack
			'c' => {
				st.mstk.clear();
			},

			//remove top a objects from stack
			'C' => {
				let int = round(na);
				if let Some(mut num) = int.to_usize() {
					let len = st.mstk.len();
					if num>len { num = len; }	//limit clear count
					st.mstk.truncate(len-num);
				}
				else {
					writeln!(io.error, "! C: Cannot possibly remove {int} objects from the main stack")?;
					st.mstk.push(a);
				}
			},

			//duplicate top of stack
			'd' => {
				if st.mstk.is_empty() {
					writeln!(io.error, "! d: Nothing to duplicate")?;
				}
				else {
					st.mstk.extend_from_within(st.mstk.len()-1..);
				}
			},

			//duplicate top a objects
			'D' => {
				let int = round(na);
				if let Some(num) = int.to_usize() {
					if num<=st.mstk.len() {
						st.mstk.extend_from_within(st.mstk.len()-num..);
					}
					else {
						writeln!(io.error, "! D: Not enough objects to duplicate")?;
						st.mstk.push(a);
					}
				}
				else {
					writeln!(io.error, "! D: Cannot possibly duplicate {int} objects")?;
					st.mstk.push(a);
				}
			},

			//swap top 2 objects
			'r' => {
				let len = st.mstk.len();
				if len>=2 {
					st.mstk.swap(len-2, len-1);
				}
				else {
					writeln!(io.error, "! r: Not enough objects to swap")?;
				}
			},

			//rotate top a objects
			'R' => {
				let mut int = round(na);
				if int==0 { int = Integer::from(1); }	//replace 0 with effective no-op
				if let Some(num) = int.clone().abs().to_usize() {
					let len = st.mstk.len();
					if num<=len {
						let sl = st.mstk.as_mut_slice();
						if int<0 {
							sl[len-num..].rotate_left(1);	//if negative, rotate left/down
						}
						else {
							sl[len-num..].rotate_right(1);	//right/up otherwise
						}
						st.mstk = sl.to_vec();
					}
					else {
						writeln!(io.error, "! R: Not enough objects to rotate")?;
						st.mstk.push(a);
					}
				}
				else {
					writeln!(io.error, "! R: Cannot possibly rotate {} objects", int.abs())?;
					st.mstk.push(a);
				}
			},

			//push stack depth
			'z' => {
				st.mstk.push(Num(Float::with_val(st.w, st.mstk.len())));
			},
			/*----------------
				PARAMETERS
			----------------*/
			//set output precision
			'k' => {
				if let Err(e) = st.par.set_k(round(na)) {
					writeln!(io.error, "{e}")?;
					st.mstk.push(a);
				}
			},

			//set input base
			'i' => {
				if let Err(e) = st.par.set_i(round(na)) {
					writeln!(io.error, "{e}")?;
					st.mstk.push(a);
				}
			},

			//set output base
			'o' => {
				if let Err(e) = st.par.set_o(round(na)) {
					writeln!(io.error, "{e}")?;
					st.mstk.push(a);
				}
			},

			//set working precision
			'w' => {
				let i = round(na);
				if let (Some(u), false) = (i.to_u32(), i==0_u8) {
					st.w = u;
				}
				else {
					writeln!(io.error, "! w: Working precision must be in range 1 ≤ W ≤ {}", u32::MAX)?;
					st.mstk.push(a);
				}
			},

			//push output precision
			'K' => {
				st.mstk.push(Num(Float::with_val(st.w, st.par.k())));
			},

			//push input base
			'I' => {
				st.mstk.push(Num(Float::with_val(st.w, st.par.i())));
			},

			//push output base
			'O' => {
				st.mstk.push(Num(Float::with_val(st.w, st.par.o())));
			},

			//push working precision
			'W' => {
				st.mstk.push(Num(Float::with_val(st.w, st.w)));
			},

			//create new k,i,o context
			'{' => {
				st.par.create();
			},

			//revert to previous context
			'}' => {
				st.par.destroy();
			},
			/*---------------
				REGISTERS
			---------------*/
			//save to top of register
			's' => {
				if reg.is_empty() {
					reg.push(RegObj{o: a, a: Vec::new()});
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
				if let Some(ro) = reg.last() {
					st.mstk.push(ro.o.clone());
				}
				else {
					writeln!(io.error, "! l: Register # {rnum} is empty")?;
				}
			},

			//pop from top of register
			'L' => {
				if let Some(ro) = reg.pop() {
					st.mstk.push(ro.o);
				}
				else {
					writeln!(io.error, "! L: Register # {rnum} is empty")?;
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
				let int = round(nb);
				if let Some(rai) = int.to_usize() {
					if rai>=reg.last().unwrap().a.len() {
						reg.last_mut().unwrap().a.resize(rai+1, DUMMY);	//extend if required, initialize with default objects
					}
					reg.last_mut().unwrap().a[rai] = a;
				}
				else {
					writeln!(io.error, "! :: Cannot possibly save to array index {int}")?;
					st.mstk.push(a);
					st.mstk.push(b);
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
				let int = round(na);
				if let Some(rai) = int.to_usize() {
					if rai>=reg.last().unwrap().a.len() {
						reg.last_mut().unwrap().a.resize(rai+1, DUMMY);	//extend if required, initialize with default objects
					}
					st.mstk.push(reg.last().unwrap().a[rai].clone());
				}
				else {
					writeln!(io.error, "! ;: Cannot possibly load from array index {int}")?;
					st.mstk.push(a);
				}
			},

			//pop top-of-reg into buffer
			'b' => {
				if let Some(ro) = reg.pop() {
					st.ro_buf = ro;
				}
				else {
					writeln!(io.error, "! b: Register # {rnum} is empty")?;
				}
			},

			//push buffer to register
			'B' => {
				reg.push(st.ro_buf.clone());
			},

			//push register depth
			'Z' => {
				st.mstk.push(Num(Float::with_val(st.w, reg.len())));
			},

			//specify manual register pointer
			',' => {
				st.rptr = Some(
					if !strv {
						round(na)	//from number
					}
					else {
						Integer::from_digits(sa.as_bytes(), Order::Msf)	//from string bytes
					}
				);
			},
			/*------------
				MACROS
			------------*/
			//convert least significant 32 bits to one-char string or first char of string to number
			'a' => {
				if !strv {
					let ia = round(na).to_u32_wrapping();
					if let Some(res) = char::from_u32(ia) {
						st.mstk.push(Str(res.to_string()));
					}
					else {
						writeln!(io.error, "! a: Unable to convert number {ia} to character: not a valid Unicode value")?;
						st.mstk.push(a);
					}
				}
				else if sa.is_empty() {
					writeln!(io.error, "! a: Cannot convert empty string to number")?;
					st.mstk.push(a);
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, sa.chars().next().unwrap() as u32)));
				}
			},

			//convert number to UTF-8 string or back
			'A' => {
				if !strv {
					if let Ok(res) = String::from_utf8(round(na).to_digits::<u8>(Order::Msf)) {
						st.mstk.push(Str(res));
					}
					else {
						writeln!(io.error, "! A: Unable to convert number {} to string: not a valid UTF-8 sequence", round(na))?;
						st.mstk.push(a);
					}
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, Integer::from_digits(sa.as_bytes(), Order::Msf))));
				}
			},

			//execute string as macro
			'x' => {
				if strv {
					if cmdstk.last().unwrap().is_empty() {
						cmdstk.pop();	//optimize tail call
					}
					cmdstk.push(sa.chars().collect());
				} else {
					st.mstk.push(a);
				}
			},

			//invert next conditional
			'!' => {
				inv = !inv;
			},

			//conditionally execute macro
			'<'|'='|'>' => {
				if reg.is_empty() {
					writeln!(io.error, "! <=>: Register # {rnum} is empty")?;
				}
				else if let Str(mac) = &reg.last().unwrap().o {
					if inv != match cmd {
						'<' => { nb < na },	//reverse order, GNU dc convention
						'=' => { nb == na },
						'>' => { nb > na },
						_ => {false},}
					{
						if cmdstk.last().unwrap().is_empty() {
							cmdstk.pop();	//optimize tail call
						}
						cmdstk.push(mac.chars().collect());
					}
				}
				else {
					writeln!(io.error, "! <=>: Top of register # {rnum} is not a string")?;
				}
				inv = false;	//always reset inversion
			},

			//auto-macro
			'X' => {
				let int = round(nb);
				if let Some(reps) = int.to_usize() {
					if cmdstk.last().unwrap().is_empty() {
						cmdstk.pop();	//optimize tail call
					}
					cmdstk.resize(cmdstk.len()+reps, sa.chars().collect());
				}
				else {
					writeln!(io.error, "! X: Cannot possibly repeat a macro {int} times")?;
					st.mstk.push(a);
					st.mstk.push(b);
				}
			},

			//request to quit
			'q' => {
				return Ok(Some(st.rptr.clone().unwrap_or_default()));	//rptr as exit code, always wrap in Some
			},

			//quit a macro calls
			'Q' => {
				let int = round(na);
				if let Some(mut num) = int.to_usize() {
					let len = cmdstk.len();
					if num>len {num=len;}
					cmdstk.truncate(len-num);
					if cmdstk.is_empty() {
						cmdstk.push(VecDeque::new());	//guarantee at least one object
					}
				}
				else {
					writeln!(io.error, "! Q: Cannot possibly quit {int} levels")?;
					st.mstk.push(a);
				}
			},

			//prompt and execute
			'?' => {
				let mut prompt_in = String::new();
				io.input.read_line(&mut prompt_in)?;
				if cmdstk.last().unwrap().is_empty() {
					cmdstk.pop();	//optimize tail call
				}
				cmdstk.push(prompt_in.chars().collect());
			},
			/*----------
				MISC
			----------*/
			//execute file as script
			'&' => {
				if !safe {
					match std::fs::read_to_string(sa.clone()) {
						Ok(script) => {
							let mut script_nc = String::new();	//script with comments removed
							for line in script.split('\n') {
								script_nc.push_str(line.split_once('#').unwrap_or((line,"")).0);	//remove comment on every line
								script_nc.push('\n');
							}
							cmdstk.push(script_nc.chars().collect());
						},
						Err(err) => {
							writeln!(io.error, "! &: Unable to read file \"{sa}\": {err}")?;
							st.mstk.push(a);
						},
					}
				}
				else {
					writeln!(io.error, "! &: Disabled by --safe flag")?;
					st.mstk.push(a);
				}
			},

			//get environment variable
			'$' => {
				if !safe {
					match std::env::var(sa.clone()) {
						Ok(val) => {
							st.mstk.push(Str(val));
						},
						Err(err) => {
							writeln!(io.error, "! $: Unable to get value of ${sa}: {err}")?;
							st.mstk.push(a);
						},
					}
				}
				else {
					writeln!(io.error, "! $: Disabled by --safe flag")?;
					st.mstk.push(a);
				}
			},

			//execute os command(s)
			'\\' => {
				if !safe {
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
											if code!=0 {writeln!(io.error, "! \\: OS command \"{oscmd}\" exited with code {code}")?;}
										}
									}
								},
								Err(err) => {
									writeln!(io.error, "! \\: Unable to execute OS command \"{oscmd}\": {err}")?;
									st.mstk.push(a.clone());
								},
							}
						}
					}
				}
				else {
					writeln!(io.error, "! \\: Disabled by --safe flag")?;
					st.mstk.push(a);
				}
			},

			//stop on beginning of #comment
			'#' => {
				cmdstk.last_mut().unwrap().clear();
			},

			//notify on invalid command, keep going
			_ => {
				if !cmd.is_whitespace()&&cmd!='\0' { writeln!(io.error, "! Invalid command: {cmd} (U+{:04X})", cmd as u32)?; }
			},
		}
		while let Some(ptr) = cmdstk.last() {	//clean up empty command strings
			if ptr.is_empty() {
				cmdstk.pop();
			}
			else{break;}
		}
	}
	Ok(None)
}