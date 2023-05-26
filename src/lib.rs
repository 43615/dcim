use std::io::{Write, BufRead};
use std::time::{SystemTime, Duration, Instant};
use std::thread as th;
use std::collections::{VecDeque, HashMap};
use rug::{Integer, integer::Order, Complete, Float, float::{Round, Constant, Special}, ops::Pow, rand::RandState, Assign};
use rand::{RngCore, rngs::OsRng};
#[macro_use]
extern crate lazy_static;
use phf::{phf_set, phf_map};

///basic object: either number or string
#[derive(Clone)]
pub enum Obj {
	Num(Float),
	Str(String)
}
use Obj::*;
///unused/default Obj
const DUMMY: Obj = Str(String::new());

///register object, may have a dynamic array
#[derive(Clone)]
pub struct RegObj {
	o: Obj,			//principal object
	a: Vec<Obj>	//associated array
}
pub struct Register {
	v: Vec<RegObj>,
	th: Option<th::JoinHandle<Vec<Obj>>>
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
	const fn plural(&self) -> &str {
		if matches!(self, Ax|An|As) {""} else {"s"}
	}

	#[inline(always)]
	///correction messages
	const fn correct(&self) -> &str {
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
#[derive(Clone)]
pub struct ParamStk(Vec<(Integer, Integer, Integer)>);
impl ParamStk {
	#[inline(always)]
	///switch to new param context with defaults (0,10,10)
	fn create(&mut self) {
		self.0.push((Integer::from(0_u8), Integer::from(10_u8), Integer::from(10_u8)));
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
		if n>=0 {
			self.0.last_mut().unwrap().0 = n;
			Ok(())
		}
		else {Err("Output precision must be at least 0")}
	}
	#[inline(always)]
	///checked edit of current input base
	fn set_i(&mut self, n: Integer) -> Result<(), &'static str> {
		if n>=2 {
			self.0.last_mut().unwrap().1 = n;
			Ok(())
		}
		else {Err("Input base must be at least 2")}
	}
	#[inline(always)]
	///checked edit of current output base
	fn set_o(&mut self, n: Integer) -> Result<(), &'static str> {
		if n>=2 {
			self.0.last_mut().unwrap().2 = n;
			Ok(())
		}
		else {Err("Output base must be at least 2")}
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
const REG_DEF: Register = Register {
	v: Vec::new(),
	th: None
};

///Bundled state storage for one instance of dc:im
pub struct State<'a> {
	///main stack
	pub mstk: Vec<Obj>,
	///hashmap of arbitrarily-numbered registers
	pub regs: HashMap<Integer, Register>,
	///RegObj buffer
	pub ro_buf: RegObj,
	///manual register pointer
	pub rptr: Option<Integer>,
	///random number generator
	pub rng: RandState<'a>,
	///parameters K,I,O
	pub par: ParamStk,
	///working precision W
	pub w: u32
}
impl Default for State<'_> {
	///Arbitrary initial values one might want to change:
	///- RNG: Mersenne twister seeded with 1024 bits of OS randomness
	///- Parameter stack: one entry, (K, I, O) = (0, 10, 10)
	///- Working precision: 64 bits
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
			w: 64
		}
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

	let mut man = Integer::from(0_u8);	//resulting mantissa
	let mut scale = Integer::from(1_u8);	//scale to divide by
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

	if mneg {man *= -1_i8;}

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

///all commands that use a register
static USES_REG: phf::Set<char> = phf_set! {
	'F','s','S','l','L',':',';','b','B','Z','<','=','>','m','M'
};

///command signatures and adicities
static CMD_SIGS: phf::Map<char, (CmdSig, u8)> = phf_map! {
	'+' => (AxBx, 2),
	'^' => (AxBx, 2),

	'|' => (AxBxCx, 3),

	'-' => (AxBn, 2),
	'*' => (AxBn, 2),
	'/' => (AxBn, 2),
	'%' => (AxBn, 2),
	'~' => (AxBn, 2),
	':' => (AxBn, 2),

	'm' => (As, 1),
	'&' => (As, 1),
	'$' => (As, 1),
	'\\' => (As, 1),

	'n' => (Ax, 1),
	'P' => (Ax, 1),
	'a' => (Ax, 1),
	'A' => (Ax, 1),
	'"' => (Ax, 1),
	'x' => (Ax, 1),
	'g' => (Ax, 1),
	's' => (Ax, 1),
	'S' => (Ax, 1),
	',' => (Ax, 1),

	'X' => (AsBn, 2),

	'v' => (An, 1),
	'T' => (An, 1),
	'N' => (An, 1),
	'C' => (An, 1),
	'D' => (An, 1),
	'R' => (An, 1),
	'k' => (An, 1),
	'i' => (An, 1),
	'o' => (An, 1),
	'w' => (An, 1),
	';' => (An, 1),
	'Q' => (An, 1),
	'M' => (An, 1),

	'V' => (AnBn, 2),
	'G' => (AnBn, 2),
	't' => (AnBn, 2),
	'<' => (AnBn, 2),
	'=' => (AnBn, 2),
	'>' => (AnBn, 2),
};

lazy_static! {
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
		m.insert("c", FltGen::val(299_792_458_u32));
		m.insert("hbar", FltGen(Box::new(|prec| FltGen::sci(662_607_015_u32, -42_i8).0(prec) / FltGen::rec("pi", &|n| n*2_u8).0(prec))));
		m.insert("G", FltGen::sci(6674_u16, -3_i8));
		m.insert("qe", FltGen::sci(1_602_176_634_u32, -28_i8));
		m.insert("NA", FltGen::sci(602_214_076_u32, 31_u8));
		m.insert("kB", FltGen::sci(1_380_649_u32, -29_i8));
		m.insert("u", FltGen::sci(1_660_539_066_u32, -36_i8));
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
		m.insert("AU", FltGen::val(149_597_870_700_u64));
		m.insert("ly", FltGen::val(9_460_730_472_580_800_u64));
		m.insert("pc", FltGen(Box::new(|prec| Float::with_val(prec, 96_939_420_213_600_000_u64)/Float::with_val(prec, Constant::Pi))));
		/*-------------------------------
			AREA & VOLUME UNITS
			with no length equivalent
		-------------------------------*/
		for q in ["ac","acre"] {m.insert(q, FltGen::sci(40_468_564_224_u64, -7_i8));}
		m.insert("l", FltGen::sci(1_u8, -3_i8));
		m.insert("ifloz", FltGen::sci(284_130_625_u32, -13_i8));
		m.insert("ipt", FltGen::rec("ifloz", &|n| n*20_u8));
		m.insert("iqt", FltGen::rec("ifloz", &|n| n*40_u8));
		m.insert("igal", FltGen::rec("ifloz", &|n| n*160_u8));
		for q in ["ibu","ibsh"] {m.insert(q, FltGen::rec("ifloz", &|n| n*1280_u16));}
		m.insert("ufldr", FltGen::sci(36_966_911_953_125_u64, -19_i8));
		m.insert("tsp", FltGen::rec("ufldr", &|n| n/3_u8*4_u8));
		m.insert("tbsp", FltGen::rec("ufldr", &|n| n*4_u8));
		m.insert("ufloz", FltGen::rec("ufldr", &|n| n*8_u8));
		m.insert("upt", FltGen::rec("ufloz", &|n| n*16_u8));
		m.insert("uqt", FltGen::rec("ufloz", &|n| n*32_u8));
		m.insert("ugal", FltGen::rec("ufloz", &|n| n*128_u8));
		m.insert("bbl", FltGen::rec("ugal", &|n| n*42_u8));
		m.insert("udpt", FltGen::sci(5_506_104_713_575_u64, -16_i8));
		m.insert("udqt", FltGen::rec("udpt", &|n| n*2_u8));
		m.insert("udgal", FltGen::rec("udpt", &|n| n*8_u8));
		for q in ["ubu","ubsh"] {m.insert(q, FltGen::rec("udpt", &|n| n*64_u8));}
		m.insert("dbbl", FltGen::sci(115_627_123_584_i64, -12_i8));
		/*----------------
			MASS UNITS
		----------------*/
		m.insert("ct", FltGen::sci(2_u8, -4_i8));
		m.insert("oz", FltGen::sci(28_349_523_125_u64, -12_i8));
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
		m.insert("ag", FltGen::rec("d", &|n| n*3_652_425_u32/10000_u16));
		/*-----------------
			OTHER UNITS
		-----------------*/
		m.insert("J", FltGen::val(1_u8));
		m.insert("cal", FltGen::sci(4184_u16, -3_i8));
		m.insert("Pa", FltGen::val(1_u8));
		m.insert("atm", FltGen::val(101_325_u32));
		m.insert("psi", FltGen::sci(6_894_757_293_168_u64, -9_i8));
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
///trim trailing 0s and .
fn trim_0s(v: &mut Vec<u8>) {
	while v.last()==Some(&b'0') {v.pop();}
	if v.last()==Some(&b'.') {v.pop();}
}

#[inline(always)]
///custom number printing function:
///if output base is over 36, prints in custom "any-base" notation,
///otherwise, applies precision and converts from exponential notation if not too small
fn flt_to_str(mut num: Float, obase: Integer, oprec: Integer) -> String {
	//handle special cases
	if !num.is_normal() {
		let mut ret = String::new();

		if num.is_sign_negative() {ret.push('-');}

		if num.is_zero() {ret.push('0');}
		else if num.is_infinite() {ret.push('∞');}
		else {ret.push_str("NaN");}	//only remaining case

		return ret;
	}

	if obase>36_u8 {	//any-base printing (original base conversion algorithm out of necessity, limited precision possible)
		let mut outstr = String::from(if num.is_sign_negative() {"(-"} else {"("});	//apply negative sign
		num.abs_mut();
		let mut scale = 0;	//amount to shift fractional separator in output
		while !num.is_integer()&&(oprec==0_u8||scale<oprec) {	//turn into integer scaled by power of obase, apply output precision if enabled
			let temp = num.clone() * &obase;	//preview scale-up
			if temp.is_infinite() {	//possible with high precision due to Float's exponent limitation
				num /= &obase;	//prevent overflow in later "extra precision" part
				break;	//disregard further precision
			}
			num = temp;	//if ok, commit to scale-up
			scale += 1;
		}
		num *= &obase;	//get extra precision for last digit
		let mut int = num.to_integer().unwrap();	//convert to Integer
		int /= &obase;	//undo extra precision
		let mut dig = Integer::from(1_u8);	//current digit value
		while dig<=int {
			dig *= &obase;	//get highest required digit value
		}
		dig /= &obase;	//correct off-by-one error
		loop {	//separate into digits
			let (quot, rem) = int.clone().div_rem_euc(dig.clone());	//separate into amount of current digit and remainder
			outstr.push_str(&quot.to_string());	//print amount of current digit
			outstr.push(' ');
			int = rem;	//switch to remainder
			if dig==1_u8 {break;}	//stop when all digits done
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
		//raw string from rug, byte vec of nonzero size
		let mut ret = num.to_string_radix(
			obase.to_i32().unwrap(),	//range already checked
			oprec.to_usize()	//None = too large anyway, print all
		);
		let bytes = unsafe {ret.as_mut_vec()};	//only dealing with ascii, chillax

		let mneg = bytes[0]==b'-';	//mantissa negative?

		if let Some(elen) =	//check if in scientific notation and get length of exp part
			bytes.iter().rev().take(12)	//only consider the last 12 chars, longest possible exponent is @-1073741824
			.position(|c| *c==if obase<=10_u8 {b'e'} else {b'@'})	//rug prints with 'e' if O<=10
		{
			let mut epart = bytes.split_off(bytes.len()-elen);	//isolate exponent
			bytes.pop();	//remove exponent symbol

			if epart[0]==b'-' {	//negative exponent
				if elen==2 {	//exponent above -10, convert to normal notation
					let eint = unsafe {String::from_utf8_unchecked(epart)[1..].parse::<usize>().unwrap()};	//numerical value of epart / total # of 0s
					let mut buf = vec![b'0', b'.'];	//start with fixed 0.
					buf.resize(eint + 1, b'0');	//0s before mantissa
					buf.push(bytes[mneg as usize]);	//digit before original .
					buf.extend(bytes.split_off(2 + mneg as usize));	//remaining digits
					bytes.clear();	//start fresh
					if mneg {bytes.push(b'_');}	//negative sign
					bytes.append(&mut buf);
					trim_0s(bytes);
				}
				else {	//reassemble scientific notation
					if mneg {bytes[0] = b'_';}	//negative sign
					trim_0s(bytes);
					bytes.push(b'@');    //add unified exponent symbol
					epart[0]=b'_';
					bytes.append(&mut epart);    //and exponent itself
				}
			}
			else {	//just reassemble
				if mneg {bytes[0] = b'_';}	//negative sign
				trim_0s(bytes);
				bytes.push(b'@');    //add unified exponent symbol
				bytes.append(&mut epart);    //and exponent itself
			}
		}
		else {	//in normal notation
			if mneg {bytes[0] = b'_';}	//negative sign
			if bytes.iter().any(|x| *x==b'.') {	//. present, safe to trim end
				trim_0s(bytes);
			}
		}

		ret
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

///Happy results of [`exec()`]
pub enum ExecDone {
	///Commands ran to completion, state should probably be kept
	Finished,
	///Exit request by `q` with exit code, state should probably be discarded
	Quit(i32)
}
use ExecDone::*;

/**
*CORE LANGUAGE IMPLEMENTATION*

# Parameters

## `st`
Reference to the state storage to work on, modified in-place

## `io`
Optional bundle of IO streams, fields are used as follows:
- input: Read by the command `?` one line at a time
- output: Normal printing by the commands `pfnPF`
- error: Receives dc:im error messages, one per line
If `None` is given, IO is disabled and the resulting `State` must be read manually to get useful results.

## `safe`
Safety toggle: Disables commands that interact with the OS, as well as terminating pseudoconstants.
Be aware that this does not prevent infinite loops (`[lax]salax`).

## `cmds`
dc:im commands to execute

# Returns
See [`ExecDone`]

# Errors
Only if a write/read on an IO stream fails

# Panics
Probably never™
*/
pub fn exec(st: &mut State, io: Option<&mut IOTriple>, safe: bool, cmds: &str) -> std::io::Result<ExecDone> {
	let no_io = IOTriple {
		input: &mut std::io::BufReader::new(std::io::empty()),
		output: &mut std::io::sink(),
		error: &mut std::io::sink()
	};
	let (input, output, error): (&mut dyn BufRead, &mut dyn Write, &mut dyn Write) = if let Some(tri) = io {
		(tri.input, tri.output, tri.error)
	}
	else {
		(no_io.input, no_io.output, no_io.error)
	};
	let mut cmdstk: Vec<VecDeque<char>> = vec!(cmds.chars().collect());	//stack of command strings to execute, enables pseudorecursive macro calls
	let mut inv = false;	//invert next comparison

	let mut dummy_reg = REG_DEF;	//required for let syntax, never accessed

	//default contents of string/number slots
	let sx_dummy = String::new();
	let nx_dummy = Float::new(1);

	while !cmdstk.is_empty() {	//last().unwrap() is guaranteed to not panic within

		let mut cmd = cmdstk.last_mut().unwrap().pop_front().unwrap_or_default();	//get next command

		let (reg, rnum): (&mut Register, Integer) = if USES_REG.contains(&cmd) {	//get register reference, before the other checks since it's syntactically significant ("sq" etc)
			let i = if let Some(i) = st.rptr.take() {i}	//take index from reg ptr
			else if let Some(c) = cmdstk.last_mut().unwrap().pop_front() {Integer::from(c as u32)}	//steal next command char as index
			else {
				writeln!(error, "! Command '{cmd}' needs a register number")?;
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
			writeln!(error, "! Command '{cmd}' needs {} argument{}", adi, sig.plural())?;
			continue;
		}

		let (c, b, a) = match adi {	//pop required objects from stack
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
			writeln!(error, "! Wrong argument type{} for command '{cmd}': {}", sig.plural(), sig.correct())?;
			match adi {	//push Objs back
				1 => {st.mstk.push(a);},
				2 => {st.mstk.push(a); st.mstk.push(b);},
				3 => {st.mstk.push(a); st.mstk.push(b); st.mstk.push(c);},
				_ => {}
			}
			continue;
		}

		if let Some(cmderr) = match cmd {
			/*------------------
				OBJECT INPUT
			------------------*/
			//standard number input, force with single quote to use letters
			'0'..='9'|'.'|'_'|'\''|'@' => {
				if st.par.i()>36_u8 {
					Some("! Any-base input must be used for input bases over 36".into())
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
							if cmd == '.' { if frac { break; } frac = true; } //break on encountering second '.'
							if cmd == '@' { neg = false; }	//allow for second negative sign in exponent
							numstr.push(cmd);
						}
						//'_' needs to be replaced with '-'
						else if cmd == '_' {
							if neg { break; } neg = true;	//break on encountering second '_'
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
							None
						},
						Err(err) => {
							Some(format!("! Unable to parse number \"{numstr}\": {err}"))
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
					Ok(n) => {st.mstk.push(Num(n)); None}
					Err(e) => {Some(format!("! Unable to parse any-base number: {e}"))}
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
						break None;
					}
					if cmdstk.last().unwrap().is_empty() {	//only reached on improper string
						break Some(format!("! Unable to parse string \"[{res}\": missing closing bracket"));
					}
					cmd = cmdstk.last_mut().unwrap().pop_front().unwrap();
				}
			},
			/*--------------
				PRINTING
			--------------*/
			//print top with newline
			'p' => {
				match st.mstk.last() {
					Some(Num(n)) => {writeln!(output, "{}", flt_to_str(n.clone(), st.par.o(), st.par.k()))?;},
					Some(Str(s)) => {writeln!(output, "[{s}]")?;},
					None => {}
				}
				None
			},

			//print full stack top to bottom
			'f' => {
				if !st.mstk.is_empty() {
					for i in st.mstk.iter().rev() {
						match i {
							Num(n) => {writeln!(output, "{}", flt_to_str(n.clone(), st.par.o(), st.par.k()))?;},
							Str(s) => {writeln!(output, "[{s}]")?;},
						}
					}
				}
				None
			},

			//pop and print without newline
			'n' => {
				if !strv {
					write!(output, "{}", flt_to_str(na.clone(), st.par.o(), st.par.k()))?;
					output.flush().unwrap();
				}
				else {
					write!(output, "{sa}")?;
					output.flush().unwrap();
				}
				None
			},

			//pop and print with newline
			'P' => {
				if !strv {writeln!(output, "{}", flt_to_str(na.clone(), st.par.o(), st.par.k()))?;}
				else {writeln!(output, "{sa}")?;}
				None
			},

			//print register
			'F' => {
				if !reg.v.is_empty(){
					for i in (0..reg.v.len()).rev() {
						match &reg.v[i].o {
							Num(n) => {writeln!(output, "{}", flt_to_str(n.clone(), st.par.o(), st.par.k()))?;},
							Str(s) => {writeln!(output, "[{s}]")?;},
						}
						let width = (reg.v[i].a.len()-1).to_string().len();	//length of longest index number
						for (ai, o) in reg.v[i].a.iter().enumerate() {
							match o {
								Num(n) => {writeln!(output, "\t{ai:>width$}: {}", flt_to_str(n.clone(), st.par.o(), st.par.k()))?;},
								Str(s) => {writeln!(output, "\t{ai:>width$}: [{s}]")?;},
							}
						}
					}
				}
				None
			},
			/*----------------
				ARITHMETIC
				+str manip
			----------------*/
			//add or concatenate strings
			'+' => {
				if !strv {st.mstk.push(Num(Float::with_val(st.w, na + nb)));}
				else {st.mstk.push(Str(sa.clone() + sb));}
				None
			},

			//subtract or remove chars from string
			'-' => {
				if !strv {st.mstk.push(Num(Float::with_val(st.w, na - nb))); None}
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
						None
					}
					else {
						Some(format!("Cannot possibly remove {ib} characters from a string"))
					}
				}
			},

			//multiply or repeat/invert string
			'*' => {
				if !strv {st.mstk.push(Num(Float::with_val(st.w, na * nb))); None}
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
						None
					}
					else {
						Some(format!("Cannot possibly repeat a string {ib} times"))
					}
				}
			},

			//divide or shorten string to length
			'/' => {
				if !strv {
					if nb.is_zero() {
						Some("Division by zero".into())
					}
					else {
						st.mstk.push(Num(Float::with_val(st.w, na / nb)));
						None
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
						None
					}
					else {
						Some(format!("Cannot possibly shorten a string to {ib} characters"))
					}
				}
			},

			//modulo or isolate char
			'%' => {
				if !strv {
					let ia = round(na);
					let ib = round(nb);
					if ib==0 {
						Some("Reduction mod 0".into())
					}
					else {
						st.mstk.push(Num(Float::with_val(st.w, ia % ib)));
						None
					}
				}
				else {
					let ib = round(nb);
					if let Some(n) = ib.to_usize() {
						if let Some(c) = sa.chars().nth(n) {
							st.mstk.push(Str(c.into()));
							None
						}
						else {
							Some(format!("String is too short for index {n}"))
						}
					}
					else {
						Some(format!("Cannot possibly extract character at index {ib}"))
					}
				}
			},

			//euclidean division or split string
			'~' => {
				if !strv {
					let ia = round(na);
					let ib = round(nb);
					if ib==0_u8 {
						Some("Reduction mod 0".into())
					}
					else {
						let (quot, rem)=ia.div_rem_euc(ib);
						st.mstk.push(Num(Float::with_val(st.w, quot)));
						st.mstk.push(Num(Float::with_val(st.w, rem)));
						None
					}
				}
				else {
					let ib = round(nb);
					if let Some(n) = ib.to_usize() {
						st.mstk.push(Str(sa.chars().take(n).collect()));
						st.mstk.push(Str(sa.chars().skip(n).collect()));
						None
					}
					else {
						Some(format!("Cannot possibly split a string at character {ib}"))
					}
				}
			},

			//exponentiation or find in string
			'^' => {
				if !strv {
					if *na<0_u8 && nb.clone().abs()<1_u8{
						Some("Root of negative number".into())
					}
					else {
						st.mstk.push(Num(Float::with_val(st.w, na.pow(nb))));
						None
					}
				}
				else if let Some(bidx) = sa.find(sb) {	//find byte index
					let cidx = sa.char_indices().position(|x| x.0==bidx).unwrap();	//corresp. char index
					st.mstk.push(Num(Float::with_val(st.w, cidx)));
					None
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, -1_i8)));	//not found, silent error
					None
				}
			},

			//modular exponentiation or find/replace in string
			'|' => {
				if !strv {
					let ia = round(na);
					let ib = round(nb);
					let ic = round(nc);
					if ic==0_u8 {
						Some("Reduction mod 0".into())
					}
					else if let Ok(res) = ia.clone().pow_mod(&ib, &ic) {
						st.mstk.push(Num(Float::with_val(st.w, res)));
						None
					}
					else {
						Some(format!("{ia} doesn't have an inverse mod {ic}"))
					}
				}
				else {
					st.mstk.push(Str(sa.replace(sb, sc)));
					None
				}
			},

			//square root
			'v' => {
				if *na<0_u8 {
					Some("Root of negative number".into())
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, na.clone().sqrt())));
					None
				}
			},

			//bth root
			'V' => {
				if *na<0_u8 && nb.clone().abs()>1{
					Some("Root of negative number".into())
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, na.pow(nb.clone().recip()))));
					None
				}
			},

			//length of string or natural logarithm
			'g' => {
				if !strv {
					if *na<=0_u8 {
						Some("Logarithm of non-positive number".into())
					}
					else {
						st.mstk.push(Num(Float::with_val(st.w, na.clone().ln())));
						None
					}
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, sa.chars().count())));
					None
				}
			},

			//base b logarithm
			'G' => {
				if *na<=0_u8 {
					Some("Logarithm of non-positive number".into())
				}
				else if *nb==1_u8||*nb<=0_u8{
					Some("Logarithm with base ≤0 or =1".into())
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, na.clone().ln()/nb.clone().ln())));
					None
				}
			},

			//trig functions
			't' => {
				let int = round(nb);
				match match int.to_i8() {
					Some(i) if (-6..=6).contains(&i) => {
						match i {
							//deg -> rad
							0 => Ok(na * Float::with_val(st.w, Constant::Pi) / 180_u8),

							//trig
							1 => Ok(Float::with_val(st.w, na.sin_ref())),
							2 => Ok(Float::with_val(st.w, na.cos_ref())),
							3 => Ok(Float::with_val(st.w, na.tan_ref())),

							//hyper
							4 => Ok(Float::with_val(st.w, na.sinh_ref())),
							5 => Ok(Float::with_val(st.w, na.cosh_ref())),
							6 => Ok(Float::with_val(st.w, na.tanh_ref())),

							//inv trig
							-1 => if na.clone().abs()<=1_u8 {
								Ok(Float::with_val(st.w, na.asin_ref()))
							}
							else {
								Err("Inverse sine of value outside [-1,1]")
							},
							-2 => if na.clone().abs()<=1_u8 {
								Ok(Float::with_val(st.w, na.acos_ref()))
							}
							else {
								Err("Inverse cosine of value outside [-1,1]")
							},
							-3 => Ok(Float::with_val(st.w, na.atan_ref())),

							//inv hyper
							-4 => Ok(Float::with_val(st.w, na.asinh_ref())),
							-5 => if na>=&1_u8 {
								Ok(Float::with_val(st.w, na.acosh_ref()))
							}
							else {
								Err("Inverse hyperbolic cosine of value below 1")
							},
							-6 => if na.clone().abs()<1_u8 {
								Ok(Float::with_val(st.w, na.atanh_ref()))
							}
							else {
								Err("Inverse hyperbolic tangent of value outside (-1,1)")
							},

							//impossible
							_ => Err("")
						}
					}.map_err(|e| e.into()),
					_ => {
						Err(format!("Function # {int} doesn't exist"))
					}
				}
				{
					Ok(n) => {
						st.mstk.push(Num(n));
						None
					}
					Err(e) => {
						Some(e)
					}
				}
			},

			//delay
			'T' => {
				let int = round(na);
				if let Some(u) = int.to_u64() {
					th::sleep(Duration::from_millis(u));
					None
				}
				else {
					Some(format!("Cannot possibly wait {int} milliseconds"))
				}
			},

			//random integer [0;a)
			'N' => {
				let int = round(na);
				if int<=0_u8 {
					Some("Upper bound for random value must be above 0".into())
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, int.random_below(&mut st.rng))));
					None
				}
			},

			//constant/conversion factor lookup or convert number to string
			'"' => {
				if !strv {
					st.mstk.push(Str(flt_to_str(na.clone(), st.par.o(), st.par.k())));
					None
				}
				else {
					match sa.matches(' ').count() {
						0 => {	//normal lookup
							if let Some(res) = get_constant(st.w, sa, safe) {
								st.mstk.push(Num(res));
								None
							}
							else {
								Some("! \": Constant/conversion factor not found".into())
							}
						},
						1 => {	//conversion shorthand, left divided by right
							let (sl, sr) = sa.split_once(' ').unwrap();
							if let (Some(nl), Some(nr)) = (get_constant(st.w, sl, safe), get_constant(st.w, sr, safe)) {
								st.mstk.push(Num(nl/nr));
								None
							}
							else {
								Some("! \": Constant/conversion factor not found".into())
							}
						},
						_ => {
							Some("! \": Too many spaces in constant/conversion query".into())
						},
					}
				}
			},

			/*------------------------
				STACK MANIPULATION
			------------------------*/
			//clear stack
			'c' => {
				st.mstk.clear();
				None
			},

			//remove top a objects from stack
			'C' => {
				let int = round(na);
				if let Some(mut num) = int.to_usize() {
					let len = st.mstk.len();
					if num>len { num = len; }	//limit clear count
					st.mstk.truncate(len-num);
					None
				}
				else {
					Some(format!("Cannot possibly remove {int} objects from the main stack"))
				}
			},

			//duplicate top of stack
			'd' => {
				if st.mstk.is_empty() {
					Some("Nothing to duplicate".into())
				}
				else {
					st.mstk.extend_from_within(st.mstk.len()-1..);
					None
				}
			},

			//duplicate top a objects
			'D' => {
				let int = round(na);
				if let Some(num) = int.to_usize() {
					if num<=st.mstk.len() {
						st.mstk.extend_from_within(st.mstk.len()-num..);
						None
					}
					else {
						Some("Not enough objects to duplicate".into())
					}
				}
				else {
					Some(format!("Cannot possibly duplicate {int} objects"))
				}
			},

			//swap top 2 objects
			'r' => {
				let len = st.mstk.len();
				if len>=2 {
					st.mstk.swap(len-2, len-1);
					None
				}
				else {
					Some("Not enough objects to swap".into())
				}
			},

			//rotate top a objects
			'R' => {
				let mut int = round(na);
				if int==0_u8 { int = Integer::from(1_u8); }	//replace 0 with effective no-op
				if let Some(num) = int.clone().abs().to_usize() {
					let len = st.mstk.len();
					if num<=len {
						let sl = st.mstk.as_mut_slice();
						if int<0_u8 {
							sl[len-num..].rotate_left(1);	//if negative, rotate left/down
						}
						else {
							sl[len-num..].rotate_right(1);	//right/up otherwise
						}
						st.mstk = sl.to_vec();
						None
					}
					else {
						Some("Not enough objects to rotate".into())
					}
				}
				else {
					Some(format!("Cannot possibly rotate {} objects", int.abs()))
				}
			},

			//push stack depth
			'z' => {
				st.mstk.push(Num(Float::with_val(st.w, st.mstk.len())));
				None
			},
			/*----------------
				PARAMETERS
			----------------*/
			//set output precision
			'k' => {
				if let Err(e) = st.par.set_k(round(na)) {
					Some(e.into())
				}
				else {None}
			},

			//set input base
			'i' => {
				if let Err(e) = st.par.set_i(round(na)) {
					Some(e.into())
				}
				else {None}
			},

			//set output base
			'o' => {
				if let Err(e) = st.par.set_o(round(na)) {
					Some(e.into())
				}
				else {None}
			},

			//set working precision
			'w' => {
				let i = round(na);
				if let (Some(u), false) = (i.to_u32(), i==0_u8) {
					st.w = u;
					None
				}
				else {
					Some(format!("Working precision must be in range 1 ≤ W ≤ {}", u32::MAX))
				}
			},

			//push output precision
			'K' => {
				st.mstk.push(Num(Float::with_val(st.w, st.par.k())));
				None
			},

			//push input base
			'I' => {
				st.mstk.push(Num(Float::with_val(st.w, st.par.i())));
				None
			},

			//push output base
			'O' => {
				st.mstk.push(Num(Float::with_val(st.w, st.par.o())));
				None
			},

			//push working precision
			'W' => {
				st.mstk.push(Num(Float::with_val(st.w, st.w)));
				None
			},

			//create new k,i,o context
			'{' => {
				st.par.create();
				None
			},

			//revert to previous context
			'}' => {
				st.par.destroy();
				None
			},
			/*---------------
				REGISTERS
			---------------*/
			//save to top of register
			's' => {
				if reg.v.is_empty() {
					reg.v.push(RegObj{o: a.clone(), a: Vec::new()});
				}
				else {
					reg.v.last_mut().unwrap().o = a.clone();
				}
				None
			},

			//push to top of register
			'S' => {
				reg.v.push(RegObj{o: a.clone(), a: Vec::new()});
				None
			},

			//load from top of register
			'l' => {
				if let Some(ro) = reg.v.last() {
					st.mstk.push(ro.o.clone());
					None
				}
				else {
					Some(format!("Register # {rnum} is empty"))
				}
			},

			//pop from top of register
			'L' => {
				if let Some(ro) = reg.v.pop() {
					st.mstk.push(ro.o);
					None
				}
				else {
					Some(format!("Register # {rnum} is empty"))
				}
			},

			//save to top-of-register's array
			':' => {
				if reg.v.is_empty() {
					reg.v.push(RegObj {
						o: DUMMY,	//create default register object if empty
						a: Vec::new()
					});
				}
				let int = round(nb);
				if let Some(rai) = int.to_usize() {
					if rai>=reg.v.last().unwrap().a.len() {
						reg.v.last_mut().unwrap().a.resize(rai+1, DUMMY);	//extend if required, initialize with default objects
					}
					reg.v.last_mut().unwrap().a[rai] = a.clone();
					None
				}
				else {
					Some(format!("Cannot possibly save to array index {int}"))
				}
			},

			//load from top-of-register's array
			';' => {
				if reg.v.is_empty() {
					reg.v.push(RegObj {
						o: DUMMY,	//create default register object if empty
						a: Vec::new()
					});
				}
				let int = round(na);
				if let Some(rai) = int.to_usize() {
					if rai>=reg.v.last().unwrap().a.len() {
						reg.v.last_mut().unwrap().a.resize(rai+1, DUMMY);	//extend if required, initialize with default objects
					}
					st.mstk.push(reg.v.last().unwrap().a[rai].clone());
					None
				}
				else {
					Some(format!("Cannot possibly load from array index {int}"))
				}
			},

			//pop top-of-reg into buffer
			'b' => {
				if let Some(ro) = reg.v.pop() {
					st.ro_buf = ro;
					None
				}
				else {
					Some(format!("Register # {rnum} is empty"))
				}
			},

			//push buffer to register
			'B' => {
				reg.v.push(st.ro_buf.clone());
				None
			},

			//push register depth
			'Z' => {
				st.mstk.push(Num(Float::with_val(st.w, reg.v.len())));
				None
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
				None
			},
			/*------------
				MACROS
			------------*/
			//convert least significant 32 bits to one-char string or first char of string to number
			'a' => {
				if !strv {
					let ia = round(na).to_u32_wrapping();
					if let Some(res) = char::from_u32(ia) {
						st.mstk.push(Str(res.into()));
						None
					}
					else {
						Some(format!("Unable to convert number {ia} to character: not a valid Unicode value"))
					}
				}
				else if sa.is_empty() {
					Some("Cannot convert empty string to number".into())
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, sa.chars().next().unwrap() as u32)));
					None
				}
			},

			//convert number to UTF-8 string or back
			'A' => {
				if !strv {
					if let Ok(res) = String::from_utf8(round(na).to_digits::<u8>(Order::Msf)) {
						st.mstk.push(Str(res));
						None
					}
					else {
						Some(format!("Unable to convert number {} to string: not a valid UTF-8 sequence", round(na)))
					}
				}
				else {
					st.mstk.push(Num(Float::with_val(st.w, Integer::from_digits(sa.as_bytes(), Order::Msf))));
					None
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
					st.mstk.push(a.clone());
				}
				None
			},

			//conditionally execute macro
			'<'|'='|'>' => {
				if reg.v.is_empty() {
					Some(format!("! <=>: Register # {rnum} is empty"))
				}
				else if let Str(mac) = &reg.v.last().unwrap().o {
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
					None
				}
				else {
					Some(format!("! <=>: Top of register # {rnum} is not a string"))
				}
			},

			//auto-macro
			'X' => {
				let int = round(nb);
				if let Some(reps) = int.to_usize() {
					if cmdstk.last().unwrap().is_empty() {
						cmdstk.pop();	//optimize tail call
					}
					cmdstk.resize(cmdstk.len()+reps, sa.chars().collect());
					None
				}
				else {
					Some(format!("Cannot possibly repeat a macro {int} times"))
				}
			},

			//run macro in child thread
			'm' => {
				if reg.th.is_some() {
					Some(format!("Register # {rnum} is already occupied by a thread"))
				}
				else {
					//snapshot of current state
					let snap = State {
						mstk: st.mstk.clone(),
						ro_buf: st.ro_buf.clone(),
						rptr: st.rptr.clone(),
						rng: st.rng.clone(),
						par: st.par.clone(),
						w: st.w,
						regs: {    //processing to remove JoinHandles
							let mut m: HashMap<Integer, Register> = HashMap::new();
							for curr in st.regs.iter() {
								m.insert(curr.0.clone(), Register {
									v: curr.1.v.clone(),
									th: None
								});
							}
							m
						}
					};

					let cmds = sa.clone();

					//start thread
					let handle = th::spawn(move || {
						let mut subst = State {	//sub-state
							mstk: snap.mstk,
							ro_buf: snap.ro_buf,
							rptr: snap.rptr,
							par: snap.par,
							w: snap.w,
							regs: snap.regs,
							.. Default::default()
						};
						let _ = exec(&mut subst, None, safe, &cmds);
						subst.mstk
					});

					//lock register
					st.regs.get_mut(&rnum).unwrap().th = Some(handle);
					None
				}
			},

			//wait for macro to finish
			'M' => {
				if reg.th.is_none() {
					Some(format!("Register # {rnum} is not occupied by a thread"))
				}
				else {
					let handle = reg.th.take().unwrap();
					let timeout = Instant::now() + Duration::from_millis(round(na).to_u64().unwrap_or(u64::MAX));
					loop {
						if handle.is_finished() {
							for o in handle.join().unwrap() {	//put results into register
								reg.v.push(RegObj {
									a: Vec::new(),
									o
								});
							}
							break;
						}
						else if Instant::now()>=timeout {	//not finished
							reg.th = Some(handle);	//put handle back
							break;
						}
						th::sleep(Duration::from_millis(1));
					}
					None
				}
			},

			//request to quit
			'q' => {
				return Ok(Quit(st.rptr.clone().unwrap_or_default().to_i32().unwrap_or_default()));	//rptr as exit code
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
					None
				}
				else {
					Some(format!("Cannot possibly quit {int} levels"))
				}
			},

			//prompt and execute
			'?' => {
				let mut prompt_in = String::new();
				input.read_line(&mut prompt_in)?;
				prompt_in = prompt_in.trim_end_matches(['\n','\r']).into();
				if inv {
					st.mstk.push(Str(prompt_in));
				}
				else {
					if cmdstk.last().unwrap().is_empty() {
						cmdstk.pop();    //optimize tail call
					}
					cmdstk.push(prompt_in.chars().collect());
				}
				None
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
							None
						},
						Err(err) => {
							Some(format!("Unable to read file \"{sa}\": {err}"))
						},
					}
				}
				else {
					Some("Disabled by --safe flag".into())
				}
			},

			//get environment variable
			'$' => {
				if !safe {
					match std::env::var(sa.clone()) {
						Ok(val) => {
							st.mstk.push(Str(val));
							None
						},
						Err(err) => {
							Some(format!("Unable to get value of ${sa}: {err}"))
						},
					}
				}
				else {
					Some("Disabled by --safe flag".into())
				}
			},

			//execute os command(s)
			'\\' => {
				if !safe {
					if let Some((var, val)) = sa.split_once('=') {	//set variable
						std::env::set_var(var, val);
						None
					}
					else {	//normal command
						let mut args: Vec<&str> = sa.trim().split(' ').collect();
						match std::process::Command::new(args.remove(0)).args(args).spawn() {
							Ok(mut child) => {
								let stat = child.wait().unwrap();
								match stat.code() {
									Some(code) if code!=0 => {Some(format!("OS command \"{sa}\" exited with code {code}"))},
									_ => None
								}
							},
							Err(err) => {
								st.mstk.push(a.clone());
								Some(format!("Unable to execute OS command \"{sa}\": {err}"))
							},
						}
					}
				}
				else {
					Some("Disabled by --safe flag".into())
				}
			},

			//stop on beginning of #comment
			'#' => {
				cmdstk.last_mut().unwrap().clear();
				None
			},

			//notify on invalid command, keep going
			_ => {
				if !cmd.is_whitespace()&&cmd!='\0'&&cmd!='!' { Some(format!("Invalid command (U+{:04X})", cmd as u32)) }
				else {None}
			},
		}
		{
			writeln!(error, "! {cmd}: {cmderr}")?;	//print command error
			match adi {	//return arguments to stack
				1 => {st.mstk.push(a);},
				2 => {st.mstk.push(a); st.mstk.push(b);},
				3 => {st.mstk.push(a); st.mstk.push(b); st.mstk.push(c);},
				_ => {}
			}
		}
		inv = false;	//reset inversion
		if cmd=='!' {inv = true;}	//invert next command
		while let Some(ptr) = cmdstk.last() {	//clean up empty command strings
			if ptr.is_empty() {
				cmdstk.pop();
				inv = false;	//reset inversion
			}
			else{break;}
		}
	}
	Ok(Finished)
}