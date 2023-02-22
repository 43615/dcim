use dcim::*;
use read_input::prelude::*;

const HELPMSG: &str = "\
╭─────────────────────────╮
│   ╷           •         │
│   │                     │
│ ╭─┤  ╭─╴  •  ╶┤   ┌─┬─╮ │
│ │ │  │        │   │ │ │ │
│ ╰─┘  ╰─╴  •  ╶┴╴  ╵   ╵ │
╰─────────────────────────╯

dc improved - Expanded rewrite of a classic RPN calculator / esoteric programming language
Core principles of GNU dc are preserved, full documentation at https://github.com/43615/dcim/wiki

Command line options:
(order/position/repetition of --flags doesn't matter)

<nothing>
	Defaults to \"-i\".

--inter|-i [PROMPT]
	Interactive mode, standard prompt-eval loop. A custom prompt may be provided, default is \"> \".

--expr|-e [--inter|-i] EXPR1 [EXPR2] [EXPR3] ...
	Expression mode, executes expressions in order. If combined with -i, enters interactive mode after expressions are finished.

[--file|-f] [--inter|-i] FILE1 [FILE2] [FILE3] ...
	File mode, executes contents of files in order. May also be combined with -i.
	For each line in the file(s), comments (following the first #) are removed before execution.
	-f is optional: If at least one option is provided without any --flags, file mode is implied.

--safe|-s
	Safety flag, disables commands that interact with the OS (&, $, \\).
	Allows for safe public exposure of a dc:im terminal without allowing RCE on the host.

--help|-h
	Print this help message and exit.\
";

fn main() {
	/*-------------------
		PARSE OPTIONS
	-------------------*/

	let (mut i, mut e, mut f, mut s) = (false, false, false, false);	//flags
	let mut names: Vec<String> = Vec::new();	//buf for filenames/expressions
	for arg in std::env::args().skip(1) {	//get args, skip name of binary
		if let Some(flag) = arg.strip_prefix("--") {	//long variants
			match flag {
				"inter" => {i=true;}
				"expr" => {e=true;}
				"file" => {f=true;}
				"safe" => {s=true;}
				"help" => {	//prioritized, always terminate
					println!("{HELPMSG}");
					std::process::exit(0);
				}
				_ => {
					eprintln!("! Unrecognized option: --{flag}, use -h for help");
					std::process::exit(1);
				}
			}
			continue;
		}
		if arg.starts_with('-') {	//short variants, multiple at once possible
			for flag in arg.chars() {
				match flag {
					'-' => {}	//allow -f-i or similar
					'i' => {i=true;}
					'e' => {e=true;}
					'f' => {f=true;}
					's' => {s=true;}
					'h' => {	//prioritized, always terminate
						println!("{HELPMSG}");
						std::process::exit(0);}
					_ => {
						eprintln!("! Unrecognized option: -{flag}, use -h for help");
						std::process::exit(1);
					}
				}
			}
			continue;
		}
		names.push(arg);	//not a flag
	}

	/*--------------
		DO STUFF
	--------------*/

	//create state storage
	let mut st = State::default();

	//decide operating mode
	match (i, e, f) {
		(false, false, false) => {	//no flags
			if names.is_empty() {
				inter_mode(&mut st, None, s);	//interactive with default prompt
			}
			else {
				file_mode(&mut st, names, false, s);	//-f is optional
			}
		}
		(true, false, false) => {inter_mode(&mut st, names.first().cloned(), s);}	//interactive with custom prompt
		(_, true, false) => {expr_mode(&mut st, names, i, s);}	//expr mode, pass i on
		(_, false, true) => {file_mode(&mut st, names, i, s);}	//file mode, pass i on
		(_, true, true) => {	//invalid combination
			eprintln!("! Invalid options: both -e and -f present");
			std::process::exit(1);
		}
	}
}

///infinite prompt-eval loop
fn inter_mode(st: &mut State, prompt: Option<String>, safe: bool) {
	let inputter = input::<String>().repeat_msg(prompt.unwrap_or_else(|| "> ".into()));
	let mut io = stdio!();
	loop {
		#[allow(unused_must_use)]
		{exec(st, &mut io, safe, &inputter.get());}
	}
}

///takes input from cmd args
fn expr_mode(st: &mut State, exprs: Vec<String>, inter: bool, safe: bool) {
	if exprs.is_empty() {
		eprintln!("! No expression provided");
	}
	else {
		let mut io = stdio!();
		for expr in exprs {
			#[allow(unused_must_use)]
			{exec(st, &mut io, safe, &expr);}
		}
	}
	if inter {
		inter_mode(st, None, safe);
	}
}

///takes input from file contents, removes #comments
fn file_mode(st: &mut State, files: Vec<String>, inter: bool, safe: bool) {
	if files.is_empty() {
		eprintln!("! No file name provided");
	}
	else {
		let mut io = stdio!();
		for file in files {
			match std::fs::read_to_string(&file) {
				Ok(script) => {
					let mut script_nc = String::new();	//script with comments removed
					for line in script.split('\n') {
						script_nc.push_str(line.split_once('#').unwrap_or((line,"")).0);	//remove comment on every line
						script_nc.push('\n');
					}
					#[allow(unused_must_use)]
					{exec(st, &mut io, safe, &script_nc);}
				},
				Err(error) => {
					eprintln!("! Unable to read file \"{file}\": {error}");
				},
			}
		}
	}
	if inter {
		inter_mode(st, None, safe);
	}
}