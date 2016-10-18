pub mod parser;
pub mod syntree;
pub use self::parser as asmparser;
pub mod expresolver;
pub use self::expresolver::Resolver as ExpressionResolver;

pub struct BuilderArguments
{
	pub external_u32s: Vec<u32>
}

#[cfg(test)]
mod combined_test
{
	use std;
	use std::io::prelude::*;
	use itertools::Itertools;
	use super::super::lazylines::*;

	#[test] fn defresolve()
	{
		let mut testcase = ".define TEST 1\n.define TEST2 TEST + 2\n.define TEST3 TEST4 + 1\n.define TEST4 TEST2\n.define TEST5 TEST7\n.define TEST6 TEST7\n.define TEST7 TEST8 + TEST3\n.define TEST8 0".to_owned();
		let testcase_chars = testcase.chars().collect_vec();
		let testcase_lines = LazyLinesChars::new(&testcase_chars);
		let (labels, deflist_ir) = super::asmparser::parse_lines(testcase_lines);
		
		let args = super::BuilderArguments { external_u32s: vec![128] };
		let deflist = super::ExpressionResolver::resolve(args, deflist_ir, labels);

		unimplemented!();
	}
	#[test] fn parse_objective()
	{
		let mut testcase = String::new();
		std::fs::File::open("../assets/devconf/commands.gpu").unwrap().read_to_string(&mut testcase).unwrap();
		let testcase_chars = testcase.chars().collect_vec();
		let testcase_lines = LazyLinesChars::new(&testcase_chars);
		let (labels, deflist_ir) = super::asmparser::parse_lines(testcase_lines);
		
		let args = super::BuilderArguments { external_u32s: vec![128] };
		let deflist = super::ExpressionResolver::resolve(args, deflist_ir, labels);

		unimplemented!();
	}
}
