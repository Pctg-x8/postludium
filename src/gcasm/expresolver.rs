// Expression Resolver

use std;
use super::syntree::*;
use std::collections::HashMap;
use super::BuilderArguments;
use parsetools::ParseTools;
use interlude::ffi::*;

#[derive(Clone, Copy, Debug)]
pub enum DefinedData { Number(i64), Floating(f64) }
impl DefinedData
{
	pub fn require_number(self) -> Option<i64> { match self { DefinedData::Number(n) => Some(n), _ => None } }
	pub fn require_floating(self) -> f64 { match self { DefinedData::Number(n) => n as f64, DefinedData::Floating(n) => n } }
	pub fn is_floating(self) -> bool { match self { DefinedData::Floating(_) => true, _ => false } }
}
macro_rules!DefOperator
{
	($base: ident for DefinedData { fn $name: ident () { left $op: tt right } }) =>
	{
		impl std::ops::$base for DefinedData
		{
			type Output = Self;
			fn $name(self, rhs: Self) -> Self
			{
				if let DefinedData::Number(l) = self
				{
					if let DefinedData::Number(r) = rhs { DefinedData::Number(l $op r) }
					else { DefinedData::Floating(self.require_floating() $op rhs.require_floating()) }
				}
				else { DefinedData::Floating(self.require_floating() $op rhs.require_floating()) }
			}
		}
	}
}
DefOperator!(Add for DefinedData { fn add() { left + right } });
DefOperator!(Sub for DefinedData { fn sub() { left - right } });
DefOperator!(Mul for DefinedData { fn mul() { left * right } });
DefOperator!(Div for DefinedData { fn div() { left / right } });
DefOperator!(Rem for DefinedData { fn rem() { left % right } });
impl std::ops::Neg for DefinedData
{
	type Output = Self;

	fn neg(self) -> Self
	{
		match self
		{
			DefinedData::Number(n) => DefinedData::Number(-n),
			DefinedData::Floating(f) => DefinedData::Floating(-f)
		}
	}
}
pub type ResolvedDefinitions<'a> = HashMap<&'a [char], DefinedData>;

pub enum ExpressionResolveError
{
	UndefinedRef(String), RequireInteger, ArgumentIndexOutOfRange(usize), ArgumentRefNotAllowed
}
impl std::fmt::Display for ExpressionResolveError
{
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match self
		{
			&ExpressionResolveError::UndefinedRef(ref d) => write!(fmt, "{}: Referencing undefined definition", d),
			&ExpressionResolveError::ArgumentIndexOutOfRange(d) => write!(fmt, "{} is out of range of argument(s)", d),
			&ExpressionResolveError::RequireInteger => write!(fmt, "Integer required"),
			&ExpressionResolveError::ArgumentRefNotAllowed => write!(fmt, "Referencing argument is not allowed here")
		}
	}
}
impl std::fmt::Debug for ExpressionResolveError { fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result { std::fmt::Display::fmt(self, fmt) } }
pub type ExpressionResolveResult<T> = Result<T, ExpressionResolveError>;

lazy_static!
{
	// Memory Access Mask //
	static ref BC_INDEX_READ: Vec<char> = "INDEX_READ".chars().collect();
	static ref BC_VERTEX_ATTRIBUTE_READ: Vec<char> = "VERTEX_ATTRIBUTE_READ".chars().collect();
	static ref BC_UNIFORM_READ: Vec<char> = "UNIFORM_READ".chars().collect();
	static ref BC_MEMORY_READ: Vec<char> = "MEMORY_READ".chars().collect();
	static ref BC_TRANSFER_READ: Vec<char> = "TRANSFER_READ".chars().collect();
	static ref BC_TRANSFER_WRITE: Vec<char> = "TRANSFER_WRITE".chars().collect();
	static ref BC_COLOR_ATTACHMENT_WRITE: Vec<char> = "COLOR_ATTACHMENT_WRITE".chars().collect();
	// Pipeline Stage Mask //
	static ref BC_TOP: Vec<char> = "TOP".chars().collect();
	static ref BC_BOTTOM: Vec<char> = "BOTTOM".chars().collect();
	static ref BC_COLOR_ATTACHMENT_OUTPUT: Vec<char> = "COLOR_ATTACHMENT_OUTPUT".chars().collect();
	static ref BC_TRANSFER: Vec<char> = "TRANSFER".chars().collect();
	// Image Layout Names //
	static ref BC_PRESENT_SRC: Vec<char> = "PRESENT_SRC".chars().collect();
	static ref BC_COLOR_ATTACHMENT_OPT: Vec<char> = "COLOR_ATTACHMENT_OPT".chars().collect();
	// Internal Indices //
	static ref BC_FRAMEBUFFER_IMAGE: Vec<char> = "FRAMEBUFFER_IMAGE".chars().collect();
	static ref BC_IMAGE_SUBRESOURCE_COLOR: Vec<char> = "IMAGE_SUBRESOURCE_COLOR".chars().collect();
}
pub fn builtin_defs<'a>(name: &'a [char]) -> Option<i64>
{
	PartialEqualityMatchMap!(name;
	{
		&**BC_INDEX_READ => VK_ACCESS_INDEX_READ_BIT as i64,
		&**BC_VERTEX_ATTRIBUTE_READ => VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT as i64,
		&**BC_UNIFORM_READ => VK_ACCESS_UNIFORM_READ_BIT as i64,
		&**BC_MEMORY_READ => VK_ACCESS_MEMORY_READ_BIT as i64,
		&**BC_TRANSFER_READ => VK_ACCESS_TRANSFER_READ_BIT as i64,
		&**BC_TRANSFER_WRITE => VK_ACCESS_TRANSFER_WRITE_BIT as i64,
		&**BC_COLOR_ATTACHMENT_WRITE => VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT as i64,
		&**BC_TOP => VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT as i64,
		&**BC_BOTTOM => VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT as i64,
		&**BC_TRANSFER => VK_PIPELINE_STAGE_TRANSFER_BIT as i64,
		&**BC_COLOR_ATTACHMENT_OUTPUT => VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT as i64,
		&**BC_PRESENT_SRC => VkImageLayout::PresentSrcKHR as i64,
		&**BC_COLOR_ATTACHMENT_OPT => VkImageLayout::ColorAttachmentOptimal as i64,
		&**BC_FRAMEBUFFER_IMAGE => -1,
		&**BC_IMAGE_SUBRESOURCE_COLOR => 0
	})
}

impl<'a> ExpressionNode<'a>
{
	pub fn resolve_soft(&self, args: &BuilderArguments, defs: &ResolvedDefinitions<'a>, iarg: Option<&Vec<DefinedData>>) -> ExpressionResolveResult<DefinedData>
	{
		match self
		{
			&ExpressionNode::Number(n) => Ok(DefinedData::Number(n as i64)),
			&ExpressionNode::Floating(n) => Ok(DefinedData::Floating(n)),
			&ExpressionNode::ConstantRef(refn) => if let Some(v) = builtin_defs(refn) { Ok(DefinedData::Number(v)) }
				else if let Some(&v) = defs.get(refn) { Ok(v) } else { Err(ExpressionResolveError::UndefinedRef(refn.clone_as_string())) },
			&ExpressionNode::InjectionArgRef(refn) => if let Some(iargs) = iarg
			{
				if iargs.len() > refn as usize { Ok(iargs[refn as usize]) } else { Err(ExpressionResolveError::ArgumentIndexOutOfRange(refn as usize)) }
			}
			else { Err(ExpressionResolveError::ArgumentRefNotAllowed) },
			&ExpressionNode::Negated(ref x) => x.resolve_soft(args, defs, iarg).map(|x| -x),
			&ExpressionNode::ExternalU32(ref x) => x.resolve_i64(args, defs, iarg).and_then(|x| if (x as usize) < args.external_u32s.len()
			{
				Ok(DefinedData::Number(args.external_u32s[x as usize] as i64))
			}
			else { Err(ExpressionResolveError::ArgumentIndexOutOfRange(x as usize)) }),
			&ExpressionNode::Add(ref a, ref b) => a.resolve_soft(args, defs, iarg).and_then(|x| b.resolve_soft(args, defs, iarg).map(move |y| x + y)),
			&ExpressionNode::Sub(ref a, ref b) => a.resolve_soft(args, defs, iarg).and_then(|x| b.resolve_soft(args, defs, iarg).map(move |y| x - y)),
			&ExpressionNode::Mul(ref a, ref b) => a.resolve_soft(args, defs, iarg).and_then(|x| b.resolve_soft(args, defs, iarg).map(move |y| x * y)),
			&ExpressionNode::Div(ref a, ref b) => a.resolve_soft(args, defs, iarg).and_then(|x| b.resolve_soft(args, defs, iarg).map(move |y| x / y)),
			&ExpressionNode::Mod(ref a, ref b) => a.resolve_soft(args, defs, iarg).and_then(|x| b.resolve_soft(args, defs, iarg).map(move |y| x % y)),
			&ExpressionNode::And(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| DefinedData::Number(x & y))),
			&ExpressionNode::Or (ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| DefinedData::Number(x | y))),
			&ExpressionNode::Xor(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| DefinedData::Number(x ^ y)))
		}
	}
	pub fn resolve(&self, args: &BuilderArguments, defs: &ResolvedDefinitions<'a>, iarg: Option<&Vec<DefinedData>>) -> ExpressionResolveResult<f64>
	{
		match self
		{
			&ExpressionNode::Number(n) => Ok(n as f64),
			&ExpressionNode::Floating(n) => Ok(n),
			&ExpressionNode::ConstantRef(refn) => if let Some(v) = builtin_defs(refn) { Ok(v as f64) }
				else if let Some(&v) = defs.get(refn) { Ok(v.require_floating()) } else { Err(ExpressionResolveError::UndefinedRef(refn.clone_as_string())) },
			&ExpressionNode::InjectionArgRef(refn) => if let Some(iargs) = iarg
			{
				if iargs.len() > refn as usize { Ok(iargs[refn as usize].require_floating()) } else { Err(ExpressionResolveError::ArgumentIndexOutOfRange(refn as usize)) }
			}
			else { Err(ExpressionResolveError::ArgumentRefNotAllowed) },
			&ExpressionNode::Negated(ref x) => x.resolve(args, defs, iarg).map(|x| -x),
			&ExpressionNode::ExternalU32(ref x) => x.resolve(args, defs, iarg).and_then(|x| if (x as usize) < args.external_u32s.len()
			{
				Ok(args.external_u32s[x as usize] as f64)
			} else { Err(ExpressionResolveError::ArgumentIndexOutOfRange(x as usize)) }),
			&ExpressionNode::Add(ref a, ref b) => a.resolve(args, defs, iarg).and_then(|x| b.resolve(args, defs, iarg).map(move |y| x + y)),
			&ExpressionNode::Sub(ref a, ref b) => a.resolve(args, defs, iarg).and_then(|x| b.resolve(args, defs, iarg).map(move |y| x - y)),
			&ExpressionNode::Mul(ref a, ref b) => a.resolve(args, defs, iarg).and_then(|x| b.resolve(args, defs, iarg).map(move |y| x * y)),
			&ExpressionNode::Div(ref a, ref b) => a.resolve(args, defs, iarg).and_then(|x| b.resolve(args, defs, iarg).map(move |y| x / y)),
			&ExpressionNode::Mod(ref a, ref b) => a.resolve(args, defs, iarg).and_then(|x| b.resolve(args, defs, iarg).map(move |y| x % y)),
			_ => Err(ExpressionResolveError::RequireInteger)
		}
	}
	pub fn resolve_i64(&self, args: &BuilderArguments, defs: &ResolvedDefinitions<'a>, iarg: Option<&Vec<DefinedData>>) -> ExpressionResolveResult<i64>
	{
		match self
		{
			&ExpressionNode::Number(n) => Ok(n as i64),
			&ExpressionNode::Floating(_) => Err(ExpressionResolveError::RequireInteger),
			&ExpressionNode::ConstantRef(refn) => if let Some(v) = builtin_defs(refn) { Ok(v) }
				else if let Some(&v) = defs.get(refn) { v.require_number().ok_or(ExpressionResolveError::RequireInteger) }
				else { Err(ExpressionResolveError::UndefinedRef(refn.clone_as_string())) },
			&ExpressionNode::InjectionArgRef(refn) => if let Some(iargs) = iarg
			{
				if iargs.len() > refn as usize { iargs[refn as usize].require_number().ok_or(ExpressionResolveError::RequireInteger) }
				else { Err(ExpressionResolveError::ArgumentIndexOutOfRange(refn as usize)) }
			}
			else { Err(ExpressionResolveError::ArgumentRefNotAllowed) },
			&ExpressionNode::Negated(ref x) => x.resolve_i64(args, defs, iarg).map(|x| -x),
			&ExpressionNode::ExternalU32(ref x) => x.resolve_i64(args, defs, iarg).and_then(|x| if (x as usize) < args.external_u32s.len()
			{
				Ok(args.external_u32s[x as usize] as i64)
			} else { Err(ExpressionResolveError::ArgumentIndexOutOfRange(x as usize)) }),
			&ExpressionNode::Add(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| x + y)),
			&ExpressionNode::Sub(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| x - y)),
			&ExpressionNode::Mul(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| x * y)),
			&ExpressionNode::Div(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| x / y)),
			&ExpressionNode::Mod(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| x % y)),
			&ExpressionNode::And(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| x & y)),
			&ExpressionNode::Or(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| x | y)),
			&ExpressionNode::Xor(ref a, ref b) => a.resolve_i64(args, defs, iarg).and_then(|x| b.resolve_i64(args, defs, iarg).map(move |y| x ^ y))
		}
	}
}
