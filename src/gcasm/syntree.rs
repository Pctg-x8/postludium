// Syntax Tree

use std;
use std::collections::{LinkedList, HashMap};

#[derive(Clone, Copy)]
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
					else { DefinedData::Floating(l.require_floating() $op r.require_floating()) }
				}
				else { DefinedData::Floating(l.require_floating() $op r.require_floating()) }
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

pub enum ExpressionResolveError<'a>
{
	UndefinedRef(&'a [char]), RequireInteger, ArgumentIndexOutOfRange(usize), ArgumentRefNotAllowed
}
pub type ExpressionResolveResult<'a, T> = Result<T, ExpressionResolveError<'a> >;

// Expression
#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionNode<'a>
{
	Number(u32), Floating(f64), ConstantRef(&'a [char]), InjectionArgRef(u64),
	Negated(Box<ExpressionNode<'a>>), ExternalU32(Box<ExpressionNode<'a>>),
	Add(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
	Sub(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
	Mul(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
	Div(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
	Mod(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
	And(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
	Or(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>),
	Xor(Box<ExpressionNode<'a>>, Box<ExpressionNode<'a>>)
}
impl<'a> ExpressionNode<'a>
{
	pub fn resolve_soft(&self, args: &BuilderArguments, defs: &DefinedMacros<'a>, iarg: Option<&Vec<DefinedData>>) -> ExpressionResolveResult<DefinedData>
	{
		match self
		{
			&ExpressionNode::Number(n) => Ok(DefinedData::Number(n as i64)),
			&ExpressionNode::Floating(n) => Ok(DefinedData::Floating(n)),
			&ExpressionNode::ConstantRef(refn) => if let Some(&v) = defs.get(refn) { v } else { Err(ExpressionResolveError::UndefinedRef(refn)) },
			&ExpressionNode::InjectionArgRef(refn) => if let Some(iargs) = iarg
			{
				if iargs.len() > refn as usize { iargs[refn as usize] } else { Err(ExpressionResolveError::ArgumentIndexOutOfRange(refn as usize)) }
			}
			else { Err(ExpressionResolveError::ArgumentRefNotAllowed) },
			&ExpressionNode::Negated(ref x) => x.resolve_soft(args, defs, iarg).map(|x| -x),
			&ExpressionNode::ExternalU32(ref x) => x.resolve_soft(args, defs, iarg).and_then(|x| if (x as usize) < args.external_u32s.len()
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
	pub fn resolve(&self, args: &BuilderArguments, defs: &DefinedMacros<'a>, iarg: Option<&Vec<DefinedData>>) -> ExpressionResolveResult<f64>
	{
		match self
		{
			&ExpressionNode::Number(n) => Ok(n as f64),
			&ExpressionNode::Floating(n) => Ok(n),
			&ExpressionNode::ConstantRef(refn) => if let Some(&v) = defs.get(refn) { Ok(v.require_floating()) } else { Err(ExpressionResolveError::UndefinedRef(refn)) },
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
	pub fn resolve_i64(&self, args: &BuilderArguments, defs: DefinedMacros<'a>) -> ExpressionResolveResult<i64>
	{
		match self
		{
			&ExpressionNode::Number(n) => Ok(n as i64),
			&ExpressionNode::Floating(n) => Err(ExpressionResolveError::RequireInteger),
			&ExpressionNode::ConstantRef(refn) => if let Some(&v) = defs.get(refn) { v.require_number().ok_or(ExpressionResolveError::RequireInteger) }
				else { Err(ExpressionResolveError::UndefinedRef(refn)) },
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
// Commands
#[derive(Debug, PartialEq, Clone)]
pub enum CommandNode<'a>
{
	// Graphics Binders //
	BindPipelineState(ExpressionNode<'a>),
	BindDescriptorSet(ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>),
	BindVertexBuffer(ExpressionNode<'a>, ExpressionNode<'a>),
	BindIndexBuffer(ExpressionNode<'a>),
	PushConstant(ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>),
	// Graphics Drawers //
	Draw(ExpressionNode<'a>, ExpressionNode<'a>),
	DrawIndexed(ExpressionNode<'a>, ExpressionNode<'a>),
	// Memory Barriers //
	BufferBarrier(ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>),
	ImageBarrier(ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>),
	// Copying Commands //
	CopyBuffer(ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>, ExpressionNode<'a>),
	// Assembly Intrinsics //
	InjectCommands(&'a [char], Vec<ExpressionNode<'a>>)
}
#[derive(Debug, PartialEq)]
pub enum InternalLabelType { Primary, Secondary, Injected }
#[derive(Debug, PartialEq, Clone)]
pub enum LabelType<'a> { Primary, Secondary, Injected(ExpressionNode<'a>) }
impl<'a> LabelType<'a>
{
	pub fn is_injected(&self) -> bool { match self { &LabelType::Injected(_) => true, _ => false } }
}
#[derive(PartialEq, Debug, Clone)]
pub enum RenderedSubpass<'a> { Pre, Post, Sub(ExpressionNode<'a>) }
#[derive(PartialEq, Debug, Clone)]
pub enum LabelRenderedFB<'a>
{
	Swapchain(RenderedSubpass<'a>),
	Backbuffer(ExpressionNode<'a>, RenderedSubpass<'a>)
}
#[derive(Debug, PartialEq, Clone)]
pub enum LabelAttribute<'a>
{
	Graphics(LabelType<'a>, LabelRenderedFB<'a>), Transfer(LabelType<'a>), Injected(ExpressionNode<'a>)
}
impl<'a> LabelAttribute<'a>
{
	pub fn is_injected(&self) -> bool { match self { &LabelAttribute::Injected(_) => true, _ => false } }
}
#[derive(Debug, PartialEq)]
pub enum LabelAttributes<'a>
{
	CommandType(InternalLabelType), InjectedArgs(ExpressionNode<'a>), RenderDesc(LabelRenderedFB<'a>), TransferMark
}
#[derive(Debug, PartialEq)]
pub struct LabelBlock<'a> { pub attributes: LabelAttribute<'a>, name: &'a [char], pub commands: LinkedList<CommandNode<'a>> }
impl<'a> LabelBlock<'a>
{
	pub fn new(attributes: LabelAttribute<'a>, name: &'a [char]) -> Self
	{
		LabelBlock { attributes: attributes, name: name, commands: LinkedList::new() }
	}
	pub fn add_command(&mut self, cmd: CommandNode<'a>) { self.commands.push_back(cmd); }
}
