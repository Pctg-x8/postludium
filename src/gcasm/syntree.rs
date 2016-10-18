// Syntax Tree

use std::collections::LinkedList;
use super::BuilderArguments;
use super::expresolver::*;

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
	pub fn injection_arg_count(&self, env: &BuilderArguments, defs: &ResolvedDefinitions<'a>) -> ExpressionResolveResult<usize>
	{
		match self
		{
			&LabelAttribute::Injected(ref ac) => ac.resolve_i64(env, defs, None).map(|x| x as usize),
			_ => unreachable!()
		}
	}
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
