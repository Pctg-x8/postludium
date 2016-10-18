// Expression Resolvers

use std;
use std::collections::{HashMap, HashSet, LinkedList};
use super::syntree::*;
use super::expresolver::*;
use super::BuilderArguments;
use itertools::Itertools;
use parsetools::ParseTools;

pub enum ResolveError
{
	SelfRecurse(String), UndefinedLabel(String), TooFewArguments, ExpressionIssues(ExpressionResolveError)
}
impl std::fmt::Display for ResolveError
{
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match self
		{
			&ResolveError::SelfRecurse(ref d) => write!(fmt, "{}: Self-recursed definition", d),
			&ResolveError::UndefinedLabel(ref d) => write!(fmt, "{}: Referencing undefined or illegal label", d),
			&ResolveError::TooFewArguments => write!(fmt, "Too few arguments for injection"),
			&ResolveError::ExpressionIssues(ref e) => write!(fmt, "{}", e)
		}
	}
}
impl std::fmt::Debug for ResolveError { fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result { std::fmt::Display::fmt(self, fmt) } }
impl std::convert::From<ExpressionResolveError> for ResolveError
{
	fn from(e: ExpressionResolveError) -> Self { ResolveError::ExpressionIssues(e) }
}
type ResolvingResult<T> = Result<T, ResolveError>;

#[derive(Debug)]
pub enum ResolvedCommand
{
	// Graphics Binders //
	BindPipelineState(u32),
	BindDescriptorSet(u32, u32, u32),
	BindVertexBuffer(u32, u32),
	BindIndexBuffer(u32),
	PushConstant(u32, u32, DefinedData),
	// Graphics Drawers //
	Draw(u32, u32),
	DrawIndexed(u32, u32),
	// Memory Barriers //
	BufferBarrier(u32, u32, u32, i32, u32, u32),
	ImageBarrier(u32, u32, u32, u32, u32, u32, u32, u32),
	// Copying Commands //
	CopyBuffer(u32, u32, u32, i32, u32)
}
type ResolvedCommands = LinkedList<ResolvedCommand>;

#[derive(Debug)]
pub enum ResolvedLabelCommandLevel { Primary, Secondary }
#[derive(Debug)]
pub enum ResolvedSubpassIndex { Pre, Post, Pass(u32) }
#[derive(Debug)]
pub enum ResolvedLabelRenderedFB { Swapchain(ResolvedSubpassIndex), Custom(u32, ResolvedSubpassIndex) }
#[derive(Debug)]
pub struct ResolvedRenderingLabelBlock { cmdlevel: ResolvedLabelCommandLevel, rendered_fb: ResolvedLabelRenderedFB, commands: ResolvedCommands }
#[derive(Debug)]
pub struct ResolvedTransferLabelBlock { cmdlevel: ResolvedLabelCommandLevel, commands: ResolvedCommands }
pub struct Resolver<'a> { args: BuilderArguments, defs: HashMap<&'a [char], DefinedData> }
#[derive(Debug)]
pub struct Resolved<'a>
{
	args: BuilderArguments,
	render_blocks: HashMap<&'a [char], ResolvedRenderingLabelBlock>,
	transfer_blocks: HashMap<&'a [char], ResolvedTransferLabelBlock>
}
pub trait ResolvedExpressionList<'a> where Self: std::marker::Sized + IntoIterator<Item=&'a &'a ExpressionNode<'a>>
{
	fn resolve_i64(self, env: &BuilderArguments, defs: &ResolvedDefinitions<'a>, iargs: Option<&Vec<DefinedData>>) -> ResolvingResult<Vec<i64>>
	{
		self.into_iter().map(|e| e.resolve_i64(env, defs, iargs).map_err(ResolveError::from)).collect()
	}
}
impl<'a> ResolvedExpressionList<'a> for &'a [&'a ExpressionNode<'a>; 2] {}
impl<'a> ResolvedExpressionList<'a> for &'a [&'a ExpressionNode<'a>; 3] {}
impl<'a> ResolvedExpressionList<'a> for &'a [&'a ExpressionNode<'a>; 5] {}
impl<'a> ResolvedExpressionList<'a> for &'a [&'a ExpressionNode<'a>; 6] {}
impl<'a> ResolvedExpressionList<'a> for &'a [&'a ExpressionNode<'a>; 7] {}
impl<'a> ResolvedExpressionList<'a> for &'a [&'a ExpressionNode<'a>; 8] {}
impl<'a> Resolver<'a>
{
	pub fn resolve(args: BuilderArguments, defs: HashMap<&'a [char], ExpressionNode<'a>>, labels: HashMap<&'a [char], LabelBlock<'a>>)
		-> ResolvingResult<Resolved<'a>>
	{
		Self::resolve_defs(args, defs).and_then(|resolver| resolver.resolve_commands(labels))
	}

	pub fn resolve_defs(args: BuilderArguments, src: HashMap<&'a [char], ExpressionNode<'a>>) -> ResolvingResult<Self>
	{
		let mut defdeps = HashMap::new();
		for (&n, x) in src.iter()
		{
			defdeps.insert(n, Self::find_deps(x));
		}
		let mut added_names = HashSet::new();
		let deflist_vl = try!(defdeps.iter().map(|(d, dd)| Self::build_deflist(&mut added_names, &defdeps, d, dd)).collect::<Result<Vec<_>, _>>());
		let deflist = deflist_vl.into_iter().flatten().collect_vec();
		// println!("{:?}", deflist);

		let mut dst = Resolver { args: args, defs: HashMap::new() };
		for n in deflist
		{
			let xv = try!(src.get(&n).unwrap().resolve_soft(&dst.args, &dst.defs, None));
			dst.defs.insert(n, xv);
		}
		Ok(dst)
	}
	// find dependency(a -> [b, c]: a depends (has references to) b and c)
	fn find_deps(current: &ExpressionNode<'a>) -> Vec<&'a [char]>
	{
		match current
		{
			&ExpressionNode::ConstantRef(refn) => if builtin_defs(refn).is_none() { vec![refn] } else { Vec::new() },
			&ExpressionNode::ExternalU32(ref x) | &ExpressionNode::Negated(ref x) => Self::find_deps(x),
			&ExpressionNode::Add(ref x, ref y) | &ExpressionNode::Sub(ref x, ref y) |
			&ExpressionNode::Mul(ref x, ref y) | &ExpressionNode::Div(ref x, ref y) | &ExpressionNode::Mod(ref x, ref y) |
			&ExpressionNode::And(ref x, ref y) | &ExpressionNode::Or(ref x, ref y) | &ExpressionNode::Xor(ref x, ref y) =>
			{
				Self::find_deps(x).into_iter().chain(Self::find_deps(y)).collect_vec()
			},
			_ => Vec::new()
		}
	}
	fn build_deflist(added: &mut HashSet<&'a [char]>, tree: &HashMap<&'a [char], Vec<&'a [char]>>, current: &'a [char], current_deps: &Vec<&'a [char]>) -> ResolvingResult<LinkedList<&'a [char]>>
	{
		let deps = try!(current_deps.iter().map(|d| if *d == current { Err(ResolveError::SelfRecurse(d.clone_as_string())) } else if let Some(dd) = tree.get(d) { Ok((d, dd)) }
			else { Err(ResolveError::ExpressionIssues(ExpressionResolveError::UndefinedRef(d.clone_as_string()))) }).collect::<Result<Vec<_>, _>>());
		let mut ll = try!(deps.into_iter().map(|(d, dd)| Self::build_deflist(added, tree, d, dd)).collect::<Result<Vec<_>, _>>().map(|lv| lv.into_iter().flatten().collect::<LinkedList<_>>()));
		if !added.contains(&current)
		{
			added.insert(current);
			ll.push_back(current);
		}
		Ok(ll)
	}

	pub fn resolve_commands(self, src: HashMap<&'a [char], LabelBlock<'a>>) -> ResolvingResult<Resolved<'a>>
	{
		let (lazy_resolved, imm_resolved): (HashMap<&'a [char], LabelBlock<'a>>, HashMap<&'a [char], LabelBlock<'a>>)
			= src.into_iter().partition(|&(_, ref b)| b.attributes.is_injected());
		let mut resolved_render_labels = HashMap::new();
		let mut resolved_transfer_labels = HashMap::new();
		for (name, blk) in imm_resolved
		{
			let commands = try!(self.resolve_in_label(None, &lazy_resolved, &blk.commands));
			match blk.attributes
			{
				LabelAttribute::Graphics(level, fb) =>
				{
					resolved_render_labels.insert(name, ResolvedRenderingLabelBlock
					{
						cmdlevel: match level { LabelType::Primary => ResolvedLabelCommandLevel::Primary, LabelType::Secondary => ResolvedLabelCommandLevel::Secondary, _ => unreachable!() },
						rendered_fb: try!(match fb
						{
							LabelRenderedFB::Swapchain(sub) => self.parse_subpass_index(sub).map(ResolvedLabelRenderedFB::Swapchain),
							LabelRenderedFB::Backbuffer(fbid, sub) => fbid.resolve_i64(&self.args, &self.defs, None).and_then(|fbid| self.parse_subpass_index(sub).map(move |sub| ResolvedLabelRenderedFB::Custom(fbid as u32, sub)))
						}),
						commands: commands
					});
				},
				LabelAttribute::Transfer(level) =>
				{
					resolved_transfer_labels.insert(name, ResolvedTransferLabelBlock
					{
						cmdlevel: match level { LabelType::Primary => ResolvedLabelCommandLevel::Primary, LabelType::Secondary => ResolvedLabelCommandLevel::Secondary, _ => unreachable!() },
						commands: commands	
					});
				},
				_ => unreachable!()
			}
		}

		Ok(Resolved
		{
			args: self.args,
			render_blocks: resolved_render_labels, transfer_blocks: resolved_transfer_labels
		})
	}
	fn resolve_in_label(&self, iargs: Option<Vec<DefinedData>>, inj_labels: &'a HashMap<&'a [char], LabelBlock<'a>>, lb: &'a LinkedList<CommandNode<'a>>) -> ResolvingResult<ResolvedCommands>
	{
		let mut resolved = ResolvedCommands::new();

		for cmd in lb
		{
			match cmd
			{
				&CommandNode::BindPipelineState(ref psid) => resolved.push_back(try!
				{
					psid.resolve_i64(&self.args, &self.defs, iargs.as_ref()).map(|arg| ResolvedCommand::BindPipelineState(arg as u32))
				}),
				&CommandNode::BindDescriptorSet(ref dlid, ref slot, ref dsid) => resolved.push_back(try!
				{
					[dlid, slot, dsid].resolve_i64(&self.args, &self.defs, iargs.as_ref())
						.map(|args| ResolvedCommand::BindDescriptorSet(args[0] as u32, args[1] as u32, args[2] as u32))
				}),
				&CommandNode::BindVertexBuffer(ref slot, ref vbid) => resolved.push_back(try!
				{
					[slot, vbid].resolve_i64(&self.args, &self.defs, iargs.as_ref())
						.map(|args| ResolvedCommand::BindVertexBuffer(args[0] as u32, args[1] as u32))
				}),
				&CommandNode::BindIndexBuffer(ref ibid) => resolved.push_back(try!
				{
					ibid.resolve_i64(&self.args, &self.defs, iargs.as_ref()).map(|arg| ResolvedCommand::BindPipelineState(arg as u32))
				}),
				&CommandNode::PushConstant(ref dlid, ref pcid, ref val) => resolved.push_back(try!
				{
					[dlid, pcid].resolve_i64(&self.args, &self.defs, iargs.as_ref())
						.and_then(|ids| val.resolve_soft(&self.args, &self.defs, iargs.as_ref()).map(|val| (ids, val)).map_err(ResolveError::from))
						.map(|(ids, val)| ResolvedCommand::PushConstant(ids[0] as u32, ids[1] as u32, val))
				}),
				&CommandNode::Draw(ref vc, ref ic) => resolved.push_back(try!
				{
					[vc, ic].resolve_i64(&self.args, &self.defs, iargs.as_ref())
						.map(|args| ResolvedCommand::Draw(args[0] as u32, args[1] as u32))
				}),
				&CommandNode::DrawIndexed(ref vc, ref ic) => resolved.push_back(try!
				{
					[vc, ic].resolve_i64(&self.args, &self.defs, iargs.as_ref())
						.map(|args| ResolvedCommand::DrawIndexed(args[0] as u32, args[1] as u32))
				}),
				&CommandNode::BufferBarrier(ref sps, ref dps, ref offs, ref size, ref smu, ref dmu) => resolved.push_back(try!
				{
					[sps, dps, offs, size, smu, dmu].resolve_i64(&self.args, &self.defs, iargs.as_ref())
						.map(|args| ResolvedCommand::BufferBarrier(args[0] as u32, args[1] as u32, args[2] as u32, args[3] as i32, args[4] as u32, args[5] as u32))
				}),
				&CommandNode::ImageBarrier(ref sps, ref dps, ref img, ref ims, ref smu, ref dmu, ref sil, ref dil) => resolved.push_back(try!
				{
					[sps, dps, img, ims, smu, dmu, sil, dil].resolve_i64(&self.args, &self.defs, iargs.as_ref()).map(|args|
						ResolvedCommand::ImageBarrier(args[0] as u32, args[1] as u32, args[2] as u32, args[3] as u32, args[4] as u32, args[5] as u32, args[6] as u32, args[7] as u32))
				}),
				&CommandNode::CopyBuffer(ref sbid, ref dbid, ref sof, ref size, ref dof) => resolved.push_back(try!
				{
					[sbid, dbid, sof, size, dof].resolve_i64(&self.args, &self.defs, iargs.as_ref()).map(|args|
						ResolvedCommand::CopyBuffer(args[0] as u32, args[1] as u32, args[2] as u32, args[3] as i32, args[4] as u32))
				}),
				&CommandNode::InjectCommands(name, ref _iargs) => try!(if let Some(lb_target) = inj_labels.get(name)
				{
					_iargs.iter().map(|v| v.resolve_soft(&self.args, &self.defs, iargs.as_ref()).map_err(ResolveError::from)).collect::<ResolvingResult<Vec<_>>>()
						.and_then(|iargs_resolved| if try!(lb_target.attributes.injection_arg_count(&self.args, &self.defs)) != iargs_resolved.len()
						{
							Err(ResolveError::TooFewArguments)
						}
						else
						{
							self.resolve_in_label(Some(iargs_resolved), inj_labels, &lb_target.commands)
						}).map(|mut rescommands| resolved.append(&mut rescommands))
				}
				else { return Err(ResolveError::UndefinedLabel(name.clone_as_string())); })
			}
		}

		Ok(resolved)
	}
	fn parse_subpass_index(&self, ex: RenderedSubpass<'a>) -> ExpressionResolveResult<ResolvedSubpassIndex>
	{
		match ex
		{
			RenderedSubpass::Pre => Ok(ResolvedSubpassIndex::Pre),
			RenderedSubpass::Post => Ok(ResolvedSubpassIndex::Post),
			RenderedSubpass::Sub(ex) => ex.resolve(&self.args, &self.defs, None).map(|si| ResolvedSubpassIndex::Pass(si as u32))
		}
	}
}
