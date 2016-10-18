// Expression Resolvers

use std;
use std::collections::{HashMap, HashSet, LinkedList};
use super::syntree::*;
use super::BuilderArguments;
use interlude::ffi::*;
use itertools::Itertools;
use parsetools::ParseTools;

pub enum ResolveError<'a>
{
	SelfRecurse(&'a [char]), UndefinedRef(&'a [char]), InjectionArgNotAllowed, UndefinedLabel(&'a [char]), InjectionArgRefOutOfBounds(u64)
}
impl<'a> std::fmt::Display for ResolveError<'a>
{
	fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match self
		{
			&ResolveError::SelfRecurse(d) => write!(fmt, "{}: Self-recursed definition", d.clone_as_string()),
			&ResolveError::UndefinedRef(d) => write!(fmt, "{}: Referencing undefined definition", d.clone_as_string()),
			&ResolveError::InjectionArgNotAllowed => write!(fmt, "InjectionArg is not allowed here"),
			&ResolveError::UndefinedLabel(d) => write!(fmt, "{}: Referencing undefined or illegal label", d.clone_as_string()),
			&ResolveError::InjectionArgRefOutOfBounds(n) => write!(fmt, "{} is out of bounds of injection arguments", n)
		}
	}
}
impl<'a> std::fmt::Debug for ResolveError<'a> { fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result { std::fmt::Display::fmt(self, fmt) } }
type ResolvingResult<'a, T> = Result<T, ResolveError<'a>>;

lazy_static!
{
	static ref BC_INDEX_READ: Vec<char> = "INDEX_READ".chars().collect_vec();
	static ref BC_VERTEX_ATTRIBUTE_READ: Vec<char> = "VERTEX_ATTRIBUTE_READ".chars().collect_vec();
	static ref BC_UNIFORM_READ: Vec<char> = "UNIFORM_READ".chars().collect_vec();
}
pub enum ResolvedCommand
{
	// Graphics Binders //
	BindPipelineState(u32),
	BindDescriptorSet(u32, u32, u32),
	BindVertexBuffer(u32, u32),
	BindIndexBuffer(u32),
	PushConstant(u32, u32, f64),
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

pub enum ResolvedLabelCommandLevel { Primary, Secondary }
pub enum ResolvedSubpassIndex { Pre, Post, Pass(u32) }
pub enum ResolvedLabelRenderedFB { Swapchain(ResolvedSubpassIndex), Custom(u32, ResolvedSubpassIndex) }
pub struct ResolvedRenderingLabelBlock { cmdlevel: ResolvedLabelCommandLevel, rendered_fb: ResolvedLabelRenderedFB, commands: ResolvedCommands }
pub struct ResolvedTransferLabelBlock { cmdlevel: ResolvedLabelCommandLevel, commands: ResolvedCommands }
pub struct Resolver<'a> { args: BuilderArguments, defs: HashMap<&'a [char], f64> }
pub struct Resolved<'a>
{
	args: BuilderArguments, defs: HashMap<&'a [char], f64>,
	render_blocks: HashMap<&'a [char], ResolvedRenderingLabelBlock>,
	transfer_blocks: HashMap<&'a [char], ResolvedTransferLabelBlock>
}
macro_rules! MultiResolver
{
	{ ($($ex: expr => $t: ty),*) => $reducer: path } =>
	{
		$reducer($(try!($ex.resolve_i64(&self.args, &self.defs, iargs)) as $t),*)
	}
}
impl<'a> Resolver<'a>
{
	pub fn resolve(args: BuilderArguments, defs: HashMap<&'a [char], ExpressionNode<'a>>, labels: HashMap<&'a [char], LabelBlock<'a>>)
	{
		Self::resolve_defs(args, defs).map(|resolver| resolver.resolve_commands(labels));
	}

	fn builtin_defs(name: &'a [char]) -> Option<u32>
	{
		PartialEqualityMatchMap!(name;
		{
			&**BC_INDEX_READ => VK_ACCESS_INDEX_READ_BIT,
			&**BC_VERTEX_ATTRIBUTE_READ => VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT,
			&**BC_UNIFORM_READ => VK_ACCESS_UNIFORM_READ_BIT
		})
	}

	pub fn resolve_defs(args: BuilderArguments, src: HashMap<&'a [char], ExpressionNode<'a>>) -> ResolvingResult<'a, Self>
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
			let xv = try!(src.get(&n).unwrap().resolve(&args, &dst.defs, None));
			dst.defs.insert(n, xv);
		}
		Ok(dst)
	}
	// find dependency(a -> [b, c]: a depends (has references to) b and c)
	fn find_deps(current: &ExpressionNode<'a>) -> Vec<&'a [char]>
	{
		match current
		{
			&ExpressionNode::ConstantRef(refn) => if Self::builtin_defs(refn).is_none() { vec![refn] } else { Vec::new() },
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
	fn build_deflist(added: &mut HashSet<&'a [char]>, tree: &HashMap<&'a [char], Vec<&'a [char]>>, current: &'a [char], current_deps: &Vec<&'a [char]>) -> ResolvingResult<'a, LinkedList<&'a [char]>>
	{
		let deps = try!(current_deps.iter().map(|d| if *d == current { Err(ResolveError::SelfRecurse(d)) } else if let Some(dd) = tree.get(d) { Ok((d, dd)) }
			else { Err(ResolveError::UndefinedRef(d)) }).collect::<Result<Vec<_>, _>>());
		let mut ll = try!(deps.into_iter().map(|(d, dd)| Self::build_deflist(added, tree, d, dd)).collect::<Result<Vec<_>, _>>().map(|lv| lv.into_iter().flatten().collect::<LinkedList<_>>()));
		if !added.contains(&current)
		{
			added.insert(current);
			ll.push_back(current);
		}
		Ok(ll)
	}

	pub fn resolve_commands(self, mut src: HashMap<&'a [char], LabelBlock<'a>>) -> ResolvingResult<'a, (HashMap<&'a [char], ResolvedRenderingLabelBlock>, HashMap<&'a [char], ResolvedTransferLabelBlock>)>
	{
		let (lazy_resolved, imm_resolved): (HashMap<&'a [char], LabelBlock<'a>>, HashMap<&'a [char], LabelBlock<'a>>) = src.into_iter().partition(|&(_, ref b)| b.attributes.is_injected());
		let mut resolved_render_labels = HashMap::new();
		let mut resolved_transfer_labels = HashMap::new();
		for (name, blk) in imm_resolved
		{
			let commands = try!(self.resolve_in_label(None, &lazy_resolved, &blk.commands));
			match blk.attribute
			{
				LabelAttribute::Graphics(level, fb) => resolved_render_labels.insert(name, ResolvedRenderingLabelBlock
				{
					cmdlevel: match level { LabelType::Primary => ResolvedLabelCommandLevel::Primary, LabelType::Secondary => ResolvedLabelCommandLevel::Secondary, _ => unreachable!() },
					rendered_fb: try!(match fb
					{
						LabelRenderedFB::Swapchain(sub) => self.parse_subpass_index(sub).map(ResolvedLabelRenderedFB::Swapchain),
						LabelRenderedFB::Backbuffer(fbid, sub) => fbid.resolve_i64(&self.args, &self.defs, None).and_then(|fbid| self.parse_subpass_index(sub).map(move |sub| ResolvedLabelRenderedFB::Custom(fbid, sub)))
					}),
					commands: commands
				}),
				LabelAttribute::Transfer(level) => resolved_transfer_labels.insert(name, ResolvedTransferLabelBlock
				{
					cmdlevel: match level { LabelType::Primary => ResolvedLabelCommandLevel::Primary, LabelType::Secondary => ResolvedLabelCommandLevel::Seconadry, _ => unreachable!() },
					commands: commands	
				})
			}
		}
		Ok((resolved_render_labels, resolved_transfer_labels))
	}
	fn resolve_in_label(&self, iargs: Option<Vec<f64>>, inj_labels: &HashMap<&'a [char], LabelBlock<'a>>, lb: &LinkedList<CommandNode<'a>>) -> ResolvingResult<'a, ResolvedCommands>
	{
		let resolved = ResolvedCommands::new();

		for cmd in lb
		{
			match cmd
			{
				&CommandNode::BindPipelineState(ref psid) =>
					resolved.push_back(MultiResolver!{ (psid => u32) => ResolvedCommand::BindPipelineState }),
				&CommandNode::BindDescriptorSet(ref dlid, ref slot, ref dsid) =>
					resolved.push_back(MultiResolver!{ (dlid => u32, slot => u32, dsid => u32) => ResolvedCommand::BindDescriptorSet }),
				&CommandNode::BindVertexBuffer(ref slot, ref vbid) =>
					resolved.push_back(MultiResolver!{ (slot => u32, vbid => u32) => ResolvedCommand::BindVertexBuffer }),
				&CommandNode::BindIndexBuffer(ref ibid) =>
					resolved.push_back(MultiResolver!{ (ibid => u32) => ResolvedCommand::BindIndexBuffer }),
				&CommandNode::PushConstant(ref dlid, ref pcid, ref val) => try!(resolved.push_back(dlid.resolve_i64(&self.args, &self.defs, iargs)
					.and_then(|dlid| pcid.resolve_i64(&self.args, &self.defs, iargs).and_then(move |pcid| val.resolve_soft(&self.args, &self.defs, iargs).map(move |val| (dlid as u32, pcid as u32, iargs))))
					.map(ResolvedCommand::PushConstant))),
				&CommandNode::Draw(ref vc, ref ic) =>
					resolved.push_back(MultiResolver!{ (vc => u32, ic => u32) => ResolvedCommand::Draw }),
				&CommandNode::DrawIndexed(ref vc, ref ic) =>
					resolved.push_back(MultiResolver!{ (vc => u32, ic => u32) => ResolvedCommand::DrawIndexed }),
				&CommandNode::BufferBarrier(ref sps, ref dps, ref offs, ref size, ref smu, ref dmu) =>
					resolved.push_back(MultiResolver!{ (sps => u32, dps => u32, offs => u32, size => i32, smu => u32, dmu => u32) => ResolvedCommand::BufferBarrier }),
				&CommandNode::ImageBarrier(ref sps, ref dps, ref img, ref ims, ref smu, ref dmu, ref sil, ref dil) => 
					resolved.push_back(MultiResolver!{ (sps => u32, dps => u32, img => u32, ims => u32, smu => u32, dmu => u32, sil => u32, dil => u32) => ResolvedCommand::ImageBarrier }),
				&CommandNode::CopyBuffer(ref sbid, ref dbid, ref sof, ref size, ref dof) =>
					resolved.push_back(MultiResolver!{ (sbid => u32, dbid => u32, sof => u32, size => i32, dof => u32) => ResolvedCommand::CopyBuffer }),
				&CommandNode::InjectCommands(name, ref iargs) => try!(if let Some(lb_target) = inj_labels.get(name)
				{
					iargs.iter().map(|v| v.resolve_soft(&self.args, &self.defs, iargs)).collect::<ExpressionResolvingResult>()
						.and_then(|iargs_resolved| self.resolve_in_label(Some(iargs_resolved), inj_labels, &lb_target.commands))
						.map(|rescommands| resolved.append(&mut rescommands))
				}
				else { return Err(ResolveError::UndefinedLabel(name)); })
			}
		}

		Ok(resolved)
	}
	fn parse_subpass_index(&self, ex: &RenderedSubpass<'a>) -> ExpressionResolveResult<ResolvedSubpassIndex>
	{
		match ex
		{
			&RenderedSubpass::Pre => Ok(ResolvedSubpassIndex::Pre),
			&RenderedSubpass::Post => Ok(ResolvedSubpassIndex::Post),
			&RenderedSubpass::Sub(ex) => ex.resolve(&self.args, &self.defs, None).map(|si| ResolvedSubpassIndex::Pass(si as u32))
		}
	}
}
