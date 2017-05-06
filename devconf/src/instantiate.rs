
use interlude::*;
use interlude::ffi::*;
use std::ops::Deref;
use itertools::Itertools;
use syntree::*;
use std;
use ErrorReporter;
use std::io::prelude::*;
use std::borrow::Cow;

macro_rules! println_err
{
	($f: expr $(, $a: expr)*) => (writeln!(std::io::stderr(), $f $(, $a)*).unwrap())
}

pub struct DeviceResources
{
	pub renderpasses: NamedContents<RenderPass>,
	pub descriptor_set_layouts: NamedContents<DescriptorSetLayout>
}

const OPERATION_FAILED: OperationResult = OperationResult::Failed(std::borrow::Cow::Borrowed(""));

impl DeviceResources
{
	pub fn instantaite<Engine: AssetProvider + Deref<Target = GraphicsInterface>>(parsed: &ParsedDeviceResources, engine: &Engine) -> Self
	{
		let mut sink = DeviceResources
		{
			renderpasses: NamedContents::new(), descriptor_set_layouts: NamedContents::new()
		};

		let r = instantiate_renderpasses(&parsed.renderpasses, &mut sink.renderpasses, engine)
			.propagate_failure(instantiate_renderpasses(&parsed.simple_rps, &mut sink.renderpasses, engine))
			.propagate_failure(instantiate_renderpasses(&parsed.presented_rps, &mut sink.renderpasses, engine));
		
		if r == OPERATION_FAILED { panic!("Some errors occured in instantiating."); }

		sink
	}
}

trait RenderPassInstantiate
{
	fn instantiate(&self, engine: &GraphicsInterface) -> RenderPass;
}
fn instantiate_renderpasses<T: RenderPassInstantiate>(source: &NamedContents<T>, sink: &mut NamedContents<RenderPass>, engine: &GraphicsInterface) -> OperationResult
{
	let mut opr = OperationResult::Success;
	let r = source.make_reverse_dict();
	for (x, rp) in source.iter().enumerate()
	{
		if let InsertionResult::Duplicated(n) = sink.insert_auto(r.get(&x).map(|&x| x.into()), rp.instantiate(engine))
		{
			println_err!("Duplication detected in RenderPasses: {}", n);
			opr = OPERATION_FAILED;
		}
	}
	opr
}
impl RenderPassInstantiate for RenderPassData
{
	fn instantiate(&self, engine: &GraphicsInterface) -> RenderPass
	{
		let attachments = self.attachments.iter().map(|a| AttachmentDesc
		{
			format: a.format.value_ref().unwrap_as_resolved(), initial_layout: a.layouts.from, final_layout: a.layouts.to,
			clear_on_load: a.clear_on_load, preserve_stored_value: a.preserve_content,
			.. Default::default()
		}).collect_vec();
		let subpasses = self.subpasses.iter().map(|p| PassDesc
		{
			input_attachment_indices: p.inputs.iter().map(|x| AttachmentRef::input(x.value_ref().unwrap_as_resolved())).collect(),
			color_attachment_indices: p.color_outs.iter().map(|x| AttachmentRef::color(x.value_ref().unwrap_as_resolved())).collect(),
			.. Default::default()
		}).collect_vec();
		let deps = self.deps.iter().map(|p| PassDependency
		{
			src: p.passtrans.from.value_ref().unwrap_as_resolved(), dst: p.passtrans.to.value_ref().unwrap_as_resolved(),
			src_stage_mask: *p.stage_bits.value_ref(), dst_stage_mask: *p.stage_bits.value_ref(),
			src_access_mask: *p.access_mask.from.value_ref(), dst_access_mask: *p.access_mask.to.value_ref(),
			depend_by_region: p.by_region
		}).collect_vec();

		RenderPass::new(engine, &attachments[..], &subpasses[..], &deps[..]).or_crash()
	}
}
impl RenderPassInstantiate for SimpleRenderPassData
{
	fn instantiate(&self, engine: &GraphicsInterface) -> RenderPass
	{
		let attachment = AttachmentDesc
		{
			format: self.format.value_ref().unwrap_as_resolved(), clear_on_load: self.clear_on_load,
			initial_layout: VkImageLayout::ColorAttachmentOptimal, final_layout: VkImageLayout::ShaderReadOnlyOptimal,
			preserve_stored_value: true, .. Default::default()
		};
		let subpass = PassDesc { color_attachment_indices: vec![AttachmentRef::color(0)], .. Default::default() };

		RenderPass::new(engine, &[attachment], &[subpass], &[]).or_crash()
	}
}
impl RenderPassInstantiate for PresentedRenderPassData
{
	fn instantiate(&self, engine: &GraphicsInterface) -> RenderPass
	{
		let attachment = AttachmentDesc
		{
			format: self.format.value_ref().unwrap_as_resolved(), clear_on_load: self.clear_on_load,
			initial_layout: VkImageLayout::ColorAttachmentOptimal, final_layout: VkImageLayout::PresentSrcKHR,
			preserve_stored_value: true, .. Default::default()
		};
		let subpass = PassDesc { color_attachment_indices: vec![AttachmentRef::color(0)], .. Default::default() };

		RenderPass::new(engine, &[attachment], &[subpass], &[]).or_crash()
	}
}

/// Array of Contents that named once or more.
pub struct NamedContentsReverse<T>(Vec<(T, Vec<String>)>);
impl<T> std::ops::Deref for NamedContentsReverse<T> { type Target = [(T, Vec<String>)]; fn deref(&self) -> &Self::Target { &self.0 } }
impl<T> NamedContentsReverse<T>
{
	/// Make object from source by draining contents and names
	///
	/// # Examples
	/// ```
	/// # extern crate devconf;
	/// # fn main() {
	/// use devconf::NamedContentsReverse;
	/// # use devconf::syntree::NamedContents;
	///
	/// # let mut nc = NamedContents::new();
	/// # nc.insert("testA".into(), 0);
	/// # nc.insert("testB".into(), 1);
	/// # nc.insert_vunique("testC".into(), std::borrow::Cow::Owned(0));
	/// # nc.insert_unnamed(2);
	/// let ncr = NamedContentsReverse::make(nc);
	/// # assert!(ncr[0].1.iter().find(|&x| x == "testA").is_some());
	/// # assert!(ncr[0].1.iter().find(|&x| x == "testC").is_some());
	/// # assert_eq!(ncr[1], (1, vec!["testB".into()]));
	/// # assert_eq!(ncr[2], (2, Vec::new()));
	/// # }
	/// ```
	pub fn make(source: &mut NamedContents<T>) -> Self
	{
		let (mut revdict, contents) = source.drain_to_reverse_dict();
		NamedContentsReverse(contents.drain(..).enumerate().map(|(i, c)| (c, revdict.remove(&i).unwrap())).collect())
	}
	fn into_iter(self) -> std::vec::IntoIter<(T, Vec<String>)> { self.0.into_iter() }
}

pub mod ir
{
	//! Intermediate Representation for DeviceConfiguration
	use std::ops::Range; use interlude; use interlude::ffi::*;
	use syntree::{NamedContents, Transition};

	// Primitive Type and Dynamic Types
	pub type ByteRange = Range<usize>;
	#[derive(PartialEq, Eq, Clone, Copy, Debug)] pub enum FormatReferer { ScreenFormat }
	#[derive(PartialEq, Eq, Clone, Copy, Debug)] pub enum ExternU32Referer { ScreenWidth, ScreenHeight }
	#[derive(PartialEq, Eq, Clone, Debug)] pub enum Format
	{
		Constant(VkFormat), BuiltIn(FormatReferer), User(usize)
	}
	#[derive(PartialEq, Eq, Clone, Debug)] pub enum UintValue
	{
		Constant(u32), BuiltIn(ExternU32Referer), User(usize)
	}

	// Complex Structures
	#[derive(Debug)] pub struct DescriptorSetLayout(pub Vec<interlude::Descriptor>);
	#[derive(Debug, PartialEq, Eq, Clone)] pub struct RenderPassCreationInfo { pub attachments: Vec<usize>, pub subpasses: Vec<usize>, pub deps: Vec<usize> }
	#[derive(PartialEq, Eq, Clone, Debug)] pub struct PushConstantLayout(pub ByteRange, pub interlude::ShaderStage);
	#[derive(PartialEq, Eq, Clone, Debug)] pub struct PipelineLayout(pub Vec<usize>, pub Vec<PushConstantLayout>);
	#[derive(PartialEq, Eq, Clone, Debug)] pub struct AttachmentDesc
	{
		pub format: Format, pub layout: Transition<VkImageLayout>, pub clear_on_load: Option<bool>, pub preserve_stored_value: bool
	}

	#[derive(Debug)] pub struct DeviceConfigurations
	{
		pub descriptor_set_layouts: NamedContents<DescriptorSetLayout>,
		pub pipeline_layout: NamedContents<PipelineLayout>,
		pub rp_attachment_descs: Vec<AttachmentDesc>,
		pub rp_subpass_descs: Vec<interlude::PassDesc>,
		pub rp_subpass_deps: Vec<interlude::PassDependency>,
		pub renderpasses: NamedContents<RenderPassCreationInfo>
	}
}
impl LocationPacked<Format>
{
	fn resolve(&self, parsed: &ParsedDeviceResources, err: &mut ErrorReporter) -> Option<ir::Format>
	{
		match self
		{
			&LocationPacked(_, Format::Value(v)) => Some(ir::Format::Constant(v)),
			&LocationPacked(_, Format::Ref(ref r)) if r == "ScreenFormat" => Some(ir::Format::BuiltIn(ir::FormatReferer::ScreenFormat)),
			&LocationPacked(ref loc, Format::Ref(ref r)) => if let Some(x) = parsed.externs.reverse_index(r)
			{
				if let &ExternalResourceData::Uint(_) = &parsed.externs[x] { Some(ir::Format::User(x)) }
				else
				{
					err.report_fmt(format_args!("Extern \"{}\" is defined as incompatible type", r), Some(&From::from(loc)));
					None
				}
			}
			else
			{
				err.report_fmt(format_args!("Extern \"{}\" was not found", r), Some(&From::from(loc)));
				None
			}
		}
	}
}
impl RPAttachment
{
	fn resolve(&self, parsed: &ParsedDeviceResources, err: &mut ErrorReporter) -> Option<ir::AttachmentDesc>
	{
		self.format.resolve(parsed, err).map(|format| ir::AttachmentDesc
		{
			format, layout: self.layouts.clone(), clear_on_load: self.clear_on_load, preserve_stored_value: self.preserve_content
		})
	}
}
fn insert_unique<T: Eq>(v: &mut Vec<T>, value: T) -> usize
{
	if let Some((n, _)) = v.iter().enumerate().find(|&(_, v)| *v == value) { n }
	else { v.push(value); v.len() - 1 }
}
impl ir::DeviceConfigurations
{
	pub fn resolve_all(parsed: &mut ParsedDeviceResources, er: &mut ErrorReporter) -> Self
	{
		let mut pipeline_layout = NamedContents::new();
		for (content, names) in NamedContentsReverse::make(&mut parsed.pipeline_layouts).into_iter()
		{
			if let Some(resolved) = content.resolve(parsed, er)
			{
				let linked = ir::PipelineLayout(resolved.descs, resolved.pushconstants.into_iter()
					.map(|c| &parsed.push_constant_layouts[c]).map(|c| ir::PushConstantLayout(c.range.clone(), c.visibility)).collect());
				let plx = pipeline_layout.insert_unnamed_vunique(Cow::Owned(linked));
				for name in names.into_iter().map(Cow::Owned) { pipeline_layout.make_link(name, plx); }
			}
		}
		let (mut rp_attachment_descs, mut rp_subpass_descs, mut rp_subpass_deps, mut renderpasses) = (Vec::new(), Vec::new(), Vec::new(), NamedContents::new());
		for (RenderPassData { attachments, subpasses, deps }, names) in NamedContentsReverse::make(&mut parsed.renderpasses).into_iter()
		{
			let rpas = attachments.iter().map(|a| a.resolve(parsed, er).map(|ar| insert_unique(&mut rp_attachment_descs, ar))).collect::<Option<Vec<_>>>();
			let rpss = subpasses.iter().map(|a| a.resolve(&attachments, er).map(|ar| insert_unique(&mut rp_subpass_descs, PassDesc
			{
				input_attachment_indices: ar.inputs.into_iter().map(|x| AttachmentRef::input(x as u32)).collect(),
				color_attachment_indices: ar.color_outs.into_iter().map(|x| AttachmentRef::color(x as u32)).collect(),
				.. Default::default()
			}))).collect::<Option<Vec<_>>>();
			let rpds = deps.into_iter().map(|a| a.resolve(&subpasses, er).map(|ar|
			{
				let stage_bits = ar.stage_bits.unwrap();
				insert_unique(&mut rp_subpass_deps, PassDependency
				{
					src: ar.passtrans.from as u32, dst: ar.passtrans.to as u32,
					src_stage_mask: stage_bits, dst_stage_mask: stage_bits,
					src_access_mask: ar.access_mask.from.unwrap(), dst_access_mask: ar.access_mask.to.unwrap(),
					depend_by_region: ar.by_region
				})
			})).collect::<Option<Vec<_>>>();

			if let Some(rpc) = rpas.and_then(|attachments| rpss.and_then(|subpasses| rpds.map(|deps| ir::RenderPassCreationInfo { attachments, subpasses, deps })))
			{
				let rpx = renderpasses.insert_unnamed_vunique(Cow::Owned(rpc));
				for name in names.into_iter().map(Cow::Owned) { renderpasses.make_link(name, rpx); }
			}
		}

		ir::DeviceConfigurations
		{
			descriptor_set_layouts: unsafe { std::mem::transmute(std::mem::replace(&mut parsed.descriptor_set_layouts, NamedContents::new())) },
			pipeline_layout, rp_attachment_descs, rp_subpass_descs, rp_subpass_deps, renderpasses
		}
	}
}
