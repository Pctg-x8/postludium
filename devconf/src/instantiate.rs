
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
	/// let ncr = NamedContentsReverse::make(&mut nc);
	/// # assert!(ncr[0].1.iter().find(|&x| x == "testA").is_some());
	/// # assert!(ncr[0].1.iter().find(|&x| x == "testC").is_some());
	/// # assert_eq!(ncr[1], (1, vec!["testB".into()]));
	/// # assert_eq!(ncr[2], (2, Vec::new()));
	/// # }
	/// ```
	pub fn make(source: &mut NamedContents<T>) -> Self
	{
		let &mut NamedContents(ref mut names, ref mut contents) = source;
		NamedContentsReverse(contents.drain(..).enumerate().map(|(i, c)|
		{
			let (drained, retained) = names.drain().partition(|&(_, lr)| lr == i);
			*names = retained;
			(c, drained.into_iter().map(|(n, _)| n).collect())
		}).collect())
	}
	fn into_iter(self) -> std::vec::IntoIter<(T, Vec<String>)> { self.0.into_iter() }
}

use std::ops::Range; use interlude;
pub type ByteRange = Range<usize>;
#[derive(Debug)] pub struct DescriptorSetLayout(Vec<Descriptor>);
#[derive(PartialEq, Eq, Clone, Debug)] pub struct PushConstantLayout(ByteRange, interlude::ShaderStage);
#[derive(PartialEq, Eq, Clone, Debug)] pub struct PipelineLayout(Vec<usize>, Vec<PushConstantLayout>);
#[derive(Debug)] pub struct DeviceConfigurations
{
	pub descriptor_set_layouts: NamedContents<DescriptorSetLayout>,
	pub pipeline_layout: NamedContents<PipelineLayout>
}
impl DeviceConfigurations
{
	pub fn resolve_all(parsed: &mut ParsedDeviceResources, er: &mut ErrorReporter) -> DeviceConfigurations
	{
		let mut pipeline_layout = NamedContents::new();
		for (content, names) in NamedContentsReverse::make(&mut parsed.pipeline_layouts).into_iter()
		{
			if let Some(resolved) = content.resolve(parsed, er)
			{
				let linked = PipelineLayout(resolved.descs, resolved.pushconstants.into_iter()
					.map(|c| &parsed.push_constant_layouts[c]).map(|c| PushConstantLayout(c.range.clone(), c.visibility)).collect());
				if names.is_empty() { pipeline_layout.insert_unnamed_vunique(Cow::Owned(linked)); }
				else
				{
					for name in names { pipeline_layout.insert_vunique(Cow::Owned(name), Cow::Borrowed(&linked)); }
				}
			}
		}

		DeviceConfigurations
		{
			descriptor_set_layouts: unsafe { std::mem::transmute(std::mem::replace(&mut parsed.descriptor_set_layouts, NamedContents::new())) },
			pipeline_layout
		}
	}
}
