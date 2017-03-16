
use interlude::*;
use interlude::ffi::*;
use parser::*;
use std::ops::Deref;
use itertools::Itertools;
use syntree::{ OperationResult, NamedContents, ParsedDeviceResources };
use std;
use std::io::prelude::*;

macro_rules! println_err
{
	($f: expr $(, $a: expr)*) => (writeln!(std::io::stderr(), $f $(, $a)*).unwrap())
}

pub struct DeviceResources
{
	pub renderpasses: NamedContents<RenderPass>,
	pub descriptor_set_layouts: NamedContents<DescriptorSetLayout>
}

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
		
		if r == OperationResult::Failed { panic!("Some errors occured in instantiating."); }

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
		match r.get(&x)
		{
			Some(&n) => if sink.insert_unique(n.into(), rp.instantiate(engine)) == OperationResult::Failed
			{
				println_err!("Duplication detected in RenderPasses: {}", n);
				opr = OperationResult::Failed;
			},
			None => sink.insert_unnamed(rp.instantiate(engine))
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
