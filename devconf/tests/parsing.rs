// Parsing Tests

extern crate parsetools;
extern crate itertools;
extern crate devconf;
extern crate interlude;

use devconf::*;
use devconf::syntree::*;
use interlude::ffi::*;

#[test] fn parse_script()
{
	let testcase = "
$FirstRP: RenderPass
- Attachments:
-- $Backbuffer: R16G16B16A16 SFLOAT, ColorAttachmentOptimal, ClearOnLoad
-- $TonemapRes: $ScreenFormat, ColorAttachmentOptimal -> ShaderReadOnlyOptimal, PreserveContent
- Subpasses:
-- $StdRender: RenderTo $Backbuffer
#From
-- $Tonemapping: RenderTo $TonemapRes From $Backbuffer
- Dependencies:
# Declare Deps
--$StdRender -> $Tonemapping: ColorAttachmentWrite To ShaderRead @ FragmentShaderStage, ByRegion

$SMAAEdgeDetectRP: SimpleRenderPass
- Format: R8G8 UNORM
- ClearMode: OnLoad
$SMAABlendWeightRP: SimpleRenderPass
- Format: R8G8B8A8 UNORM
- ClearMode: OnLoad
$SMAACombineRP: PresentedRenderPass
- Format: R8G8B8A8 UNORM
#- ClearMode: None
";
	let pdr = load_configurations(std::path::PathBuf::new().into(), |_| Ok(testcase.to_owned()));
	assert_eq!(pdr.renderpasses["FirstRP"].attachments["Backbuffer"], RPAttachment
	{
		format: LocationPacked(Location(4, 16), Format::Value(VkFormat::R16G16B16A16_SFLOAT)), clear_on_load: Some(true), preserve_content: false,
		layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ColorAttachmentOptimal }
	});
	assert_eq!(pdr.renderpasses["FirstRP"].attachments["TonemapRes"], RPAttachment
	{
		format: LocationPacked(Location(5, 16), Format::Ref("ScreenFormat".into())), clear_on_load: None, preserve_content: true,
		layouts: Transition { from: VkImageLayout::ColorAttachmentOptimal, to: VkImageLayout::ShaderReadOnlyOptimal }
	});
	assert_eq!(pdr.renderpasses["FirstRP"].subpasses["StdRender"], RPSubpassDesc
	{
		color_outs: vec![LocationPacked(Location(7, 24), ConfigInt::Ref("Backbuffer".into()))], inputs: Vec::new()
	});
	assert_eq!(pdr.renderpasses["FirstRP"].subpasses["Tonemapping"], RPSubpassDesc
	{
		color_outs: vec![LocationPacked(Location(9, 26), ConfigInt::Ref("TonemapRes".into()))],
		inputs: vec![LocationPacked(Location(9, 43), ConfigInt::Ref("Backbuffer".into()))]
	});
	assert_eq!(pdr.renderpasses["FirstRP"].deps[0], RPSubpassDeps
	{
		passtrans: Transition
		{
			from: LocationPacked(Location(12, 2), ConfigInt::Ref("StdRender".into())),
			to: LocationPacked(Location(12, 16), ConfigInt::Ref("Tonemapping".into()))
		}, access_mask: Transition
		{
			from: LocationPacked(Location(12, 30), AccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)),
			to: LocationPacked(Location(12, 54), AccessFlags(VK_ACCESS_SHADER_READ_BIT))
		},
		stage_bits: LocationPacked(Location(12, 67), VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT), by_region: true
	});
	assert_eq!(pdr.simple_rps["SMAAEdgeDetectRP"], SimpleRenderPassData
	{
		format: LocationPacked(Location(15, 10), Format::Value(VkFormat::R8G8_UNORM)), clear_on_load: Some(true)
	});
	assert_eq!(pdr.simple_rps["SMAABlendWeightRP"], SimpleRenderPassData
	{
		format: LocationPacked(Location(18, 10), Format::Value(VkFormat::R8G8B8A8_UNORM)),
		clear_on_load: Some(true)
	});
	assert_eq!(pdr.presented_rps["SMAACombineRP"], PresentedRenderPassData
	{
		format: LocationPacked(Location(21, 10), Format::Value(VkFormat::R8G8B8A8_UNORM)),
		clear_on_load: None
	});
}
