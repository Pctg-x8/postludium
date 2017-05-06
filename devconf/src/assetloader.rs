//! Asset Loader

use interlude::*;
use interlude::ffi::*;
use std::collections::HashMap;
use syntree::*;
use std::ops::Deref;
use ErrorReporter;

pub struct VertexShaderModule(ShaderModule);
pub struct FragmentShaderModule(ShaderModule);
pub struct GeometryShaderModule(ShaderModule);
pub struct TessControlShaderModule(ShaderModule);
pub struct TessEvaluationShaderModule(ShaderModule);
pub trait ClassifiedShaderModuleConst { const STAGE: VkShaderStageFlags; }
pub trait ClassifiedShaderModule { fn stage(&self) -> VkShaderStageFlags; }
impl ClassifiedShaderModuleConst for VertexShaderModule { const STAGE: VkShaderStageFlags = VK_SHADER_STAGE_VERTEX_BIT; }
impl ClassifiedShaderModuleConst for FragmentShaderModule { const STAGE: VkShaderStageFlags = VK_SHADER_STAGE_FRAGMENT_BIT; }
impl ClassifiedShaderModuleConst for GeometryShaderModule { const STAGE: VkShaderStageFlags = VK_SHADER_STAGE_GEOMETRY_BIT; }
impl ClassifiedShaderModuleConst for TessControlShaderModule { const STAGE: VkShaderStageFlags = VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT; }
impl ClassifiedShaderModuleConst for TessEvaluationShaderModule { const STAGE: VkShaderStageFlags = VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT; }
impl<T> ClassifiedShaderModule for T where T: ClassifiedShaderModuleConst { fn stage(&self) -> VkShaderStageFlags { Self::STAGE } }
pub enum PipelineShaderClass
{
	Vertex(PipelineShaderProgram<VertexShader>), TessControl(PipelineShaderProgram<TessellationControlShader>),
	TessEvaluation(PipelineShaderProgram<TessellationEvaluationShader>),
	Geometry(PipelineShaderProgram<GeometryShader>), Fragment(PipelineShaderProgram<FragmentShader>)
}
pub struct LoadedAssets
{
	pub shaders: HashMap<String, Box<ClassifiedShaderModule>>,
	pub pipeline_shaders: NamedContents<PipelineShaderClass>
}

pub fn load_assets<Engine: AssetProvider + Deref<Target = GraphicsInterface>>(engine: &Engine, source: &ParsedDeviceResources, err: &mut ErrorReporter) -> LoadedAssets
{
	fn shader_insert_dupcheck<F, SM>(assets: &mut LoadedAssets, err: &mut ErrorReporter, loc: &Location, name: String, loader: F)
		where F: FnOnce() -> EngineResult<SM>, SM: ClassifiedShaderModuleConst + 'static
	{
		let duplication_detected = if let Some(loaded) = assets.shaders.get(&name)
		{
			if SM::STAGE != loaded.stage()
			{
				err.report_fmt(format_args!("Shader Asset {} has already loaded with different shader stage({})", name, loaded.stage()), Some(&From::from(loc)));
			}
			true
		}
		else { false };

		if !duplication_detected
		{
			match loader()
			{
				Ok(b) => { assets.shaders.insert(name, box b); },
				Err(e) => err.report_fmt(format_args!("Failed to load asset {}: {:?}", name, e), Some(&From::from(loc)))
			}
		}
	}
	let mut assets = LoadedAssets { shaders: HashMap::new(), pipeline_shaders: NamedContents::new() };

	fn load_assets_impl<E, C, SM>(sink: &mut LoadedAssets, parsed_assets: &NamedContents<IndependentShaderStageInfo>, err: &mut ErrorReporter, engine: &E, constructor: C)
		where E: AssetProvider + Deref<Target = GraphicsInterface>, C: Fn(ShaderModule) -> SM, SM: ClassifiedShaderModuleConst + 'static
	{
		for s in parsed_assets.iter()
		{
			if let &LocationPacked(ref l, AssetResource::PathRef(ref path)) = &s.asset
			{
				let asset_refkey = path.join(".");
				shader_insert_dupcheck(sink, err, l, asset_refkey, || ShaderModule::from_asset(engine, path).map(&constructor));
			}
		}
	}
	for &VertexShaderStageInfo { asset: LocationPacked(ref loc, ref ares), .. } in source.ind_shaders.vertex.iter()
	{
		if let &AssetResource::PathRef(ref path) = ares
		{
			let asset_refkey = path.join(".");
			shader_insert_dupcheck(&mut assets, err, loc, asset_refkey, || ShaderModule::from_asset(engine, path).map(VertexShaderModule));
		}
	}
	load_assets_impl(&mut assets, &source.ind_shaders.fragment, err, engine, FragmentShaderModule);
	load_assets_impl(&mut assets, &source.ind_shaders.geometry, err, engine, GeometryShaderModule);
	load_assets_impl(&mut assets, &source.ind_shaders.tesscontrol, err, engine, TessControlShaderModule);
	load_assets_impl(&mut assets, &source.ind_shaders.tessevaluation, err, engine, TessEvaluationShaderModule);

	assets
}
