//! Asset Loader

use interlude::*;
use interlude::ffi::*;
use std::collections::HashMap;
use syntree::*;
use std::ops::Deref;
use resolver::ErrorReporter;

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
		if let Some(loaded) = assets.shaders.get(&name)
		{
			if SM::STAGE != loaded.stage()
			{
				err.error(format!("Asset {} has loaded with different shader stage({})", name, loaded.stage()), loc);
			}
			return;
		}

		match loader()
		{
			Ok(b) => { assets.shaders.insert(name, box b); },
			Err(e) => err.error(format!("Failed to load asset {}: {:?}", name, e), loc)
		}
	}
	let mut assets = LoadedAssets { shaders: HashMap::new(), pipeline_shaders: NamedContents::new() };

	fn load_assets_impl<E, C, SM>(sink: &mut LoadedAssets, parsed_assets: &NamedContents<IndependentPipelineShaderStageInfo>, err: &mut ErrorReporter, engine: &E, constructor: C)
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
	load_assets_impl(&mut assets, &source.ind_shaders.vertex, err, engine, VertexShaderModule);
	load_assets_impl(&mut assets, &source.ind_shaders.fragment, err, engine, FragmentShaderModule);
	load_assets_impl(&mut assets, &source.ind_shaders.geometry, err, engine, GeometryShaderModule);
	load_assets_impl(&mut assets, &source.ind_shaders.tesscontrol, err, engine, TessControlShaderModule);
	load_assets_impl(&mut assets, &source.ind_shaders.tessevaluation, err, engine, TessEvaluationShaderModule);
	
	/*
	for ind_shader in source.ind_shaders.iter()
	{
		if let &LocationPacked(l, c, AssetResource::PathRef(ref path)) = &ind_shader.asset
		{
			let asset_refkey = path.join(".");
			if let Err(asset_refkey) = match ind_shader.stage
			{
				VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT =>
					shader_insert_dupcheck(&mut assets, asset_refkey, TessellationControlShader::from_asset(engine, path, "main").unwrap()),
				VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT =>
					shader_insert_dupcheck(&mut assets, asset_refkey, TessellationEvaluationShader::from_asset(engine, path, "main").unwrap()),
				VK_SHADER_STAGE_GEOMETRY_BIT => shader_insert_dupcheck(&mut assets, asset_refkey, GeometryShader::from_asset(engine, path, "main").unwrap()),
				VK_SHADER_STAGE_FRAGMENT_BIT => shader_insert_dupcheck(&mut assets, asset_refkey, FragmentShader::from_asset(engine, path, "main").unwrap()),
				_ => unreachable!()
			} { panic!("Shader Asset {} (at {}:{}) was attempted to load as different types", asset_refkey, l, c); }
		}
	}
	*/

	assets
}
