//! Asset Loader

use interlude::*;
use interlude::ffi::*;
use std::collections::HashMap;
use parser::{ParsedDeviceResources, NamedContents};
use std::rc::Rc;
use std::ops::Deref;
use items::*;

trait ShaderClassifiable { fn classified(self) -> ShaderClass; }
impl ShaderClassifiable for Rc<VertexShader> { fn classified(self) -> ShaderClass { ShaderClass::Vertex(self) } }
impl ShaderClassifiable for Rc<TessellationControlShader> { fn classified(self) -> ShaderClass { ShaderClass::TessControl(self) } }
impl ShaderClassifiable for Rc<TessellationEvaluationShader> { fn classified(self) -> ShaderClass { ShaderClass::TessEvaluation(self) } }
impl ShaderClassifiable for Rc<GeometryShader> { fn classified(self) -> ShaderClass { ShaderClass::Geometry(self) } }
impl ShaderClassifiable for Rc<FragmentShader> { fn classified(self) -> ShaderClass { ShaderClass::Fragment(self) } }
pub enum ShaderClass
{
	Vertex(Rc<VertexShader>), TessControl(Rc<TessellationControlShader>), TessEvaluation(Rc<TessellationEvaluationShader>),
	Geometry(Rc<GeometryShader>), Fragment(Rc<FragmentShader>)
}
impl ShaderClass
{
	fn stage_bits(&self) -> VkShaderStageFlags
	{
		match self
		{
			&ShaderClass::Vertex(_) => VK_SHADER_STAGE_VERTEX_BIT, &ShaderClass::Geometry(_) => VK_SHADER_STAGE_GEOMETRY_BIT,
			&ShaderClass::TessControl(_) => VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT, &ShaderClass::TessEvaluation(_) => VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT,
			&ShaderClass::Fragment(_) => VK_SHADER_STAGE_FRAGMENT_BIT
		}
	}
}
pub enum PipelineShaderClass
{
	Vertex(PipelineShaderProgram<VertexShader>), TessControl(PipelineShaderProgram<TessellationControlShader>),
	TessEvaluation(PipelineShaderProgram<TessellationEvaluationShader>),
	Geometry(PipelineShaderProgram<GeometryShader>), Fragment(PipelineShaderProgram<FragmentShader>)
}
pub struct LoadedAssets
{
	pub shaders: HashMap<String, ShaderClass>,
	pub pipeline_shaders: NamedContents<PipelineShaderClass>
}

pub fn load_assets<Engine: AssetProvider + Deref<Target = GraphicsInterface>>(engine: &Engine, source: &ParsedDeviceResources) -> LoadedAssets
{
	fn shader_insert_dupcheck<S: Shader>(assets: &mut LoadedAssets, name: String, shader: Rc<S>) -> Result<(), String> where Rc<S> : ShaderClassifiable
	{
		if let Some(loaded) = assets.shaders.get(&name)
		{
			if S::as_stage_bits() != loaded.stage_bits() { return Err(name) }
		}
		assets.shaders.entry(name).or_insert(shader.classified());
		Ok(())
	}
	let mut assets = LoadedAssets { shaders: HashMap::new(), pipeline_shaders: NamedContents::new() };
	
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

	assets
}
