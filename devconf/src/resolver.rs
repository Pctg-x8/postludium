// Resolving ConfigInt to contextual integer values

use interlude::ffi::*;
use std;
use syntree::*;

pub struct ErrorReporter { has_error: bool }
impl ErrorReporter
{
	pub fn new() -> Self { ErrorReporter { has_error: false } }
	pub fn error(&mut self, mut msg: String, location: &Location)
	{
		use std::io::Write;

		self.has_error = true;
		msg.push_str(&format!(" at line {}, col {}\n", location.0, location.1));
		std::io::stderr().write_all(msg.as_bytes()).unwrap();
	}
}

// Values to ConfigInt conversions
trait ScalarConversion<T> : Sized { fn sconv(self) -> T; }
impl ScalarConversion<usize> for u32 { fn sconv(self) -> usize { self as usize } }
fn resolve_config_int<F, CV>(config: LocationPacked<ConfigInt>, error: &mut ErrorReporter, resolver: F) -> Option<CV>
	where F: FnOnce(&str) -> Option<CV>, u32: ScalarConversion<CV>
{
	let LocationPacked(loc, cint) = config;
	match cint
	{
		ConfigInt::Ref(ref refn) => if let Some(v) = resolver(refn) { Some(v) } else
		{
			error.error(format!("Unknown ConfigInt Reference to {}", refn), &loc);
			None
		},
		ConfigInt::Value(v) => Some(v.sconv())
	}
}
#[cfg(test)] mod tests
{
	use super::*;

	#[test] fn configint_resolver()
	{
		let mut er = ErrorReporter::new();
		assert_eq!(resolve_config_int(LocationPacked(Location(1, 0), ConfigInt::Ref("testing".into())), &mut er, |_| Some(0)), Some(0));
		assert_eq!(resolve_config_int(LocationPacked(Location(1, 0), ConfigInt::Ref("testing".into())), &mut er, |_| None), None);
		assert_eq!(resolve_config_int(LocationPacked(Location(1, 0), ConfigInt::Value(3)), &mut er, |_| None), Some(3));
	}
}

impl ConfigInt
{
	pub fn unwrap_as_resolved(&self) -> u32
	{
		match self { &ConfigInt::Value(v) => v, _ => unreachable!() }
	}
}
impl Format
{
	pub fn unwrap_as_resolved(&self) -> VkFormat
	{
		match self { &Format::Value(f) => f, _ => unreachable!() }
	}
}

pub struct ResolvedRPSubpassDesc
{
	pub color_outs: Vec<usize>, pub inputs: Vec<usize>
}
impl RPSubpassDesc
{
	pub fn resolve(self, parent: &RenderPassData, er: &mut ErrorReporter) -> ResolvedRPSubpassDesc
	{
		ResolvedRPSubpassDesc
		{
			color_outs: self.color_outs.into_iter().map(|ci| resolve_config_int(ci, er, |refn| parent.attachments.reverse_index(refn)).unwrap_or(0)).collect(),
			inputs: self.inputs.into_iter().map(|ci| resolve_config_int(ci, er, |refn| parent.attachments.reverse_index(refn)).unwrap_or(0)).collect()
		}
	}
}

pub struct ResolvedRPSubpassDeps
{
	pub passtrans: Transition<usize>, pub access_mask: Transition<LocationPacked<AccessFlags>>,
	pub stage_bits: LocationPacked<VkPipelineStageFlags>, pub by_region: bool
}
impl RPSubpassDeps
{
	pub fn resolve(self, parent: &RenderPassData, er: &mut ErrorReporter) -> ResolvedRPSubpassDeps
	{
		ResolvedRPSubpassDeps
		{
			passtrans: Transition
			{
				from: resolve_config_int(self.passtrans.from, er, |refn| parent.subpasses.reverse_index(refn)).unwrap_or(0),
				to: resolve_config_int(self.passtrans.to, er, |refn| parent.subpasses.reverse_index(refn)).unwrap_or(0)
			},
			access_mask: self.access_mask, stage_bits: self.stage_bits, by_region: self.by_region
		}
	}
}

/// Short-circuit for { |x| o.f(x) }
macro_rules! Delegate
{
	($o: expr => $f: ident) => { |x| $o.$f(x) }
}

pub struct PreciseRenderPass { pub obj: usize, pub subpass: usize }
impl PreciseRenderPassRef
{
	pub fn resolve(self, parent: &ParsedDeviceResources, er: &mut ErrorReporter) -> PreciseRenderPass
	{
		let ox = resolve_config_int(self.rp, er, Delegate!(parent.renderpasses => reverse_index)).unwrap_or(0);
		if !er.has_error
		{
			let ref o = parent.renderpasses[ox];
			PreciseRenderPass
			{
				obj: ox, subpass: resolve_config_int(self.subpass, er, Delegate!(o.subpasses => reverse_index)).unwrap_or(0)
			}
		}
		else { PreciseRenderPass { obj: 0, subpass: 0 } }
	}
}
