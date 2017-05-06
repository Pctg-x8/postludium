// Resolving ConfigInt to contextual integer values

use interlude;
use interlude::ffi::*;
use syntree;
use syntree::*;
use ErrorReporter;

/// Short-circuit for { |x| o.f(x) }
macro_rules! Delegate
{
	($o: expr => $f: ident) => { |x| $o.$f(x) }
}

// Values to ConfigInt conversions
trait ScalarConversion<T> : Sized { fn _as(self) -> T; }
impl ScalarConversion<usize> for u32 { fn _as(self) -> usize { self as usize } }
impl LocationPacked<ConfigInt>
{
	fn resolve<F, CV>(&self, error: &mut ErrorReporter, resolver: F) -> Option<CV> where F: FnOnce(&str) -> Option<CV>, u32: ScalarConversion<CV>
	{
		let &LocationPacked(ref loc, ref cint) = self;
		match cint
		{
			&ConfigInt::Ref(ref refn) => if let Some(v) = resolver(refn) { Some(v) } else
			{
				error.report_fmt(format_args!("Unknown ConfigInt Reference to {}", refn), Some(&loc.into()));
				None
			},
			&ConfigInt::Value(v) => Some(v._as())
		}
	}
}
impl Transition<LocationPacked<ConfigInt>>
{
	fn resolve<F, CV>(&self, error: &mut ErrorReporter, resolver: F) -> Option<Transition<CV>> where F: Fn(&str) -> Option<CV>, u32: ScalarConversion<CV>
	{
		let (f, t) = (self.from.resolve(error, &resolver), self.to.resolve(error, &resolver));
		f.and_then(|from| t.map(|to| Transition { from, to }))
	}
}
#[cfg(test)] mod tests
{
	use super::*;
	use error::StdErrReporter;

	#[test] fn configint_resolver()
	{
		assert_eq!(LocationPacked(Location(1, 0), ConfigInt::Ref("testing".into())).resolve(&mut StdErrReporter, |_| Some(0)), Some(0));
		assert_eq!(LocationPacked(Location(1, 0), ConfigInt::Ref("testing".into())).resolve(&mut StdErrReporter, |_| None), None);
		assert_eq!(LocationPacked(Location(1, 0), ConfigInt::Value(3)).resolve(&mut StdErrReporter, |_| None), Some(3));
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
			color_outs: self.color_outs.into_iter().map(|ci| ci.resolve(er, |refn| parent.attachments.reverse_index(refn)).unwrap_or(0)).collect(),
			inputs: self.inputs.into_iter().map(|ci| ci.resolve(er, |refn| parent.attachments.reverse_index(refn)).unwrap_or(0)).collect()
		}
	}
}

pub struct ResolvedRPSubpassDeps
{
	pub passtrans: Transition<usize>, pub access_mask: Transition<LocationPacked<interlude::AccessFlags>>,
	pub stage_bits: LocationPacked<VkPipelineStageFlags>, pub by_region: bool
}
impl RPSubpassDeps
{
	pub fn resolve(self, parent: &RenderPassData, er: &mut ErrorReporter) -> Option<ResolvedRPSubpassDeps>
	{
		self.passtrans.resolve(er, Delegate!(parent.subpasses => reverse_index)).map(move |passtrans| ResolvedRPSubpassDeps
		{
			passtrans, access_mask: self.access_mask, stage_bits: self.stage_bits, by_region: self.by_region
		})
	}
}

pub struct PreciseRenderPass { pub obj: usize, pub subpass: usize }
impl PreciseRenderPassRef
{
	pub fn resolve(self, parent: &ParsedDeviceResources, er: &mut ErrorReporter) -> Option<PreciseRenderPass>
	{
		self.rp.resolve(er, Delegate!(parent.renderpasses => reverse_index)).and_then(|obj|
		{
			let ref o = parent.renderpasses[obj];
			self.subpass.resolve(er, Delegate!(o.subpasses => reverse_index)).map(|subpass| PreciseRenderPass { obj, subpass })
		})
	}
}

#[derive(PartialEq, Eq)]
pub struct PipelineLayout { pub descs: Vec<usize>, pub pushconstants: Vec<usize> }
impl syntree::PipelineLayout
{
	pub fn resolve(&self, parent: &ParsedDeviceResources, er: &mut ErrorReporter) -> Option<PipelineLayout>
	{
		let d = self.descs.iter().map(|lp| lp.resolve(er, Delegate!(parent.descriptor_set_layouts => reverse_index))).collect::<Option<_>>();
		let p = self.pushconstants.iter().map(|lp| lp.resolve(er, Delegate!(parent.push_constant_layouts => reverse_index))).collect::<Option<_>>();
		d.and_then(|descs| p.map(|pushconstants| PipelineLayout { descs, pushconstants }))
	}
}
