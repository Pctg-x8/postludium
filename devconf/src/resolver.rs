// Resolving ConfigInt to contextual integer values

use super::*;

pub struct ErrorReporter { has_error: bool }
impl ErrorReporter
{
	pub fn new() -> Self { ErrorReporter { has_error: false } }
	fn error(&mut self, mut msg: String, location: (usize, usize))
	{
		use std::io::Write;

		self.has_error = true;
		msg.push_str(&format!(" at line {}, col {}\n", location.0, location.1));
		std::io::stderr().write_all(msg.as_bytes()).unwrap();
	}
}

// Values to ConfigInt conversions
unsafe trait UnsafeConversion<T> : Sized { unsafe fn uconv(self) -> T; }
unsafe impl UnsafeConversion<usize> for u32 { unsafe fn uconv(self) -> usize { self as usize } }
impl ConfigInt
{
	fn resolve<F, CV>(self, er: &mut ErrorReporter, resolver: F) -> Option<CV>
		where F: FnOnce(&str) -> Option<CV>, u32: UnsafeConversion<CV>
	{
		match self
		{
			ConfigInt::Ref(ref refn) => if let Some(v) = resolver(refn) { Some(v) } else
			{
				er.error(format!("Unknown ConfigInt Reference to {}", refn), (0, 0));
				None
			},
			ConfigInt::Value(v) => Some(unsafe { v.uconv() })
		}
	}
}
#[cfg(test)] mod tests
{
	use super::*;

	#[test] fn configint_resolver()
	{
		let mut er = ErrorReporter::new();
		assert_eq!(ConfigInt::Ref("testing".into()).resolve(&mut er, |_| Some(0)), Some(0));
		assert_eq!(ConfigInt::Ref("testing".into()).resolve(&mut er, |_| None), None);
		assert_eq!(ConfigInt::Value(3).resolve(&mut er, |_| None), Some(3));
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
	pub passtrans: Transition<usize>, pub access_mask: Transition<VkAccessFlags>, pub stage_bits: VkPipelineStageFlags, pub by_region: bool
}
impl RPSubpassDeps
{
	pub fn resolve(self, parent: &RenderPassData, er: &mut ErrorReporter) -> ResolvedRPSubpassDeps
	{
		ResolvedRPSubpassDeps
		{
			passtrans: Transition
			{
				from: self.passtrans.from.resolve(er, |refn| parent.subpasses.reverse_index(refn)).unwrap_or(0),
				to: self.passtrans.to.resolve(er, |refn| parent.subpasses.reverse_index(refn)).unwrap_or(0)
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
		let ox = self.rp.resolve(er, Delegate!(parent.renderpasses => reverse_index)).unwrap_or(0);
		if !er.has_error
		{
			let ref o = parent.renderpasses[ox];
			PreciseRenderPass
			{
				obj: ox, subpass: self.subpass.resolve(er, Delegate!(o.subpasses => reverse_index)).unwrap_or(0)
			}
		}
		else { PreciseRenderPass { obj: 0, subpass: 0 } }
	}
}
