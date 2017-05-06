//! Error Definition and Reporter

use std::fmt::*;
use std::fmt::Result as FmtResult;
use std::result::Result;
use std::borrow::Cow;
use std::num::{ParseIntError, ParseFloatError};
use syntree::Location;
use std::convert::From;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum EnumType
{
	DeviceResource, Format, ImageLayout, ExternalResource, DescriptorKind, PrimitiveTopology(bool),
	RenderPassAttachmentOptions, PipelineStageFlag, ShaderStageFlag, AccessFlag, ClearMode,
	ComponentSwizzle, ImageUsageBits, BufferUsageBits, FilterType
}
impl Display for EnumType
{
	fn fmt(&self, fmt: &mut Formatter) -> FmtResult
	{
		use self::EnumType::*;
		fmt.write_str(match *self
		{
			DeviceResource => "Device Resource",
			Format => "Data Format",
			ImageLayout => "Image Layout",
			ExternalResource => "External Resource",
			DescriptorKind => "Descriptor Kind",
			PrimitiveTopology(false) => "Primitive Topology",
			PrimitiveTopology(true)  => "Primitive Topology with Adjacent",
			RenderPassAttachmentOptions => "Attachment Options in RenderPass",
			PipelineStageFlag => "Pipeline Stage Flag",
			ShaderStageFlag => "Shader Stage Flag",
			AccessFlag => "Access Mask",
			ClearMode => "Attachment Clearing Mode",
			ComponentSwizzle => "Component Swizzle",
			ImageUsageBits => "Image Usage",
			BufferUsageBits => "Buffer Usage",
			FilterType => "Filter Type"
		})
	}
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RequestType
{
	Name, IntValue, ImageLayout, Direction, Delimiter, Closing, Format, Bytesize, ConfigInt
}
impl Display for RequestType
{
	fn fmt(&self, fmt: &mut Formatter) -> FmtResult
	{
		use self::RequestType::*;
		fmt.write_str(match *self
		{
			Name => "Name", IntValue => "Integer Value", ImageLayout => "Image Layout",
			Direction => "Direction Token (<-, ->, To or From)",
			Delimiter => "Delimiter", Closing => "Closing Token", Format => "Data Format", Bytesize => "Bytesize",
			ConfigInt => "Integer Value or ConfigInt"
		})
	}
}
#[derive(Debug, PartialEq, Eq)]
pub enum ParseError
{
	Required(RequestType, usize),
	Expected(Cow<'static, str>, usize),
	UnknownEnumValue(EnumType, usize),
	NumericParseError(ParseIntError, usize),
	FloatingParseError(ParseFloatError, usize),
	CorruptedSubpassDesc(usize),
	UnknownConfig(&'static str),
	ConfigRequired(&'static str),
	UnknownObjectRef(&'static str, usize),
	NameNotAllowed(usize),
	EntryDuplicated(Cow<'static, str>),
	DefinitionOverrided,
}
impl ParseError
{
	fn column(&self) -> Option<usize>
	{
		use self::ParseError::*;
		match self
		{
			&Required(_, c) | &UnknownEnumValue(_, c) | &Expected(_, c) | &NumericParseError(_, c) | &FloatingParseError(_, c) |
			&CorruptedSubpassDesc(c) | &UnknownObjectRef(_, c) | &NameNotAllowed(c) => Some(c),
			_ => None
		}
	}
	pub fn xc_require_type(self, new_rt: RequestType) -> Self
	{
		if let ParseError::Required(_, c) = self { ParseError::Required(new_rt, c) } else { self }
	}
	fn format_description(&self, fmt: &mut Formatter) -> FmtResult
	{
		use self::ParseError::*;
		match self
		{
			&Required(RequestType::Name, _) => write!(fmt, "Name required following $"),
			&Required(r, _) => write!(fmt, "{} required", r),
			&UnknownEnumValue(e, _) => write!(fmt, "Unknown {} was found", e),
			&Expected(ref e, _) => write!(fmt, "Expected {} but it was not found", e),
			&NumericParseError(ref n, _) => write!(fmt, "NumericParseError: {}", n),
			&FloatingParseError(ref n, _) => write!(fmt, "FloatingParseError: {}", n),
			&CorruptedSubpassDesc(_) => write!(fmt, "Some errors are found parsing SubpassDesc"),
			&UnknownConfig(ref s) => write!(fmt, "Unknown Configuration for {} was found", s),
			&ConfigRequired(ref s) => write!(fmt, "Configuration \"{}\" required", s),
			&UnknownObjectRef(ref o, _) => write!(fmt, "Unknown {} ref", o),
			&NameNotAllowed(_) => write!(fmt, "Naming not allowed for this configuration"),
			&EntryDuplicated(ref e) => write!(fmt, "Entry {} is duplicated", e),
			&DefinitionOverrided => write!(fmt, "Multiple definitions are found")
		}
	}
}
#[derive(Debug)] pub struct ErrorLocation(pub usize, pub Option<usize>);
impl Display for ErrorLocation
{
	fn fmt(&self, fmt: &mut Formatter) -> FmtResult
	{
		if let Some(c) = self.1 { write!(fmt, "line {}, col {}", self.0, c) }
		else { write!(fmt, "line {}", self.0) }
	}
}
impl<'r> From<&'r Location> for ErrorLocation
{
	fn from(input: &'r Location) -> Self { ErrorLocation(input.0, Some(input.1)) }
}
macro_rules! el_custom_format
{
	($o: expr => $s: expr) => { format_args!($s, line: $o.0, column: $o.1) }
}
/// Error Description with Line number where error was caused
#[derive(Debug)] pub struct ParseErrorWithLine(pub ParseError, pub usize);
pub trait WithLine<T, E> { fn with_line(self, line: usize) -> Result<T, E>; }
impl<T> WithLine<T, ParseErrorWithLine> for Result<T, ParseError>
{
	fn with_line(self, line: usize) -> Result<T, ParseErrorWithLine> { self.map_err(|e| ParseErrorWithLine(e, line)) }
}
impl ParseErrorWithLine
{
	pub fn cause(&self) -> &ParseError { &self.0 }
	pub fn location(&self) -> ErrorLocation { ErrorLocation(self.1, self.0.column().map(|x| x + 1)) }
}
impl Display for ParseErrorWithLine
{
	fn fmt(&self, fmt: &mut Formatter) -> FmtResult
	{
		self.0.format_description(fmt).and_then(|_| write!(fmt, " at {}", self.location()))
	}
}

pub trait ErrorReporter
{
	fn report_fmt(&mut self, args: Arguments, location: Option<&ErrorLocation>);

	fn report(&mut self, msg: &str, location: Option<&ErrorLocation>) { self.report_fmt(format_args!("{}\n", msg), location); }
}
pub struct StdErrReporter;
impl ErrorReporter for StdErrReporter
{
	fn report_fmt(&mut self, args: Arguments, location: Option<&ErrorLocation>)
	{
		use std::io::stderr; use std::io::prelude::Write;

		if let Some(l) = location { stderr().write_fmt(format_args!("{} at {}", args, l)) }
		else { stderr().write_fmt(args) }.unwrap();
	}
}
