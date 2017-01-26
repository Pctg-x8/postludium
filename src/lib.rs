// Postludum: High-configurable Game Engine layered on interlude

#[macro_use] extern crate interlude;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate itertools;

use interlude::*;
use interlude::ffi::*;
use std::fs::File;
use itertools::Itertools;
use std::collections::HashMap;
use std::ops::Deref;
use std::borrow::Cow;
use std::io::prelude::*;

#[macro_use] mod parsetools;
use self::parsetools::ParseTools;
mod lazylines;
use self::lazylines::*;

// mod devconf;

#[derive(Clone)]
pub enum DevConfParsingResult<T>
{
	Ok(T), NumericParseError(std::num::ParseIntError),
	InvalidFormatError, InvalidUsageFlagError(String), InvalidFilterError(String),
	UnsupportedDimension, UnsupportedParameter(Cow<'static, str>), InvalidSwizzle, UnknownConfiguration
}
impl<T> DevConfParsingResult<T>
{
	#[cfg(test)]
	pub fn unwrap(self) -> T
	{
		match self
		{
			DevConfParsingResult::Ok(t) => t,
			DevConfParsingResult::NumericParseError(e) => panic!(e),
			DevConfParsingResult::InvalidFormatError => panic!("Invalid Image Format"),
			DevConfParsingResult::InvalidSwizzle => panic!("Invalid Swizzle"),
			DevConfParsingResult::InvalidUsageFlagError(s) => panic!("Invalid Usage Flag: {}", s),
			DevConfParsingResult::InvalidFilterError(s) => panic!("Invalid Filter Type: {}", s),
			DevConfParsingResult::UnsupportedDimension => panic!("Unsupported Image Dimension"),
			DevConfParsingResult::UnsupportedParameter(dep) => panic!("Unsupported Parameter for {}", dep),
			DevConfParsingResult::UnknownConfiguration => panic!("Unknown Configuration")
		}
	}
	pub fn unwrap_on_line(self, line: usize) -> T
	{
		match self
		{
			DevConfParsingResult::Ok(t) => t,
			DevConfParsingResult::NumericParseError(e) => panic!("{} at line {}", e, line),
			DevConfParsingResult::InvalidFormatError => panic!("Invalid Image Format at line {}", line),
			DevConfParsingResult::InvalidSwizzle => panic!("Invalid Swizzle at line {}", line),
			DevConfParsingResult::InvalidUsageFlagError(s) => panic!("Invalid Usage Flag: {} at line {}", s, line),
			DevConfParsingResult::InvalidFilterError(s) => panic!("Invalid Filter Type: {} at line {}", s, line),
			DevConfParsingResult::UnsupportedDimension => panic!("Unsupported Image Dimension at line {}", line),
			DevConfParsingResult::UnsupportedParameter(dep) => panic!("Unsupported Parameter for {} at line {}", dep, line),
			DevConfParsingResult::UnknownConfiguration => panic!("Unknown Configuration at line {}", line)
		}
	}
	#[cfg(test)]
	pub fn is_invalid_format_err(&self) -> bool
	{
		match self { &DevConfParsingResult::InvalidFormatError => true, _ => false }
	}
	#[cfg(test)]
	pub fn is_invalid_usage_flag_err(&self) -> bool
	{
		match self { &DevConfParsingResult::InvalidUsageFlagError(_) => true, _ => false }
	}
	#[cfg(test)]
	pub fn is_numeric_parsing_failed(&self) -> bool
	{
		match self { &DevConfParsingResult::NumericParseError(_) => true, _ => false }
	}
	#[cfg(test)]
	pub fn is_invalid_filter_type_err(&self) -> bool
	{
		match self { &DevConfParsingResult::InvalidFilterError(_) => true, _ => false }
	}
	pub fn wrap(res: Option<T>, errmap: Self) -> Self
	{
		match res
		{
			Some(e) => DevConfParsingResult::Ok(e),
			_ => errmap
		}
	}
}
impl<T: Clone> std::convert::From<Result<T, std::num::ParseIntError>> for DevConfParsingResult<T>
{
	fn from(r: Result<T, std::num::ParseIntError>) -> Self
	{
		match r { Ok(t) => DevConfParsingResult::Ok(t), Err(e) => DevConfParsingResult::NumericParseError(e) }
	}
}

#[derive(Clone, Copy)]
pub enum ImageDimensions { Single, Double, Triple }

fn is_ignored(c: char) -> bool { c == ' ' || c == '\t' }
fn not_ignored(c: char) -> bool { !is_ignored(c) }

lazy_static!
{
	static ref VAR_SCREEN_FORMAT: Vec<char>			= "$ScreenFormat".chars().collect();
	static ref CSTR_BLOCKCOMPRESSION_4: Vec<char>	= "BlockCompression4".chars().collect();
	static ref CSTR_BLOCKCOMPRESSION_5: Vec<char>	= "BlockCompression5".chars().collect();
	static ref CSTR_R8: Vec<char>					= "R8".chars().collect();
	static ref CSTR_R8G8: Vec<char>					= "R8G8".chars().collect();
	static ref CSTR_R8G8B8A8: Vec<char>				= "R8G8B8A8".chars().collect();
	static ref CSTR_R16G16B16A16: Vec<char>			= "R16G16B16A16".chars().collect();
	static ref CSTR_UNORM: Vec<char>				= "UNORM".chars().collect();
	static ref CSTR_SNORM: Vec<char>				= "SNORM".chars().collect();
	static ref CSTR_SRGB: Vec<char>					= "SRGB".chars().collect();
	static ref CSTR_SFLOAT: Vec<char>				= "SFLOAT".chars().collect();
}
pub fn parse_image_format(args: &[char], screen_format: VkFormat) -> DevConfParsingResult<VkFormat>
{
	let (bit_arrange, rest) = args.take_while(not_ignored);
	if bit_arrange == &**VAR_SCREEN_FORMAT { DevConfParsingResult::Ok(screen_format) }
	else
	{
		let (element_type, _) = rest.skip_while(is_ignored).take_while(not_ignored);
		DevConfParsingResult::wrap(PartialEqualityMatchMap!(bit_arrange;
		{
			&**CSTR_R8 => PartialEqualityMatchMap!(element_type;
			{
				&**CSTR_UNORM => VkFormat::R8_UNORM,
				&**CSTR_SNORM => VkFormat::R8_SNORM
			}),
			&**CSTR_R8G8 => PartialEqualityMatchMap!(element_type;
			{
				&**CSTR_UNORM => VkFormat::R8G8_UNORM
			}),
			&**CSTR_R8G8B8A8 => PartialEqualityMatchMap!(element_type;
			{
				&**CSTR_UNORM => VkFormat::R8G8B8A8_UNORM,
				&**CSTR_SNORM => VkFormat::R8G8B8A8_SNORM,
				&**CSTR_SRGB => VkFormat::R8G8B8A8_SRGB
			}),
			&**CSTR_R16G16B16A16 => PartialEqualityMatchMap!(element_type;
			{
				&**CSTR_SFLOAT => VkFormat::R16G16B16A16_SFLOAT
			}),
			&**CSTR_BLOCKCOMPRESSION_4 => PartialEqualityMatchMap!(element_type;
			{
				&**CSTR_UNORM => VkFormat::BC4_UNORM_BLOCK,
				&**CSTR_SNORM => VkFormat::BC4_SNORM_BLOCK
			}),
			&**CSTR_BLOCKCOMPRESSION_5 => PartialEqualityMatchMap!(element_type;
			{
				&**CSTR_UNORM => VkFormat::BC5_UNORM_BLOCK,
				&**CSTR_SNORM => VkFormat::BC5_SNORM_BLOCK
			});
			_ => None
		}), DevConfParsingResult::InvalidFormatError)
	}
}
lazy_static!
{
	static ref CSTR_SAMPLED: Vec<char> = "Sampled".chars().collect();
	static ref CSTR_COLORATTACHMENT: Vec<char> = "ColorAttachment".chars().collect();
	static ref CSTR_INPUTATTACHMENT: Vec<char> = "InputAttachment".chars().collect();
	static ref CSTR_DEVICELOCAL: Vec<char> = "DeviceLocal".chars().collect();
}
pub fn parse_image_usage_flags(args: &[char], agg_usage: VkImageUsageFlags, device_local_flag: bool) -> DevConfParsingResult<(VkImageUsageFlags, bool)>
{
	if args.is_empty() { DevConfParsingResult::InvalidUsageFlagError("".to_owned()) }
	else
	{
		let (usage_str, rest) = args.take_while(|c| not_ignored(c) && c != '/');
		let rest = rest.skip_while(is_ignored);
		let has_next = !rest.is_empty() && rest[0] == '/';
		let current_parsed = PartialEqualityMatchMap!(usage_str;
		{
			&**CSTR_SAMPLED => (VK_IMAGE_USAGE_SAMPLED_BIT, false),
			&**CSTR_COLORATTACHMENT => (VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, false),
			&**CSTR_INPUTATTACHMENT => (VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT, false),
			&**CSTR_DEVICELOCAL => (0, true)
		});

		if let Some((usage_bits, devlocal)) = current_parsed
		{
			if has_next { parse_image_usage_flags(rest.drop(1).skip_while(is_ignored), agg_usage | usage_bits, device_local_flag | devlocal) }
			else { DevConfParsingResult::Ok((agg_usage | usage_bits, device_local_flag | devlocal)) }
		}
		else { DevConfParsingResult::InvalidUsageFlagError(usage_str.clone_as_string()) }
	}
}
lazy_static!
{
	static ref VAR_SCREEN_WIDTH: Vec<char> = "$ScreenWidth".chars().collect();
	static ref VAR_SCREEN_HEIGHT: Vec<char> = "$ScreenHeight".chars().collect();
}
pub fn parse_image_extent(args: &[char], dims: ImageDimensions, screen_size: &Size2) -> DevConfParsingResult<Size3>
{
	let parse_arg = |input: &[char]| PartialEqualityMatchMap!(input;
	{
		&**VAR_SCREEN_WIDTH => Ok(screen_size.0), &**VAR_SCREEN_HEIGHT => Ok(screen_size.1);
		_ => input.clone_as_string().parse()
	});
	DevConfParsingResult::from(match dims
	{
		ImageDimensions::Single => parse_arg(args.take_while(not_ignored).0).map(|val| Size3(val, 1, 1)),
		ImageDimensions::Double =>
		{
			let (wstr, rest) = args.take_while(not_ignored);
			let (hstr, _) = rest.skip_while(is_ignored).take_while(not_ignored);

			parse_arg(wstr).and_then(|w| parse_arg(hstr).map(move |h| Size3(w, h, 1)))
		},
		ImageDimensions::Triple =>
		{
			let (wstr, rest) = args.take_while(not_ignored);
			let (hstr, rest) = rest.skip_while(is_ignored).take_while(not_ignored);
			let (dstr, _) = rest.skip_while(is_ignored).take_while(not_ignored);

			parse_arg(wstr).and_then(|w| parse_arg(hstr).and_then(move |h| parse_arg(dstr).map(move |d| Size3(w, h, d))))
		}
	})
}
lazy_static!
{
	static ref CSTR_NEAREST: Vec<char> = "Nearest".chars().collect();
	static ref CSTR_LINEAR: Vec<char> = "Linear".chars().collect();
}
pub fn parse_filter_type(args: &[char]) -> DevConfParsingResult<(interlude::Filter, interlude::Filter)>
{
	fn filter_type(slice: &[char]) -> Option<interlude::Filter>
	{
		PartialEqualityMatchMap!(slice;
		{
			&**CSTR_NEAREST => interlude::Filter::Nearest, &**CSTR_LINEAR => interlude::Filter::Linear
		})
	}

	let (magf_str, rest) = args.take_while(not_ignored);
	let rest = rest.skip_while(is_ignored);
	let magfilter = filter_type(magf_str);
	let minf_str = if rest.is_empty() { magf_str } else { rest.take_while(not_ignored).0 };
	let minfilter = filter_type(minf_str);

	match magfilter
	{
		Some(mag) => match minfilter
		{
			Some(min) => DevConfParsingResult::Ok((mag, min)),
			_ => DevConfParsingResult::InvalidFilterError(minf_str.clone_as_string())
		},
		_ => DevConfParsingResult::InvalidFilterError(magf_str.clone_as_string())
	}
}
pub fn parse_component_map(args: &[char]) -> DevConfParsingResult<interlude::ComponentMapping>
{
	fn char_to_swizzle(ch: char) -> Option<interlude::ComponentSwizzle>
	{
		match ch
		{
			'R' | 'r' => Some(interlude::ComponentSwizzle::R),
			'G' | 'g' => Some(interlude::ComponentSwizzle::G),
			'B' | 'b' => Some(interlude::ComponentSwizzle::B),
			'A' | 'a' => Some(interlude::ComponentSwizzle::A),
			_ => None
		}
	}

	match char_to_swizzle(args[0]).and_then(|r|
		char_to_swizzle(args[1]).and_then(move |g|
		char_to_swizzle(args[2]).and_then(move |b|
		char_to_swizzle(args[3]).map(move |a| interlude::ComponentMapping(r, g, b, a)))))
	{
		Some(cm) => DevConfParsingResult::Ok(cm),
		_ => DevConfParsingResult::InvalidSwizzle
	}
}
lazy_static!
{
	static ref CSTR_FORMAT: Vec<char> = "Format".chars().collect();
	static ref CSTR_EXTENT: Vec<char> = "Extent".chars().collect();
	static ref CSTR_USAGE: Vec<char> = "Usage".chars().collect();
	static ref CSTR_COMPONENTMAP: Vec<char> = "ComponentMap".chars().collect();
}
pub fn parse_configuration_image(lines_iter: &mut LazyLines, screen_size: &Size2, screen_format: VkFormat) -> DevConfImage
{
	let (headline, dim) =
	{
		let (headline, conf_head) = lines_iter.pop().unwrap();
		assert!(conf_head.starts_with(&"Image".chars().collect_vec()));
		let dim_str = conf_head.drop(5).skip_while(is_ignored).take_until(is_ignored).0.clone_as_string();
		let dim = match dim_str.as_ref()
		{
			"1D" => ImageDimensions::Single, "2D" => ImageDimensions::Double, "3D" => ImageDimensions::Triple,
			_ => DevConfParsingResult::UnsupportedDimension.unwrap_on_line(headline)
		};

		(headline, dim)
	};

	let (mut format, mut extent, mut usage, mut component_map) = (None, None, None, ComponentMapping::straight());
	while let Some((line, pline)) = lines_iter.next()
	{
		if pline.is_front_of('-')
		{
			lines_iter.drop_line();
			let (name, rest) = pline.drop(1).skip_while(is_ignored).take_while(|c| not_ignored(c) && c != ':');
			let value = rest.skip_while(is_ignored).drop(1).skip_while(is_ignored);
			PartialEqualityMatch!(name;
			{
				&**CSTR_FORMAT => format = Some(parse_image_format(value, screen_format).unwrap_on_line(line)),
				&**CSTR_EXTENT => extent = Some(parse_image_extent(value, dim, screen_size).unwrap_on_line(line)),
				&**CSTR_USAGE => usage = Some(parse_image_usage_flags(value, 0, false).unwrap_on_line(line)),
				&**CSTR_COMPONENTMAP => component_map = parse_component_map(value).unwrap_on_line(line);
				_ => DevConfParsingResult::UnsupportedParameter("Image".into()).unwrap_on_line(line)
			});
		}
		else { break; }
	}
	let (usage, devlocal) = usage.expect(&format!("Usage parameter is not presented at line {}", headline));
	match dim
	{
		ImageDimensions::Single => DevConfImage::Dim1
		{
			format: format.expect(&format!("Format parameter is not presented at line {}", headline)),
			extent: extent.expect(&format!("Extent parameter is not presented at line {}", headline)).0,
			usage: usage, device_local: devlocal, component_map: component_map
		},
		ImageDimensions::Double => DevConfImage::Dim2
		{
			format: format.expect(&format!("Format parameter is not presented at line {}", headline)),
			extent: Size2::from(extent.expect(&format!("Extent parameter is not presented at line {}", headline))),
			usage: usage, device_local: devlocal, component_map: component_map
		},
		ImageDimensions::Triple => DevConfImage::Dim3
		{
			format: format.expect(&format!("Format parameter is not presented at line {}", headline)),
			extent: extent.expect(&format!("Extent parameter is not presented at line {}", headline)),
			usage: usage, device_local: devlocal, component_map: component_map
		}
	}
}
lazy_static!
{
	static ref CSTR_FILTER: Vec<char> = "Filter".chars().collect();
}
pub fn parse_configuration_sampler(lines_iter: &mut LazyLines) -> DevConfSampler
{
	{
		let (_, conf_head) = lines_iter.pop().unwrap();
		assert!(conf_head.starts_with(&"Sampler".chars().collect_vec()));
	}

	let (mut mag_filter, mut min_filter) = (Filter::Linear, Filter::Linear);
	while let Some((l, s)) = lines_iter.next()
	{
		if s.is_front_of('-')
		{
			lines_iter.pop().unwrap();
			let (name, rest) = s.drop(1).skip_while(is_ignored).take_while(|c| not_ignored(c) && c != ':');
			let value = rest.skip_while(is_ignored).drop(1).skip_while(is_ignored);
			PartialEqualityMatch!(name;
			{
				&**CSTR_FILTER => { let (magf, minf) = parse_filter_type(value).unwrap_on_line(l); mag_filter = magf; min_filter = minf; };
				_ => DevConfParsingResult::UnsupportedParameter("Sampler".into()).unwrap_on_line(l)
			});
		}
	}

	DevConfSampler
	{
		mag_filter: mag_filter, min_filter: min_filter
	}
}

#[cfg(test)]
mod test
{
	use interlude::*;
	use interlude::ffi::*;
	use super::*;
	use super::lazylines::*;
	use itertools::Itertools;

	#[test] fn parse_image_formats()
	{
		assert_eq!(parse_image_format(&"R8G8B8A8 UNORM".chars().collect::<Vec<_>>(), VkFormat::R8G8B8A8_SRGB).unwrap(), VkFormat::R8G8B8A8_UNORM);
		assert_eq!(parse_image_format(&"$ScreenFormat".chars().collect::<Vec<_>>(), VkFormat::R16G16B16A16_UNORM).unwrap(), VkFormat::R16G16B16A16_UNORM);
		assert_eq!(parse_image_format(&"$ScreenFormat SFLOAT".chars().collect::<Vec<_>>(), VkFormat::R16G16B16A16_UNORM).unwrap(), VkFormat::R16G16B16A16_UNORM);
		assert!(parse_image_format(&"R8G8B8A8 SFLOAT".chars().collect::<Vec<_>>(), VkFormat::R8G8B8A8_SRGB).is_invalid_format_err());
		assert!(parse_image_format(&"aaa TEST".chars().collect::<Vec<_>>(), VkFormat::R8G8B8A8_SRGB).is_invalid_format_err());
	}
	#[test] fn parse_image_usage()
	{
		assert_eq!(parse_image_usage_flags(&"Sampled / DeviceLocal".chars().collect::<Vec<_>>(), 0, false).unwrap(), (VK_IMAGE_USAGE_SAMPLED_BIT, true));
		assert_eq!(parse_image_usage_flags(&"Sampled /DeviceLocal".chars().collect::<Vec<_>>(), 0, false).unwrap(), (VK_IMAGE_USAGE_SAMPLED_BIT, true));
		assert_eq!(parse_image_usage_flags(&"Sampled".chars().collect::<Vec<_>>(), 0, false).unwrap(), (VK_IMAGE_USAGE_SAMPLED_BIT, false));
		assert!(parse_image_usage_flags(&"AsColorTexture/".chars().collect::<Vec<_>>(), 0, false).is_invalid_usage_flag_err());
	}
	#[test] fn parse_image_extents()
	{
		assert_eq!(parse_image_extent(&"640".chars().collect::<Vec<_>>(), ImageDimensions::Single, &Size2(1920, 1080)).unwrap(), Size3(640, 1, 1));
		assert_eq!(parse_image_extent(&"640 480".chars().collect::<Vec<_>>(), ImageDimensions::Double, &Size2(1920, 1080)).unwrap(), Size3(640, 480, 1));
		assert_eq!(parse_image_extent(&"640 480 16".chars().collect::<Vec<_>>(), ImageDimensions::Triple, &Size2(1920, 1080)).unwrap(), Size3(640, 480, 16));
		assert_eq!(parse_image_extent(&"$ScreenWidth $ScreenHeight".chars().collect::<Vec<_>>(), ImageDimensions::Double, &Size2(1920, 1080)).unwrap(), Size3(1920, 1080, 1));
		assert_eq!(parse_image_extent(&"640 $ScreenHeight".chars().collect::<Vec<_>>(), ImageDimensions::Double, &Size2(1920, 1080)).unwrap(), Size3(640, 1080, 1));
		assert!(parse_image_extent(&"$screenwidth $screenHeight".chars().collect::<Vec<_>>(), ImageDimensions::Double, &Size2(1920, 1080)).is_numeric_parsing_failed());
		assert!(parse_image_extent(&"$screenwidth aaa".chars().collect::<Vec<_>>(), ImageDimensions::Double, &Size2(1920, 1080)).is_numeric_parsing_failed());
	}
	#[test] fn parse_filter_types()
	{
		assert_eq!(parse_filter_type(&"Nearest ".chars().collect::<Vec<_>>()).unwrap(), (Filter::Nearest, Filter::Nearest));
		assert_eq!(parse_filter_type(&"Linear".chars().collect::<Vec<_>>()).unwrap(), (Filter::Linear, Filter::Linear));
		assert_eq!(parse_filter_type(&"Linear Nearest".chars().collect::<Vec<_>>()).unwrap(), (Filter::Linear, Filter::Nearest));
		assert!(parse_filter_type(&"Bilinear".chars().collect::<Vec<_>>()).is_invalid_filter_type_err());
	}
	#[test] fn parse_image_conf()
	{
		let testcase = "Image 2D\n- Format: R8G8B8A8 UNORM\n- Extent: $ScreenWidth $ScreenHeight\n- Usage: Sampled / ColorAttachment\nImage 2D".chars().collect_vec();
		let mut testcase_wrap = LazyLines::new(&testcase);
		let img = parse_configuration_image(&mut testcase_wrap, &Size2(640, 480), VkFormat::R8G8B8A8_UNORM);
		match img
		{
			DevConfImage::Dim2 { format, extent, usage, device_local, .. } =>
			{
				assert_eq!(format, VkFormat::R8G8B8A8_UNORM);
				assert_eq!(extent, Size2(640, 480));
				assert_eq!(usage, VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
				assert_eq!(device_local, false);
			},
			_ => unreachable!()
		}
		assert_eq!(testcase_wrap.next(), Some((5, &"Image 2D".chars().collect_vec()[..])));
	}
	#[test] fn parse_sampler_conf()
	{
		let testcase = "Sampler\n- Filter: Linear".chars().collect_vec();
		let mut testcase_wrap = LazyLines::new(&testcase);
		let smp = parse_configuration_sampler(&mut testcase_wrap);
		assert_eq!(smp.mag_filter, Filter::Linear);
	}
}

#[derive(Debug)]
enum NextInstruction
{
	Comment, Empty, Image, Sampler
}

#[derive(Debug)]
pub enum DevConfImage
{
	Dim1 { format: VkFormat, extent: u32, usage: VkImageUsageFlags, device_local: bool, component_map: interlude::ComponentMapping },
	Dim2 { format: VkFormat, extent: Size2, usage: VkImageUsageFlags, device_local: bool, component_map: interlude::ComponentMapping },
	Dim3 { format: VkFormat, extent: Size3, usage: VkImageUsageFlags, device_local: bool, component_map: interlude::ComponentMapping }
}
#[derive(Debug)]
pub struct DevConfSampler { mag_filter: interlude::Filter, min_filter: interlude::Filter }
pub struct DevConfImages
{
	image_views_1d: Vec<interlude::ImageView1D>, image_views_2d: Vec<interlude::ImageView2D>, image_views_3d: Vec<interlude::ImageView3D>,
	samplers: Vec<interlude::Sampler>,
	device_images: interlude::DeviceImage, staging_images: Option<interlude::StagingImage>
}
pub struct DevConfImagesWithStaging
{
	image_views_1d: Vec<interlude::ImageView1D>, image_views_2d: Vec<interlude::ImageView2D>, image_views_3d: Vec<interlude::ImageView3D>,
	samplers: Vec<interlude::Sampler>,
	#[allow(dead_code)] device_images: interlude::DeviceImage, staging_images: interlude::StagingImage
}
impl DevConfImages
{
	pub fn from_file<Engine: AssetProvider + Deref<Target = GraphicsInterface>>(engine: &Engine, asset_path: &str, screen_size: &Size2, screen_format: VkFormat)
		-> Self
	{
		let path = engine.parse_asset(asset_path, "pdc");
		info!(target: "Postludium", "Parsing Device Configuration {:?}...", path);
		let fchars = File::open(path).and_then(|mut fp| { let mut s = String::new(); fp.read_to_string(&mut s).map(|_| s.chars().collect_vec()) }).unwrap();
		let mut flines = LazyLines::new(&fchars);

		let (mut images, mut samplers) = (Vec::new(), Vec::new());
		while let Some(next) = flines.next().map(|(headline, line)| if line.is_empty() { NextInstruction::Empty }
			else if line.is_front_of('#') { NextInstruction::Comment }
			else if line.starts_with(&"Image".chars().collect_vec()) { NextInstruction::Image }
			else if line.starts_with(&"Sampler".chars().collect_vec()) { NextInstruction::Sampler }
			else { DevConfParsingResult::UnknownConfiguration.unwrap_on_line(headline) })
		{
			match next
			{
				NextInstruction::Empty | NextInstruction::Comment => { flines.pop(); },
				NextInstruction::Image =>
				{
					let obj = parse_configuration_image(&mut flines, screen_size, screen_format);
					println!("Found {:?}", obj);
					images.push(obj);
				},
				NextInstruction::Sampler =>
				{
					let obj = parse_configuration_sampler(&mut flines);
					println!("Found {:?}", obj);
					samplers.push(obj);
				}
			}
		}

		// FIXME: Device Local flags for Image 3D
		let mut image_descriptors1 = HashMap::new();
		let mut image_descriptors2 = HashMap::new();
		let mut image_descriptors3 = HashMap::new();
		for img in &images
		{
			match img
			{
				&DevConfImage::Dim1 { format, extent, usage, device_local: true, .. } => { image_descriptors1.entry((format, extent, usage, true))
					.or_insert(ImageDescriptor1::new(format, extent, usage).device_local()); },
				&DevConfImage::Dim2 { format, ref extent, usage, device_local: true, .. } => { image_descriptors2.entry((format, extent, usage, true))
					.or_insert(ImageDescriptor2::new(format, extent.clone(), usage).device_local()); },
				&DevConfImage::Dim1 { format, extent, usage, device_local: false, .. } => { image_descriptors1.entry((format, extent, usage, false))
					.or_insert(ImageDescriptor1::new(format, extent, usage)); },
				&DevConfImage::Dim2 { format, ref extent, usage, device_local: false, .. } => { image_descriptors2.entry((format, extent, usage, false))
					.or_insert(ImageDescriptor2::new(format, extent.clone(), usage)); },
				&DevConfImage::Dim3 { format, ref extent, usage, .. } => { image_descriptors3.entry((format, extent, usage, false))
					.or_insert(ImageDescriptor3::new(format, extent.clone(), usage)); }
			}
		}

		let images_1d = images.iter().filter_map(|img| match img
		{
			&DevConfImage::Dim1 { format, extent, usage, device_local, component_map } => Some((format, extent, usage, device_local, component_map)),
			_ => None
		}).collect_vec();
		let images_2d = images.iter().filter_map(|img| match img
		{
			&DevConfImage::Dim2 { format, ref extent, usage, device_local, component_map } => Some((format, extent, usage, device_local, component_map)),
			_ => None
		}).collect_vec();
		let images_3d = images.iter().filter_map(|img| match img
		{
			&DevConfImage::Dim3 { format, ref extent, usage, device_local, component_map } => Some((format, extent, usage, device_local, component_map)),
			_ => None
		}).collect_vec();

		let image_descriptor_refs_1d = images_1d.iter().map(|&(format, extent, usage, device_local, _)| image_descriptors1.get(&(format, extent, usage, device_local)).unwrap()).collect_vec();
		let image_descriptor_refs_2d = images_2d.iter().map(|&(format, extent, usage, device_local, _)| image_descriptors2.get(&(format, extent, usage, device_local)).unwrap()).collect_vec();
		let image_descriptor_refs_3d = images_3d.iter().map(|&(format, extent, usage, _, _)| image_descriptors3.get(&(format, extent, usage, false)).unwrap()).collect_vec();
		let image_prealloc_with_moving = ImagePreallocator::new(engine, image_descriptor_refs_1d, image_descriptor_refs_2d, image_descriptor_refs_3d);
		let (backbuffers, staging_images) = Unrecoverable!(image_prealloc_with_moving.instantiate());
		let image_views_1d = images_1d.iter().enumerate().map(|(nr, &(format, _, _, _, component_map))|
			Unrecoverable!(ImageView1D::make_from(&backbuffers.dim1vec()[nr], format, component_map, ImageSubresourceRange::base_color()))).collect_vec();
		let image_views_2d = images_2d.iter().enumerate().map(|(nr, &(format, _, _, _, component_map))|
			Unrecoverable!(ImageView2D::make_from(&backbuffers.dim2vec()[nr], format, component_map, ImageSubresourceRange::base_color()))).collect_vec();
		let image_views_3d = images_3d.iter().enumerate().map(|(nr, &(format, _, _, _, component_map))|
			Unrecoverable!(ImageView3D::make_from(&backbuffers.dim3vec()[nr], format, component_map, ImageSubresourceRange::base_color()))).collect_vec();

		let sampler_objects = samplers.iter().map(|dcs|
		{
			let sampler_state = SamplerState::new().filters(dcs.mag_filter, dcs.min_filter);
			Unrecoverable!(Sampler::new(engine, &sampler_state))
		}).collect_vec();

		DevConfImages
		{
			image_views_1d: image_views_1d, image_views_2d: image_views_2d, image_views_3d: image_views_3d,
			samplers: sampler_objects,
			device_images: backbuffers, staging_images: staging_images
		}
	}
	pub fn ensure_has_staging(self) -> DevConfImagesWithStaging
	{
		DevConfImagesWithStaging
		{
			image_views_1d: self.image_views_1d, image_views_2d: self.image_views_2d, image_views_3d: self.image_views_3d, samplers: self.samplers,
			device_images: self.device_images, staging_images: self.staging_images.unwrap()
		}
	}

	pub fn images_1d(&self) -> &Vec<interlude::ImageView1D> { &self.image_views_1d }
	pub fn images_2d(&self) -> &Vec<interlude::ImageView2D> { &self.image_views_2d }
	pub fn images_3d(&self) -> &Vec<interlude::ImageView3D> { &self.image_views_3d }
	pub fn samplers(&self) -> &Vec<interlude::Sampler> { &self.samplers }
}
impl DevConfImagesWithStaging
{
	pub fn images_1d(&self) -> &Vec<interlude::ImageView1D> { &self.image_views_1d }
	pub fn images_2d(&self) -> &Vec<interlude::ImageView2D> { &self.image_views_2d }
	pub fn images_3d(&self) -> &Vec<interlude::ImageView3D> { &self.image_views_3d }
	pub fn samplers(&self) -> &Vec<interlude::Sampler> { &self.samplers }

	pub fn staging_images(&self) -> &Vec<interlude::LinearImage2D> { self.staging_images.dim2vec() }
	pub fn map_staging_images_memory(&self) -> interlude::MemoryMappedRange { Unrecoverable!(self.staging_images.map()) }
	pub fn staging_offsets(&self) -> &Vec<VkDeviceSize> { self.staging_images.image2d_offsets() }
}
