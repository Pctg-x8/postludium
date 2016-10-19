// Label Block Arranger

use super::resolver::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct ArrangedCommands
{
	rendering: Option<LBRenderingCommands>, transfering: Option<LBSubpass>
}
#[derive(Debug)]
pub enum LBRenderTargetRef { Swapchain, Custom(u32) }
#[derive(Debug)]
pub struct LBRenderingCommands { render_passes: Vec<LBRenderPass> }
#[derive(Debug)]
pub struct LBRenderPass { render_target_ref: LBRenderTargetRef, prepass: Option<LBSubpass>, postpass: Option<LBSubpass>, subpasses: Vec<Option<LBSubpass>> }
#[derive(Debug)]
pub struct LBSubpass { commands: ResolvedCommands }

pub fn arrange_label_blocks<'a>(resolved: Resolved<'a>) -> ArrangedCommands
{
	let rendering_commands = if resolved.render_blocks.is_empty() { None }
	else
	{
		let mut passes = Vec::new();
		let mut resolved_labels = resolved.render_blocks;
		while !resolved_labels.is_empty()
		{
			let first_rendered_fb =
			{
				let (_, first_pass) = resolved_labels.iter().next().unwrap();
				first_pass.rendered_fb
			};
			let (g_first_pass, g_rest_pass): (HashMap<_, _>, HashMap<_, _>)
				= resolved_labels.into_iter().partition(|&(_, ref p)| p.rendered_fb.eq_framebuffer_ref(&first_rendered_fb));
			
			let render_target_ref = match first_rendered_fb
			{
				ResolvedLabelRenderedFB::Swapchain(_) => LBRenderTargetRef::Swapchain,
				ResolvedLabelRenderedFB::Custom(fbr, _) => LBRenderTargetRef::Custom(fbr)
			};
			let (mut pre, mut post, mut subpasses) = (None, None, Vec::new());
			for (_, blk) in g_first_pass
			{
				match &blk.rendered_fb
				{
					&ResolvedLabelRenderedFB::Swapchain(ref s) | &ResolvedLabelRenderedFB::Custom(_, ref s) => match s
					{
						&ResolvedSubpassIndex::Pre => pre = Some(LBSubpass { commands: blk.commands }),
						&ResolvedSubpassIndex::Post => post = Some(LBSubpass { commands: blk.commands }),
						&ResolvedSubpassIndex::Pass(s) => if subpasses.len() > s as usize
						{
							subpasses[s as usize] = Some(LBSubpass { commands: blk.commands });
						}
						else
						{
							while subpasses.len() < s as usize { subpasses.push(None); }
							subpasses.push(Some(LBSubpass { commands: blk.commands }));
						}
					}
				}
			}
			passes.push(LBRenderPass { render_target_ref: render_target_ref, prepass: pre, postpass: post, subpasses: subpasses });

			resolved_labels = g_rest_pass;
		}
		Some(LBRenderingCommands { render_passes: passes })
	};
	let transfer_commands = if resolved.transfer_blocks.is_empty() { None }
	else
	{
		Some(LBSubpass { commands: resolved.transfer_blocks.into_iter().flat_map(|(_, tfcs)| match tfcs.cmdlevel
		{
			ResolvedLabelCommandLevel::Primary => tfcs.commands,
			ResolvedLabelCommandLevel::Secondary => panic!("FIXME: Seconary Command Generation for Transfer Commands")
		}).collect::<ResolvedCommands>() })
	};

	ArrangedCommands { rendering: rendering_commands, transfering: transfer_commands }
}
