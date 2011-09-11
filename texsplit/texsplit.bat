@for /f %%x in ('identify -format "%%[fx:w/16]" terrain.png') do @set w=%%x
@set /a o0 = 0
@set /a o1 = %w * 1
@set /a o2 = %w * 2
@set /a o3 = %w * 3
@set /a o4 = %w * 4
@set /a o5 = %w * 5
@set /a o6 = %w * 6
@set /a o7 = %w * 7
@set /a o8 = %w * 8
@set /a o9 = %w * 9
@set /a oa = %w * 10
@set /a ob = %w * 11
@set /a oc = %w * 12
@set /a od = %w * 13
@set /a oe = %w * 14
@set /a of = %w * 15

@echo Identified texture pack as %w%x%w%

@echo Converting row 1...

@convert terrain.png -crop %w%x%w%+%o0%+%o0% -fill hsl(27%%,90%%,40%%) -colorize 50,50,50 -brightness-contrast 0x+10 png24:tex/grass_top.png
@convert terrain.png -crop %w%x%w%+%o1%+%o0% png24:tex/stone.png
@convert terrain.png -crop %w%x%w%+%o2%+%o0% png24:tex/dirt.png
@convert terrain.png -crop %w%x%w%+%o3%+%o0% png24:tex/dirt_grass.png
@convert terrain.png -crop %w%x%w%+%o4%+%o0% png24:tex/wood.png
@convert terrain.png -crop %w%x%w%+%o5%+%o0% png24:tex/slab_side.png
@convert terrain.png -crop %w%x%w%+%o6%+%o0% png24:tex/slab_top.png
@convert terrain.png -crop %w%x%w%+%o7%+%o0% png24:tex/brick.png
@convert terrain.png -crop %w%x%w%+%o8%+%o0% png24:tex/tnt_side.png
@convert terrain.png -crop %w%x%w%+%o9%+%o0% png24:tex/tnt_top.png
@convert terrain.png -crop %w%x%w%+%oa%+%o0% png24:tex/tnt_bottom.png
@convert terrain.png -crop %w%x%w%+%ob%+%o0% png24:tex/cobweb.png
@convert terrain.png -crop %w%x%w%+%ob%+%o0% -channel A -negate -separate png32:tex/cobweb_alpha.png
@convert terrain.png -crop %w%x%w%+%oc%+%o0% png24:tex/flower_red.png
@convert terrain.png -crop %w%x%w%+%oc%+%o0% -channel A -negate -separate png32:tex/flower_red_alpha.png
@convert terrain.png -crop %w%x%w%+%od%+%o0% png24:tex/flower_yellow.png
@convert terrain.png -crop %w%x%w%+%od%+%o0% -channel A -negate -separate png32:tex/flower_yellow_alpha.png
@rem
@convert terrain.png -crop %w%x%w%+%of%+%o0% png24:tex/sapling_oak.png
@convert terrain.png -crop %w%x%w%+%of%+%o0% -channel A -negate -separate png32:tex/sapling_oak_alpha.png

@echo Converting row 2...

@convert terrain.png -crop %w%x%w%+%o0%+%o1% png24:tex/cobblestone.png
@convert terrain.png -crop %w%x%w%+%o1%+%o1% png24:tex/bedrock.png
@convert terrain.png -crop %w%x%w%+%o2%+%o1% png24:tex/sand.png
@convert terrain.png -crop %w%x%w%+%o3%+%o1% png24:tex/gravel.png
@convert terrain.png -crop %w%x%w%+%o4%+%o1% png24:tex/log_oak.png
@convert terrain.png -crop %w%x%w%+%o5%+%o1% png24:tex/log_top.png
@convert terrain.png -crop %w%x%w%+%o6%+%o1% png24:tex/iron.png
@convert terrain.png -crop %w%x%w%+%o7%+%o1% png24:tex/gold.png
@convert terrain.png -crop %w%x%w%+%o8%+%o1% png24:tex/diamond.png
@convert terrain.png -crop %w%x%w%+%o9%+%o1% png24:tex/chest_top.png
@convert terrain.png -crop %w%x%w%+%oa%+%o1% png24:tex/chest_side.png
@convert terrain.png -crop %w%x%w%+%ob%+%o1% png24:tex/chest_front.png
@convert terrain.png -crop %w%x%w%+%oc%+%o1% png24:tex/mushroom_red.png
@convert terrain.png -crop %w%x%w%+%oc%+%o1% -channel A -negate -separate png32:tex/mushroom_red_alpha.png
@convert terrain.png -crop %w%x%w%+%od%+%o1% png24:tex/mushroom_brown.png
@convert terrain.png -crop %w%x%w%+%od%+%o1% -channel A -negate -separate png32:tex/mushroom_brown_alpha.png
@rem
@rem

@echo Converting row 3...

@convert terrain.png -crop %w%x%w%+%o0%+%o2% png24:tex/ore_gold.png
@convert terrain.png -crop %w%x%w%+%o1%+%o2% png24:tex/ore_iron.png
@convert terrain.png -crop %w%x%w%+%o2%+%o2% png24:tex/ore_coal.png
@convert terrain.png -crop %w%x%w%+%o3%+%o2% png24:tex/bookshelf.png
@convert terrain.png -crop %w%x%w%+%o4%+%o2% png24:tex/moss_stone.png
@convert terrain.png -crop %w%x%w%+%o5%+%o2% png24:tex/obsidian.png
@rem
@convert terrain.png -crop %w%x%w%+%o7%+%o2% -fill hsl(27%%,90%%,20%%) -colorize 50,50,50 -brightness-contrast 0x+10 png24:tex/tall_grass.png
@convert terrain.png -crop %w%x%w%+%o7%+%o2% -channel A -negate -separate png32:tex/tall_grass_alpha.png
@rem
@convert terrain.png -crop %w%x%w%+%o9%+%o2% png24:tex/chest_left_front.png
@convert terrain.png -crop %w%x%w%+%oa%+%o2% png24:tex/chest_right_front.png
@convert terrain.png -crop %w%x%w%+%ob%+%o2% png24:tex/workbench_top.png
@convert terrain.png -crop %w%x%w%+%oc%+%o2% png24:tex/furnace_front.png
@convert terrain.png -crop %w%x%w%+%od%+%o2% png24:tex/furnace_side.png
@convert terrain.png -crop %w%x%w%+%oe%+%o2% png24:tex/dispenser_front.png
@rem

@echo Converting row 4...

@convert terrain.png -crop %w%x%w%+%o0%+%o3% png24:tex/sponge.png
@convert terrain.png -crop %w%x%w%+%o1%+%o3% png24:tex/glass.png
@convert terrain.png -crop %w%x%w%+%o1%+%o3% -channel A -negate -separate png32:tex/glass_alpha.png
@convert terrain.png -crop %w%x%w%+%o2%+%o3% png24:tex/ore_diamond.png
@convert terrain.png -crop %w%x%w%+%o3%+%o3% png24:tex/ore_redstone.png
@convert terrain.png -crop %w%x%w%+%o4%+%o3% -fill hsl(30%%,90%%,20%%) -colorize 70,70,70 -brightness-contrast 0x+10 png24:tex/leaves_oak.png
@convert terrain.png -crop %w%x%w%+%o4%+%o3% -channel A -negate -separate png32:tex/leaves_oak_alpha.png
@rem
@convert terrain.png -crop %w%x%w%+%o6%+%o3% png24:tex/stone_brick.png
@convert terrain.png -crop %w%x%w%+%o7%+%o3% png24:tex/dead_shrub.png
@convert terrain.png -crop %w%x%w%+%o7%+%o3% -channel A -negate -separate png32:tex/dead_shrub_alpha.png
@convert terrain.png -crop %w%x%w%+%o8%+%o3% png24:tex/fern.png
@convert terrain.png -crop %w%x%w%+%o8%+%o3% -channel A -negate -separate png32:tex/fern_alpha.png
@convert terrain.png -crop %w%x%w%+%o9%+%o3% png24:tex/chest_right_back.png
@convert terrain.png -crop %w%x%w%+%oa%+%o3% png24:tex/chest_left_back.png
@convert terrain.png -crop %w%x%w%+%ob%+%o3% png24:tex/workbench_side.png
@convert terrain.png -crop %w%x%w%+%oc%+%o3% png24:tex/workbench_front.png
@convert terrain.png -crop %w%x%w%+%od%+%o3% png24:tex/furnace_lit.png
@convert terrain.png -crop %w%x%w%+%oe%+%o3% png24:tex/furnace_top.png
@convert terrain.png -crop %w%x%w%+%of%+%o3% png24:tex/sapling_pine.png
@convert terrain.png -crop %w%x%w%+%of%+%o3% -channel A -negate -separate png32:tex/sapling_pine_alpha.png

@echo Converting row 5...

@convert terrain.png -crop %w%x%w%+%o0%+%o4% png24:tex/wool_white.png
@convert terrain.png -crop %w%x%w%+%o1%+%o4% png24:tex/spawner.png
@convert terrain.png -crop %w%x%w%+%o1%+%o4% -channel A -negate -separate png32:tex/spawner_alpha.png
@convert terrain.png -crop %w%x%w%+%o2%+%o4% png24:tex/snow.png
@convert terrain.png -crop %w%x%w%+%o3%+%o4% png24:tex/ice.png
@convert terrain.png -crop %w%x%w%+%o3%+%o4% -channel A -negate -separate png32:tex/ice_alpha.png
@convert terrain.png -crop %w%x%w%+%o4%+%o4% png24:tex/dirt_snow.png
@convert terrain.png -crop %w%x%w%+%o5%+%o4% png24:tex/cactus_top.png
@convert terrain.png -crop %w%x%w%+%o5%+%o4% -channel A -negate -separate png32:tex/cactus_top_alpha.png
@convert terrain.png -crop %w%x%w%+%o6%+%o4% png24:tex/cactus_side.png
@convert terrain.png -crop %w%x%w%+%o6%+%o4% -channel A -negate -separate png32:tex/cactus_side_alpha.png
@convert terrain.png -crop %w%x%w%+%o7%+%o4% png24:tex/cactus_bottom.png
@convert terrain.png -crop %w%x%w%+%o7%+%o4% -channel A -negate -separate png32:tex/cactus_bottom_alpha.png
@convert terrain.png -crop %w%x%w%+%o8%+%o4% png24:tex/clay.png
@convert terrain.png -crop %w%x%w%+%o9%+%o4% png24:tex/sugar_cane.png
@convert terrain.png -crop %w%x%w%+%o9%+%o4% -channel A -negate -separate png32:tex/sugar_cane_alpha.png
@convert terrain.png -crop %w%x%w%+%oa%+%o4% png24:tex/jukebox_side.png
@convert terrain.png -crop %w%x%w%+%ob%+%o4% png24:tex/jukebox_top.png
@rem
@rem
@rem
@convert terrain.png -crop %w%x%w%+%of%+%o4% png24:tex/sapling_birch.png
@convert terrain.png -crop %w%x%w%+%of%+%o4% -channel A -negate -separate png32:tex/sapling_birch_alpha.png

@echo Converting row 6...

@convert terrain.png -crop %w%x%w%+%o0%+%o5% png24:tex/torch.png
@convert terrain.png -crop %w%x%w%+%o0%+%o5% -channel A -negate -separate png32:tex/torch_alpha.png
@convert terrain.png -crop %w%x%w%+%o1%+%o5% png24:tex/door_wood_top.png
@convert terrain.png -crop %w%x%w%+%o1%+%o5% -channel A -negate -separate png32:tex/door_wood_top_alpha.png
@convert terrain.png -crop %w%x%w%+%o2%+%o5% png24:tex/door_iron_top.png
@convert terrain.png -crop %w%x%w%+%o2%+%o5% -channel A -negate -separate png32:tex/door_iron_top_alpha.png
@convert terrain.png -crop %w%x%w%+%o3%+%o5% png24:tex/ladder.png
@convert terrain.png -crop %w%x%w%+%o3%+%o5% -channel A -negate -separate png32:tex/ladder_alpha.png
@convert terrain.png -crop %w%x%w%+%o4%+%o5% png24:tex/hatch.png
@convert terrain.png -crop %w%x%w%+%o4%+%o5% -channel A -negate -separate png32:tex/hatch_alpha.png
@convert terrain.png -crop %w%x%w%+%o5%+%o5% png24:tex/iron_bars.png
@convert terrain.png -crop %w%x%w%+%o5%+%o5% -channel A -negate -separate png32:tex/iron_bars_alpha.png
@convert terrain.png -crop %w%x%w%+%o6%+%o5% png24:tex/farmland_wet.png
@convert terrain.png -crop %w%x%w%+%o7%+%o5% png24:tex/farmland_dry.png
@convert terrain.png -crop %w%x%w%+%o8%+%o5% png24:tex/crops_0.png
@convert terrain.png -crop %w%x%w%+%o8%+%o5% -channel A -negate -separate png32:tex/crops_0_alpha.png
@convert terrain.png -crop %w%x%w%+%o9%+%o5% png24:tex/crops_1.png
@convert terrain.png -crop %w%x%w%+%o9%+%o5% -channel A -negate -separate png32:tex/crops_1_alpha.png
@convert terrain.png -crop %w%x%w%+%oa%+%o5% png24:tex/crops_2.png
@convert terrain.png -crop %w%x%w%+%oa%+%o5% -channel A -negate -separate png32:tex/crops_2_alpha.png
@convert terrain.png -crop %w%x%w%+%ob%+%o5% png24:tex/crops_3.png
@convert terrain.png -crop %w%x%w%+%ob%+%o5% -channel A -negate -separate png32:tex/crops_3_alpha.png
@convert terrain.png -crop %w%x%w%+%oc%+%o5% png24:tex/crops_4.png
@convert terrain.png -crop %w%x%w%+%oc%+%o5% -channel A -negate -separate png32:tex/crops_4_alpha.png
@convert terrain.png -crop %w%x%w%+%od%+%o5% png24:tex/crops_5.png
@convert terrain.png -crop %w%x%w%+%od%+%o5% -channel A -negate -separate png32:tex/crops_5_alpha.png
@convert terrain.png -crop %w%x%w%+%oe%+%o5% png24:tex/crops_6.png
@convert terrain.png -crop %w%x%w%+%oe%+%o5% -channel A -negate -separate png32:tex/crops_6_alpha.png
@convert terrain.png -crop %w%x%w%+%of%+%o5% png24:tex/crops_7.png
@convert terrain.png -crop %w%x%w%+%of%+%o5% -channel A -negate -separate png32:tex/crops_7_alpha.png

@echo Converting row 7...

@convert terrain.png -crop %w%x%w%+%o0%+%o6% png24:tex/lever.png
@convert terrain.png -crop %w%x%w%+%o0%+%o6% -channel A -negate -separate png32:tex/lever_alpha.png
@convert terrain.png -crop %w%x%w%+%o1%+%o6% png24:tex/door_wood_bottom.png
@convert terrain.png -crop %w%x%w%+%o2%+%o6% png24:tex/door_iron_bottom.png
@convert terrain.png -crop %w%x%w%+%o3%+%o6% png24:tex/redstone_torch_on.png
@convert terrain.png -crop %w%x%w%+%o3%+%o6% -channel A -negate -separate png32:tex/redstone_torch_on_alpha.png
@convert terrain.png -crop %w%x%w%+%o4%+%o6% png24:tex/stone_brick_moss.png
@convert terrain.png -crop %w%x%w%+%o5%+%o6% png24:tex/stone_brick_cracked.png
@convert terrain.png -crop %w%x%w%+%o6%+%o6% png24:tex/pumpkin_top.png
@convert terrain.png -crop %w%x%w%+%o7%+%o6% png24:tex/netherrack.png
@convert terrain.png -crop %w%x%w%+%o8%+%o6% png24:tex/soul_sand.png
@convert terrain.png -crop %w%x%w%+%o9%+%o6% png24:tex/glowstone.png
@convert terrain.png -crop %w%x%w%+%oa%+%o6% png24:tex/piston_sticky.png
@convert terrain.png -crop %w%x%w%+%ob%+%o6% png24:tex/piston.png
@convert terrain.png -crop %w%x%w%+%oc%+%o6% png24:tex/piston_side.png
@convert terrain.png -crop %w%x%w%+%od%+%o6% png24:tex/piston_bottom.png
@convert terrain.png -crop %w%x%w%+%oe%+%o6% png24:tex/piston_top.png
@convert terrain.png -crop %w%x%w%+%of%+%o6% -fill hsl(34%%,51%%,35%%) -colorize 50,50,50 -brightness-contrast 0x+10 png24:tex/stalk_pumpkin.png
@convert terrain.png -crop %w%x%w%+%of%+%o6% -channel A -negate -separate png32:tex/stalk_pumpkin_alpha.png


@echo Converting row 8...

@convert terrain.png -crop %w%x%w%+%o0%+%o7% png24:tex/rails_bend.png
@convert terrain.png -crop %w%x%w%+%o0%+%o7% -channel A -negate -separate png32:tex/rails_bend_alpha.png
@convert terrain.png -crop %w%x%w%+%o1%+%o7% png24:tex/wool_black.png
@convert terrain.png -crop %w%x%w%+%o2%+%o7% png24:tex/wool_gray.png
@convert terrain.png -crop %w%x%w%+%o3%+%o7% png24:tex/redstone_torch_off.png
@convert terrain.png -crop %w%x%w%+%o3%+%o7% -channel A -negate -separate png32:tex/redstone_torch_off_alpha.png
@convert terrain.png -crop %w%x%w%+%o4%+%o7% png24:tex/log_pine.png
@convert terrain.png -crop %w%x%w%+%o5%+%o7% png24:tex/log_birch.png
@convert terrain.png -crop %w%x%w%+%o6%+%o7% png24:tex/pumpkin_side.png
@convert terrain.png -crop %w%x%w%+%o7%+%o7% png24:tex/pumpkin_front.png
@convert terrain.png -crop %w%x%w%+%o8%+%o7% png24:tex/pumpkin_lit.png
@convert terrain.png -crop %w%x%w%+%o9%+%o7% png24:tex/cake_top.png
@convert terrain.png -crop %w%x%w%+%o9%+%o7% -channel A -negate -separate png32:tex/cake_top_alpha.png
@convert terrain.png -crop %w%x%w%+%oa%+%o7% png24:tex/cake_side.png
@convert terrain.png -crop %w%x%w%+%oa%+%o7% -channel A -negate -separate png32:tex/cake_side_alpha.png
@convert terrain.png -crop %w%x%w%+%ob%+%o7% png24:tex/cake_inside.png
@convert terrain.png -crop %w%x%w%+%ob%+%o7% -channel A -negate -separate png32:tex/cake_inside_alpha.png
@convert terrain.png -crop %w%x%w%+%oc%+%o7% png24:tex/cake_bottom.png
@convert terrain.png -crop %w%x%w%+%oc%+%o7% -channel A -negate -separate png32:tex/cake_bottom_alpha.png
@convert terrain.png -crop %w%x%w%+%od%+%o7% png24:tex/mushroom_cap_red.png
@convert terrain.png -crop %w%x%w%+%oe%+%o7% png24:tex/mushroom_cap_brown.png
@convert terrain.png -crop %w%x%w%+%of%+%o7% -fill hsl(34%%,51%%,35%%) -colorize 50,50,50 -brightness-contrast 0x+10 png24:tex/stalk_melon.png
@convert terrain.png -crop %w%x%w%+%of%+%o7% -channel A -negate -separate png32:tex/stalk_melon_alpha.png

@echo Converting row 9...

@convert terrain.png -crop %w%x%w%+%o0%+%o8% png24:tex/rails.png
@convert terrain.png -crop %w%x%w%+%o0%+%o8% -channel A -negate -separate png32:tex/rails_alpha.png
@convert terrain.png -crop %w%x%w%+%o1%+%o8% png24:tex/wool_red.png
@convert terrain.png -crop %w%x%w%+%o2%+%o8% png24:tex/wool_pink.png
@convert terrain.png -crop %w%x%w%+%o3%+%o8% png24:tex/repeater_off.png
@convert terrain.png -crop %w%x%w%+%o4%+%o8% -fill hsl(35%%,90%%,20%%) -colorize 70,70,70 -brightness-contrast 0x+10 png24:tex/leaves_pine.png
@convert terrain.png -crop %w%x%w%+%o4%+%o8% -channel A -negate -separate png32:tex/leaves_pine_alpha.png
@rem
@convert terrain.png -crop %w%x%w%+%o6%+%o8% png24:tex/bed_top_foot.png
@convert terrain.png -crop %w%x%w%+%o7%+%o8% png24:tex/bed_top_head.png
@convert terrain.png -crop %w%x%w%+%o8%+%o8% png24:tex/melon_side.png
@convert terrain.png -crop %w%x%w%+%o9%+%o8% png24:tex/melon_top.png
@rem
@rem
@rem
@convert terrain.png -crop %w%x%w%+%od%+%o8% png24:tex/mushroom_stalk_side.png
@convert terrain.png -crop %w%x%w%+%oe%+%o8% png24:tex/mushroom_stalk_top.png
@convert terrain.png -crop %w%x%w%+%of%+%o8% -fill hsl(34%%,51%%,35%%) -colorize 50,50,50 -brightness-contrast 0x+10 png24:tex/vines.png
@convert terrain.png -crop %w%x%w%+%of%+%o8% -channel A -negate -separate png32:tex/vines_alpha.png

@echo Converting row 10...

@convert terrain.png -crop %w%x%w%+%o0%+%o9% png24:tex/lapis.png
@convert terrain.png -crop %w%x%w%+%o1%+%o9% png24:tex/wool_green.png
@convert terrain.png -crop %w%x%w%+%o2%+%o9% png24:tex/wool_lime.png
@convert terrain.png -crop %w%x%w%+%o3%+%o9% png24:tex/repeater_on.png
@convert terrain.png -crop %w%x%w%+%o4%+%o9% png24:tex/glass_pane_top.png
@convert terrain.png -crop %w%x%w%+%o4%+%o9% -channel A -negate -separate png32:tex/glass_pane_top_alpha.png
@convert terrain.png -crop %w%x%w%+%o5%+%o9% png24:tex/bed_foot.png
@convert terrain.png -crop %w%x%w%+%o5%+%o9% -channel A -negate -separate png32:tex/bed_foot_alpha.png
@convert terrain.png -crop %w%x%w%+%o6%+%o9% png24:tex/bed_side_foot.png
@convert terrain.png -crop %w%x%w%+%o6%+%o9% -channel A -negate -separate png32:tex/bed_side_foot_alpha.png
@convert terrain.png -crop %w%x%w%+%o7%+%o9% png24:tex/bed_side_head.png
@convert terrain.png -crop %w%x%w%+%o7%+%o9% -channel A -negate -separate png32:tex/bed_side_head_alpha.png
@convert terrain.png -crop %w%x%w%+%o8%+%o9% png24:tex/bed_head.png
@convert terrain.png -crop %w%x%w%+%o8%+%o9% -channel A -negate -separate png32:tex/bed_head_alpha.png
@rem
@rem
@rem
@rem
@rem
@rem
@rem

@echo Converting row 11...

@convert terrain.png -crop %w%x%w%+%o0%+%oa% png24:tex/ore_lapis.png
@convert terrain.png -crop %w%x%w%+%o1%+%oa% png24:tex/wool_brown.png
@convert terrain.png -crop %w%x%w%+%o2%+%oa% png24:tex/wool_yellow.png
@convert terrain.png -crop %w%x%w%+%o3%+%oa% png24:tex/rails_powered_off.png
@convert terrain.png -crop %w%x%w%+%o3%+%oa% -channel A -negate -separate png32:tex/rails_powered_off_alpha.png
@convert terrain.png -crop %w%x%w%+%o4%+%oa% -fill hsl(0%%,90%%,30%%) -colorize 70,70,70 -brightness-contrast 0x+10 png24:tex/redstone_cross.png
@convert terrain.png -crop %w%x%w%+%o4%+%oa% -channel A -negate -separate png32:tex/redstone_cross_alpha.png
@convert terrain.png -crop %w%x%w%+%o5%+%oa% -fill hsl(0%%,90%%,30%%) -colorize 70,70,70 -brightness-contrast 0x+10 png24:tex/redstone.png
@convert terrain.png -crop %w%x%w%+%o5%+%oa% -channel A -negate -separate png32:tex/redstone_alpha.png
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem

@echo Converting row 12...

@convert terrain.png -crop %w%x%w%+%o0%+%ob% png24:tex/sandstone_top.png
@convert terrain.png -crop %w%x%w%+%o1%+%ob% png24:tex/wool_blue.png
@convert terrain.png -crop %w%x%w%+%o2%+%ob% png24:tex/wool_light_blue.png
@convert terrain.png -crop %w%x%w%+%o3%+%ob% png24:tex/rails_powered_on.png
@convert terrain.png -crop %w%x%w%+%o3%+%ob% -channel A -negate -separate png32:tex/rails_powered_on_alpha.png
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem

@echo Converting row 13...

@convert terrain.png -crop %w%x%w%+%o0%+%oc% png24:tex/sandstone_side.png
@convert terrain.png -crop %w%x%w%+%o1%+%oc% png24:tex/wool_purple.png
@convert terrain.png -crop %w%x%w%+%o2%+%oc% png24:tex/wool_magenta.png
@convert terrain.png -crop %w%x%w%+%o3%+%oc% png24:tex/rails_detector.png
@convert terrain.png -crop %w%x%w%+%o3%+%oc% -channel A -negate -separate png32:tex/rails_detector_alpha.png
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@convert terrain.png -crop %w%x%w%+%od%+%oc% png24:tex/water.png
@convert terrain.png -crop %w%x%w%+%od%+%oc% -channel A -negate -separate png32:tex/water_alpha.png
@rem
@rem

@echo Converting row 14...

@convert terrain.png -crop %w%x%w%+%o0%+%od% png24:tex/sandstone_bottom.png
@convert terrain.png -crop %w%x%w%+%o1%+%od% png24:tex/wool_cyan.png
@convert terrain.png -crop %w%x%w%+%o2%+%od% png24:tex/wool_orange.png
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@convert terrain.png -crop %w%x%w%+%od%+%od% png24:tex/unknown.png
@rem
@rem

@echo Converting row 15...

@rem
@convert terrain.png -crop %w%x%w%+%o1%+%oe% png24:tex/wool_light_gray.png
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@convert terrain.png -crop %w%x%w%+%od%+%oe% png24:tex/lava.png
@rem
@rem

@echo Converting row 16...

@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem
@rem

@convert fire.png -channel A -negate -separate png32:tex/fire_alpha.png
@convert portal.png -channel A -negate -separate png32:tex/portal_alpha.png
@copy fire.png tex
@copy portal.png tex
@copy sign.png tex

@echo Done
pause