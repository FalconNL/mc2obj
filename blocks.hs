([(0,0), (1,0), (1,1), (0,1) --block: 1-4
 ,(1,1/2),(0,1/2) --slab: 5-6
 ,( 7/16, 8/16),( 9/16, 8/16),( 9/16,10/16),( 7/16,10/16) --torch: 7-10
 ,(0,0),(0,0),(0,0),(0,0)
 ,( 6/16,    0),(10/16,    0),(10/16,    1),( 6/16,    1) --fence: 15-18
 ,( 6/16, 6/16),(10/16, 6/16),(10/16,10/16),( 6/16,10/16) --fence top: 19-22
 ,(-6/16, 1/16),( 6/16, 1/16),( 6/16, 4/16),(-6/16, 4/16) --fence_topbeam_side: 23-26
 ,(-6/16, 7/16),( 6/16, 7/16),( 6/16,10/16),(-6/16,10/16) --fence_bottombeam_side: 27-30
 ,( 7/16, 2/16),( 9/16, 2/16),( 9/16,14/16),( 7/16,14/16) --fence_beam_top: 31-34
 ,(    1,12/16),(    0,12/26) --piston: 35-36
 ,(1/2,1),(1/2,1/2) --stairs: 37-38
 ,(1,2/16),(0,2/16) --snow: 39-40
 ]
,[(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1) --block: 1-4
 ,(-sqrt 2 / 2,-sqrt 2 / 2,0),(sqrt 2 / 2,sqrt 2 / 2,0),(sqrt 2 / 2,-sqrt 2 / 2,0),(-sqrt 2 / 2,sqrt 2 / 2,0) --plant: 5-8
 ,(0,-sqrt 2 / 2,sqrt 2 / 2),(0,sqrt 2 / 2,sqrt 2 / 2),(-sqrt 2 / 2,0,sqrt 2 / 2),(sqrt 2 / 2,0,sqrt 2 / 2) --incline rails: 9-12
 ]
,[( 0, \_ _  -> [])
 ,( 1, \_ ns -> block ns "Stone")
 ,( 2, \_ ns@(_,_,_,_,_,t,_) -> case t of 78 -> top ns "Snow" ++ bottom ns "Dirt" ++ sides ns "Dirt_Snow"
                                          _  -> top ns "Grass_Top" ++ bottom ns "Dirt" ++ sides ns "Dirt_Grass")
 ,( 3, \_ ns -> block ns "Dirt")
 ,( 4, \_ ns -> block ns "Cobblestone")
 ,( 5, \_ ns -> block ns "Wood")
 ,( 6, \d ns -> plant $ case d of 1 -> "Sapling_Pine"
                                  2 -> "Sapling_Birch"
                                  _ -> "Sapling_Oak")
 ,( 7, \_ ns -> block ns "Bedrock")
 ,( 8, \_ ns -> block ns "Water")
 ,( 9, \_ ns -> block ns "Unknown")
 ,(10, \_ ns -> block ns "Lava")
 ,(11, \_ ns -> block ns "Unknown")
 ,(12, \_ ns -> block ns "Sand")
 ,(13, \_ ns -> block ns "Gravel")
 ,(14, \_ ns -> block ns "Ore_Gold")
 ,(15, \_ ns -> block ns "Ore_Iron")
 ,(16, \_ ns -> block ns "Ore_Coal")
 ,(17, \d ns -> top ns "Log_Top" ++ bottom ns "Log_Top" ++ sides ns (case d of 1 -> "Log_Pine"
                                                                               2 -> "Log_Birch"
                                                                               _ -> "Log_Oak"))
 ,(18, \d ns -> leaf ns (case mod d 4 of 1 -> "Leaves_Pine"
                                         _ -> "Leaves_Oak"))
 ,(19, \_ ns -> block ns "Sponge")
 ,(20, \_ ns -> block ns "Glass")
 ,(21, \_ ns -> block ns "Ore_Lapis")
 ,(22, \_ ns -> block ns "Lapis")
 ,(23, \d _  -> furnace "Dispenser_Front" "Furnace_Side" "Furnace_Top" d)
 ,(25, \_ ns -> block ns "Jukebox_Side")
 ,(26, \d ns -> case div d 8 of
    0 -> combine $ rotateZ (case mod d 4 of 0 -> 3; 1 -> 0; 2 -> 1; 3 -> 2) (3,1,4,2,5,6)
                   ([],inset_east "Bed_Side_Foot",inset_south "Bed_Foot",flipH (inset_west "Bed_Side_Foot"),bed 1 False,[])
    _ -> combine $ rotateZ (case mod d 4 of 0 -> 3; 1 -> 0; 2 -> 1; 3 -> 2)
                   (3,1,4,2,5,6) (inset_north "Bed_Head",inset_east "Bed_Side_Head",[],flipH (inset_west "Bed_Side_Head"),bed 1 True,[]))
 ,(27, \d _  -> track_straight d $ if div d 8 == 1 then "Rails_Powered_On" else "Rails_Powered_Off")
 ,(28, \d _  -> track_straight d "Rails_Detector")
 ,(29, \_ ns -> block ns "Unknown")
 ,(30, \_ ns -> plant "Cobweb")
 ,(31, \d ns -> plant $ case d of 1 -> "Tall_Grass"
                                  2 -> "Fern"
                                  _ -> "Dead_Shrub")
 ,(32, \_ ns -> plant "Dead_Shrub")
 ,(33, \_ ns -> block ns "Unknown")
 ,(34, \_ ns -> block ns "Unknown")
 ,(35, \d ns -> block ns $ "Wool_" ++ (case d of 0  -> "White"
                                                 1  -> "Orange"
                                                 2  -> "Magenta"
                                                 3  -> "Light_Blue"
                                                 4  -> "Yellow"
                                                 5  -> "Lime"
                                                 6  -> "Pink"
                                                 7  -> "Gray"
                                                 8  -> "Light_Gray"
                                                 9  -> "Cyan"
                                                 10 -> "Purple"
                                                 11 -> "Blue"
                                                 12 -> "Brown"
                                                 13 -> "Green"
                                                 14 -> "Red"
                                                 15 -> "Black"))
 ,(36, \_ _  -> [])
 ,(37, \_ ns -> plant "Flower_Yellow")
 ,(38, \_ ns -> plant "Flower_Red")
 ,(39, \_ ns -> plant "Mushroom_Brown")
 ,(40, \_ ns -> plant "Mushroom_Red")
 ,(41, \_ ns -> block ns "Gold")
 ,(42, \_ ns -> block ns "Iron")
 ,(43, \d ns -> case d of 0 -> top ns "Slab_Top"      ++ bottom ns "Slab_Top"         ++ sides ns "Slab_Side"
                          1 -> top ns "Sandstone_Top" ++ bottom ns "Sandstone_Bottom" ++ sides ns "Sandstone_Side"
                          2 -> block ns "Wood"
                          3 -> block ns "Cobblestone")
 ,(44, \d ns -> slab_top      (case d of 0 -> "Slab_Top";  1 -> "Sandstone_Top";    2 -> "Wood"; 3 -> "Cobblestone") ++
                bottom ns     (case d of 0 -> "Slab_Top";  1 -> "Sandstone_Bottom"; 2 -> "Wood"; 3 -> "Cobblestone") ++
                slab_sides ns (case d of 0 -> "Slab_Side"; 1 -> "Sandstone_Side";   2 -> "Wood"; 3 -> "Cobblestone"))
 ,(45, \_ ns -> block ns "Moss_Stone")
 ,(46, \_ ns -> top ns "TNT_Top" ++ bottom ns "TNT_Bottom" ++ sides ns "TNT_Side")
 ,(47, \_ ns -> top ns "Wood" ++ bottom ns "Wood" ++ sides ns "Bookshelf")
 ,(48, \_ ns -> block ns "Moss_Stone")
 ,(49, \_ ns -> block ns "Obsidian")
 ,(50, \d _  -> torch d "Torch")
 ,(51, \_ ns -> sides ns "Fire")
 ,(52, \_ ns -> block ns "Spawner")
 ,(53, \d _  -> stairs "Wood" d)
 ,(54, \_ ns -> block ns "Unknown")
 ,(55, \_ ns -> block ns "Unknown")
 ,(56, \_ ns -> block ns "Ore_Diamond")
 ,(57, \_ ns -> block ns "Diamond")
 ,(58, \_ ns -> top ns "Workbench_Top" ++ bottom ns "Wood" ++ north ns "Workbench_Front" ++
                south ns "Workbench_Front" ++ east ns "Workbench_Side" ++ west ns "Workbench_Side")
 ,(59, \d ns -> (inset_sides (1/4) $ "Crops_" ++ show d) ++ (inset_sides (3/4) $ "Crops_" ++ show d))
 ,(60, \d ns -> top ns (if d == 0 then "Farmland_Dry" else "Farmland_Wet") ++ bottom ns "Dirt" ++ sides ns "Dirt")
 ,(61, \d _  -> furnace "Furnace_Front" "Furnace_Side" "Furnace_Top" d)
 ,(62, \d _  -> furnace "Furnace_Lit" "Furnace_Side" "Furnace_Top" d)
 ,(63, \_ ns -> block ns "Unknown")
 ,(64, \_ ns -> block ns "Unknown")
 ,(65, \d _  -> combine $ rotateZ (case d of 2 -> 3; 3 -> 1; 4 -> 2; _ -> 0) (3,1,4,2,5,6)
                                  ([],[],moveY (1 - epsilon) $ inset_south "Ladder",[],[],[]))
 ,(66, \_ ns -> block ns "Unknown")
 ,(67, \d _  -> stairs "Cobblestone" d)
 ,(68, \_ ns -> block ns "Unknown")
 ,(69, \_ ns -> block ns "Unknown")
 ,(70, \_ ns -> block ns "Unknown")
 ,(71, \_ ns -> block ns "Unknown")
 ,(72, \_ ns -> block ns "Unknown")
 ,(73, \_ ns -> block ns "Ore_Redstone")
 ,(74, \_ ns -> block ns "Ore_Redstone")
 ,(75, \d _  -> torch d "Redstone_Torch_Off")
 ,(76, \d _  -> torch d "Redstone_Torch_On")
 ,(77, \_ ns -> block ns "Unknown")
 ,(78, \_ _  -> snow)
 ,(79, \_ ns -> block ns "Ice")
 ,(80, \_ ns -> block ns "Snow")
 ,(81, \_ ns -> inset_sides (1/16) "Cactus_Side" ++ top ns "Cactus_Top" ++ bottom ns "Cactus_Bottom")
 ,(82, \_ ns -> block ns "Clay")
 ,(83, \_ ns -> plant "Sugar_Cane")
 ,(84, \_ ns -> top ns "Jukebox_Top" ++ bottom ns "Jukebox_Side" ++ sides ns "Jukebox_Side")
 ,(85, \_ ns -> fence ns)
 ,(86, \d _  -> furnace "Pumpkin_Front" "Pumpkin_Side" "Pumpkin_Top"
                        (case d of 0 -> 3; 1 -> 4; 2 -> 2; _ -> 5))
 ,(87, \_ ns -> block ns "Netherrack")
 ,(88, \_ ns -> block ns "Soul_Sand")
 ,(89, \_ ns -> block ns "Glowstone")
 ,(90, \_ ns -> block ns "Unknown")
 ,(91, \d _  -> furnace "Pumpkin_Lit" "Pumpkin_Side" "Pumpkin_Top"
                        (case d of 0 -> 3; 1 -> 4; 2 -> 2; _ -> 5))
 ,(92, \_ ns -> block ns "Unknown")
 ,(93, \_ ns -> block ns "Unknown")
 ,(94, \_ ns -> block ns "Unknown")
 ,(95, \_ ns -> block ns "Unknown")
 ,(96, \_ ns -> block ns "Unknown")
 ]
)