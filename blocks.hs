[( 0, \_ _  -> [])
,( 1, \_    -> block "Stone")
,( 2, \_ ns@(_,_,_,_,_,t,_) -> case t of 78 -> blockSTB "Dirt_Snow" "Snow" "Dirt" ns 
                                         _  -> blockSTB "Dirt_Grass" "Grass_Top" "Dirt" ns)
,( 3, \_    -> block "Dirt")
,( 4, \_    -> block "Cobblestone")
,( 5, \_    -> block "Wood")
,( 6, \d _  -> plant $ case d of 1 -> "Sapling_Pine"
                                 2 -> "Sapling_Birch"
                                 _ -> "Sapling_Oak")
,( 7, \_    -> block "Bedrock")
,( 8, \_    -> block "Water")
,( 9, \_    -> block "Unknown")
,(10, \_    -> block "Lava")
,(11, \_    -> block "Unknown")
,(12, \_    -> block "Sand")
,(13, \_    -> block "Gravel")
,(14, \_    -> block "Ore_Gold")
,(15, \_    -> block "Ore_Iron")
,(16, \_    -> block "Ore_Coal")
,(17, \d    -> blockSTB (case d of 1 -> "Log_Pine"
                                   2 -> "Log_Birch"
                                   _ -> "Log_Oak") "Log_Top" "Log_Top")
,(18, \d    -> leaf (case mod d 4 of 1 -> "Leaves_Pine"
                                     _ -> "Leaves_Oak"))
,(19, \_    -> block "Sponge")
,(20, \_    -> block "Glass")
,(21, \_    -> block "Ore_Lapis")
,(22, \_    -> block "Lapis")
,(23, \d    -> blockFST "Dispenser_Front" "Furnace_Side" "Furnace_Top" (case d of 2 -> 3; 3 -> 1; 4 -> 2; _ -> 0))
,(25, \_    -> block "Jukebox_Side")
,(26, \d    -> bed (div d 8 == 1) (case mod d 4 of 0 -> 1; 1 -> 2; 2 -> 3; _ -> 0))
,(27, \d _  -> trackStraight d $ if div d 8 == 1 then "Rails_Powered_On" else "Rails_Powered_Off")
,(28, \d _  -> trackStraight d "Rails_Detector")
,(29, \_    -> block "Unknown")
,(30, \_ _  -> plant "Cobweb")
,(31, \d _  -> plant $ case d of 1 -> "Tall_Grass"
                                 2 -> "Fern"
                                 _ -> "Dead_Shrub")
,(32, \_ _  -> plant "Dead_Shrub")
,(33, \_    -> block "Unknown")
,(34, \_    -> block "Unknown")
,(35, \d    -> block $ "Wool_" ++ (case d of 1  -> "Orange"
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
                                             15 -> "Black"
                                             _  -> "White"))
,(36, \_ _  -> [])
,(37, \_ _  -> plant "Flower_Yellow")
,(38, \_ _  -> plant "Flower_Red")
,(39, \_ _  -> plant "Mushroom_Brown")
,(40, \_ _  -> plant "Mushroom_Red")
,(41, \_    -> block "Gold")
,(42, \_    -> block "Iron")
,(43, \d    -> case d of 1 -> blockSTB "Sandstone_Side" "Sandstone_Top" "Sandstone_Bottom"
                         2 -> block "Wood"
                         3 -> block "Cobblestone"
                         _ -> blockSTB "Slab_Side" "Slab_Top" "Slab_Top")
,(44, \d    -> case d of 1 -> slab "Sandstone_Side" "Sandstone_Top" "Sandstone_Bottom"
                         2 -> slab "Wood" "Wood" "Wood"
                         3 -> slab "Cobblestone" "Cobblestone" "Cobblestone"
                         _ -> slab "Slab_Side" "Slab_Top" "Slab_Top")
,(45, \_    -> block "Moss_Stone")
,(46, \_    -> blockSTB "TNT_Side" "TNT_Top" "TNT_Bottom")
,(47, \_    -> blockSTB "Bookshelf" "Wood" "Wood")
,(48, \_    -> block "Moss_Stone")
,(49, \_    -> block "Obsidian")
,(50, \d _  -> torch d "Torch")
,(51, \_ _  -> sides "Fire" 0 ++ sides "Fire" 1)
,(52, \_    -> block "Spawner")
,(53, \d    -> stairs "Wood" (case d of 0 -> 1; 1 -> 3; 2 -> 2; _ -> 0))
,(54, \_    -> block "Unknown")
,(55, \_    -> block "Unknown")
,(56, \_    -> block "Ore_Diamond")
,(57, \_    -> block "Diamond")
,(58, \_    -> workbench "Workbench_Front" "Workbench_Side" "Workbench_Top" "Wood")
,(59, \d _  -> sides ("Crops_" ++ show d) (1/4) ++ sides ("Crops_" ++ show d) (3/4))
,(60, \d    -> blockSTB "Dirt" (if d == 0 then "Farmland_Dry" else "Farmland_Wet") "Dirt")
,(61, \d    -> blockFST "Furnace_Front" "Furnace_Side" "Furnace_Top" (case d of 2 -> 3; 3 -> 1; 4 -> 2; _ -> 0))
,(62, \d    -> blockFST "Furnace_Lit" "Furnace_Side" "Furnace_Top" (case d of 2 -> 3; 3 -> 1; 4 -> 2; _ -> 0))
,(63, \_    -> block "Unknown")
,(64, \_    -> block "Unknown")
,(65, \d _  -> map (second . rotateZ $ case d of 2 -> 1; 3 -> 3; 4 -> 0; _ -> 2) $
                   faceNorth (0,0) (1,1) (1 - epsilon) "Ladder")
,(66, \_    -> block "Unknown")
,(67, \d    -> stairs "Cobblestone" (case d of 0 -> 1; 1 -> 3; 2 -> 2; _ -> 0))
,(68, \_    -> block "Unknown")
,(69, \_    -> block "Unknown")
,(70, \_    -> block "Unknown")
,(71, \_    -> block "Unknown")
,(72, \_    -> block "Unknown")
,(73, \_    -> block "Ore_Redstone")
,(74, \_    -> block "Ore_Redstone")
,(75, \d _  -> torch d "Redstone_Torch_Off")
,(76, \d _  -> torch d "Redstone_Torch_On")
,(77, \_    -> block "Unknown")
,(78, \_    -> snow "Snow")
,(79, \_    -> block "Ice")
,(80, \_    -> block "Snow")
,(81, \_ ns -> sides "Cactus_Side" (1/16) ++ occlude True ns
                   (faceTop (0,0) (1,1) 0 "Cactus_Top" ++
                    faceBottom (0,0) (1,1) 0 "Cactus_Bottom"))
,(82, \_    -> block "Clay")
,(83, \_ _  -> plant "Sugar_Cane")
,(84, \_    -> blockSTB "Jukebox_Side" "Jukebox_Top" "Jukebox_Side")
,(85, \_    -> fence)
,(86, \d    -> blockFST "Pumpkin_Front" "Pumpkin_Side" "Pumpkin_Top" (case d of 0 -> 1; 1 -> 2; 2 -> 3; _ -> 0))
,(87, \_    -> block "Netherrack")
,(88, \_    -> block "Soul_Sand")
,(89, \_    -> block "Glowstone")
,(90, \_    -> block "Unknown")
,(91, \d    -> blockFST "Pumpkin_Lit" "Pumpkin_Side" "Pumpkin_Top" (case d of 0 -> 1; 1 -> 2; 2 -> 3; _ -> 0))
,(92, \_    -> block "Unknown")
,(93, \_    -> block "Unknown")
,(94, \_    -> block "Unknown")
,(95, \_    -> block "Unknown")
,(96, \_    -> block "Unknown")
]