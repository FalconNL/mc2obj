module BlockUtil where

import qualified Data.IntSet as IS

north  (c,n,_,_,_,_,_) m = if hide c n then [] else [(m, [(( 0, 0,0), 1, 3), ((-1, 0,0), 2, 3), ((-1, 0,1), 3, 3), (( 0, 0,1), 4, 3)])]
east   (c,_,e,_,_,_,_) m = if hide c e then [] else [(m, [(( 0,-1,0), 1, 1), (( 0, 0,0), 2, 1), (( 0, 0,1), 3, 1), (( 0,-1,1), 4, 1)])]
south  (c,_,_,s,_,_,_) m = if hide c s then [] else [(m, [((-1,-1,0), 1, 4), (( 0,-1,0), 2, 4), (( 0,-1,1), 3, 4), ((-1,-1,1), 4, 4)])]
west   (c,_,_,_,w,_,_) m = if hide c w then [] else [(m, [((-1, 0,0), 1, 2), ((-1,-1,0), 2, 2), ((-1,-1,1), 3, 2), ((-1, 0,1), 4, 2)])]
top    (c,_,_,_,_,t,_) m = if hide c t then [] else [(m, [((-1,-1,1), 1, 5), (( 0,-1,1), 2, 5), (( 0, 0,1), 3, 5), ((-1, 0,1), 4, 5)])]
bottom (c,_,_,_,_,_,b) m = if hide c b then [] else [(m, [((-1, 0,0), 1, 6), (( 0, 0,0), 2, 6), (( 0,-1,0), 3, 6), ((-1,-1,0), 4, 6)])]

plant1 m = (m, [])

flat m = [(m, [((-1,-1,0.001), 1, 5), ((0,-1,0.001), 2, 5), ((0,0,0.001), 3, 5), ((-1,0,0.001), 4, 5)])]

slab_north (c,n,_,_,_,_,_) m = if hide c n then [] else [(m, [(( 0, 0,0  ), 1, 3), ((-1, 0,0  ), 2, 3), ((-1, 0,0.5), 5, 3), (( 0, 0,0.5), 6, 3)])]
slab_east  (c,_,e,_,_,_,_) m = if hide c e then [] else [(m, [(( 0,-1,0  ), 1, 1), (( 0, 0,0  ), 2, 1), (( 0, 0,0.5), 5, 1), (( 0,-1,0.5), 6, 1)])]
slab_south (c,_,_,s,_,_,_) m = if hide c s then [] else [(m, [((-1,-1,0  ), 1, 4), (( 0,-1,0  ), 2, 4), (( 0,-1,0.5), 5, 4), ((-1,-1,0.5), 6, 4)])]
slab_west  (c,_,_,_,w,_,_) m = if hide c w then [] else [(m, [((-1, 0,0  ), 1, 2), ((-1,-1,0  ), 2, 2), ((-1,-1,0.5), 5, 2), ((-1, 0,0.5), 6, 2)])]
slab_top m = [(m, [((-1,-1,0.5), 1, 5), (( 0,-1,0.5), 2, 5), (( 0, 0,0.5), 3, 5), ((-1, 0,0.5), 4, 5)])]

sides ns m      = ($ m) =<< [north ns, east ns, south ns, west ns]
slab_sides ns m = ($ m) =<< [slab_north ns, slab_east ns, slab_south ns, slab_west ns]
block ns m      = ($ m) =<< [north ns, east ns, south ns, west ns, top ns, bottom ns]

hide c t = c == t || IS.member (fromIntegral t) solidIDs

solidIDs = IS.fromAscList $ [1..5] ++ [7] ++ [11..17] ++ [19] ++ [21..25] ++
                            [29,33,35,36] ++ [41..43] ++ [45..49] ++ [54,56,57,58] ++
                            [60..62] ++ [73,74,78,79,80,82,84] ++ [86..89] ++ [91,95]