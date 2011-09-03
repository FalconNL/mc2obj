module BlockUtil where

import qualified Data.IntSet as IS

type Vertex = (Double, Double, Double)
type Face = [(Vertex, Int, Int)]

north  (c,n,_,_,_,_,_) m = if hide c n then [] else inset_north  m
east   (c,_,e,_,_,_,_) m = if hide c e then [] else inset_east   m
south  (c,_,_,s,_,_,_) m = if hide c s then [] else inset_south  m
west   (c,_,_,_,w,_,_) m = if hide c w then [] else inset_west   m
top    (c,_,_,_,_,t,_) m = if hide c t then [] else inset_top    m
bottom (c,_,_,_,_,_,b) m = if hide c b then [] else inset_bottom m

inset_north  m = [(m, [(( 0, 0,0), 1, 3), ((-1, 0,0), 2, 3), ((-1, 0,1), 3, 3), (( 0, 0,1), 4, 3)])]
inset_east   m = [(m, [(( 0,-1,0), 1, 1), (( 0, 0,0), 2, 1), (( 0, 0,1), 3, 1), (( 0,-1,1), 4, 1)])]
inset_south  m = [(m, [((-1,-1,0), 1, 4), (( 0,-1,0), 2, 4), (( 0,-1,1), 3, 4), ((-1,-1,1), 4, 4)])]
inset_west   m = [(m, [((-1, 0,0), 1, 2), ((-1,-1,0), 2, 2), ((-1,-1,1), 3, 2), ((-1, 0,1), 4, 2)])]
inset_top    m = [(m, [((-1, 0,1), 1, 5), ((-1,-1,1), 2, 5), (( 0,-1,1), 3, 5), (( 0, 0,1), 4, 5)])]
inset_bottom m = [(m, [((-1, 0,0), 1, 6), (( 0, 0,0), 2, 6), (( 0,-1,0), 3, 6), ((-1,-1,0), 4, 6)])]

leaf_north  (_,n,_,_,_,_,_) m = if IS.member (fromIntegral n) solidIDs then [] else inset_north  m
leaf_east   (_,_,e,_,_,_,_) m = if IS.member (fromIntegral e) solidIDs then [] else inset_east   m
leaf_south  (_,_,_,s,_,_,_) m = if IS.member (fromIntegral s) solidIDs then [] else inset_south  m
leaf_west   (_,_,_,_,w,_,_) m = if IS.member (fromIntegral w) solidIDs then [] else inset_west   m
leaf_top    (_,_,_,_,_,t,_) m = if IS.member (fromIntegral t) solidIDs then [] else inset_top    m
leaf_bottom (_,_,_,_,_,_,b) m = if IS.member (fromIntegral b) solidIDs then [] else inset_bottom m
leaf ns m      = ($ m) =<< [leaf_north ns, leaf_east ns, leaf_south ns, leaf_west ns, leaf_top ns, leaf_bottom ns]

plant m = map ((,) m) [[((cx-hw,cy+hw,0), 1, 5), ((cx+hw,cy-hw,0), 2, 5), ((cx+hw,cy-hw,1), 3, 5), ((cx-hw,cy+hw,1), 4, 5)] --sw
                      ,[((cx+hw,cy-hw,0), 1, 6), ((cx-hw,cy+hw,0), 2, 6), ((cx-hw,cy+hw,1), 3, 6), ((cx+hw,cy-hw,1), 4, 6)] --ne
                      ,[((cx-hw,cy-hw,0), 1, 7), ((cx+hw,cy+hw,0), 2, 7), ((cx+hw,cy+hw,1), 3, 7), ((cx-hw,cy-hw,1), 4, 7)] --se
                      ,[((cx+hw,cy+hw,0), 1, 8), ((cx-hw,cy-hw,0), 2, 8), ((cx-hw,cy-hw,1), 3, 8), ((cx+hw,cy+hw,1), 4, 8)] --nw
                      ] where (cx,cy,hw) = (-1/2,-1/2,sqrt 2 / 4)

fence_beam rot ft = combine $ rotateZ rot (3,1,4,2,5,6)
  ([], [("Wood", [((- 7/16,- 6/16,12/16), ft 23, 1), ((- 7/16,  6/16,12/16), ft 24, 1), ((- 7/16,  6/16,15/16), ft 25, 1), ((- 7/16,- 6/16,15/16), ft 26, 1)])]
  ,[], [("Wood", [((- 9/16,  6/16,12/16), ft 23, 2), ((- 9/16,- 6/16,12/16), ft 24, 2), ((- 9/16,- 6/16,15/16), ft 25, 2), ((- 9/16,  6/16,15/16), ft 26, 2)])]
  ,    [("Wood", [((- 9/16,  6/16,15/16),    32, 5), ((- 9/16,- 6/16,15/16),    33, 5), ((- 7/16,- 6/16,15/16),    34, 5), ((- 7/16,  6/16,15/16),    31, 5)])]
  ,    [("Wood", [((- 9/16,  6/16,15/16),    32, 5), ((- 7/16,  6/16,15/16),    31, 5), ((- 7/16,- 6/16,15/16),    34, 5), ((- 9/16,- 6/16,15/16),    33, 5)])])

fence (_,n,e,_,_,_,_) =
    (if n == 85 then fence_beam 0 id ++ moveZ (-6/16) (fence_beam 0 (+ 4)) else []) ++
    (if e == 85 then fence_beam 1 id ++ moveZ (-6/16) (fence_beam 1 (+ 4)) else []) ++
    map ((,) "Wood") [[((- 6/16,- 6/16,0), 15, 3), ((-10/16,- 6/16,0), 16, 3), ((-10/16,- 6/16,1), 17, 3), ((- 6/16,- 6/16,1), 18, 3)]
                     ,[((- 6/16,-10/16,0), 15, 1), ((- 6/16,- 6/16,0), 16, 1), ((- 6/16,- 6/16,1), 17, 1), ((- 6/16,-10/16,1), 18, 1)]
                     ,[((-10/16,-10/16,0), 15, 4), ((- 6/16,-10/16,0), 16, 4), ((- 6/16,-10/16,1), 17, 4), ((-10/16,-10/16,1), 18, 4)]
                     ,[((-10/16,- 6/16,0), 15, 2), ((-10/16,-10/16,0), 16, 2), ((-10/16,-10/16,1), 17, 2), ((-10/16,- 6/16,1), 18, 2)]
                     ,[((-10/16,- 6/16,1), 19, 5), ((-10/16,-10/16,1), 20, 5), ((- 6/16,-10/16,1), 21, 5), ((- 6/16,- 6/16,1), 22, 5)]
                     ,[((-10/16,- 6/16,0), 19, 6), ((- 6/16,- 6/16,0), 20, 6), ((- 6/16,-10/16,0), 21, 6), ((-10/16,-10/16,0), 22, 6)]]

furnace mfront mside mtop d = combine $ rotateZ (case d of 2 -> 3; 3 -> 1; 4 -> 2; _ -> 0) (3,1,4,2,5,6)
                                                (inset_north mside, inset_east mside, inset_south mfront,
                                                 inset_west mside,  inset_top mtop,   inset_bottom mtop)

stair_east = [((0,-1,0), 1, 1), ((0,0,0), 2, 1), ((0,0,1), 3, 1), ((0,-1/2,1), 37, 1), ((0,-1/2,1/2), 38, 1), ((0,-1,1/2), 6, 1)]
stair_west = reverse $ map (\((_,y,z),t,_) -> ((-1,y,z),t,2)) stair_east
stair_south m = [(m, [((-1,-1,0), 1, 4), (( 0,-1,0), 2, 4), (( 0,-1,0.5), 5, 4), ((-1,-1,0.5), 6, 4)])]
stair_top m = [(m, [((-1,-1/2,1), 6, 5), (( 0,-1/2,1), 5, 5), (( 0,0,1), 3, 5), ((-1,0,1), 4, 5)])]
stairs m d = combine $ rotateZ (case d of 0 -> 2; 1 -> 0; 2 -> 3; _ -> 1) (3,1,4,2,5,6)
                               (inset_north m, [(m, stair_east)], stair_south m ++ moveY 0.5 (moveZ 0.5 $ stair_south m)
                               ,[(m, stair_west)], stair_top m ++ moveY (-1/2) (moveZ (-1/2) $ stair_top m), inset_bottom m)

snow :: [(String, Face)]
snow = map ((,) "Snow") [[(( 0, 0,   0), 1, 3), ((-1, 0,   0), 2, 3), ((-1, 0,2/16), 39, 3), (( 0, 0,2/16), 40, 3)]
                        ,[(( 0,-1,   0), 1, 1), (( 0, 0,   0), 2, 1), (( 0, 0,2/16), 39, 1), (( 0,-1,2/16), 40, 1)]
                        ,[((-1,-1,   0), 1, 4), (( 0,-1,   0), 2, 4), (( 0,-1,2/16), 39, 4), ((-1,-1,2/16), 40, 4)]
                        ,[((-1, 0,   0), 1, 2), ((-1,-1,   0), 2, 2), ((-1,-1,2/16), 39, 2), ((-1, 0,2/16), 40, 2)]
                        ,[((-1, 0,2/16), 1, 5), ((-1,-1,2/16), 2, 5), (( 0,-1,2/16),  3, 5), (( 0, 0,2/16),  4, 5)]]


piston d m = case mod d 8 of 0 -> []
                             1 -> []
                             2 -> []
                             3 -> []
                             4 -> []
                             _ -> []
    where h = if div d 8 == 1 then 12/16 else 1

epsilon = 0.001
flat = moveZ (epsilon - 1) . inset_top

modifyTexcoords, modifyNormals :: (Int -> Int) -> [(String, Face)] -> [(String, Face)]
modifyVertices :: (Vertex -> Vertex) -> [(String, Face)] -> [(String, Face)]
modifyTexcoords f = map (\(m,vs) -> (m, map (\(v,t,n) -> (v,f t,n)) vs))
modifyVertices  f = map (\(m,vs) -> (m, map (\(v,t,n) -> (f v,t,n)) vs))
modifyNormals   f = map (\(m,vs) -> (m, map (\(v,t,n) -> (v,t,f n)) vs))

flipH = modifyTexcoords (\t -> if even t then t - 1 else t + 1)
rotateFace i = modifyTexcoords (\t -> mod (t + i) 4 + 1)

moveX d = modifyVertices (\(x,y,z) -> (x+d,y,z))
moveY d = modifyVertices (\(x,y,z) -> (x,y+d,z))
moveZ d = modifyVertices (\(x,y,z) -> (x,y,z+d))

combine (n,e,s,w,t,b) = concat [n,e,s,w,t,b]

rotateZ 0     _                   fs            = fs
rotateZ times (nn,ne,ns,nw,nt,nb) (n,e,s,w,t,b) = rotateZ (times - 1) (nn,ne,ns,nw,nt,nb) (f w nn,f n ne,f e ns,f s nw,f t nt,f b nb) where
    f vside vn = modifyNormals (const vn) $
                 modifyVertices (\(x,y,z) -> (y,-1-x,z)) vside

bed i isHead = rotateFace i [(if isHead then "Bed_Top_Head" else "Bed_Top_Foot",
                               [((-1, 0,9/16), 1, 5), ((-1,-1,9/16), 2, 5), (( 0,-1,9/16), 3, 5), (( 0, 0,9/16), 4, 5)])]

incline_north m = [(m, [((-1, 0,1), 1,  9), ((-1,-1,0), 2,  9), (( 0,-1,0), 3,  9), (( 0, 0,1), 4,  9)])]
incline_south m = [(m, [((-1, 0,0), 1, 10), ((-1,-1,1), 2, 10), (( 0,-1,1), 3, 10), (( 0, 0,0), 4, 10)])]
incline_east  m = [(m, [((-1, 0,1), 2, 11), ((-1,-1,1), 3, 11), (( 0,-1,0), 4, 11), (( 0, 0,0), 1, 11)])]
incline_west  m = [(m, [((-1, 0,0), 2, 12), ((-1,-1,0), 3, 12), (( 0,-1,1), 4, 12), (( 0, 0,1), 1, 12)])]

track_straight d = case mod d 8 of 1 -> rotateFace 1 . flat
                                   2 -> incline_south
                                   3 -> incline_north
                                   4 -> incline_east
                                   5 -> incline_west
                                   _ -> flat

torch :: Int -> String -> [(String, Face)]
torch d m = if d == 5 then inset_sides (7/16) m ++ [(m, [((-9/16,-7/16,5/8), 7, 5), ((-9/16,-9/16,5/8), 8, 5), ((-7/16,-9/16,5/8), 9, 5), ((-7/16,-7/16,5/8), 10, 5)])]
                      else combine $ rotateZ (case d of 1 -> 0; 2 -> 2; 3 -> 1; _ -> 3) (0,0,0,0,0,0) ([],[],[],[],[],[]) --TODO!

slab_north (c,n,_,_,_,_,_) m = if hide c n then [] else [(m, [(( 0, 0,0), 1, 3), ((-1, 0,0), 2, 3), ((-1, 0,0.5), 5, 3), (( 0, 0,0.5), 6, 3)])]
slab_east  (c,_,e,_,_,_,_) m = if hide c e then [] else [(m, [(( 0,-1,0), 1, 1), (( 0, 0,0), 2, 1), (( 0, 0,0.5), 5, 1), (( 0,-1,0.5), 6, 1)])]
slab_south (c,_,_,s,_,_,_) m = if hide c s then [] else [(m, [((-1,-1,0), 1, 4), (( 0,-1,0), 2, 4), (( 0,-1,0.5), 5, 4), ((-1,-1,0.5), 6, 4)])]
slab_west  (c,_,_,_,w,_,_) m = if hide c w then [] else [(m, [((-1, 0,0), 1, 2), ((-1,-1,0), 2, 2), ((-1,-1,0.5), 5, 2), ((-1, 0,0.5), 6, 2)])]
slab_top m = [(m, [((-1, 0,0.5), 1, 5), ((-1,-1,0.5), 2, 5), (( 0,-1,0.5), 3, 5), (( 0, 0,0.5), 4, 5)])]

sides ns m      = ($ m) =<< [north ns, east ns, south ns, west ns]
slab_sides ns m = ($ m) =<< [slab_north ns, slab_east ns, slab_south ns, slab_west ns]
block ns m      = ($ m) =<< [north ns, east ns, south ns, west ns, top ns, bottom ns]

inset_sides d m   = ($ m) =<< [moveY (-d) . inset_north, moveY d . inset_south, moveX d . inset_west, moveX (-d) . inset_east]

hide c t = c == t || IS.member (fromIntegral t) solidIDs

solidIDs = IS.fromAscList $ [1..5] ++ [7] ++ [11..17] ++ [19] ++ [21..25] ++
                            [29,33,35,36] ++ [41..43] ++ [45..49] ++ [54,56,57,58] ++
                            [60..62] ++ [73,74,78,79,80,82,84] ++ [86..89] ++ [91,95]