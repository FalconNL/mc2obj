module BlockUtil where

import qualified Data.IntSet as IS

type Vertex    = (Double, Double, Double)
type TexCoord  = (Double, Double)
type Face      = [(Vertex, TexCoord, Vertex)]
type Neighbors = (Int, Int, Int, Int, Int, Int, Int)
type BlockSize = ((Double, Double, Double), (Double, Double, Double))
type Materials = (String, String, String, String, String, String)

cull :: Bool -> Neighbors -> [(String, Face)] -> [(String, Face)]
cull self (c,n,e,s,w,t,b) = filter (\(_,vs) -> not $
    (hide self c n && test (\(_,y,_) -> y ==  0) vs) ||
    (hide self c e && test (\(x,_,_) -> x ==  0) vs) ||
    (hide self c s && test (\(_,y,_) -> y == -1) vs) ||
    (hide self c w && test (\(x,_,_) -> x == -1) vs) ||
    (hide self c t && test (\(_,_,z) -> z ==  1) vs) ||
    (hide self c b && test (\(_,_,z) -> z ==  0) vs))
    where test f = all (\(v,_,_) -> f v)

hide :: Bool -> Int -> Int -> Bool
hide self c t = (self && c == t) || IS.member (fromIntegral t) solidIDs

solidIDs :: IS.IntSet
solidIDs = IS.fromAscList $ [1..5] ++ [7] ++ [12..17] ++ [19] ++ [21..25] ++
                            [35,36] ++ [41..43] ++ [45..49] ++ [54,56,57,58] ++
                            [60..62] ++ [73,74,78,79,80,82,84] ++ [86..89] ++ [91,95]

north, east, south, west, top, bottom :: Vertex
north  = ( 0, 1, 0)
east   = ( 1, 0, 0)
south  = ( 0,-1, 0)
west   = (-1, 0, 0)
top    = ( 0, 0, 1)
bottom = ( 0, 0,-1)

face :: TexCoord -> TexCoord -> Vertex -> [Vertex] -> String -> [(String, Face)]
face (blX,blY) (trX,trY) vn cs m = [(m, zip3 cs [(blX,blY), (trX,blY), (trX,trY), (blX,trY)] (repeat vn))]

faceNorth, faceEast, faceSouth, faceWest, faceTop, faceBottom :: TexCoord -> TexCoord -> Double -> String -> [(String, Face)]
faceNorth  bl@(blX,blY) tr@(trX,trY) o = face bl tr north  [( -blX,    -o, blY), ( -trX,    -o, blY), ( -trX,    -o, trY), ( -blX,    -o, trY)]
faceEast   bl@(blX,blY) tr@(trX,trY) o = face bl tr east   [(   -o, blX-1, blY), (   -o, trX-1, blY), (   -o, trX-1, trY), (   -o, blX-1, trY)]
faceSouth  bl@(blX,blY) tr@(trX,trY) o = face bl tr south  [(blX-1,   o-1, blY), (trX-1,   o-1, blY), (trX-1,   o-1, trY), (blX-1,   o-1, trY)]
faceWest   bl@(blX,blY) tr@(trX,trY) o = face bl tr west   [(  o-1,  -blX, blY), (  o-1,  -trX, blY), (  o-1,  -trX, trY), (  o-1,  -blX, trY)]
faceTop    bl@(blX,blY) tr@(trX,trY) o = face bl tr top    [(blY-1,  -blX, 1-o), (blY-1,  -trX, 1-o), (trY-1,  -trX, 1-o), (trY-1,  -blX, 1-o)]
faceBottom bl@(blX,blY) tr@(trX,trY) o = face bl tr bottom [( -blY,  -blX,   o), ( -blY,  -trX,   o), ( -trY,  -trX,   o), ( -trY,  -blX,   o)]

rotateY, rotateZ :: Int -> Face -> Face
rotateY rot f = iterate (map (\((x,y,z),t,(nx,ny,nz)) -> ((z-1,y,-x),t,(nz-1,ny,-nx)))) f !! mod rot 4
rotateZ rot f = iterate (map (\((x,y,z),t,(nx,ny,nz)) -> ((y,-1-x,z),t,(ny,-1-nx,nz)))) f !! mod rot 4

freeRotateZ :: Double -> Face -> Face
freeRotateZ a = map (\(v,t,n) -> (r v,t,r n))
    where r (x,y,z) = (x'*cos a-y'*sin a-1/2,x'*sin a+y'*cos a-1/2,z) where (x',y') = (x+1/2,y+1/2)

modifyVertices :: (Vertex -> Vertex) -> [(String, Face)] -> [(String, Face)]
modifyVertices f = map (second $ map (\(v,t,n) -> (f v,t,n)))
modifyTexcoords :: (TexCoord -> TexCoord) -> [(String, Face)] -> [(String, Face)]
modifyTexcoords f = map (second $ map (\(v,t,n) -> (v,f t,n)))

box :: Materials -> BlockSize -> [(String, Face)]
box (mn,me,ms,mw,mt,mb) ((s,w,b), (n,e,t)) = faceNorth  (1-e,   b) (1-w,  t) (1-n) mn ++
                                             faceEast   (  s,   b) (  n,  t) (1-e) me ++
                                             faceSouth  (  w,   b) (  e,  t)     s ms ++
                                             faceWest   (1-n,   b) (1-s,  t)     w mw ++
                                             faceTop    (1-n,   w) (1-s,  e) (1-t) mt ++
                                             faceBottom (1-n, 1-e) (1-s,1-w)     b mb

fullBlock :: BlockSize
fullBlock = ((0,0,0), (1,1,1))

uniform :: String -> Materials
uniform m    = (m,m,m,m,m,m)
stb :: String -> String -> String -> Materials
stb ms mt mb = (ms,ms,ms,ms,mt,mb)

epsilon :: Double
epsilon = 0.001

first :: (a -> c) -> (a, b) -> (c, b)
first f (a,b) = (f a, b)

second :: (b -> c) -> (a, b) -> (a, c)
second f (a,b) = (a, f b)

------------
-- Blocks --
------------

block :: String -> Neighbors -> [(String, Face)]
block m ns = cull True ns $ box (uniform m) fullBlock

blockSTB :: String -> String -> String -> Neighbors -> [(String, Face)]
blockSTB ms mt mb ns = cull True ns $ box (stb ms mt mb) fullBlock

blockFST :: String -> String -> String -> Int -> Neighbors -> [(String, Face)]
blockFST mf ms mt rot ns = cull True ns . map (second $ rotateZ rot) $ box (ms,ms,mf,ms,mt,mt) fullBlock

plant :: String -> [(String, Face)]
plant m = map ((,) m . (`rotateZ` sw)) [0..3] where
    sw = zip3 [(cx-d,cy+d,0),(cx+d,cy-d,0),(cx+d,cy-d,1),(cx-d,cy+d,1)]
              [(0,0),(1,0),(1,1),(0,1)] (repeat (-sqrt 2 / 2,-sqrt 2 / 2,0))
    (cx, cy, d)  = ((-1-epsilon)/2, (-1-epsilon)/2, sqrt 2 / 4)

liquid :: String -> Double -> Neighbors -> [(String, Face)]
liquid m h ns = cull False {- replace with True once properly implemented -} ns $ box (uniform m) ((0,0,0),(1,1,h))

sides :: String -> Double -> [(String, Face)]
sides m o = [faceNorth, faceEast, faceSouth, faceWest] >>= \f -> f (0,0) (1,1) o m

leaf :: String -> Neighbors -> [(String, Face)]
leaf m ns = cull False ns $ box (uniform m) fullBlock ++
    box (uniform m) ((1-epsilon,1-epsilon,1-epsilon), (epsilon,epsilon,epsilon))

slab :: String -> String -> String -> Neighbors -> [(String, Face)]
slab ms mt mb ns = cull True ns $ box (stb ms mt mb) ((0,0,0), (1,1,1/2))

workbench :: String -> String -> String -> String -> Neighbors -> [(String, Face)]
workbench mf ms mt mb ns = cull True ns $ box (mf,ms,mf,ms,mt,mb) fullBlock

snow :: String -> Neighbors -> [(String, Face)]
snow m ns = cull True ns $ box (uniform m) ((0,0,0), (1,1,1/8))

stairs :: String -> Int -> Neighbors -> [(String, Face)]
stairs m rot ns = cull False ns . map (second $ rotateZ rot) $
    stairNorth ++ stairSouth ++
    faceWest (0,0) (1,1/2) 0 m ++ faceWest (0,1/2) (1,1) (1/2) m ++
    faceTop (0,0) (1,1/2) (1/2) m ++ faceTop (0,1/2) (1,1) 0 m ++
    faceEast (0,0) (1,1) 0 m ++ faceBottom (0,0) (1,1) 0 m
    where stairNorth = map ((,) m) [[((   0,0,  0), (  0,  0), east), ((  -1,0,  0), (  1,  0), east)
                                    ,((  -1,0,1/2), (  1,1/2), east), ((-1/2,0,1/2), (1/2,1/2), east)]
                                   ,[((   0,0,  0), (  0,  0), east), ((-1/2,0,1/2), (1/2,1/2), east)
                                    ,((-1/2,0,  1), (1/2,  1), east), ((   0,0,  1), (  0,  1), east)]]
          stairSouth = map (second $ reverse . map (\((x,_,z),t,_) -> ((x,-1,z),t,south))) stairNorth

bed :: Bool -> Int -> Neighbors -> [(String, Face)]
bed isHead rot ns = cull False ns . map (second $ rotateZ rot) $
    (if isHead then faceSouth (0,0) (1,9/16) 0 f else faceNorth (0,0) (1,9/16) 0 f) ++
    faceTop (0,0) (1,1) (7/16) t ++ faceWest (0,0) (1,9/16) 0 s ++
    modifyTexcoords (first (1 -)) (faceEast (0,0) (1,9/16) 0 s)
    where [f,s,t] = map (++ if isHead then "Head" else "Foot") ["Bed_", "Bed_Side_", "Bed_Top_"]

--TODO: texcoords
fence :: Neighbors -> [(String, Face)]
fence (_,n,e,_,_,_,_) = (if n == 85 then fence_beam 3 (6/16) ++ fence_beam 3 (12/16) else []) ++
                        (if e == 85 then fence_beam 0 (6/16) ++ fence_beam 0 (12/16) else []) ++
                        box (uniform "Wood") ((3/8,3/8,0), (5/8,5/8,1)) where
    fence_beam rot h = map (second $ rotateZ rot) $ box (uniform "Wood") ((7/16,5/8,h), (9/16,11/8,h+3/16))

torch :: Int -> String -> [(String, Face)]
torch d m = if d == 5 then sides m (7/16) ++ [(m, [((-9/16,-7/16,5/8), (7/16, 8/16), top),
                                                   ((-9/16,-9/16,5/8), (9/16, 8/16), top),
                                                   ((-7/16,-9/16,5/8), (9/16,10/16), top),
                                                   ((-7/16,-7/16,5/8), (7/16,10/16), top)])]
                      else map (second $ rotateZ (case d of 1 -> 0; 2 -> 2; 3 -> 1; _ -> 3)) .
                           modifyVertices (\(x,y,z) -> (x,case z of 0 -> y+1/2
                                                                    1 -> y
                                                                    _ -> y+(1-z)/2,z+3/16)) $ torch 5 m

trackFlat :: String -> [(String, Face)]
trackFlat = faceTop (0,0) (1,1) (1 - epsilon)

trackIncline :: String -> [(String, Face)]
trackIncline m = [(m, [((-1,-1,  epsilon), (0,0), vn), (( 0,-1,  epsilon), (1,0), vn)
                      ,(( 0, 0,1+epsilon), (1,1), vn), ((-1, 0,1+epsilon), (0,1), vn)])]
    where vn = (0,-sqrt 2 / 2,sqrt 2 / 2)

trackStraight :: Int -> String -> [(String, Face)]
trackStraight d m = case mod d 8 of 1 -> map (second $ rotateZ 1) $ trackFlat m
                                    2 -> map (second $ rotateZ 2) $ trackIncline m
                                    3 -> map (second $ rotateZ 0) $ trackIncline m
                                    4 -> map (second $ rotateZ 1) $ trackIncline m
                                    5 -> map (second $ rotateZ 3) $ trackIncline m
                                    _ -> trackFlat m

track :: Int -> String -> String -> [(String, Face)]
track d ms mb = if d < 6 then trackStraight d ms else case d of
    6 -> bend
    7 -> map (second $ rotateZ 1) bend
    8 -> map (second $ rotateZ 2) bend
    _ -> map (second $ rotateZ 3) bend
    where bend = trackFlat mb

piston :: Int -> String -> Neighbors -> [(String, Face)]
piston d m ns = cull False ns $ case mod d 8 of
    0 -> map (second $ rotateY 2) base
    1 -> base
    2 -> map (second $ rotateY 1) base
    3 -> map (second $ rotateZ 2 . rotateY 1) base
    4 -> map (second $ rotateZ 3 . rotateY 1) base
    _ -> map (second $ rotateZ 1 . rotateY 1) base
    where base = box (stb ms (if extended then mt else m) mb)
                     ((0,0,0),(1,1,if extended then 3/4 else 1))
          extended = div d 8 == 1
          ms = "Piston_Side"
          mt = "Piston_Top"
          mb = "Piston_Bottom"

pistonExtension :: Int -> Neighbors -> [(String, Face)]
pistonExtension d ns = cull False ns $ case mod d 8 of
    0 -> map (second $ rotateY 2) extension
    1 -> extension
    2 -> map (second $ rotateY 1) extension
    3 -> map (second $ rotateZ 2 . rotateY 1) extension
    4 -> map (second $ rotateZ 3 . rotateY 1) extension
    _ -> map (second $ rotateZ 1 . rotateY 1) extension
    where extension = map ((,) ms . (`rotateZ` rodSouth)) [0..3] ++
                      box (stb ms mt mb) ((0,0,3/4),(1,1,1))
          rodSouth = [((-5/8,-5/8,-1/4),(0,  1),south), ((-3/8,-5/8,-1/4),(0,3/4),south)
                     ,((-3/8,-5/8, 3/4),(1,3/4),south), ((-5/8,-5/8, 3/4),(1,  1),south)]
          ms = "Piston_Side"
          mt = if div d 8 == 1 then "Piston_Sticky" else "Piston"
          mb = "Piston"

button :: Int -> Neighbors -> [(String, Face)]
button rot ns = cull False ns . map (second $ rotateZ rot) $
    box (uniform "Stone") ((7/8, 5/16, 3/8), (1, 11/16, 5/8))

portal :: Neighbors -> [(String, Face)]
portal ns@(_,n,_,s,_,_,_) = cull True ns .
    map (second . rotateZ $ if n == 90 || s == 90 then 1 else 0) $
    box (uniform "Portal") ((3/8,0,0), (5/8,1,1))

pressurePlate :: String -> Neighbors -> [(String, Face)]
pressurePlate m ns = cull False ns $ box (uniform m) ((1/16,1/16,0), (15/16,15/16,1/16))

hatch :: Int -> Neighbors -> [(String, Face)]
hatch d ns = cull False ns $ case div d 4 of
    0 -> flat
    _ -> map (second $ rotateZ (case mod d 4 of 0 -> 0; 1 -> 2; 2 -> 3; _ -> 1) . rotateY 1) flat
    where flat = faceTop (0,0) (1,1) (13/16) "Hatch" ++ faceBottom (0,0) (1,1) 0 "Hatch" ++
                     ([faceNorth, faceEast, faceSouth, faceWest] >>= \f -> f (0,0) (1,3/16) 0 "Hatch")

chest :: (Int, Int, Int, Int, Int, Int, Int) -> [(String, Face)]
chest ns@(_,n,e,s,w,_,_) = cull True ns $
    if n == 54 then map (second $ rotateZ 1) r else
    if e == 54 then l else
    if s == 54 then map (second $ rotateZ 1) l else
    if w == 54 then r else
    blockFST "Chest_Front" "Chest_Side" "Chest_Top" (if n /= 0 then 0 else
                                                     if s /= 0 then 2 else
                                                     if w /= 0 then 3 else 1) ns
    where l = box ("Chest_Left_Back","","Chest_Left_Front","Chest_Side","Chest_Top","Chest_Top") ((0,0,0),(1,1,1))
          r = box ("Chest_Right_Back","Chest_Side","Chest_Right_Front","","Chest_Top","Chest_Top") ((0,0,0),(1,1,1))

door :: String -> String -> Int -> Neighbors -> [(String, Face)]
door mt mb d ns = cull False ns . map (second $ rotateZ (3 * div (mod d 8) 4 + case mod d 4 of 0 -> 2; 1 -> 3; 2 -> 0; _ -> 1)) $
    case div d 8 of 0 -> faceSouth (0,0) (1,1) 0 mb ++ modifyTexcoords (first (1 -)) (faceNorth (0,0) (1,1) 0 mb) ++
                         faceWest (13/16,0) (1,1) 0 mb ++ faceEast (0,0) (3/16,1) 0 mb ++
                         faceBottom (13/16,0) (1,1) 0 mb
                    _ -> faceSouth (0,0) (1,1) 0 mt ++ modifyTexcoords (first (1 -)) (faceNorth (0,0) (1,1) 0 mt) ++
                         faceWest (13/16,0) (1,1) 0 mb ++ faceEast (0,0) (3/16,1) 0 mb ++
                         faceTop (13/16,0) (1,1) 0 mb

cake :: Int -> Neighbors -> [(String, Face)]
cake d ns = cull False ns $
    faceSouth  (1/16,0) ((15-2*d')/16,1/2) (1/16) ms ++
    faceWest   (1/16,0) (15/16,1/2) (1/16) ms ++
    faceNorth  ((1+2*d')/16,0) (15/16,1/2) (1/16) ms ++
    faceEast   (1/16,0) (15/16,1/2) ((1+2*d')/16) (if d == 0 then ms else "Cake_Inside") ++
    faceTop    (1/16,1/16) (15/16,15/16) (1/2) "Cake_Top" ++
    faceBottom (1/16,1/16) (15/16,15/16) 0 "Cake_Bottom"
    where ms = "Cake_Side"
          d' = fromIntegral d

repeater :: Int -> String -> String -> Neighbors -> [(String, Face)]
repeater d mb mt ns = cull True ns . map (second $ rotateZ (case mod d 4 of 0 -> 2; 1 -> 1; 2 -> 0; _ -> 3)) $
    modifyVertices (\(x,y,z) -> (x,y,z-7/8)) (box (uniform mb) ((0,0,7/8),(1,1,1))) ++
    modifyVertices (\(x,y,z) -> (x+5/16,y,z-3/16)) (torch 5 mt) ++
    modifyVertices (\(x,y,z) -> (x - (2 * fromIntegral (div d 4) - 1)/16,y,z-3/16)) (torch 5 mt)

sign :: Int -> String -> Neighbors -> [(String, Face)]
sign d m ns = map (second $ freeRotateZ (-pi / 2 - fromIntegral d * pi / 8)) $
    modifyVertices (\(x,y,z) -> (x,y-11/24,z+1/3)) (wallSign 5 m ns) ++
    [(m, [((-13/24,-13/24,  0),(   0,1/16),south ), ((-11/24,-13/24,  0),(1/32,1/16),south ), ((-11/24,-13/24,14/24),(1/32,1/2),south ), ((-13/24,-13/24,14/24),(   0,1/2),south )])] ++
    [(m, [((-11/24,-11/24,  0),(1/32,1/16),north ), ((-13/24,-11/24,  0),(2/32,1/16),north ), ((-13/24,-11/24,14/24),(2/32,1/2),north ), ((-11/24,-11/24,14/24),(1/32,1/2),north )])] ++
    [(m, [((-11/24,-13/24,  0),(2/32,1/16),east  ), ((-11/24,-11/24,  0),(3/32,1/16),east  ), ((-11/24,-11/24,14/24),(3/32,1/2),east  ), ((-11/24,-13/24,14/24),(2/32,1/2),east  )])] ++
    [(m, [((-13/24,-11/24,  0),(3/32,1/16),west  ), ((-13/24,-13/24,  0),(4/32,1/16),west  ), ((-13/24,-13/24,14/24),(4/32,1/2),west  ), ((-13/24,-11/24,14/24),(3/32,1/2),west  )])]

wallSign :: Int -> String -> Neighbors -> [(String, Face)]
wallSign d m ns = cull True ns . map (second $ rotateZ (case d of 2 -> 3; 3 -> 1; 4 -> 2; _ -> 0)) $
    [(m, [((-1,-1/12,1/4),( 1/32, 1/ 2),south ), (( 0,-1/12,1/4),(13/32, 1/ 2),south ), (( 0,-1/12,3/4),(13/32,15/16),south ), ((-1,-1/12,3/4),( 1/32,15/16),south )])] ++
    [(m, [(( 0,    0,1/4),(13/32, 1/ 2),north ), ((-1,    0,1/4),(25/32, 1/ 2),north ), ((-1,    0,3/4),(25/32,15/16),north ), (( 0,    0,3/4),(13/32,15/16),north )])] ++
    [(m, [(( 0,-1/12,1/4),(    0, 1/ 2),east  ), (( 0,    0,1/4),( 1/32, 1/ 2),east  ), (( 0,    0,3/4),( 1/32,15/16),east  ), (( 0,-1/12,3/4),(    0,15/16),east  )])] ++
    [(m, [((-1,    0,1/4),(    0, 1/ 2),west  ), ((-1,-1/12,1/4),( 1/32, 1/ 2),west  ), ((-1,-1/12,3/4),( 1/32,15/16),west  ), ((-1,    0,3/4),(    0,15/16),west  )])] ++
    [(m, [((-1,-1/12,3/4),( 1/32,15/16),top   ), (( 0,-1/12,3/4),(13/32,15/16),top   ), (( 0,    0,3/4),(13/32,    1),top   ), ((-1,    0,3/4),( 1/32,    1),top   )])] ++
    [(m, [((-1,-1/12,3/4),( 1/32,15/16),bottom), (( 0,-1/12,3/4),(13/32,15/16),bottom), (( 0,    0,3/4),(13/32,    1),bottom), ((-1,    0,3/4),( 1/32,    1),bottom)])]

redstone :: String -> String -> Neighbors -> [(String, Face)]
redstone _ mc _ = trackFlat mc
