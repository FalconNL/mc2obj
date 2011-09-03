module BlockUtil where

import qualified Data.IntSet as IS

type Vertex    = (Double, Double, Double)
type TexCoord  = (Double, Double)
type Face      = [(Vertex, TexCoord, Vertex)]
type Neighbors = (Int, Int, Int, Int, Int, Int, Int)
type BlockSize = ((Double, Double, Double), (Double, Double, Double))
type Materials = (String, String, String, String, String, String)

occlude :: Bool -> Neighbors -> [(String, Face)] -> [(String, Face)]
occlude self (c,n,e,s,w,t,b) = filter (\(_,vs) -> not $
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
solidIDs = IS.fromAscList $ [1..5] ++ [7] ++ [11..17] ++ [19] ++ [21..25] ++
                            [29,33,35,36] ++ [41..43] ++ [45..49] ++ [54,56,57,58] ++
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
rotateY rot f = iterate (map (\((x,y,z),t,(nx,ny,nz)) -> ((z-1,y,-x),t,(nz-1,ny,-nx)))) f !! (mod rot 4)
rotateZ rot f = iterate (map (\((x,y,z),t,(nx,ny,nz)) -> ((y,-1-x,z),t,(ny,-1-nx,nz)))) f !! (mod rot 4)

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

second :: (b -> c) -> (a, b) -> (a, c)
second f (a,b) = (a, f b)

------------
-- Blocks --
------------

block :: String -> Neighbors -> [(String, Face)]
block m ns = occlude True ns $ box (uniform m) fullBlock

blockSTB :: String -> String -> String -> Neighbors -> [(String, Face)]
blockSTB ms mt mb ns = occlude True ns $ box (stb ms mt mb) fullBlock

blockFST :: String -> String -> String -> Int -> Neighbors -> [(String, Face)]
blockFST mf ms mt rot ns = occlude True ns . map (second $ rotateZ rot) $ box (ms,ms,mf,ms,mt,mt) fullBlock

plant :: String -> [(String, Face)]
plant m = map ((,) m . (`rotateZ` sw)) [0..3] where
    sw = zip3 [(cx-d,cy+d,0),(cx+d,cy-d,0),(cx+d,cy-d,1),(cx-d,cy+d,1)]
              [(0,0),(1,0),(1,1),(0,1)] (repeat (-sqrt 2 / 2,-sqrt 2 / 2,0))
    (cx, cy, d)  = ((-1-epsilon)/2, (-1-epsilon)/2, sqrt 2 / 4)

sides :: String -> Double -> [(String, Face)]
sides m o = [faceNorth, faceEast, faceSouth, faceWest] >>= \f -> f (0,0) (1,1) o m

leaf :: String -> Neighbors -> [(String, Face)]
leaf m ns = occlude False ns $ box (uniform m) fullBlock

slab :: String -> String -> String -> Neighbors -> [(String, Face)]
slab ms mt mb ns = occlude True ns $ box (stb ms mt mb) ((0,0,0), (1,1,1/2))

workbench :: String -> String -> String -> String -> Neighbors -> [(String, Face)]
workbench mf ms mt mb ns = occlude True ns $ box (mf,ms,mf,ms,mt,mb) fullBlock

snow :: String -> Neighbors -> [(String, Face)]
snow m ns = occlude True ns $ box (uniform m) ((0,0,0), (1,1,1/8))

stairs :: String -> Int -> Neighbors -> [(String, Face)]
stairs m rot ns = occlude False ns . map (second $ rotateZ rot) $
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
bed isHead rot ns = occlude False ns . map (second $ rotateZ rot) $
    (if isHead then faceSouth (0,0) (1,9/16) 0 f else faceNorth (0,0) (1,9/16) 0 f) ++
    faceTop (0,0) (1,1) (7/16) t ++ faceWest (0,0) (1,9/16) 0 s ++
    modifyTexcoords (\(tx,ty) -> (1-tx,ty)) (faceEast (0,0) (1,9/16) 0 s)
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
                      else [] --TODO: rotateZ (case d of 1 -> 0; 2 -> 2; 3 -> 1; _ -> 3)

trackFlat :: String -> [(String, Face)]
trackFlat = faceTop (0,0) (1,1) (1 - epsilon)

trackIncline :: String -> [(String, Face)]
trackIncline m = [(m, [((-1,-1,  epsilon), (0,0), vn), (( 0,-1,  epsilon), (1,0), vn)
                      ,(( 0, 0,1+epsilon), (1,1), vn), ((-1, 0,1+epsilon), (0,1), vn)])]
    where vn = (0,-sqrt 2 / 2,sqrt 2 / 2)

trackStraight :: Int -> String -> [(String, Face)]
trackStraight d m = case mod d 8 of 1 -> map (second $ rotateZ 1) $ trackFlat m
                                    2 -> map (second $ rotateZ 0) $ trackIncline m
                                    3 -> map (second $ rotateZ 2) $ trackIncline m
                                    4 -> map (second $ rotateZ 1) $ trackIncline m
                                    5 -> map (second $ rotateZ 3) $ trackIncline m
                                    _ -> trackFlat m

-- piston d m = case mod d 8 of 0 -> []
--                              1 -> []
--                              2 -> []
--                              3 -> []
--                              4 -> []
--                              _ -> []
--     where h = if div d 8 == 1 then 12/16 else 1