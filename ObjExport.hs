module ObjExport (Face, Location, Orientation(..), export) where

import Control.Arrow
import Data.Binary
import Data.List
import qualified Data.List.Key as K
import qualified Data.Map as M
import Text.Printf

data Orientation = E | W | N | S | T | B deriving (Enum, Show)

type Location = (Int, Int, Int)
type Face = (Location, Word8, Orientation)

export :: FilePath -> [Face] -> IO ()
export file faces = writeFile file $ printf "mtllib minecraft.mtl\n\n%s\n%s\n%s\n%s"
    vertices texcoords normals geom where
    (vertices, geom) = geometry faces
    texcoords        = "vt 0 0\nvt 1 0\nvt 1 1\nvt 0 1\n"
    normals          = "vn 1 0 0\nvn -1 0 0\nvn 0 1 0\nvn 0 -1 0\nvn 0 0 1\nvn 0 0 -1\n"

geometry :: [Face] -> (String, String)
geometry = (\(vs,fs) -> (unlines . map ((\(x,y,z) -> printf "v %d %d %d" x y z) . snd) .
                         sort . map (\(a,b) -> (b,a)) $ M.assocs vs, unlines $ reverse fs)) .
    foldl' (\(a,b) g -> let (_, t, o) = head g in (foldl' addFace (a, ("usemtl " ++ material t o) : b) g))
           (M.empty, []) . order (\(_, t, o) -> material t o)

addFace :: (M.Map Location Int, [String]) -> Face -> (M.Map Location Int, [String])
addFace (vs, fs) (loc, t, o) = if t == 0 then (vs, fs) else (second
    ((: fs) . ("f " ++) . unwords . map (\(ti,vi) -> printf "%d/%d/%d" vi ti $ fromEnum o + 1) .
        zip [1 :: Int ..])) . insertVertices vs $ faceVerts loc o

insertVertices :: M.Map Location Int -> [Location] -> (M.Map Location Int, [Int])
insertVertices = foldr (\v (m, is) -> maybe (M.insert v (M.size m + 1) m, (M.size m + 1) : is)
    (\i -> (m, i:is)) $ M.lookup v m) . flip (,) []

faceVerts :: Location -> Orientation -> [Location]
faceVerts (x,y,z) N = [(-z  ,-x  ,y  ), (-z-1,-x  ,y  ), (-z-1,-x  ,y+1), (-z  ,-x  ,y+1)]
faceVerts (x,y,z) E = [(-z  ,-x-1,y  ), (-z  ,-x  ,y  ), (-z  ,-x  ,y+1), (-z  ,-x-1,y+1)]
faceVerts (x,y,z) S = [(-z-1,-x-1,y  ), (-z  ,-x-1,y  ), (-z  ,-x-1,y+1), (-z-1,-x-1,y+1)]
faceVerts (x,y,z) W = [(-z-1,-x  ,y  ), (-z-1,-x-1,y  ), (-z-1,-x-1,y+1), (-z-1,-x  ,y+1)]
faceVerts (x,y,z) T = [(-z-1,-x-1,y+1), (-z  ,-x-1,y+1), (-z  ,-x  ,y+1), (-z-1,-x  ,y+1)]
faceVerts (x,y,z) B = [(-z-1,-x  ,y  ), (-z  ,-x  ,y  ), (-z  ,-x-1,y  ), (-z-1,-x-1,y  )]

material :: Word8 -> Orientation -> String
material  1 _ = "Stone"
material  2 T = "Grass_Top"
material  2 B = "Dirt"
material  2 _ = "Grass_Side"
material  3 _ = "Dirt"
material  4 _ = "Cobblestone"
material  7 _ = "Bedrock"
material  8 _ = "Water"
material  9 _ = "Water"
material 10 _ = "Lava"
material 11 _ = "Lava"
material 12 _ = "Sand"
material 13 _ = "Gravel"
material 14 _ = "Ore_Gold"
material 15 _ = "Ore_Iron"
material 16 _ = "Ore_Coal"
material 17 _ = "Log_Oak"
material 18 _ = "Leaf_Oak"
material 21 _ = "Ore_Lapis"
material 24 T = "Sandstone_Top"
material 24 B = "Sandstone_Bottom"
material 24 _ = "Sandstone_Side"
material 48 _ = "Moss"
material 49 _ = "Obsidian"
material 52 _ = "Spawner"
material 54 T = "Chest_Top" --Needs neighbour
material 54 B = "Chest_Top" --Needs neighbour
material 54 _ = "Chest_Side" --Needs data, neighbour
material 56 _ = "Ore_Diamond"
material 73 _ = "Ore_Redstone"
material 74 _ = "Ore_Redstone"
material 78 T = "Snow"
material 78 B = "Dirt"
material 78 _ = "Snow_Side"
material 79 _ = "Ice"
material 82 _ = "Clay"
material 86 T = "Pumpkin_Top"
material 86 B = "Pumpkin_Top"
material 86 _ = "Pumpkin_Side" --Needs data
material 95 T = "Chest_Top" --Needs neighbour
material 95 B = "Chest_Top" --Needs neighbour
material 95 _ = "Chest_Side" --Needs data, neighbour
material  _ _ = "Unknown"

order :: Ord b => (a -> b) -> [a] -> [[a]]
order f = K.group f . K.sort f