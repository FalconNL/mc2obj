{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module ObjExport where

import Control.Arrow
import Data.Binary
import Data.List
import qualified Data.List.Key as K
import qualified Data.IntMap as I
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import System.FilePath
import Text.Printf
import Region

data Orientation = E | W | N | S | T | B deriving (Enum, Show)

type Location = (Int, Int, Int)
type Face = [(Location, Int, Int)]
type Coord = (Int, Int)
type Chunk = (V.Vector Word8, V.Vector Word8)

chunkW, chunkH :: Int
(chunkW, chunkH) = (16, 128)

export :: FilePath -> FilePath -> [Coord] -> IO ()
export regionDir file chunks = do
    world <- loadWorld regionDir chunks
    let (vertices, geom) = foldl' (chunkGeom world) (M.empty, []) chunks
    writeFile file $ printf "mtllib minecraft.mtl\n\n%s\n%s\n%s\n%s"
        (unlines . map ((\(x,y,z) -> printf "v %d %d %d" x y z) . snd) .
         sort . map (\(a,b) -> (b,a)) $ M.assocs vertices) texcoords normals (unlines geom) where
    texcoords = "vt 0 0\nvt 1 0\nvt 1 1\nvt 0 1\n" :: String
    normals   = "vn 1 0 0\nvn -1 0 0\nvn 0 1 0\nvn 0 -1 0\nvn 0 0 1\nvn 0 0 -1\n" :: String

loadWorld :: FilePath -> [Coord] -> IO (M.Map Coord Chunk)
loadWorld regionDir = fmap (M.fromList . concat) .
    mapM (\(rX, rZ) -> do
        (Region region) <- loadRegion $ regionDir </> printf "r.%d.%d.mcr" rX rZ
        return . map (\(i, chunk) -> ((32 * rX + mod i 32, 32 * rZ + div i 32),
                                      chunkData chunk)) $ I.assocs region) .
    S.toList . S.fromList . map (\(cX, cZ) -> (div cX 32, div cZ 32))

chunkData :: NBT -> (V.Vector Word8, V.Vector Word8)
chunkData nbt = (getArray $ navigate ["Level", "Blocks"] nbt,
                 getArray $ navigate ["Level", "Data"] nbt)
    where getArray ~(Just (TAG_Byte_Array a)) = a

chunkGeom :: M.Map Coord Chunk -> (M.Map Location Int, [String]) -> Coord ->
             (M.Map Location Int, [String])
chunkGeom world (!verts, !geoms) (cX, cZ) = (verts', (unlines $ (printf "g chunk.%d.%d" cX cZ) : geom) : geoms) where
    (verts', geom) = foldl' (\a matGroup -> second (printf "\nusemtl %s\n" (fst $ head matGroup) :) $
        foldr (\(_, f) (vs, gs) -> addFace (vs, gs) f) a matGroup) (verts, []) .
        groupOn fst $ concat [ blockGeometry world (cX * chunkW + x, y, cZ * chunkW + z)
                             | x <- [0..chunkW - 1], y <- [0..chunkH - 1], z <- [0..chunkW - 1]]

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f = K.group f . K.sort f

addFace :: (M.Map Location Int, [String]) -> Face -> (M.Map Location Int, [String])
addFace (!vs, !gs) f = (second
    ((: gs) . ("f " ++) . unwords . zipWith (\(_,ti,ni) vi -> printf "%d/%d/%d" vi ti ni) f)) .
    insertVertices vs $ map (\(v,_,_) -> v) f

insertVertices :: M.Map Location Int -> [Location] -> (M.Map Location Int, [Int])
insertVertices = foldr (\v (m, is) -> maybe (M.insert v (M.size m + 1) m, (M.size m + 1) : is)
    (\i -> (m, i:is)) $ M.lookup v m) . flip (,) []

blockGeometry :: M.Map Coord Chunk -> Location -> [(String, Face)]
blockGeometry world (x,y,z) = maybe [] (\blockID -> case blockID of
    (0,_) -> []
    (t,_) -> map (\(o,_) -> (material t o, blockFace (x,y,z) o)) $
             filter (maybe True (\(t',_) -> t /= t' && IS.notMember (fromIntegral t') solidIDs) .
                 getBlock world . snd) [ (S, (x+1,y,z)), (N, (x-1,y,z)), (T, (x,y+1,z))
                                       , (B, (x,y-1,z)), (W, (x,y,z+1)), (E, (x,y,z-1))]) $
    getBlock world (x,y,z)

getBlock :: M.Map Coord Chunk -> Location -> Maybe (Word8, Word8)
getBlock world (x,y,z) = if y < 0 || y >= chunkH then Nothing else
    fmap ((V.! blockOffset) *** (V.! dataOffset)) $ M.lookup (div x chunkW, div z chunkW) world where
    blockOffset = y + mod z chunkW * chunkH + mod x chunkW * chunkW * chunkH
    dataOffset  = div (y + mod z chunkW * chunkH + mod x chunkW * chunkW * chunkH) 2

solidIDs :: IS.IntSet
solidIDs = IS.fromAscList $ [1..5] ++ [7] ++ [11..17] ++ [19] ++ [21..25] ++
                            [29,33,35,36] ++ [41..43] ++ [45..49] ++ [54,56,57,58] ++
                            [60..62] ++ [73,74,78,79,80,82,84] ++ [86..89] ++ [91,95]

blockFace :: Location -> Orientation -> Face
blockFace (x,y,z) E = [((-z  ,-x-1,y  ), 1, 1), ((-z  ,-x  ,y  ), 2, 1), ((-z  ,-x  ,y+1), 3, 1), ((-z  ,-x-1,y+1), 4, 1)]
blockFace (x,y,z) W = [((-z-1,-x  ,y  ), 1, 2), ((-z-1,-x-1,y  ), 2, 2), ((-z-1,-x-1,y+1), 3, 2), ((-z-1,-x  ,y+1), 4, 2)]
blockFace (x,y,z) N = [((-z  ,-x  ,y  ), 1, 3), ((-z-1,-x  ,y  ), 2, 3), ((-z-1,-x  ,y+1), 3, 3), ((-z  ,-x  ,y+1), 4, 3)]
blockFace (x,y,z) S = [((-z-1,-x-1,y  ), 1, 4), ((-z  ,-x-1,y  ), 2, 4), ((-z  ,-x-1,y+1), 3, 4), ((-z-1,-x-1,y+1), 4, 4)]
blockFace (x,y,z) T = [((-z-1,-x-1,y+1), 1, 5), ((-z  ,-x-1,y+1), 2, 5), ((-z  ,-x  ,y+1), 3, 5), ((-z-1,-x  ,y+1), 4, 5)]
blockFace (x,y,z) B = [((-z-1,-x  ,y  ), 1, 6), ((-z  ,-x  ,y  ), 2, 6), ((-z  ,-x-1,y  ), 3, 6), ((-z-1,-x-1,y  ), 4, 6)]

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