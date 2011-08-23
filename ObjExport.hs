{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module ObjExport (defaultOptions, export, exportWith,
    bottom, sides, yFrom, yTo) where

import Control.Arrow
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import qualified Data.List.Key as K
import qualified Data.IntMap as I
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word
import System.FilePath
import System.IO
import Text.Printf
import Region

data Orientation = E | W | N | S | T | B deriving (Enum, Eq, Show)
data ExportOptions = ExportOptions { bottom :: Bool, sides :: Bool, yFrom :: Int, yTo :: Int }

type Location = (Int, Int, Int)
type Face = [(Location, Int, Int)]
type Coord = (Int, Int)
type Chunk = (B.ByteString, B.ByteString)
type BorderedChunk = (Maybe Chunk, Maybe Chunk, Maybe Chunk, Maybe Chunk, Maybe Chunk)

chunkW, chunkH :: Int
(chunkW, chunkH) = (16, 128)

export :: FilePath -> FilePath -> [Coord] -> IO ()
export = exportWith defaultOptions

exportWith :: ExportOptions -> FilePath -> FilePath -> [Coord] -> IO ()
exportWith options regionDir file chunks = do
    h <- openFile file WriteMode
    B.hPutStrLn h "mtllib minecraft.mtl\n\n"
    world <- loadWorld regionDir chunks
    mapM_ (\(i,c) -> B.hPutStrLn h =<< chunkGeom options c i) world
    hClose h
    putStrLn "Done."

defaultOptions :: ExportOptions
defaultOptions = ExportOptions False False 0 127

loadWorld :: FilePath -> [Coord] -> IO [(Coord, BorderedChunk)]
loadWorld regionDir cs = do
    let regionID (cX,cZ) = (div cX 32, div cZ 32)
    let neededChunks = S.fromList cs
    regions <- mapM (\(rX,rZ) -> fmap (((,) (rX,rZ)) . I.map chunkData) $
                                 loadRegion (regionDir </> printf "r.%d.%d.mcr" rX rZ)) .
               S.toList $ S.map regionID neededChunks
    let bordered = M.fromList $ map (\c -> (c, (Nothing, Nothing, Nothing, Nothing, Nothing))) cs

    return $ M.assocs $ foldl' (\m ((rX,rZ), r) ->
          S.fold (\(cX,cZ) a -> let chunk = I.lookup (mod cX 32 + 32 * mod cZ 32) r in
                M.adjust (\(_,n,e,s,w) -> (chunk,n,e,s,w)) (cX,cZ) $
                M.adjust (\(c,_,e,s,w) -> (c,chunk,e,s,w)) (cX+1,cZ) $
                M.adjust (\(c,n,_,s,w) -> (c,n,chunk,s,w)) (cX,cZ+1) $
                M.adjust (\(c,n,e,_,w) -> (c,n,e,chunk,w)) (cX-1,cZ) $
                M.adjust (\(c,n,e,s,_) -> (c,n,e,s,chunk)) (cX,cZ-1) $
                a
            ) m $ S.filter ((== (rX,rZ)) . regionID) neededChunks
        ) bordered regions

chunkData :: Tag -> Chunk
chunkData nbt = (getArray $ navigate ["Level", "Blocks"] nbt,
                 getArray $ navigate ["Level", "Data"] nbt)
    where getArray ~(Just (TAG_Byte_Array a)) = a

chunkGeom :: ExportOptions -> BorderedChunk -> (Int, Int) -> IO BC.ByteString
chunkGeom options (c,n,e,s,w) (cX,cZ) = do
    let (verts, geom) = foldl' (\a matGroup -> second (BC.pack (printf "\nusemtl %s\n" . fst $ head matGroup) `B.append`) $
                foldr (\(_, f) (vs, gs) -> addFace (vs, gs) f) a matGroup) (M.empty, B.empty) .
                groupOn fst $ concat [ blockGeometry options blockLookup (cX * chunkW + x, y, cZ * chunkW + z)
                                     | x <- [0..chunkW - 1], y <- [max 0 (yFrom options)..min (chunkH - 1) (yTo options)]
                                     , z <- [0..chunkW - 1]]
    let vertices = B.concat . M.elems $ M.foldrWithKey (\(x,y,z) i vs ->
            M.insert (-i) (BC.intercalate " " $ "v" : map (BC.pack . show) [x,y,z] ++ ["\n"]) vs) M.empty verts
    putStrLn $ printf "Processing chunk (%d, %d)" cX cZ
    return $! B.concat [BC.pack $ printf "g chunk.%d.%d\n\n" cX cZ, vertices, texcoords, normals, geom]
    where
        texcoords = "\nvt 0 0\nvt 1 0\nvt 1 1\nvt 0 1\n"
        normals   = "\nvn 1 0 0\nvn -1 0 0\nvn 0 1 0\nvn 0 -1 0\nvn 0 0 1\nvn 0 0 -1\n\n"
        
        blockLookup :: Location -> (Word8, Word8)
        blockLookup (x,y,z) = if y < yFrom options || y > yTo options then (if bottom options then (0,0) else (1,0)) else
            let i = y + mod z chunkW * chunkH + mod x chunkW * chunkW * chunkH
            in  maybe (if sides options then (0,0) else (1,0))
                      (flip B.index i *** ((if even i then flip div 16 else flip mod 16) . flip B.index (div i 2))) $
                    case (compare cX $ div x chunkW, compare cZ $ div z chunkW) of
                         (LT,_) -> s
                         (GT,_) -> n
                         (_,LT) -> w
                         (_,GT) -> e
                         _      -> c

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f = K.group f . K.sort f

addFace :: (M.Map Location Int, B.ByteString) -> Face -> (M.Map Location Int, B.ByteString)
addFace (!vs, !gs) f = (second
    (flip B.append gs . BC.intercalate " " . ("f" :) . (++ ["\n"]) .
        zipWith (\(_,ti,ni) vi -> BC.intercalate "/" $ map (BC.pack . show) [-vi,ti,ni]) f)) $
    foldr (\(v,_,_) (m, is) -> maybe (M.insert v (M.size m + 1) m, (M.size m + 1) : is)
        (\i -> (m, i:is)) $ M.lookup v m) (vs, []) f

blockGeometry :: ExportOptions -> (Location -> (Word8, Word8)) -> Location -> [(String, Face)]
blockGeometry options blockRef (x,y,z) = case blockRef (x,y,z) of
    (0,_) -> []
    (t,_) -> map (\(o,_) -> (material t o, blockFace (x,y,z) o)) $
             filter ((\(t',_) -> t /= t' && IS.notMember (fromIntegral t') solidIDs) . blockRef . snd) $
             filter (\(s,_) -> y /= 0 || s /= B || bottom options)
                 [ (S, (x+1,y,z)), (N, (x-1,y,z)), (T, (x,y+1,z))
                 , (B, (x,y-1,z)), (W, (x,y,z+1)), (E, (x,y,z-1))]

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