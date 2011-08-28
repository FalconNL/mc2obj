{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module ObjExport (defaultOptions, export, exportWith, loadBlockDefs,
    bottom, sides, yFrom, yTo) where

import Control.Arrow
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import qualified Data.List.Key as K
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.Interpreter
import System.FilePath
import System.IO
import Text.Printf
import Region

data Orientation = E | W | N | S | T | B deriving (Enum, Eq, Show)
data ExportOptions = ExportOptions { bottom :: Bool, sides :: Bool, yFrom :: Int, yTo :: Int }

type Location = (Int, Int, Int)
type Vertex = (Double, Double, Double)
type Face = [(Vertex, Int, Int)]
type Coord = (Int, Int)
type Chunk = (B.ByteString, B.ByteString)
type BorderedChunk = (Maybe Chunk, Maybe Chunk, Maybe Chunk, Maybe Chunk, Maybe Chunk)
type BlockDefs = ([(Double, Double)], [Vertex], I.IntMap (Int -> (Int, Int, Int, Int, Int, Int, Int) -> [(String, Face)]))

chunkW, chunkH :: Int
(chunkW, chunkH) = (16, 128)

export :: BlockDefs -> FilePath -> FilePath -> [Coord] -> IO ()
export = exportWith defaultOptions

exportWith :: ExportOptions -> BlockDefs -> FilePath -> FilePath -> [Coord] -> IO ()
exportWith options blockDefs regionDir file chunks = do
    h <- openFile file WriteMode
    B.hPutStrLn h "mtllib minecraft.mtl\n"
    world <- loadWorld regionDir chunks
    mapM_ (\(i,c) -> B.hPutStrLn h =<< chunkGeom options blockDefs c i) world
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

chunkGeom :: ExportOptions -> BlockDefs -> BorderedChunk -> (Int, Int) -> IO BC.ByteString
chunkGeom options (texcoords, normals, blockDefs) (c,n,e,s,w) (cX,cZ) = do
    let (verts, geom) = foldl' (\a matGroup -> second (BC.pack (printf "\nusemtl %s\n" . fst $ head matGroup) `B.append`) $
                foldr (\(_, f) (vs, gs) -> addFace (vs, gs) f) a matGroup) (M.empty, B.empty) .
                groupOn fst $ concat [ maybe [] (\f -> map (second $ map (\((vx,vy,vz),vt,vn) -> ((vx-fromIntegral z,vy-fromIntegral x,vz+fromIntegral y),vt,vn))) $ f (snd $ blockLookup (x,y,z))
                                         ((fst $ blockLookup (x,y,z)), (fst $ blockLookup (x-1,y,z)), (fst $ blockLookup (x,y,z-1)),
                                          (fst $ blockLookup (x+1,y,z)), (fst $ blockLookup (x,y,z+1)), (fst $ blockLookup (x,y+1,z)), (fst $ blockLookup (x,y-1,z))))
                                         (I.lookup (fst $ blockLookup (x,y,z)) blockDefs)
                                     | x' <- [0..chunkW - 1]
                                     , y' <- [max 0 (yFrom options)..min (chunkH - 1) (yTo options)]
                                     , z' <- [0..chunkW - 1]
                                     , let (x,y,z) = (cX * chunkW + x', y', cZ * chunkW + z')]
    let vertices = B.concat . M.elems $ M.foldrWithKey (\(x,y,z) i vs ->
            M.insert (-i) (BC.intercalate " " $ "v" : map (BC.pack . printf "%.3f") [x,y,z] ++ ["\n"]) vs) M.empty verts
    putStrLn $ printf "Processing chunk (%d, %d)" cX cZ
    return $! B.concat [BC.pack $ printf "g chunk.%d.%d\n\n" cX cZ, vertices,
        BC.pack $ (\(tx,ty) -> printf "vt %.3f %.3f\n" tx ty) =<< texcoords,
        BC.pack $ (\(nx,ny,nz) -> printf "vn %.3f %.3f %.3f\n" nx ny nz) =<< normals, geom]
    where
        blockLookup :: Location -> (Int, Int)
        blockLookup (x,y,z) = if y < yFrom options || y > yTo options then (if bottom options then (0,0) else (1,0)) else
            let i = y + mod z chunkW * chunkH + mod x chunkW * chunkW * chunkH
            in  maybe (if sides options then (0,0) else (1,0))
                      ((fromIntegral . flip B.index i) ***
                       (fromIntegral . (if even i then flip div 16 else flip mod 16) . flip B.index (div i 2))) $
                    case (compare cX $ div x chunkW, compare cZ $ div z chunkW) of
                         (LT,_) -> s
                         (GT,_) -> n
                         (_,LT) -> w
                         (_,GT) -> e
                         _      -> c

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f = K.group f . K.sort f

addFace :: (M.Map Vertex Int, B.ByteString) -> Face -> (M.Map Vertex Int, B.ByteString)
addFace (!vs, !gs) f = (second
    (flip B.append gs . BC.intercalate " " . ("f" :) . (++ ["\n"]) .
        zipWith (\(_,ti,ni) vi -> BC.intercalate "/" $ map (BC.pack . show) [-vi,ti,ni]) f)) $
    foldr (\(v,_,_) (m, is) -> maybe (M.insert v (M.size m + 1) m, (M.size m + 1) : is)
        (\i -> (m, i:is)) $ M.lookup v m) (vs, []) f

loadBlockDefs :: FilePath -> IO ([(Double, Double)], [Vertex], I.IntMap (Int -> (Int, Int, Int, Int, Int, Int, Int) -> [(String, Face)]))
loadBlockDefs file = fmap (either (error . show) id) $
    runInterpreter . blocks =<< readFile file

blocks :: String -> Interpreter BlockDefs
blocks file = do
    loadModules ["blocks_util.hs"]
    setImportsQ [("Prelude", Nothing)]
    setTopLevelModules ["BlockUtil"]
    fmap (\(ts,ns,bs) -> (ts,ns,I.fromList $ map (\(i,x) -> (fromIntegral i,x)) bs)) $
        interpret file (as :: ([(Double, Double)], [Vertex], [(Int, Int -> (Int, Int, Int, Int, Int, Int, Int) -> [(String, Face)])]))