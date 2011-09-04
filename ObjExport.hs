{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module ObjExport (export, ExportOptions(..)) where

import Control.Arrow
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List
import qualified Data.List.Key as K
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S
import System.FilePath
import System.IO
import Text.Printf
import Region

data Orientation = E | W | N | S | T | B deriving (Enum, Eq, Show)
data ExportOptions = ExportOptions { showBottom :: Bool, showSides :: Bool, yFrom :: Int, yTo :: Int }

type TexCoord = (Double, Double)
type Vertex = (Double, Double, Double)
type Face = [(Vertex, TexCoord, Vertex)]
type Coord = (Int, Int)
type Chunk = (B.ByteString, B.ByteString)
type BorderedChunk = (Maybe Chunk, Maybe Chunk, Maybe Chunk, Maybe Chunk, Maybe Chunk)
type Neighbors = (Int, Int, Int, Int, Int, Int, Int)
type BlockDefs = I.IntMap (Int -> Neighbors -> [(String, Face)])
type Indexes = (M.Map Vertex Int, M.Map TexCoord Int, M.Map Vertex Int)

chunkW, chunkH :: Int
(chunkW, chunkH) = (16, 128)

export :: ExportOptions -> BlockDefs -> FilePath -> FilePath -> [Coord] -> IO ()
export options blockDefs regionDir file chunks = do
    h <- openFile file WriteMode
    B.hPutStrLn h "mtllib minecraft.mtl\n"
    world <- loadWorld regionDir chunks
    mapM_ (\(i,c) -> B.hPutStrLn h =<< chunkGeom options blockDefs c i) world
    hClose h
    putStrLn "Done."

loadWorld :: FilePath -> [Coord] -> IO [(Coord, BorderedChunk)]
loadWorld regionDir cs = do
    let regionID (cX,cZ) = (div cX 32, div cZ 32)
    let neededChunks = S.fromList cs
    regions <- mapM (\(rX,rZ) -> fmap ((,) (rX,rZ) . I.map chunkData) $
                                 loadRegion (regionDir </> printf "r.%d.%d.mcr" rX rZ)) .
               S.toList $ S.map regionID neededChunks
    let bordered = M.fromList $ map (\c -> (c, (Nothing, Nothing, Nothing, Nothing, Nothing))) cs

    return $ M.assocs $ foldl' (\m ((rX,rZ), r) ->
            S.fold (\(cX,cZ) a -> let chunk = I.lookup (mod cX 32 + 32 * mod cZ 32) r in
                  M.adjust (\(_,n,e,s,w) -> (chunk,n,e,s,w)) (cX,cZ) $
                  M.adjust (\(c,_,e,s,w) -> (c,chunk,e,s,w)) (cX+1,cZ) $
                  M.adjust (\(c,n,_,s,w) -> (c,n,chunk,s,w)) (cX,cZ+1) $
                  M.adjust (\(c,n,e,_,w) -> (c,n,e,chunk,w)) (cX-1,cZ) $
                  M.adjust (\(c,n,e,s,_) -> (c,n,e,s,chunk)) (cX,cZ-1) a
            ) m $ S.filter ((== (rX,rZ)) . regionID) neededChunks
        ) bordered regions

chunkData :: Tag -> Chunk
chunkData nbt = (getArray $ navigate ["Level", "Blocks"] nbt,
                 getArray $ navigate ["Level", "Data"] nbt)
    where getArray ~(Just (TAG_Byte_Array a)) = a

chunkGeom :: ExportOptions -> BlockDefs -> BorderedChunk -> (Int, Int) -> IO BC.ByteString
chunkGeom options blockDefs (c,n,e,s,w) (cX,cZ) = do
    let blockLookup (x,y,z) = if y < yFrom options || y > yTo options then (if showBottom options then (0,0) else (1,0)) else
            let i = y + mod z chunkW * chunkH + mod x chunkW * chunkW * chunkH
            in  maybe (if showSides options then (0,0) else (1,0))
                      ((fromIntegral . flip B.index i) ***
                       (fromIntegral . (if odd i then flip div 16 else flip mod 16) . flip B.index (div i 2))) $
                    case (compare cX $ div x chunkW, compare cZ $ div z chunkW) of
                         (LT,_) -> s; (GT,_) -> n; (_,LT) -> w; (_,GT) -> e; _ -> c
    let faces = concat [ maybe [] (\f -> map (second $ map (\((vx,vy,vz),vt,vn) -> ((vx-fromIntegral z,vy-fromIntegral x,vz+fromIntegral y),vt,vn))) $
                           f (snd $ blockLookup (x,y,z)) (bc,bn,be,bs,bw,bt,bb))
                           (I.lookup (fst $ blockLookup (x,y,z)) blockDefs)
                       | x' <- [0..chunkW - 1]
                       , y' <- [max 0 (yFrom options)..min (chunkH - 1) (yTo options)]
                       , z' <- [0..chunkW - 1]
                       , let (x,y,z) = (cX * chunkW + x', y', cZ * chunkW + z')
                       , let [bc,bn,be,bs,bw,bt,bb] = map (fst . blockLookup)
                                [(x,y,z), (x-1,y,z), (x,y,z-1), (x+1,y,z), (x,y,z+1), (x,y+1,z), (x,y-1,z)]]
    let ((!vm, !tm, !nm), !geom) = foldl' (\a matGroup -> second (BC.pack (printf "\nusemtl %s\n" . fst $ head matGroup) `B.append`) $
            foldr (\(_, f) (vs, gs) -> addFace (vs, gs) f) a matGroup) ((M.empty, M.empty, M.empty), B.empty) $ groupOn fst faces
    putStrLn $ printf "Processing chunk (%d, %d)" cX cZ
    return $! B.concat [BC.pack $ printf "g chunk.%d.%d\n\n" cX cZ,
                        indexString "v"  (\(x,y,z) -> [x,y,z]) vm, 
                        indexString "vt" (\(x,y)   -> [x,y]  ) tm,
                        indexString "vn" (\(x,y,z) -> [x,y,z]) nm,
                        geom]

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f = K.group f . K.sort f

indexString :: BC.ByteString -> (a -> [Double]) -> M.Map a Int -> BC.ByteString
indexString prefix toCoords = BC.unlines . map (BC.unwords . (prefix :) .
        map (\x -> BC.pack $ printf (if abs (fromIntegral (round x) - x) < 0.005 then "%.0f" else "%.3f") x) . toCoords . fst) .
    K.sort (negate . snd) . M.assocs

addFace :: (Indexes, B.ByteString) -> Face -> (Indexes, B.ByteString)
addFace ((!vs, !ts, !ns), !gs) = second
    ((`B.append` gs) . (`B.append` "\n") . BC.intercalate " " . ("f" :)) .
    foldr (\(v,t,n) ((vm, tm, nm), is) ->
        let (vm',vi) = getIndex v vm
            (tm',ti) = getIndex t tm
            (nm',ni) = getIndex n nm
        in  ((vm', tm', nm'), BC.intercalate "/" (map (BC.pack . show) [-vi,-ti,-ni]) : is)) ((vs, ts, ns), []) where
    getIndex x xm = maybe (M.insert x (M.size xm + 1) xm, M.size xm + 1) ((,) xm) $ M.lookup x xm