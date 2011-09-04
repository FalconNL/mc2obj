{-# LANGUAGE DeriveDataTypeable #-}

import qualified Control.Exception as E
import Control.Monad
import System.Console.CmdArgs.Implicit
import System.Directory
import System.FilePath
import System.IO
import BlockDefs
import ObjExport

data Args = Args { objFolder :: FilePath, worldFolder :: FilePath
                 , minY :: Int, maxY :: Int
                 , rect :: Maybe (Int, Int, Int, Int), circle :: Maybe (Int, Int, Int)
                 , nether :: Bool, bottom :: Bool, sides :: Bool
                 } deriving (Show, Data, Typeable)

mc2obj :: Args
mc2obj = Args { objFolder    = "export" &= name "o"    &= help "The location of the .obj file" &= typFile
              , worldFolder  = ""       &= argPos 0 &= typ "WORLD_DIRECTORY"
              , minY         =       0                 &= help "minimum Y coordinate (bedrock = 0)"
              , maxY         =     127                 &= help "maximum Y coordinate (sealevel = 63)"
              , rect         = Nothing  &= name "r"    &= help "rectangle minX,minZ,maxX,maxZ"
              , circle       = Nothing  &= name "c"    &= help "circle centerX,centerZ,radius"
              , nether       =   False  &= name "n"    &= help "output Nether instead of regular world"
              , bottom       =   False  &= name "b"    &= help "output bottom of world"
              , sides        =   False  &= name "s"    &= help "output sides of world"
              } &= summary version

version :: String
version = "mc2obj v0.3\nA program to convert Minecraft worlds to .obj files\nWritten by Remco Niemeijer (2011)"

main :: IO ()
main = do arguments <- E.onException (cmdArgs mc2obj) (hFlush stderr >> putStrLn "\nUse mc2obj -? for help information")
          let chunks = case (rect arguments, circle arguments) of
                            (Just (x1,z1,x2,z2),_) -> [(x,z) | x <- [x1..x2], z <- [z1..z2]]
                            (_,Just (cx,cz,r))     -> [(x,z) | x <- [cx-r..cx+r], z <- [cz-r..cz+r]
                                                             ,fromIntegral ((x-cx)*(x-cx)) + fromIntegral ((z-cz)*(z-cz)) <
                                                              (fromIntegral r+1/2)*(fromIntegral r+1/2)]
                            _ -> []
          let worldName = takeFileName $ worldFolder arguments
          let texFolder = "texsplit/tex"
          createDirectoryIfMissing True $ objFolder arguments
          createDirectoryIfMissing True $ objFolder arguments </> "tex"
          copyFile "mtl/minecraft.mtl" (objFolder arguments </> "minecraft.mtl")
          mapM_ (\x -> copyFile x $ objFolder arguments </> "tex" </> takeFileName x) =<<
              filterM doesFileExist . map (texFolder </>) =<< getDirectoryContents "texsplit/tex"
          case chunks of
               [] -> putStrLn "Error: no region specified or incorrect region"
               cs -> do putStrLn "Compiling block data..."
                        export ExportOptions { showBottom = bottom arguments
                                             , showSides  = sides arguments
                                             , yFrom = minY arguments, yTo = maxY arguments }
                               blockDefs
                               (worldFolder arguments </> if nether arguments then "DIM-1/region" else "region")
                               (objFolder arguments </> worldName <.> "obj") cs