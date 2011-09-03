--import System.Console.CmdArgs.Implicit
import ObjExport

main :: IO ()
main = do putStrLn "Compiling block data..."
          blockDefs <- loadBlockDefs "blocks.hs"
          exportWith (defaultOptions { yFrom = 60, yTo = 127 })
                     blockDefs
                     "mc2obj test/region" "e:/mc2obj/test.obj"
                     [(x,z) | x <- [0..1], z <- [0..1]]