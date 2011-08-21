import ObjExport

main :: IO ()
main = export "e:/mc2obj/regions" "e:/mc2obj/test.obj" [(x,z) | x <- [0..7], z <- [0..7]]