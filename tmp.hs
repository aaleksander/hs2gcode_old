import Hs2gcode.Commands
import Hs2gcode.Primitives
import Hs2gcode.Meta
import Graphics.Gloss

sz = 5 -- безопасная высота
main_path = run 0 0 sz 
			++ z_cut (rect 0 0 50 100) 0 (-10) 3 200 1000
			++ 	run 0 0 sz 
			++ 	rect 0 0 60 110

path = 	
		meta meta1
--	++	run 0 0 sz
--	++	f' 1000
--	++ 	z_cut main_path (-1) (-10) 5 500 1300
--	++	run 0 0 sz




meta1 = [
	Pnt 100 100,
	Pnt 200 100,
	Pnt 200 300,
	Pnt 100 300
	]


main = do
--	export $ meta_tmp meta1
	export $ meta meta1
--	display (InWindow "My Window" (1024, 768) (100, 100)) white (gcode2Picture [])
--	putStr $ show $ uniq l1 l2
--	putStr "G21\n"
--	putStr $ show $ toTriple meta1
--	export $ meta_tmp meta1
--	putStr "\n"
--	putStr $ show $ gcodeOptimize $ meta_tmp meta1
--	putStr "\nm2"
	putStr "\n"
