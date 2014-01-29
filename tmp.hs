
import Hs2gcode.Commands
import Hs2gcode.Primitives
import Hs2gcode.Meta
import Graphics.Gloss

meta1 = [
	Pnt 100 100,
	Pnt 200 100,
	Pnt 200 300,
	Pnt 100 300
	]

sz = 5 -- безопасная высота
main_path = 	run 0 0 sz 
			++	rect 0 0 50 100 
			++ 	run 0 0 sz 
			++ 	rect 0 0 60 110

path = 	
		meta meta1
--	++	run 0 0 sz
--	++	f' 1000
--	++ 	z_cut main_path (-1) (-10) 5 500 1300
--	++	run 0 0 sz


split_path :: [MetaPnt] -> [(MetaPnt, MetaPnt, MetaPnt)]
split_path list = undefined

toTriple :: [a] -> [(a, a, a)]
toTriple l = zip3 (shift_right l) l (shift_left l)
	where 	
		shift_left l = tail l ++ take 1 l
		shift_right l = last l : init l

meta_tmp :: [MetaPnt] -> [Command]
meta_tmp list = foldr (++) [] [oneSegment x | x <- toTriple list]
	where oneSegment (prev, curr, next)
		| 	(Pnt cx cy) <- curr, (Pnt nx ny) <- next 
				= g1' [_x nx, _y ny]

gcode2Picture :: [Command] -> Picture
gcode2Picture list = _ggg list []
	where
		_ggg (com:xs) acc
			| [] <- xs = Pictures ((coord2Line com) : acc )
			| (G0 (Params par)) <- com = _ggg xs (acc ++ [coord2Line par])

coord2Line :: [Coord] -> (Double, Double)
coord2Line (c1: c2: [])
	| (X vx) <- c1, (Y vy) <- c2 = (vx, vy)
	| (Y vy) <- c1, (X vx) <- c2 = (vx, vy)

--	Pictures [ Line [(0, 0), (100, 100), (200, 100.5)] ]

--pic = Pictures [ Line [(0, 0), (100, 100), (200, 100.5)] ]

main = do
	-- export main_path
	display (InWindow "My Window" (1024, 768) (100, 100)) white (gcode2Picture [])
--	putStr $ show $ uniq l1 l2
--	putStr "G21\n"
--	putStr $ show $ toTriple meta1
--	putStr $ show $ meta_tmp meta1
--	putStr "\n"
--	putStr $ show $ gcodeOptimize $ meta_tmp meta1
--	putStr "\nm2"
	putStr "\n"
