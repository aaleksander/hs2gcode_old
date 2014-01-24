
import Hs2gcode.Commands
import Hs2gcode.Primitives

sz = 5 -- безопасная высота

main_path = rect 0 0 50 100 ++ run 0 0 sz ++ rect 0 0 60 110

path = 
		run 0 0 sz
	++ 	z_cut main_path (-1) (-10) 5 500 1300
	++	run 0 0 sz

main = do
	putStr "G21\n"
	export path
	putStr "m2"
	putStr "\n"

