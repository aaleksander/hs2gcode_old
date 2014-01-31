import Hs2gcode.Commands
import Hs2gcode.Primitives
import Hs2gcode.Viewer2D

import Control.Monad.State

path = [ f 100, g1 [X 100, Y 100], g1 [X 200], g1 [Y 300], g1 [X 100], g1 [Y 100] ]

hasAxe :: Position -> [Position] -> Bool
hasAxe _ [] = False
hasAxe (X v) ((X _):xs) = True || hasAxe (X v) xs
hasAxe (Y v) ((Y _):xs) = True || hasAxe (Y v) xs
hasAxe (Z v) ((Z _):xs) = True || hasAxe (Z v) xs

--есть список координат, мы задаем отправляем новые координаты и получаем новую позицию станка
updatePosition :: [Position] -> [Position] -> [Position]
updatePosition state [] = state
updatePosition state (x:xs)
	| not ( hasAxe x state ) = state ++ [x] ++ updatePosition state xs
	| hasAxe x state = updatePosition state xs

--задаем  станку с такой-то позицией команду и получаем новую позицию
cnc :: [Position] -> Command -> [Position]
cnc pos (G0 list) = updatePosition pos list
cnc pos (G1 list) = updatePosition pos list
cnc pos _ = pos

--получает список команд и возвращает список позиций в которых побывает станок
simulate :: [Command] -> [[Position]]
simulate [] = []
simulate (x:xs) = _sim [] x : simulate xs
	where _sim list com = cnc list com

main = do
	mapM_ (putStr.(++"\n").show) $ simulate path
	putStr "\n"

	putStr "The end\n"	
