import Hs2gcode.Commands


path = 	[
	f 100
--	g1 [_x 100, _y 100],
--	g1 [_x 200, _y 100],
--	g1 [_x 200, _y 200],
--	g1 [_x 100, _y 200],
--	g1 [_x 100, _y 100]
	]

pathOptimize :: [Command] -> [Command]
pathOptimize list = list

--получает на вход статус станка (где находится) и команду. Возвращает оптимизированную команду
opt :: [Coord] -> Command -> [Coord]
opt list com
	| not (elem (commandType com) [CTG0, CTG1]) = list
	| (G0 (Params coords)) <- com = _update list coords 
--	| (Coord dir v) <- command, length (filter (\(Coord d v) -> d == dir)) > 0 = [Coord dir 11]
	where 
		_update dest (x:xs) 
			| [] <- xs = dest
			| length (filter (\a -> a === x) list) > 0 = []
			-- | otherwise = _update ()

--заменить в списке координат одно значение
replaceCoord::[Coord] -> Dir -> Double -> [Coord]
replaceCoord list dir val = list

main = do	
	putStr $ show $ (replaceCoord [_x 100, _y 100] X 200)

	putStr "\n"
	-- export $ pathOptimize path