module Hs2gcode.Primitives where


import Hs2gcode.Commands

--обойти прямоугольник по контуру
rect :: Double -> Double -> Double -> Double -> [Command]
rect xx yy xs ys = --откуда начать, ширина, длина
	[		
		g1 [y (yy + ys)],
		g1 [x (xx + xs)],
		g1 [y yy],
		g1 [x xx]
	]

rect_area :: Double -> Double -> Double -> Double -> Double -> [Command]
rect_area xx yy xs ys step= 
	case xs > ys of
	True -> rect xx yy xs ys
	False -> rect xx yy 10 10

--генерит список с четкими границами
--шаг должен быть всегда положительный (он сам определит, в какую сторону интерполировать)
gen_list :: Double -> Double -> Double -> [Double]
gen_list z1 z2 step
	| z2 > z1 = _gen1 z1 []
	| z2 < z1 = _gen2 z1 []
	where 
		_gen1 curr_z acc 
			| curr_z >= z2 = acc ++ [z2] -- вышли за пределы массива
			| curr_z < z2 = _gen1 (curr_z + step) (acc ++ [curr_z]) -- генерим массив
		_gen2 curr_z acc 
			| curr_z <= z2 = acc ++ [z2] -- вышли за пределы массива
			| curr_z > z2 = _gen2 (curr_z - step) (acc ++ [curr_z]) -- генерим массив


-- Вгрызается командами в заготовку
z_cut :: [Command] -> Double -> Double -> Double -> Int -> Int -> [Command]
z_cut list z1 z2 step f1 f2 = 
	concat $ [setF f1 ++ [g1 [z zz]] ++ setF f2 ++ list | zz <- gen_list z1 z2 step]