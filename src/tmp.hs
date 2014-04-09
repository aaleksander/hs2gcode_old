--import Control.Monad.Writer

--data Command = G0 | G1 | F | Comment String

--g0 :: CNCWriter
--g0 = write G0

--g1 :: CNCWriter
--g1 = write G1

--f :: CNCWriter
--f  = write F

--comment :: String -> CNCWriter
--comment s = write $ Comment s
--instance Show Command where
--	show (Comment l) = "(" ++ l ++ ")"
--	show G0 = "G0"
--	show G1 = "G1"
--	show F = "F"

--type CNCWriter = Writer [Command] Command

--write f = writer (aa, [aa]) where aa = f

--export f = mapM_ (putStrLn.show) y
--	where (_, y) = runWriter f

--func1 = do
--	comment "начало функции"
--	f
--	g0
--	g1 
--	comment "конец"

--connect '0' '1' '0' = do comment "единичка между нулями"
--connect '0' '0' '0' = do comment "сплошный нули"
--connect '1' '0' '0' = do comment "один ноль ноль"
--connect '0' '0' '1' = do comment "ноль ноль один"

--meta2CNC :: String -> CNCWriter
--meta2CNC y = do
--	res
--	write first --замыкаем
--	where
--		res = parse (last y) y [] 
--		(_, (first:_)) = runWriter res --это нужно, чтобы в конце замкнуть траекторию
--		parse prev curr acc
--			| [] <- curr = return () -- кончились точки
--			| otherwise = do
--				connect prev x $ after xs acc
--				parse x xs (acc ++ [x])
--			where
--				(x: xs) = curr
--		after l acc --берем следующую значимую точку
--			| (x:_) <- l = x --следующая точка - не MZ
--			| [] <- l, (y:_) <- acc = y

--main = 
--	export $ meta2CNC "01100"




{--Главная функция--}
froots :: Float -> Float -> [Float]
froots a b | (a<b) = map iter (otd a b)
           | otherwise = error "a bolshe b"
 
{--Уточнение корней методом итераций--}
iter :: (Float, Float) -> Float
iter (x, y) = if (abs(x-f(x))<0.0001) then f x else iter $ (f x, y)
 
{--Отделение корней--}
otd :: Float -> Float -> [(Float, Float)]
otd a b = [(a1, a1+0.01) | a1 <- [a,a+0.01 .. b-0.01], (f a1)*(f(a1+0.01))<0]
 
{--Вычисление значения функции в заданной точке--}
f :: Float -> Float
f x = 1.2/log(x)	


main = putStrLn "1"