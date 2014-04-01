module Commands where
import Test.Tasty
import Test.Tasty.HUnit
import Data.List
import CNC

main = Commands.test

--тут всякие макрокоманды
down:: Double -> CNCWriter
down z = do
	g1 [Z (-z)]

up :: Double -> CNCWriter
up z = do
	g0 [Z z]

--генерит диапазон с четкими границами
--шаг должен быть всегда положительный (он сам определит, в какую сторону интерполировать)
for z1 z2 step f
	| z2 > z1 = mapM_ f $ _gen1 (_rnd z1) []
	| z2 < z1 = mapM_ f $ _gen2 (_rnd z1) []
	where 
		_gen1 curr_z acc 
			| curr_z >= z2 = acc ++ [_rnd z2] -- вышли за пределы массива
			| curr_z < z2 = _gen1 (_rnd (curr_z + step)) (acc ++ [_rnd curr_z]) -- генерим массив
		_gen2 curr_z acc 
			| curr_z <= z2 = acc ++ [z2] -- вышли за пределы массива
			| curr_z > z2 = _gen2 (_rnd (curr_z - step)) (acc ++ [_rnd curr_z]) -- генерим массив
		_rnd x = (fromIntegral (round (x*1000.0)))/1000.0

--ТЕСТЫ

test = defaultMain Commands.tests

tests :: TestTree
tests = testGroup "Commands" [checkMoving]

checkMoving = testGroup "Перемещения"
	[
		testCase "01: up" 	 $ export' t1 @?= "G0 Z10.0\n",
		testCase "02: down"  $ export' t3 @?= "G1 Z-10.0\n",
		testCase "03: for"	 $ export' t2 @?= "G0 X1.0\nG0 X2.0\nG0 X3.0\n",
		testCase "04: for"   $ export' t4 @?= "G0 X1.0\nG0 X1.5\nG0 X2.0\nG0 X2.5\nG0 X3.0\n",
		testCase "05: for"   $ export' t5 @?= "G0 X0.0\nG0 X0.3\nG0 X0.6\nG0 X0.9\nG0 X1.0\n",
		testCase "06: for"   $ export' t6 @?= "G0 X0.0\nG0 X-0.3\nG0 X-0.6\nG0 X-0.9\nG0 X-1.0\n"
	]
	where 
		f x = g0[X x]
		t1 = do	up 10
		t2 = do	for 1 3 1 f --обычный цикл
		t3 = do	down 10
		t4 = do	for 1 3 0.5 f
		t5 = do for 0 1 0.3 f
		t6 = do for 0 (-1) 0.3 f
