module CNC where
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.State
import Data.List
import Control.Monad.Writer


--main = do CNC.test

type CNCWriter = Writer [Command] Command

data Position = --Position Axe Double
		X Double 
	|	Y Double
	| 	Z Double
	|	I Double
	|	J Double

instance Show Position where
	show (X val) = "X" ++ show val
	show (Y val) = "Y" ++ show val
	show (Z val) = "Z" ++ show val
	show (I val) = "I" ++ show val
	show (J val) = "J" ++ show val

data Command = 
		G0 [Position]
	| 	G1 [Position]
	|	G2 [Position]
	|	G3 [Position]
	|	F Double

instance Show Command where
	show (G0 l) = "G0 " ++ arr2Str l
	show (G1 l) = "G1 " ++ arr2Str l
	show (G2 l) = "G2 " ++ arr2Str l
	show (G3 l) = "G3 " ++ arr2Str l
	show (F l) =  "F " ++ show l	

arr2Str:: [Position] -> String
arr2Str [] = ""
arr2Str l = concat $ intersperse " " $ map show l


-- КОМАНДЫ ЧПУ

g1 :: [Position] -> CNCWriter
g1 par = writer (aa, [aa]) where aa = G1 par

g0 :: [Position] -> CNCWriter
g0 par = writer (aa, [aa]) where aa = G0 par

f :: Double -> CNCWriter
f par = writer (aa, [aa]) where aa = F par



--выводит последовательность команд в консоль
export f = mapM_ (putStrLn.show) y
	where (_, y) = runWriter f

--преобразовывает последовательность команд в строку
export' f =	unlines $ map (show) y
	where (_, y) = runWriter f

test = defaultMain CNC.tests

tests :: TestTree
tests = testGroup "CNC" [checkOut, checkCommands, checkParams]

checkOut = testGroup "Тестируем Вывод"
	[
		testCase "01: G1" 	 $ export' t1 @?= "G1 X100.0\n",
		testCase "02: G0 G1" $ export' t2 @?= "G0 X0.0 Z0.0\nG1 X100.0\n",
		testCase "03: F" 	 $ export' t3 @?= "F 1000.0\nG0 X0.0 Z0.0\nG1 X100.0\n"
	]
	where 
		t1 = do 
			g1 [X 100]
		t2 = do
			g0 [X 0, Z 0]
			g1 [X 100]
		t3 = do
			f 1000
			g0 [X 0, Z 0]
			g1 [X 100]

checkCommands = testGroup "Названия команд"
	[
		testCase "G0" $ show (G0 []) 	@?= "G0 ",
		testCase "G1" $ show (G1 []) 	@?= "G1 ",
		testCase "G2" $ show (G2 []) 	@?= "G2 ",
		testCase "G3" $ show (G3 [])	@?= "G3 ",
		testCase "F" $  show (F 100)	@?= "F 100.0"
	]

checkParams = testGroup "Параметры команд"
	[
		testCase "G0 " $ show( G0 [X 100, Y 200, Z 300]) 	@?= "G0 X100.0 Y200.0 Z300.0",
		testCase "G1 1" $ show( G1 [X 100, Y 200, Z 300]) 	@?= "G1 X100.0 Y200.0 Z300.0",
		testCase "G1 2" $ show( G1 [X 100, Z 300]) 			@?= "G1 X100.0 Z300.0",
		testCase "F ..."   $ show( F 123 )					@?= "F 123.0",
		testCase "G2 1" $ show( G2 [X 100, Y 200, I 150]) 	@?= "G2 X100.0 Y200.0 I150.0",
		testCase "G3 1" $ show( G3 [X 100, Y 200, I 150]) 	@?= "G3 X100.0 Y200.0 I150.0"
	]			