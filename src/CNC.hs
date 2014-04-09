module CNC where
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.State
import Data.List
import Control.Monad.Writer
import Common

--main = do CNC.test

type CNCWriter = Writer [Command] Command

data Position = --Position Axe Double
		X Double 
	|	Y Double
	| 	Z Double
	|	I Double
	|	J Double

instance Show Position where
	show (X val) = "X" ++ double2Str val
	show (Y val) = "Y" ++ double2Str val
	show (Z val) = "Z" ++ double2Str val
	show (I val) = "I" ++ double2Str val
	show (J val) = "J" ++ double2Str val

data Command = 
		G0 [Position]
	| 	G1 [Position]
	|	G2 [Position] -- дуга по часовой
	|	G3 [Position] -- дуга против часовой
	|	F Double
	|	Comment String

instance Show Command where
	show (G0 l) = "G0 " ++ arr2Str l
	show (G1 l) = "G1 " ++ arr2Str l
	show (G2 l) = "G2 " ++ arr2Str l
	show (G3 l) = "G3 " ++ arr2Str l
	show (F l) =  "F " ++ double2Str l	
	show (Comment s) = "(" ++ s ++ ")"

arr2Str:: [Position] -> String
arr2Str [] = ""
arr2Str l = concat $ intersperse " " $ map show l


double2Str :: Double -> String
double2Str x
	| abs((fromIntegral cel) - x) < eps = show $ cel
	| otherwise = show $ cncRound x
	where cel = truncate x


-- КОМАНДЫ ЧПУ

g1 :: [Position] -> CNCWriter
g1 par = write $ G1 par

g0 :: [Position] -> CNCWriter
g0 par = write $ G0 par

f :: Double -> CNCWriter
f par = write $ F par

comment :: String -> CNCWriter
comment s = write $ Comment s

g1xy x y = do
	g1[X x, Y y]

g0xy x y = do
	g0[X x, Y y]

g1z z = do g1 [Z z]
g0z z = do g0 [Z z]

g2 :: [Position] -> CNCWriter
g2 par = write $ G2 par

g3 :: [Position] -> CNCWriter
g3 par = write $ G3 par

--выводит последовательность команд в консоль
export f = mapM_ (putStrLn.show) y
	where (_, y) = runWriter f

--преобразовывает последовательность команд в строку
export' f =	unlines $ map (show) y
	where (_, y) = runWriter f

test = defaultMain CNC.tests

tests :: TestTree
tests = testGroup "CNC" [checkOut, checkCommands, checkParams, checkDouble2Str]

checkOut = testGroup "Тестируем Вывод"
	[
		testCase "01: G1" 	 $ export' t1 @?= "G1 X100\n",
		testCase "02: G0 G1" $ export' t2 @?= "G0 X0 Z0\nG1 X100\n",
		testCase "03: F" 	 $ export' t3 @?= "F 1000\nG0 X0 Z0\nG1 X100\n"
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
		testCase "F" $  show (F 100)	@?= "F 100"
	]

checkParams = testGroup "Параметры команд"
	[
		testCase "G0 " $ show( G0 [X 100, Y 200, Z 300]) 	@?= "G0 X100 Y200 Z300",
		testCase "G1 1" $ show( G1 [X 100, Y 200, Z 300]) 	@?= "G1 X100 Y200 Z300",
		testCase "G1 2" $ show( G1 [X 100, Z 300]) 			@?= "G1 X100 Z300",
		testCase "F ..."   $ show( F 123 )					@?= "F 123",
		testCase "G2 1" $ show( G2 [X 100, Y 200, I 150]) 	@?= "G2 X100 Y200 I150",
		testCase "G3 1" $ show( G3 [X 100, Y 200, I 150]) 	@?= "G3 X100 Y200 I150"
	]			

checkDouble2Str = testGroup "Перевод Double -> String"
	[
		testCase "01: 12.345" 	 $ double2Str 12.345 @?= "12.345",
		testCase "02: 12" 	 $ double2Str 12 @?= "12"
		]