module Meta where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Writer
import CNC
import Common
import Data.Maybe

main = do Meta.test
--main = export $ meta2CNC f1

data MetaPoint = 
	MPoint Double Double  --обычная точка
  | MRound Double Double Double  --скругление угла
  | MRadius Double Double Double --полноценная окружность
  | MZ -- замкнуть
  deriving (Show)

point :: Double -> Double -> CNCMeta --простой угол
point x y = write $ MPoint x y 

z :: CNCMeta -- команда "замкнуть контур"
z = write MZ 

rounding :: Double -> Double -> Double -> CNCMeta --скругленный угол
rounding x y r = write $ MRound x y r

radius :: Double -> Double -> Double -> CNCMeta --скругленный угол
radius x y r = write $ MRadius x y r

type CNCMeta = Writer [MetaPoint] MetaPoint

--TODO добавить сюда Z и safeZ для
meta2CNC f = parse Nothing y []
	where
		(_, y) = runWriter f
		parse prev curr acc
			| [] <- curr = do return () -- кончились точки

			| (x: xs) <- curr, MPoint xx yy <- x = do --простая точка. Что было до и после - неважно
				g1xy xx yy
				parse (Just x) xs (acc ++ [x])

			| (x:xs) <- curr, MRadius xx yy rr <-x, 			--радиус
				Just (MPoint x_prev y_prev) <- prev, 			--до этого была вершина
				Just (MPoint x_past y_past) <- after xs acc = do 	--после этого - опять вершина
				--расчитываем  точки соприкосновения с окружностью
				g0 [X xx, Y yy]
				g0 [X xx, Y (yy - 10)]
				parse (Just x) xs (acc ++ [x])

			| (MZ:xs) <- curr, (MPoint x y:_) <- acc = do --замыкаемся на точку
				g1xy x y
				parse Nothing xs []
			where 
				after (x:xs) acc --берем следующую значимую точку
					| MZ <- x, (y:_) <- acc = Just y --замкнулось, возвращаем первую точку из аккумулятора
					| otherwise = Just $ x --следующая точка - не MZ


test = defaultMain Meta.tests

tests :: TestTree
tests = testGroup "Meta" [checkSimple]

checkSimple = testGroup "Тестируем Мета-траектории"
	[
		testCase "01: точки" 	 $ export' t1 @?= "G1 X0.0 Y0.0\nG1 X50.0 Y50.0\nG1 X100.0 Y0.0\n",
		testCase "02: замкнутая" $ export' t2 @?= "G1 X0.0 Y0.0\nG1 X50.0 Y50.0\n"
											   ++ "G1 X100.0 Y0.0\nG1 X0.0 Y0.0\n",
		testCase "03: радиус"$ export' t3 @?= "G1 X0.0 Y0.0\nG1 X0.0 Y50.0\nG1 X100.0 Y50.0\n"
											   ++ "G2 X100.0 Y0.0 I0.0 J-25.0\nG1 X0.0 Y0.0\n"											   
	]
	where 
		meta1 = do
			point 0 0
			point 50 50
			point 100 0
		meta2 = do
			point 0 0
			point 50 50
			point 100 0
			z
		meta3 = do
			point 0 0
			point 0 50
			radius 100 25 25
			z
		t1 = do	meta2CNC meta1
		t2 = do meta2CNC meta2
		t3 = do meta2CNC meta3
		
--TODO: потестить: незамкнутая директория, где последняя точка - радиус