module Meta where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Writer
import Control.Applicative
import CNC
import Common
import Data.Maybe
import Geometry

main = do Meta.test
--main = export $ meta2CNC f1

data MetaPoint = 
	MPoint Double Double  --обычная точка
  | MRound Double Double Double  --скругление угла
  | MRadius Double Double Double Orientation --полноценная окружность
  deriving (Show)

point :: Double -> Double -> CNCMeta --простой угол
point x y = write $ MPoint x y 

rounding :: Double -> Double -> Double -> CNCMeta --скругленный угол
rounding x y r = write $ MRound x y r

radius :: Double -> Double -> Double -> Orientation -> CNCMeta --скругленный угол
radius x y r o = write $ MRadius x y r o

type CNCMeta = Writer [MetaPoint] MetaPoint

connect :: MetaPoint -> MetaPoint -> MetaPoint -> CNCWriter
connect _ (MPoint x y) _ = g1 [X x, Y y] -- по центру - точка

connect p1 r2@(MRadius x2 y2 _ _) p3 = do -- по центру - радиус
	g1 [X c_x1, Y c_y1] 
	g2 [X c_x2, Y c_y2, I (x2 - c_x1), J (y2 - c_y1)]  --рисуем дугу
    where
   		Pnt c_x1 c_y1 = connectLeft p1 r2
   		Pnt c_x2 c_y2 = connectRight r2 p3

--   *******************************
--       вычисляем начало сегмента
--   *******************************
connectLeft :: MetaPoint -> MetaPoint -> Pnt
connectLeft (MPoint x1 y1) (MRadius x2 y2 r2 o2) = Pnt c_x1 c_y1
	where (Pnt c_x1 c_y1:_) = contactPoints (Pnt x1 y1) (Circle (Pnt x2 y2) r2)

connectLeft (MRadius x1 y1 r1 o1) (MRadius x2 y2 r2 o2) = Pnt c_x c_y
	where 
		c1 = (Circle (Pnt x1 y1) r1)
		c2 = (Circle (Pnt x2 y2) r2)
		(LinePP (Pnt c_x c_y) _:_) = contactLines c2 c1

--   *******************************
--        вычисляем конец сегмента
--   *******************************
connectRight :: MetaPoint -> MetaPoint -> Pnt
connectRight (MRadius x1 y1 r1 o1) (MPoint x2 y2) = Pnt	c_x2 c_y2
	where [ _ , Pnt c_x2 c_y2] = contactPoints (Pnt x2 y2) (Circle (Pnt x1 y1) r1)

connectRight c1@(MRadius x1 y1 r1 o1) c2@(MRadius x2 y2 r2 o2) = Pnt c_x3 c_y3
	where 
		c1 = (Circle (Pnt x1 y1) r1)
		c2 = (Circle (Pnt x2 y2) r2)
		(LinePP _ (Pnt c_x3 c_y3):_) = contactLines c2 c1


--переводит MetaТраектории в команды ЧПУ
meta2CNC f = do
	res
	write first
	where
		(_, y) = runWriter f
		res = parse (last y) y [] 
		(_, (first:_)) = runWriter res --это нужно, чтобы в конце замкнуть траекторию
		parse prev curr acc
			| [] <- curr = return () -- кончились точки
			| otherwise = do
				connect prev x $ after xs acc
				parse x xs (acc ++ [x])
			where
				(x: xs) = curr

		after l acc --берем следующую значимую точку
			| (x:_) <- l = x --следующая точка - не MZ
			| [] <- l, (y:_) <- acc = y


test = defaultMain Meta.tests

tests :: TestTree
tests = testGroup "Meta" [checkSimple]

checkSimple = testGroup "Тестируем Мета-траектории"
	[
		 testCase "01: точки" 	 $  					export' (meta2CNC meta1) @?= meta1_res
		,testCase "02: точка-радиус CW-точка" $ 		export' (meta2CNC meta2) @?= meta2_res			
		,testCase "03: точка-радиус CW-радиус CW" $ 	export' (meta2CNC meta3) @?= meta3_res
		,testCase "04: радиус в начале" $ 				export' (meta2CNC meta4) @?= meta4_res
		--TODO: сделать радиусы в другую сторону
		--,testCase "05: радиус-радиус-точка" $ True @?= False
		--,testCase "06: радиус-радиус-радиус" $ True @?= False
		--,testCase "06: точка-отрицат.радиус-точка" $ True @?= False
	]
	where 
		meta1 = do point 1 0 >> point 50 50 >> point 100 0	
		meta1_res = "G1 X1 Y0\nG1 X50 Y50\nG1 X100 Y0\nG1 X1 Y0\n"

		meta2 = do point 1 0 >> point 0 50 >> radius 100 25 25 CW
		meta2_res = "G1 X1 Y0\nG1 X0 Y50\nG1 X100 Y50\nG2 X100 Y0 I0 J-25\nG1 X1 Y0\n"

		meta3 = do point 0 0 >> point 0 50 >> radius 100 40 10 CW >> radius 90 20 20 CW
		meta3_res = "G1 X0 Y0\nG1 X0 Y50\nG1 X100 Y50\nG2 X110 Y40 I0 J-10\nG1 X110 Y20\nG2 X90 Y0 I-20 J0\nG1 X0 Y0\n"

		meta4 = do radius 0 20 20 CW >> point 100 40 >> point 100 0
		meta4_res = "G1 X0 Y0\nG2 X0 Y40 I0 J20\nG1 X100 Y40\nG1 X100 Y0\nG1 X0 Y0\n"

--TODO: потестить: незамкнутая траектория, где последняя точка - радиус