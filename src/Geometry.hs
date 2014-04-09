module Geometry where

--всякая тригонометрия
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.List
import Common
--main = do Geometry.test





data Pnt = Pnt Double Double

data Orientation = CW | CCW deriving (Show) --по часовой, против часовой

--instance Show Orientation where
--	show CW = "CW"
--	show CWW = "CWW"

instance Show Pnt where
	show (Pnt x y) = "Pnt(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Pnt where
	Pnt ax ay == Pnt bx by = and [abs(ax - bx)<eps, abs(ay - by)<eps]
	a /= b = not $ a == b

(sub) (Pnt ax ay) (Pnt bx by) = Pnt (ax - bx) (ay - by)

data Circle = Circle Pnt Double

instance Show Circle where
	show (Circle (Pnt x y) r) = "Circle(" ++ show x ++ ", " ++ show y ++ ", " ++ show r ++ ")"

data Line = LinePP Pnt Pnt| LineABC Double Double Double deriving (Eq)

instance Show Line where
	show (LinePP (Pnt x1 y1) (Pnt x2 y2)) = "Line(" 
		++ (concat $ intersperse ", " (map show [x1, y1, x2, y2])) ++ ")" --все параметры через запятую и в скобки
	show (LineABC a b c) = "Line(" 
		++ (concat $ intersperse ", " (map show [a, b, c])) ++ ")" --все параметры через запятую и в скобки



data PointCircleRelation = IN_CIRCLE | ON_CIRCLE | OUT_CIRCLE deriving (Show, Eq) --отношение точки относительно окружности

--преобразует линию на основе двух точек в линию на основе abc
linePP2ABC :: Line -> Line
linePP2ABC (LinePP (Pnt x1 y1) (Pnt x2 y2)) = LineABC a b c
	where 
		a = y2 - y1
		b = x1 - x2
		c = (-a)*x1 - b*y1

--точки касания касательной из точки 'p' к окружности 'c'
contactPoints:: Pnt -> Circle -> [Pnt]
contactPoints p c
	| p `inCircle` c = []
	| p `onCircle` c  = [p]
	| p `outCircle` c = cross_circle (Circle p r) c
	where 
		--pc = pointInCircle p c	
		Circle cp cr = c
		r = sqrt (d^2 - cr^2)
			where d = dist p cp

--где находится точка относительно окружности

inCircle:: Pnt -> Circle -> Bool --лежит ли точка внутри окружности
inCircle p (Circle cp r) = d < r
	where d = dist p cp

onCircle:: Pnt -> Circle -> Bool --лежит ли точка на окружности
onCircle p (Circle cp r) = abs(d - r) < eps
	where d = dist p cp

outCircle:: Pnt -> Circle -> Bool --лежит ли точка вне окружности
outCircle p (Circle cp r) = d > r
	where d = dist p cp

--находит точки пересечения двух окружностей
cross_circle::Circle -> Circle -> [Pnt]
cross_circle c1 c2
	| p1 == p2, abs(r1 - r2) < eps = [] --окружности совпадают
	| otherwise = crossLineCircle (LineABC a b c) c1
	where 
		Circle p1 r1 = c1
		Circle p2 r2 = c2
		Pnt x1 y1 = p1
		Pnt x2 y2 = p2
		a = 2*(x2 - x1)
		b = 2*(y2 - y1)
		c = x1^2 + y1^2 - r1^2 - (x2^2 + y2^2 - r2^2)

--пересечение прямой с окружностью
crossLineCircle :: Line -> Circle -> [Pnt]
crossLineCircle l c
	| crossCount == 0 = []
	| otherwise = [ 
			addVector p (Pnt 0 0) (Pnt (-lb) la) (k/t),
			addVector p (Pnt 0 0) (Pnt lb  (-la)) (k/t) 
			]
	where 
		LineABC la lb lc = l
		Circle cp cr = c
		p = closestPoint l cp -- проекция точки на линию
		d = dist cp p --дистацния от точки до центра		
		t = dist (Pnt 0 0) (Pnt lb (-la))
		crossCount -- количество точек пересечения
			| abs (d - cr) <= eps = 1
			| cr > d 			  = 2
			| otherwise			  = 0
		k 
			| crossCount == 1 = 0
			| otherwise = sqrt(cr^2 - d^2)

contactLines:: Circle -> Circle -> [Line]
contactLines c1 c2 = 
	map (lineABC2PP.changeC.fromJust) 
		$ filter isJust [tangents (p2 `sub` p1) (r1*i) (r2*j) 
						   | j <- [(-1), 1], i <- [(-1), 1]]
	where 
		(Circle p1 r1) = c1
		(Circle p2 r2) = c2
		tangents (Pnt cx cy) r1 r2
			| d1 < (-eps) = Nothing
			| otherwise = Just $ LineABC a b c
			where 
				a = (cx*r + cy*d2)/z
				b = (cy*r - cx*d2)/z
				c = r1
				r = r2 - r1
				z = cx^2 + cy^2
				d1 = z - r^2
				d2 = sqrt $ abs d1
		l = [(-1), 2, 2]
		changeC (LineABC la lb lc) = LineABC la lb (lc - la*p1x - lb*p1y) where Pnt p1x p1y = p1		
		lineABC2PP line = LinePP pp1 pp2
			where 	
				pp1 = first $ crossLineCircle line c1
				pp2 = first $ crossLineCircle line c2
		first (x:_) = x


--добавление заданной части вектора к точке
addVector p p1 p2 k = 
	Pnt (cncRound (px + (p2x - p1x)*k)) (cncRound (py + (p2y - p1y)*k))
	where 
		Pnt px py = p
		Pnt p1x p1y = p1
		Pnt p2x p2y = p2

--проекция точки на прямую
closestPoint :: Line -> Pnt -> Pnt
closestPoint (LineABC la lb lc) (Pnt px py) = Pnt (px - la*k) (py - lb*k)
	where k= (la*px + lb*py + lc)/(la^2 + lb^2)

--дистанция между двумя точками
dist :: Pnt -> Pnt -> Double
dist (Pnt x1 y1) (Pnt x2 y2) = sqrt(xx + yy)
	where
		xx = (x1 - x2)^2
		yy = (y1 - y2)^2



test = defaultMain Geometry.tests

tests :: TestTree
tests = testGroup "Geometry" [checkPoints]

firstE (x:xs) = x

checkPoints = testGroup "Тестируем show"
	[
		testCase "01: точки" 	 $ show (Pnt 0 0) @?= "Pnt(0.0, 0.0)",
		testCase "02: окружность"$ show (Circle (Pnt 1 2) 3) @?= "Circle(1.0, 2.0, 3.0)",
		testCase "03: линии" 	 $ show (LinePP (Pnt 0 0) (Pnt 10 10)) @?= "Line(0.0, 0.0, 10.0, 10.0)",

		testCase "04: point Out Circle" $ (Pnt 0 0) `outCircle` c10_10_3 	@?= True,
		testCase "05: point IN Circle"  $ (Pnt 9 9) `inCircle`  c10_10_3 	@?= True,
		testCase "06: point ON Circle"  $ (Pnt 7 10) `onCircle` c10_10_3	@?= True,

		testCase "07: contactPoints" $ firstE (contactPoints p03 c10_0_3) @?= Pnt 10 3,

		testCase "08: contactLines" $ contactLines c0_10_5 c20_10_5 @?= [	LinePP (Pnt 0.0 5.0) 	(Pnt 20.0 5.0),
																			LinePP (Pnt 2.5 14.33) 	(Pnt 17.5 5.67),
																			LinePP (Pnt 2.5 5.67) 	(Pnt 17.5 14.33),
																			LinePP (Pnt 0.0 15.0) 	(Pnt 20.0 15.0)]
	]
	where 
		p03 = Pnt 0 3
		c0_10_5 = Circle (Pnt 0 10) 5
		c20_10_5 = Circle (Pnt 20 10) 5
		c10_0_3 = Circle (Pnt 10 0) 3
		c10_10_3 = Circle (Pnt 10 10) 3
