module Geometry where
--всякая тригонометрия
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe
import Data.List

eps = 0.000001 --минимальная дистанция

main = do Geometry.test


data Pnt = Pnt Double Double

instance Show Pnt where
	show (Pnt x y) = "Pnt(" ++ show x ++ ", " ++ show y ++ ")"

instance Eq Pnt where
	Pnt ax ay == Pnt bx by = and [abs(ax - bx)<eps, abs(ay - by)<eps]
	a /= b = not $ a == b

Pnt ax ay `sub` Pnt bx by = Pnt (ax - bx) (ay - by)

data Circle = Circle Pnt Double

instance Show Circle where
	show (Circle (Pnt x y) r) = "Circle(" ++ show x ++ ", " ++ show y ++ ", " ++ show r ++ ")"


data Line = LinePP Pnt Pnt| LineABC Double Double Double

instance Show Line where
	show (LinePP (Pnt x1 y1) (Pnt x2 y2)) = "Line(" 
		++ (concat $ intersperse ", " (map show [x1, y1, x2, y2])) ++ ")" --все параметры через запятую и в скобки

data PointCircleRelation = IN_CIRCLE | ON_CIRCLE | OUT_CIRCLE deriving (Show, Eq) --отношение точки относительно окружности

--преобразует линию на основе двух точек в линию на основе abc
linePP2ABC :: Line -> Line
linePP2ABC (LinePP (Pnt x1 y1) (Pnt x2 y2)) = LineABC a b c
	where 
		a = y2 - y1
		b = x1 - x2
		c = (-a)*x1 - b*y1
--TODO:
--    def init_p1(self, x):
--        'вычисляем одну точку по какой-нибудь координате'
--        self.p1 = Point(x, (-self.a*x - self.c)/self.b)

--    def init_p2(self, x):
--        'вычисляем одну точку по какой-нибудь координате'
--        self.p2 = Point(x, (-self.a*x - self.c)/self.b)


--точка касания касательной из точки 'p' к окружности 'c'
contactPoints:: Pnt -> Circle -> [Pnt]
contactPoints p c
	| IN_CIRCLE <- pc = []
	| ON_CIRCLE <- pc = [p]
	| OUT_CIRCLE <- pc = cross_circle (Circle p r) c
	where 
		pc = pointInCircle p c	
		Circle cp cr = c
		r = sqrt (d^2 - cr^2)
			where d = dist p cp
    --d = dist (p, c.c)
    --k = sqrt (d * d - c.r * c.r)
    --return cross_circle(p.x, p.y, k, c.c.x, c.c.y, c.r)


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

crossLineCircle = undefined

--где находится точка относительно окружности
pointInCircle:: Pnt -> Circle -> PointCircleRelation
pointInCircle p (Circle cp r)
	| abs(d - r) < eps 	= ON_CIRCLE
	| d > r  			= OUT_CIRCLE
	| d < r  			= IN_CIRCLE
	where d = dist p cp

--дистанция между двумяточками
dist :: Pnt -> Pnt -> Double
dist (Pnt x1 y1) (Pnt x2 y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)


test = defaultMain Geometry.tests

tests :: TestTree
tests = testGroup "Geometry" [checkPoints]

checkPoints = testGroup "Тестируем show"
	[
		testCase "01: точки" 	 $ show (Pnt 0 0) @?= "Pnt(0.0, 0.0)",
		testCase "02: окружность"$ show (Circle (Pnt 1 2) 3) @?= "Circle(1.0, 2.0, 3.0)",
		testCase "03: линии" 	 $ show (LinePP (Pnt 0 0) (Pnt 10 10)) @?= "Line(0.0, 0.0, 10.0, 10.0)",

		testCase "04: point Out Circle" $ pointInCircle (Pnt 0 0)  c10_10_3 	@?= OUT_CIRCLE,
		testCase "05: point IN Circle"  $ pointInCircle (Pnt 9 9)  c10_10_3 	@?= IN_CIRCLE,
		testCase "06: point ON Circle"  $ pointInCircle (Pnt 7 10) c10_10_3		@?= ON_CIRCLE,

		testCase "07: contactPoints" $ contactPoints p03 c10_0_3 @?= [Pnt 10 3]
	]
	where 
		p03 = Pnt 0 3
		c10_0_3 = Circle (Pnt 10 0) 3
		c10_10_3 = Circle (Pnt 10 10) 3
