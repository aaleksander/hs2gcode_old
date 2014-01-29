module Hs2gcode.Commands where

-- команды станка в двух вариантах
g0 :: [Coord] -> Command
g0 list = G0 $ Params list
g0' :: [Coord] -> [Command]
g0' list = [g0 list]

g1 :: [Coord] -> Command
g1 list = G1 $ Params list
g1' :: [Coord] -> [Command]
g1' list = [g1 list]

f :: Int -> Command
f val  = F val
f' :: Int -> [Command]
f' val  = [f val]

up :: Double ->[Command]
up coord = [g0 [_z coord]]


-- оси станка
data Dir = X | Y | Z 

-- функции для более удобного ввода программ
_x :: Double -> Coord
_x val = Coord X val

_y :: Double -> Coord
_y val = Coord Y val

_z :: Double -> Coord
_z val = Coord Z val

--бежим в ноль на определенной высота
run :: Double -> Double -> Double -> [Command]
run xx yy h = 	[	g0 [_z h], 
					g0 [_x xx, _y yy]]

--заглубляемся на какую-то глубину и с какой-то подачей
deep :: Double -> Int -> [Command]
deep zz ff = setF ff ++ [ g1 [_z zz]	]

setF :: Int -> [Command]
setF ff = [f ff]

--одна координата
data Coord = Coord Dir Double

-- параметры команды
data Params = Params [Coord]

--доступные команды станка
data Command = 	G0 Params
			|	G1 Params
			|	F Int

data CommandType = CTG0 | CTG1 | CTF deriving Eq
commandType :: Command -> CommandType
commandType (G0 _) = CTG0
commandType (G1 _) = CTG1
commandType (F  _) = CTF

instance Eq Coord where
	(==) (Coord d1 v1) (Coord d2 v2) = and [d1 == d2, v1 == v2]

--сравниваем только направления
(===) :: Coord -> Coord -> Bool
(===) (Coord d1 _) (Coord d2 _) = d1 == d2

instance  Eq Dir where
	(==) X X = True
	(==) Y Y = True
	(==) Z Z = True
	(==) _ _ = False


instance Show Coord where
	show (Coord d v) = show d ++ " = " ++ show v

instance Show Dir where
	show X = "X"
	show Y = "Y"
	show Z = "Z"
--		| Y <- d = "Y"
--		| Z <- d = "Z"

instance Show Command where
	show (G0 l) 	= "G0 " ++ show l
	show (G1 l) 	= "G1 " ++ show l
	show (F  v) 	= "F " 	++ show v

instance Show Params where
	show (Params l) = concat $ map _2Str l
		where _2Str (Coord d val) = show d ++ show val ++ " "


export :: [Command] -> IO ()
export list = 
	putStr $ unlines $ map show list

