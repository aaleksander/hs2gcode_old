module Hs2gcode.Commands where

g0 :: [(Dir, Double)] -> Command
g0 list = G0 $ Params list

g1 :: [(Dir, Double)] -> Command
g1 list = G1 $ Params list

f :: Int -> Command
f val  = F val

up :: Double ->[Command]
up coord = [g0 [z coord]]


--ТИПЫ и экземпляры классов

-- оси станка
data Dir = X | Y | Z 

-- функции для более удобного ввода программ
x :: Double -> (Dir, Double)
x val = (X, val)

y :: Double -> (Dir, Double)
y val = (Y, val)

z :: Double -> (Dir, Double)
z val = (Z, val)

--бежим в ноль на определенной высота
run :: Double -> Double -> Double -> [Command]
run xx yy h = 	[	g0 [z h], 
					g0 [x xx, y yy]]

--заглубляемся на какую-то глубину и с какой-то подачей
deep :: Double -> Int -> [Command]
deep zz ff = setF ff ++ [ g1 [z zz]	]

setF :: Int -> [Command]
setF ff = [f ff]


-- параметры команды
data Params = Params [(Dir, Double)]

--доступные команды станка
data Command = 	G0 Params
			|	G1 Params
			|	F Int

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
		where _2Str (d, val) = show d ++ show val ++ " "


export :: [Command] -> IO ()
export list = 
	putStr $ unlines $ map show list

