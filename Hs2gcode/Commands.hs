module Hs2gcode.Commands where

-- команды станка в двух вариантах
g0 :: [Position] -> Command
g0 list = G0 $ list
g0' :: [Position] -> [Command]
g0' list = [g0 list]

g1 :: [Position] -> Command
g1 list = G1 $ list
g1' :: [Position] -> [Command]
g1' list = [g1 list]

f :: Int -> Command
f val  = F val
f' :: Int -> [Command]
f' val  = [f val]

up :: Double ->[Command]
up coord = [g0 [Z coord]]

--бежим в ноль на определенной высота
run :: Double -> Double -> Double -> [Command]
run xx yy h = 	[	g0 [Z h], 
					g0 [X xx, Y yy]]

--заглубляемся на какую-то глубину и с какой-то подачей
deep :: Double -> Int -> [Command]
deep zz ff = setF ff ++ [ g1 [Z zz]	]

setF :: Int -> [Command]
setF ff = [f ff]

--одна координата
data Position = X Double | Y Double | Z Double

--доступные команды станка
data Command = 	G0 [Position]
			|	G1 [Position]
			|	F Int

instance Show Position where
	show (X v) = "X" ++ show v
	show (Y v) = "Y" ++ show v
	show (Z v) = "Z" ++ show v

pos2Str :: [Position] -> String
pos2Str (x:xs) = show x ++ " " ++ pos2Str xs
pos2Str [] = ""

instance Show Command where
	show (G0 l) 	= "G0 " ++ pos2Str l
	show (G1 l) 	= "G1 " ++ pos2Str l
	show (F  v) 	= "F " 	++ show v

export :: [Command] -> IO ()
export list = 
	putStr $ unlines $ map show list

