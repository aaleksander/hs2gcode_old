
data Code = G0 | G1 | F --команды

data Dir = X | Y | Z  --направление движения

x :: Double -> (Dir, Double)
x val = (X, val)

y :: Double -> (Dir, Double)
y val = (Y, val)

z :: Double -> (Dir, Double)
z val = (Z, val)

data Command = Command Code [(Dir, Double)]
				  

--конвертим пару "направление-значение" в строку
_2Str :: (Dir, Double) -> String
_2Str (d, val)
	| X <- d = "X " ++ sval
	| Y <- d = "Y " ++ sval
	| Z <- d = "Z " ++ sval
		where sval = (show val) ++ " "

--конвертим команду с координатами в строку
g2Str :: Command -> String
g2Str d 
	| Command G0 l <- d = "G0 " ++ l2Str l ++ "\n"
	| Command G1 l <- d = "G1 " ++ l2Str l ++ "\n"
	| Command F [(_, val)] <- d = "F " ++ show val ++ "\n"
	where l2Str ll= 
		foldr (++) " " $ map _2Str ll

g0 :: [(Dir, Double)] -> Command
g0 list = Command G0 list

g1 :: [(Dir, Double)] -> Command
g1 list = Command G1 list

f :: Double -> Command
f val  = Command F [(X, val)]

path = [
	f 300,
	g0 [x 0, y 0, z 0],
	g1 [x 10.0, y 20]
	]


ps :: String -> IO()
ps s = do
	putStr s
	putStr "\n"

export :: [Command] -> String
export list = 
	foldr (++) "" $ map g2Str list

main = do
	putStr $ export path
	--ps $ g0 [x 10.0]
	--ps $ g0 [x 20.0, y 10]

	putStr "\n"