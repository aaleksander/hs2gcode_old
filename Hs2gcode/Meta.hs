module Hs2gcode.Meta where

import Hs2gcode.Commands
--реализация мета-траекторий

data MetaPnt =
		  Pnt Double Double
		| RadiusPnt Double Double Double
		| RounfPnt Double Double Double
		deriving (Show)

data Meta = Meta [MetaPnt]

toTriple :: [a] -> [(a, a, a)]
toTriple l = zip3 (shift_right l) l (shift_left l)
	where 	
		shift_left l = tail l ++ take 1 l
		shift_right l = last l : init l

meta :: [MetaPnt] -> [Command]
meta list = foldr (++) [] [oneSegment x | x <- toTriple list]
	where oneSegment (prev, curr, next)
		| 	(Pnt cx cy) <- curr, (Pnt nx ny) <- next 
				= g1' [X nx, Y ny]


