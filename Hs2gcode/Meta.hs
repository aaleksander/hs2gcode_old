module Hs2gcode.Meta where

import Hs2gcode.Commands
--реализация мета-траекторий

data MetaPnt =
		  Pnt Double Double
		| RadiusPnt Double Double Double
		| RounfPnt Double Double Double
		deriving (Show)

data Meta = Meta [MetaPnt]

meta :: [MetaPnt] -> [Command]
meta (x1:x2:xs) = meta' x1 x2 xs []
	where meta' first curr list acc
		| [] <- list, (Pnt xx yy) <- first, (Pnt xxc yyc) <- curr
			= acc ++ g1' [_x xxc, _y yyc]
				  ++ g1' [_x xx, _y yy]
		| (Pnt xx yy) <- curr, (x:xs) <- list 
			= meta' first x xs (acc ++ g1' [_x xx, _y yy])


