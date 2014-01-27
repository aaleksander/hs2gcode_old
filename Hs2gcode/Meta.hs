module Hs2gcode.Meta where

import Hs2gcode.Commands

--реализация мета-траекторий

data MetaPoint = 
		Pnt |
		RadiusPnt |
		RounfPnt