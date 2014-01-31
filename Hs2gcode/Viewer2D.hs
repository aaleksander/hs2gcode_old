module Hs2gcode.Viewer2D where

import Graphics.Win32.Window
import Hs2gcode.Commands
import Graphics.Gloss

-- визуализация траектории в плоскости X, Y

size = (640, 480)

-- возвращает координаты для того, чтобы окно попало в центр рабочего стола
centerCoords :: (Int, Int) -> IO (Int, Int)
centerCoords (w, h) = do
	desktop   <- getDesktopWindow 
	(x,y,r,b) <- getWindowRect desktop
	let (width, height)  = (fromIntegral(r - x), fromIntegral(b - y))
	let (left, top) = (quot (width - w) 2, quot (height - h) 2)
	return (left, top)


show2D :: [Command] -> IO()
show2D list = do
	c <- centerCoords size
	display (InWindow "My Window" size c) white (Circle 80) 