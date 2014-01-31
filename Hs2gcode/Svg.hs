module Hs2gcode.Svg where

import Data.Char (isSpace)

str = "m 1.1,11.11 2.2,-22.22 3.3,33.33 4.4,44.44 z"

--svg в формате path-строки
--svg2GCode :: String -> [Command]
--svg2GCode = undefined

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

splitPath :: String -> [String]
splitPath [] = []
splitPath (x:xs) 	
	| [] <- xs = [[x]] --нужно, чтобы из чара сделать строку и вернуть элемент массива
	| isSep x = [x : (trim (takeWhile (isNotSep) xs))] ++ (splitPath $ dropWhile (isNotSep) xs)
	where
		isSep x = x `elem` ['m', 'l', 'z']
		isNotSep x = not $ isSep x

