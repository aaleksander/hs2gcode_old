module Tests where
import Test.Tasty
import Test.Tasty.HUnit

import CNC
import Commands

tests' :: TestTree
tests' = testGroup "Тесты" [
	CNC.tests, 
	Commands.tests]

main = do
	defaultMain tests'
	putStrLn $ show $ G1 [X 100]
