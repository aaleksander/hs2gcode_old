module Tests where
import Test.Tasty
import Test.Tasty.HUnit

import CNC
import Commands
import Meta

tests' :: TestTree
tests' = testGroup "Тесты" [
	CNC.tests, 
	Commands.tests,
	Meta.tests]

main = do
	defaultMain tests'

