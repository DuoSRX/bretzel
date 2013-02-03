module Main where

import Bretzel.Assembler
import Bretzel.Parser
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import System.Environment

main = do
	args <- getArgs
	file <- readFile (args !! 0)

	let ast = doParse exampleProg
	let prog = runPut $ put ast

	BL.writeFile "assembled.bin" prog
	print . dump $ runGet listOfWord16 prog
