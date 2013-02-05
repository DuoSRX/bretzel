module Main where

import Bretzel.Assembler
import Bretzel.Parser
import Bretzel.Emulator
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import System.Environment

main = do
	--args <- getArgs
	--file <- readFile (args !! 0)

	exampleProg <- readFile "test.dasm16"

	let ast = doParse exampleProg
	mapM (putStrLn . show) ast
	let prog = runPut $ put (Prog ast)

	BL.writeFile "assembled.bin" prog
	print . dump $ runGet listOfWord16 prog
