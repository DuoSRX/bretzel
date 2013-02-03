module Bretzel.Assembler where

import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Numeric (readHex, showHex)
import Bretzel.Parser

instance Binary Instruction where
	get = undefined
	put i = instructionToWords i

instance Binary Program where
	get = undefined
	put (Prog p) = do
		mapM instructionToWords p
		return ()

asmTest :: IO ()
asmTest = do
	let ast = doParse exampleProg
	let prog = runPut $ put ast

	BL.writeFile "assembled.bin" prog
	print . dump $ runGet listOfWord16 prog
	--input <- BL.readFile "assembled.bin"

listOfWord16 :: Get [Word16]
listOfWord16 = do
	empty <- isEmpty
	if empty 
		then return []
		else do
			w <- getWord16be
			rest <- listOfWord16
			return (w:rest)

dump :: [Word16] -> String
dump = unwords . map (\x -> showHex x "")

exampleProg = unlines [":salut ADD A, B", "JSR salut", ":what SET J, [0x1000]", "JSR what"] -- "ADD A, B", "SET [0x125], J", "JSR [0x100]", "SET PUSH, X"

instructionToWords :: Instruction -> Put
instructionToWords (Basic opcode op1 op2) = do
	let o = opcodeToWord opcode
	let b = rotateL (operandToWord op1) 5
	let a = rotateL (operandToWord op2) 10
	putWord16be (a .|. b .|. o)

	if length (nextWord op2) > 0 then putWord16be (head $ nextWord op2) else return ()
	if length (nextWord op1) > 0 then putWord16be (head $ nextWord op1) else return ()

instructionToWords (NonBasic opcode op) = do
	let o = rotateL (spOpcodeToWord opcode) 5
	let a = rotateL (operandToWord op) 10
	putWord16be (a .|. o)

	if length (nextWord op) > 0 then putWord16be (head $ nextWord op) else return ()

-- Instruction are 1-3 words long
nextWord (RefNum num) = [num]
nextWord (RegRefNW _ num) = [num]
nextWord (LitNum num) = if num <= 0x1E then [] else [num]
nextWord _ = []

regToWord :: Register -> Word16
regToWord reg = case reg of
	A -> 0x00
	B -> 0x01
	C -> 0x02
	X -> 0x03
	Y -> 0x04
	Z -> 0x05
	I -> 0x06
	J -> 0x07

operandToWord :: Operand -> Word16
operandToWord op = case op of
	(Reg reg)        -> regToWord reg
 	(RegRef reg)     -> regToWord reg + 0x08
 	(RegRefNW reg _) -> regToWord reg + 0x10
 	PUSH -> 0x18
 	POP  -> 0x18
 	PEEK -> 0x19
 	PICK -> 0x1A
 	SP   -> 0x1B
 	PC   -> 0x1C
 	EX   -> 0x1D
 	(RefNum num) -> 0x1E
 	(LitNum num) -> if num <= 0x1E then (fromIntegral num + 0x21) else 0x1F

opcodeToWord :: Opcode -> Word16
opcodeToWord op = case op of
	SET -> 0x01
	ADD -> 0x02
	SUB -> 0x03
	MUL -> 0x04
	MLI -> 0x05
	DIV -> 0x06
	DVI -> 0x07
	MOD -> 0x08
	MDI -> 0x09
	AND -> 0x0A
	BOR -> 0x0B
	XOR -> 0x0C
	SHR -> 0x0D
	ASR -> 0x0E
	SHL -> 0x0F
	IFB -> 0x10
	IFC -> 0x11
	IFE -> 0x12
	IFN -> 0x13
	IFG -> 0x14
	IFA -> 0x15
	IFL -> 0x16
	IFU -> 0x17
	ADX -> 0x1A
	SBX -> 0x1B
	STI -> 0x1E
	STD -> 0x1F

spOpcodeToWord :: SpOpcode -> Word16
spOpcodeToWord op = case op of
	JSR -> 0x01
	INT -> 0x08
	IAG -> 0x09
	IAS -> 0x0A
	RFI -> 0x0B
	IAQ -> 0x0C
	HWN -> 0x10
	HWQ -> 0x11
	HWI -> 0x12
