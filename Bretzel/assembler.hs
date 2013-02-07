module Bretzel.Assembler where

import Control.Monad (when)
import Data.Word
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Numeric (readHex, showHex)
import Text.Printf (printf)
import Bretzel.Parser

instance Binary Instruction where
	get = undefined
	put i = instructionToWords i

instance Binary Program where
	get = undefined
	put (Prog p) = do
		mapM instructionToWords p
		return ()

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
dump = unwords . map (printf "%04x")

instructionToWords :: Instruction -> Put
instructionToWords (Basic opcode op1 op2) = do
	let o = opcodeToWord opcode
	let b = rotateL (operandToWord op1) 5
	let a = rotateL (operandToWord op2) 10
	putWord16be (a .|. b .|. o)

	when (length (nextWord op2) > 0) (putWord16be (head $ nextWord op2))
	when (length (nextWord op1) > 0) (putWord16be (head $ nextWord op1))

instructionToWords (NonBasic opcode op) = do
	let o = rotateL (spOpcodeToWord opcode) 5
	let a = rotateL (operandToWord op) 10
	putWord16be (a .|. o)

	when (length (nextWord op) > 0) (putWord16be (head $ nextWord op))

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

wordToOperand :: Word16 -> Operand
wordToOperand op = case op of
	0x00 -> Reg A
	0x01 -> Reg B
	0x02 -> Reg C
	0x03 -> Reg X	
	0x04 -> Reg Y
	0x05 -> Reg Z
	0x06 -> Reg I
	0x07 -> Reg J
	0x08 -> RegRef A
	0x09 -> RegRef B
	0x0A -> RegRef C
	0x0B -> RegRef X
	0x0C -> RegRef Y
	0x0D -> RegRef Z
	0x0E -> RegRef I
	0x0F -> RegRef J
	0x1A -> PICK
	0x1B -> SP
	0x1C -> PC
	0x1D -> EX

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

wordToOpcode :: Word16 -> Opcode
wordToOpcode op = case op of
	0x01 -> SET
	0x02 -> ADD
	0x03 -> SUB
	0x04 -> MUL
	0x05 -> MLI
	0x06 -> DIV
	0x07 -> DVI
	0x08 -> MOD
	0x09 -> MDI
	0x0A -> AND
	0x0B -> BOR
	0x0C -> XOR
	0x0D -> SHR
	0x0E -> ASR
	0x0F -> SHL
	0x10 -> IFB
	0x11 -> IFC
	0x12 -> IFE
	0x13 -> IFN
	0x14 -> IFG
	0x15 -> IFA
	0x16 -> IFL
	0x17 -> IFU
	0x1A -> ADX
	0x1B -> SBX
	0x1E -> STI
	0x1F -> STD

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

wordToSpOpcode :: Word16 -> SpOpcode
wordToSpOpcode op = case op of
	0x01 -> JSR
	0x08 -> INT
	0x09 -> IAG
	0x0A -> IAS
	0x0B -> RFI
	0x0C -> IAQ
	0x10 -> HWN
	0x11 -> HWQ
	0x12 -> HWI

decodeInstruction :: Word16 -> Instruction
decodeInstruction instr = do
	let o = instr .&. 0x1F
	let a = (shiftR instr 5) .&. 0x1F
	let b = (shiftR instr 10) .&. 0x3F
	let a' = decodeOperand a
	let b' = decodeOperand b

	case o of
		-- non basic
		0x0 -> case a of
			0x01 -> NonBasic JSR b'

		-- basic
		0x01 -> Basic SET a' b'

decodeOperand :: Word16 -> Operand
decodeOperand op
	| op <= 0x07 = Reg A
	| op <= 0x0F = RegRef A
	| op <= 0x17 = RegRefNW A 0x10
	| op >= 0x20 = LitNum (op - 0x20)
	| otherwise = case op of
		0x1B -> PUSH
