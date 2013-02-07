module Bretzel.Emulator where

import Bretzel.Parser
import Bretzel.Assembler

import Control.Monad
import Data.Array.IO
import Data.IORef
import Data.Word (Word16, Word)
import Data.Bits
import Debug.Trace

data CPU = CPU {
	ram :: IOArray Word16 Word16
  , pc :: IORef Word16 -- PC
  , sp :: IORef Word16 -- SP
  , ex :: IORef Word16 -- EX
  , ar :: IORef Word16 -- A
  , br :: IORef Word16 -- B
  , cr :: IORef Word16 -- C
  , xr :: IORef Word16 -- X
  , yr :: IORef Word16 -- Y
  , zr :: IORef Word16 -- Z
  , ir :: IORef Word16 -- I
  , jr :: IORef Word16 -- J
  , cycles :: IORef Int
}

makeCPU :: IO CPU
makeCPU = do
	mem <- newArray(0, 10000) 0
	pc' <- newIORef 0
	sp' <- newIORef 0xFFFF
	ex' <- newIORef 0
	ar' <- newIORef 0
	br' <- newIORef 0
	cr' <- newIORef 0
	xr' <- newIORef 0
	yr' <- newIORef 0
	zr' <- newIORef 0
	ir' <- newIORef 0
	jr' <- newIORef 0
	cycles' <- newIORef 0

	return CPU {
		  ram = mem
		, pc = pc'
		, sp = sp'
		, ex = ex'
		, ar = ar'
		, br = br'
		, cr = br'
		, xr = br'
		, yr = br'
		, zr = br'
		, ir = br'
		, jr = br'
		, cycles = cycles'
	}

readMem :: CPU -> Word16 -> IO Word16
readMem cpu addr = do
	val <- readArray (ram cpu) addr
	return val

writeMem :: CPU -> Word16 -> Word16 -> IO ()
writeMem cpu addr val = do
	writeArray (ram cpu) addr val
	return ()

modifyMem :: CPU -> Word16 -> (Word16 -> Word16) -> IO ()
modifyMem cpu addr f = do
	val <- readMem cpu addr
	writeMem cpu addr (f val)
	return ()

loadValue :: CPU -> Operand -> IO Word16
loadValue cpu op = case op of
	Reg A -> readIORef (ar cpu)
	Reg B -> readIORef (br cpu)
	Reg C -> readIORef (cr cpu)
	Reg X -> readIORef (xr cpu)
	Reg Y -> readIORef (yr cpu)
	Reg Z -> readIORef (zr cpu)
	Reg I -> readIORef (ir cpu)
	Reg J -> readIORef (jr cpu)
	SP -> readIORef (sp cpu)
	PC -> readIORef (pc cpu)
	EX -> readIORef (ex cpu)
	LitNum n -> return n
	RegRef r -> do
		addr <- loadValue cpu (Reg r)
		res <- readMem cpu addr
		return res
	RefNum n -> do
		addr <- loadValue cpu (LitNum n)
		res <- readMem cpu addr
		return res

getReg :: CPU -> Operand -> IORef Word16
getReg cpu (Reg A) = (ar cpu)
getReg cpu (Reg B) = (br cpu)
getReg cpu (Reg C) = (cr cpu)
getReg cpu (Reg X) = (xr cpu)
getReg cpu (Reg Y) = (yr cpu)
getReg cpu (Reg Z) = (zr cpu)
getReg cpu (Reg I) = (ir cpu)
getReg cpu (Reg J) = (jr cpu)
getReg cpu SP = (sp cpu)
getReg cpu PC = (pc cpu)
getReg cpu EX = (pc cpu)

getValue :: CPU -> Operand -> IORef Word16
getValue cpu r@(Reg _) = getReg cpu r
getValue cpu SP = getReg cpu SP
getValue cpu PC = getReg cpu PC
getValue cpu EX = getReg cpu EX

modify :: CPU -> Operand -> (Word16 -> Word16) -> IO ()
modify cpu r@(Reg _) f = modifyIORef (getValue cpu r) f
modify cpu (RefNum n) f = modifyMem cpu n f

write :: CPU -> Operand -> Word16 -> IO ()
write cpu r@(Reg _) value = writeIORef (getValue cpu r) value
write cpu EX value = writeIORef (ex cpu) value

exec :: CPU -> Instruction -> IO ()
exec cpu (Basic SET a b) = loadValue cpu b >>= write cpu a

exec cpu (Basic ADD b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	write cpu b (x + y)
	let overflow = (fromIntegral x + fromIntegral y) > (0xFFFF :: Int)
	if overflow then write cpu EX 0x1
			    else write cpu EX 0x0

exec cpu (Basic SUB b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	write cpu b (y - x)
	let underflow = (fromIntegral y - fromIntegral x) < (0 :: Int)
	if underflow then write cpu EX 0xFFFF
		         else write cpu EX 0x0

exec cpu (Basic MUL b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	write cpu b (x * y)
	let overflow = (shiftR (fromIntegral y * fromIntegral x) 16) .&. 0xFFFF :: Word
	if y == 0 then write cpu EX 0x0
		      else write cpu EX $ fromIntegral overflow

exec cpu (Basic DIV b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	if x == 0
		then do
			write cpu b 0x0
			write cpu EX 0x0
    	else do
    		let overflow = ((shiftL (fromIntegral x) 16) `div` (fromIntegral y)) .&. 0xFFFF :: Word
    		write cpu b (y `div` x)
    		write cpu EX $ fromIntegral overflow

exec cpu (Basic MOD b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	if x == 0 then write cpu b 0
		      else write cpu b $ (fromIntegral y) `mod` (fromIntegral x)

exec cpu (Basic AND b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	write cpu b (y .&. x)

exec cpu (Basic BOR b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	write cpu b (y .|. x)

exec cpu (Basic XOR b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	write cpu b (xor y x)

exec cpu (Basic STI b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	write cpu b x
	modify cpu (Reg I) (+1)
	modify cpu (Reg J) (+1)

exec cpu (Basic STD b a) = do
	x <- loadValue cpu a
	y <- loadValue cpu b
	write cpu b x
	modify cpu (Reg I) (subtract 1)
	modify cpu (Reg J) (subtract 1)

run :: CPU -> [Instruction] -> IO ()
run cpu instructions = mapM_ (exec cpu) instructions

emu :: IO ()
emu = do
	cpu <- makeCPU
	writeArray (ram cpu) 2 100

	let instr = [Basic SET (Reg A) (LitNum 0x18),
				 Basic SET (Reg B) (LitNum 0x4),
				 Basic DIV (Reg A) (Reg B)]

	run cpu instr

	a <- readIORef (ar cpu)
	print a
	b <- readIORef (br cpu)
	print b
