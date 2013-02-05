module Bretzel.Emulator where

import Bretzel.Parser
import Bretzel.Assembler

import Control.Monad
import Data.Array.IO
import Data.IORef
import Data.Word (Word16)

data CPU = CPU {
	ram :: IOArray Word16 Word16
  , pc :: IORef Word16 -- PC
  , sp :: IORef Word16 -- SP
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

getValue :: CPU -> Operand -> IORef Word16
getValue cpu r@(Reg _) = getReg cpu r
getValue cpu SP = getReg cpu SP
getValue cpu PC = getReg cpu PC

modify :: CPU -> Operand -> (Word16 -> Word16) -> IO ()
modify cpu r@(Reg _) f = modifyIORef (getValue cpu r) f
modify cpu (RefNum n) f = modifyMem cpu n f

exec :: CPU -> Instruction -> IO ()
exec cpu (Basic SET a b) = loadValue cpu b >>= \x -> modify cpu a (\a -> x)
exec cpu (Basic ADD a b) = loadValue cpu b >>= \x -> modify cpu a (+x)
exec cpu (Basic SUB a b) = loadValue cpu b >>= \x -> modify cpu a (subtract x)
exec cpu (Basic MUL a b) = loadValue cpu b >>= \x -> modify cpu a (*x)

run :: CPU -> [Instruction] -> IO ()
run cpu instructions = mapM_ (exec cpu) instructions

emu :: IO ()
emu = do
	cpu <- makeCPU
	writeIORef (ar cpu) 2
	writeArray (ram cpu) 2 100

	let instr = [Basic SET (RefNum 2) (LitNum 10),
				 Basic SET (Reg B) (Reg A),
				 Basic ADD (Reg B) (LitNum 10),
				 Basic MUL (Reg B) (Reg A),
				 Basic SUB (Reg B) (LitNum 1),
				 Basic ADD (Reg B) (RefNum 2)]

	run cpu instr

	w <- readIORef (br cpu)

	print w
