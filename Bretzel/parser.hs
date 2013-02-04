module Bretzel.Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import System.Environment
import Control.Monad
import Numeric (readHex)
import Data.Word (Word16)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

data Opcode = SET
            | ADD
            | SUB
            | MUL
            | MLI
            | DIV
            | DVI
            | MOD
            | MDI
            | AND
            | BOR
            | XOR
            | SHR
            | ASR
            | SHL
            | IFB
            | IFC
            | IFE
            | IFN
            | IFG
            | IFA
            | IFL
            | IFU
            | ADX
            | SBX
            | STI
            | STD
            deriving (Show)

data SpOpcode = JSR 
              | INT
              | IAG
              | IAS 
              | RFI
              | IAQ
              | HWN
              | HWQ
              | HWI
              deriving (Show)

data Register = A | B | C | X | Y | Z | I | J deriving (Show)

data Operand = Reg Register
             | RegRef Register          -- [register]
             | RegRefNW Register Word16 -- [register + next word]
             | RefNum Word16            -- [0x1000]
             | LitNum Word16
             | Identifier String        -- target label
             | PUSH
             | POP
             | PEEK
             | PICK
             | SP
             | PC
             | EX
             deriving (Show)

data Instruction = Basic Opcode Operand Operand
                 | NonBasic SpOpcode Operand
                 | Lab Label deriving (Show)

data Label = Label String deriving (Show)

data Program = Prog [Instruction] deriving (Show)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (emptyDef {
    P.commentLine = ";"
  , P.reservedNames = ["A", "B", "C", "X", "Y", "Z", "I", "J",
                       "SET", "ADD", "SUB", "MUL", "MLI", "DIV",
                       "DVI", "MOD", "MDI", "AND", "BOR", "XOR",
                       "SHR", "ASR", "SHL", "IFB", "IFC", "IFE",
                       "IFN", "IFG", "IFA", "IFL", "IFU", "ADX",
                       "SBX", "STI", "STD",
                       "INT", "IAG", "IAS", "RFI", "IAQ", "HWN",
                       "HWQ", "HWI",
                       "PUSH", "POP", "PEEK", "PICK",
                       "SP", "PC", "EX"]})

whiteSpace = P.whiteSpace lexer
reserved   = P.reserved lexer
colon      = P.colon lexer
natural    = P.natural lexer
identifier = P.identifier lexer

instrSize (Basic _ op1 op2) = 1 + opSize op1 + opSize op2
instrSize (NonBasic _ op)   = 1 + opSize op

opSize (RegRefNW _  _) = 1
opSize (RefNum _) = 1
opSize (LitNum num) = if num <= 0x1E then 0 else 1
opSize _ = 0
                 
doParse :: String -> [Instruction]
doParse input = case parse parseProgram "dcpu" input of
  Left err  -> trace (show err) []
  Right val -> do
    let labels = (registerLabels (concat val) 0 Map.empty) -- create a map with the labels
    removeLabels $ map (replaceLabels labels) (concat val) -- replace the labels with the addresses

registerLabels :: [Instruction] -> Word16 -> Map.Map String Word16 -> Map.Map String Word16
registerLabels [] _ m = m
registerLabels ((Lab (Label l)):xs) i m = registerLabels xs i (Map.insert l i m)
registerLabels (x:xs) i m = registerLabels xs (i + (instrSize x)) m

replaceLabels :: Map.Map String Word16 -> Instruction -> Instruction
replaceLabels m (Basic op (Identifier op1) (Identifier op2)) = Basic op (findLabel op1 m) (findLabel op2 m) 
replaceLabels m (Basic op (Identifier op1) op2)  = Basic op (findLabel op1 m) op2
replaceLabels m (Basic op op1 (Identifier op2)) = Basic op op1 (findLabel op2 m)
replaceLabels m (NonBasic op (Identifier op1)) = NonBasic op (findLabel op1 m)
replaceLabels _ x = x

findLabel :: String -> Map.Map String Word16 -> Operand
findLabel instr m = case Map.lookup instr m of
  (Just x) -> LitNum x
  Nothing  -> error "invalid label"

removeLabels :: [Instruction] -> [Instruction]
removeLabels = filter (not . isLabel)
  where isLabel (Lab _) = True
        isLabel _ = False

--parseProgram :: Parser Program
parseProgram = manyTill parseLine eof

parseLine :: Parser [Instruction]
parseLine = do optional whiteSpace
               lab <- optionMaybe parseLabel
               whiteSpace
               instr <- optionMaybe parseInstruction
               optional whiteSpace
               return $ case (lab, instr) of
                 (Just x, Nothing)  -> [Lab x]
                 (Just x, Just y)   -> [Lab x, y]
                 (Nothing, Just y)  -> [y]
                 (_, _)             -> []

parseInstruction :: Parser Instruction
parseInstruction = parseBasic <|> parseSpecial

parseBasic :: Parser Instruction
parseBasic = do opcode <- parseOpcode
                whiteSpace
                op1 <- parseOperand
                optional whiteSpace
                char ','
                optional whiteSpace
                op2 <- parseOperand
                return $ Basic opcode op1 op2

parseSpecial :: Parser Instruction
parseSpecial = do opcode <- parseSpecialOpcode
                  op <- parseOperand
                  return $ NonBasic opcode op

parseOpcode :: Parser Opcode
parseOpcode = try (reserved "SET" >> return SET)
          <|> try (reserved "ADD" >> return ADD)
          <|> try (reserved "SUB" >> return SUB)
          <|> try (reserved "MUL" >> return MUL)
          <|> try (reserved "MLI" >> return MLI)
          <|> try (reserved "DIV" >> return DIV)
          <|> try (reserved "DVI" >> return DVI)
          <|> try (reserved "MOD" >> return MOD)
          <|> try (reserved "MDI" >> return MDI)
          <|> try (reserved "AND" >> return AND)
          <|> try (reserved "BOR" >> return BOR)
          <|> try (reserved "XOR" >> return XOR)
          <|> try (reserved "SHR" >> return SHR)
          <|> try (reserved "ASR" >> return ASR)
          <|> try (reserved "SHL" >> return SHL)
          <|> try (reserved "IFB" >> return IFB)
          <|> try (reserved "IFC" >> return IFC)
          <|> try (reserved "IFE" >> return IFE)
          <|> try (reserved "IFN" >> return IFN)
          <|> try (reserved "IFG" >> return IFG)
          <|> try (reserved "IFA" >> return IFA)
          <|> try (reserved "IFL" >> return IFL)
          <|> try (reserved "IFU" >> return IFU)
          <|> try (reserved "ADX" >> return ADX)
          <|> try (reserved "SBX" >> return SBX)
          <|> try (reserved "STI" >> return STI)
          <|> (reserved "STD" >> return STD)

parseSpecialOpcode :: Parser SpOpcode
parseSpecialOpcode = try (reserved "JSR" >> return JSR)
                 <|> try (reserved "INT" >> return INT)
                 <|> try (reserved "IAG" >> return IAG)
                 <|> try (reserved "IAS" >> return IAS)
                 <|> try (reserved "RFI" >> return RFI)
                 <|> try (reserved "IAQ" >> return IAQ)
                 <|> try (reserved "HWN" >> return HWN)
                 <|> try (reserved "HWQ" >> return HWQ)
                 <|> (reserved "HWI" >> return HWI)

parseOperand :: Parser Operand
parseOperand = parseRegister
           <|> parseSpecialReg
           <|> parseNumber           
           <|> (try parseRefNum <|> try parseRegRef <|> try parseRegRefNWHexa <|> parseRegRefNW)
           <|> parseIdentifier

parseRegister :: Parser Operand
parseRegister = do x <- register
                   return $ Reg x

parseSpecialReg :: Parser Operand
parseSpecialReg = try (reserved "PUSH" >> return PUSH)
              <|> try (reserved "PEEK" >> return PEEK)
              <|> try (reserved "POP" >> return POP)
              <|> (reserved "PICK" >> return PICK)

parseRegRef :: Parser Operand
parseRegRef = do char '['
                 x <- register
                 char ']'
                 return $ RegRef x

-- refactor this with parseRegRefNW
parseRegRefNWHexa :: Parser Operand
parseRegRefNWHexa = do char '['
                       reg <- register
                       char '+'
                       num <- natural
                       char ']'
                       return $ RegRefNW reg (fromIntegral num)

parseRegRefNW :: Parser Operand
parseRegRefNW = do char '['
                   reg <- register
                   char '+'
                   num <- natural
                   char ']'
                   return $ RegRefNW reg (fromIntegral num)

parseRefNum :: Parser Operand
parseRefNum = do char '['
                 x <- natural
                 char ']'
                 return $ RefNum (fromIntegral x)

parseNumber :: Parser Operand
parseNumber = do x <- natural
                 return $ LitNum (fromIntegral x)

parseLabel :: Parser Label
parseLabel = do colon
                x <- identifier
                return $ Label x

parseIdentifier :: Parser Operand
parseIdentifier = do i <- many1 alphaNum
                     return $ Identifier i

register :: Parser Register
register = do whiteSpace
              x <- oneOf "ABCXYZIJ"
              whiteSpace
              return $ charToReg x

charToReg r = case r of
    'A' -> A
    'B' -> B
    'C' -> C
    'X' -> X
    'Y' -> Y
    'Z' -> Z
    'I' -> I
    'J' -> J
