module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import System.Environment
import Control.Monad
import Numeric (readHex)
import Data.Word (Word16)

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
             | RegRef Register
             | RefNum Word16
             | LitNum Word16
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
                       "SET", "ADD", "SUB", "MUL"]})

whiteSpace = P.whiteSpace lexer
reserved   = P.reserved lexer

doParse input = case parse parseProgram "dcpu" input of
  Left err  -> undefined
  Right val -> val

parseProgram :: Parser Program
parseProgram = do x <- many parseLine
                  return $ Prog x

parseLine :: Parser Instruction
parseLine = do x <- parseInstruction
               optional $ char '\n'
               return x

parseInstruction :: Parser Instruction
parseInstruction = try parseBasic <|> parseSpecial

parseBasic :: Parser Instruction
parseBasic = do opcode <- parseOpcode
                whiteSpace
                op1 <- parseOperand
                whiteSpace >> char ',' >> whiteSpace
                op2 <- parseOperand
                return $ Basic opcode op1 op2

parseSpecial :: Parser Instruction
parseSpecial = do opcode <- parseSpecialOpcode
                  whiteSpace
                  op <- parseOperand
                  return $ NonBasic opcode op

parseOpcode :: Parser Opcode
parseOpcode = try (reserved "SET" >> return SET)
          <|> (reserved "ADD" >> return ADD)
          <|> (reserved "SUB" >> return SUB)
          <|> (reserved "MUL" >> return MUL)

parseSpecialOpcode :: Parser SpOpcode
parseSpecialOpcode = try (string "JSR" >> return JSR)
                 <|> (string "INT" >> return INT)

parseOperand :: Parser Operand
parseOperand = parseRegister
           <|> parseSpecialReg
           <|> parseHexa
           <|> parseNumber
           <|> (try parseRefNum <|> parseRegRef)

parseRegister :: Parser Operand
parseRegister = do x <- oneOf "ABCXYZIJ"
                   return $ Reg (charToReg x)

parseRegRef :: Parser Operand
parseRegRef = do char '['
                 x <- oneOf "ABCXYZIJ"
                 char ']'
                 return $ RegRef (charToReg x)

parseRefNum :: Parser Operand
parseRefNum = do char '['
                 string "0x"
                 x <- many1 digit
                 char ']'
                 return $ RefNum (fst . head $ readHex x)

parseHexa :: Parser Operand
parseHexa = do string "0x"
               x <- many1 digit
               return $ LitNum (fst . head $ readHex x)

parseNumber :: Parser Operand
parseNumber = do x <- many1 digit
                 return $ LitNum (read x)

parseLabel :: Parser Label
parseLabel = do colon
                x <- many1 alphaNum
                return $ Label x

parseSpecialReg :: Parser Operand
parseSpecialReg = try (string "PUSH" >> return PUSH)
              <|> try (string "PEEK" >> return PEEK)
              <|> try (string "POP" >> return POP)
              <|> (string "PICK" >> return PICK) 

charToReg r = case r of
    'A' -> A
    'B' -> B
    'C' -> C
    'X' -> X
    'Y' -> Y
    'Z' -> Z
    'I' -> I
    'J' -> J
