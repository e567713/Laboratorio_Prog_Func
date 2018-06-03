-- LABORATORIO DE PROGRAMACION FUNCIONAL 2018
-- MODULO DE SINTAXIS DEL LENGUAJE DE MAQUINA


module MachineLang where

-- AST del codigo de maquina

type Code = [Instr]

data Instr = NEG
           | ADD
           | SUB
           | MUL
           | DIV
           | MOD
           | CMP
           | PUSH  Integer
           | JUMP  Shift
           | JMPZ  Shift
           | LOAD  Var
           | STORE Var
           | READ
           | WRITE
           | SKIP
 deriving Show

type Shift = Int
type Var   = String

