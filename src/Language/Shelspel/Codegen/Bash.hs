{-# LANGUAGE TypeFamilies #-}

module Language.Shelspel.Codegen.Bash where

import Language.Shelspel.AST
import Language.Shelspel.Codegen.Types

import Control.Monad


codegen :: Program -> String
codegen prog = script
    where bash = flip execState empty $ cgen prog
          script = foldl (++) "" $ reverse bash

type Bash = [String]

empty :: Bash
empty = [] 

type instance S = Bash
type instance R = ()


-- --------------------------------------------------------------------------------

-- | Weave two actions through the code generator. The first is called
-- on each element, then the second action is executed.
weave :: (a -> State S R) -> State S R -> [a] -> State S R
weave f a = mapM_ (\x -> f x >> a)

-- | Weave a call to 'cgen' over each list.
-- 
-- This can be used, eg, to to intercalate semicolons or newlines:
-- > nl `cgweave` program
-- > semic `cgweave` program
cgweave :: CGen x => State S R -> [x] -> State S R
cgweave = weave cgen

-- | Insert an element into the stream
stream :: String -> State S R
stream s = modify (s:)

-- | Insert a newline (\n) into the stream
nl :: State S R
nl = stream "\n"

-- | Call the element as a subprocess. E.g.
-- 
-- > $(echo "hello world")
subproc :: CGen x => x -> State S R
subproc x = do
  stream "$("
  cgen x
  stream ")"

-- | Evaluate the expression as a mathematical expression
-- 
-- > $(( 40 + 2 ))
math :: Expr -> State S R
math e = do
  stream "$(("
  stream " "
  cgen e
  stream " "
  stream "))"

-- | Insert a semicolon (;)
semic :: State S R
semic = stream ";"

-- -------------------------------------------------------------------------------- --

instance CGen Expr where
    cgen (Literal s) = stream $ "\"" ++ s ++ "\""
    cgen (Var i) = stream $ "${" ++ i ++ "}"
    cgen (BinOp o x y) =
        do
          stream "$(( "
          cgen x
          stream " "
          cgen o
          stream " "
          cgen y
          stream " ))"

instance CGen Op where
    cgen Add = stream "+"
    cgen Sub = stream "-"
    cgen Mul = stream "*"
    cgen Div = stream "/"
    cgen (Compare o) = cgen o

instance CGen Ordering where
    cgen LT = stream "-lt"
    cgen EQ = stream "-eq"
    cgen GT = stream "-gt"

instance CGen CompoundStatement where

    cgen (Match e ms) =
        do
          stream "case "
          cgen e
          stream " in"
          nl
          forM_ ms $ \(m, as) -> do
            cgen m
            stream ")"
            nl
            nl `cgweave` as
            nl
            semic >> semic

    cgen (For i a cs) =
        do
          stream "for "
          stream i
          stream " in "
          subproc a
          semic
          stream "do"
          nl
          nl `cgweave` cs
          stream "done"

    cgen (While a cs) =
        do
          stream "while"
          stream " "
          stream "[["
          stream " "
          cgen a
          stream " "
          stream "]]"
          nl
          stream "do"
          nl
          nl `cgweave` cs
          stream "done"


instance CGen Action where
    cgen (Call i as) =
        do
          stream i
          stream " "
          (stream " ") `cgweave` as

    cgen (Cmpd s) = cgen s

    cgen (Pipe c a) =
        do
          cgen c
          stream " | "
          cgen a

    cgen (Sink a s p m) =
        do
          cgen a
          stream " "
          cgen m
          stream p
          stream " "
          cgen s

    cgen (Store i c) =
        do
          stream i
          stream "="
          subproc c

    cgen (Eval e) =
        do
          stream "# Bare expressions are not supported: "
          cgen e

    cgen (Result c) = stream $ "return " ++ show c

instance CGen Mode where
    cgen Write = stream ">"
    cgen Append = stream ">>"

instance CGen Captured where
    cgen (Captured s a) =
        do
          cgen a
          stream " "
          cgen s

instance CGen Stream where
    cgen Stdout = stream ""
    cgen Stderr = stream "3>&1 1>&2- 2>&3-" -- swap stderr and stdout
    cgen All    = stream "2>&1"

instance CGen Cmd where
    cgen (Execute a) = cgen a

    cgen (Funcdef i ps cs) =
        do
          stream "function"
          stream " "
          stream i
          stream "()"
          stream " "
          stream "{"
          nl
          let define i n = stream "local " >> def i n
              def i n = cgen $ Define n $ Literal $ "${" ++ show i ++ "}"
          weave (uncurry define) nl $ zip [1..] ps
          nl
          nl `cgweave` cs
          stream "}"
          nl

    cgen (Define i e) =
        do
          stream i
          stream "="
          cgen e

instance CGen Program where
    cgen (Program cs) = nl `cgweave` cs
