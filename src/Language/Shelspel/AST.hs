
module Language.Shelspel.AST where

-- | Identifiers
type Id = String

-- | Unix exit codes
type ExitCode = Int

-- | The root node for a Shelspel program
newtype Program = Program [Cmd]
            deriving (Eq, Show)

type Result = ExitCode

data Cmd = Execute Action
         | Funcdef Id [Id] [Cmd]
         | Define  Id Expr
         deriving (Eq, Show)

-- | Actions can produce side-effects
data Action = Call  Id [Expr] -- ^ call a program or function
            | Cmpd  CompoundStatement -- ^ switching or looping
            | Pipe  Captured Action -- ^ compose actions
            | Sink  Action Stream FilePath Mode -- ^ save the output
            | Store Id Captured -- ^ store the result in a variable
            | Eval  Expr
            | Result Result -- ^ like break, return, except, rolled into one: exit the current context immediately
              deriving (Eq, Show)

data Mode = Write | Append
          deriving (Eq, Show)

data Stream = Stdout | Stderr | All
            deriving (Eq, Show)

data Captured = Captured Stream Action
             deriving (Eq, Show)

data CompoundStatement = Match Expr [(Expr, [Cmd])]
                       | For Id Action [Cmd]
                       | While Action [Cmd]
                       deriving (Eq, Show)

data Op = Add | Sub | Mul | Div
        | Compare Ordering
        deriving (Eq, Show)

data Expr = Literal String
          | Var Id
          | BinOp Op Expr Expr
          deriving (Eq, Show)


-- -------------------------------------------------------------------------------- --
-- some test definitions

ls = Call "ls" []

mdrun = Call "mdrun" $ map Literal $ words $ "-ff amber03 -water none -ignh"

loop = While (Result 0) [Execute mdrun]

dfunc = Funcdef "func" ["rest"] [Execute $ Call "echo" [Literal "hello"],
                                 Execute $ Call "echo" [Literal "world"],
                                 Execute $ Call "echo" [Var "rest"]
                                ]

d42 = Define "const" $ Literal "42"

prog = Program [ Execute ls
--               , Execute mdrun
--               , Execute $ Cmpd loop
               , dfunc
               , d42
               , Execute $ Call "echo" [Var "const"]
               , Execute $ Call "func" [Literal "Fezzes are cool"]
               ]
