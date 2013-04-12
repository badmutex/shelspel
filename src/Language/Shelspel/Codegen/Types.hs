{-# LANGUAGE TypeFamilies #-}

module Language.Shelspel.Codegen.Types where

import Language.Shelspel.AST

-- -------------------------------------------------------------------------------- --
-- Code generation stores the generated code in a State monad

data State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
    fmap f m = State $ \s -> let (s',   a) = runState m s
                             in  (s', f a)

instance Monad (State s) where
    return a = State $ \s -> (s, a)
    m >>= f  = State $ \s -> let (s', a) = runState m s
                             in runState (f a) s'


evalState :: State s a -> s -> a
evalState m = snd . runState m

execState :: State s a -> s -> s
execState m = fst . runState m

get :: State s s
get = State $ \s -> (s, s)

set :: s -> State s ()
set s = State $ \_ -> (s, ())

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

-- -------------------------------------------------------------------------------- --
-- Code generation interface

type family S
type family R

class CGen a where
    cgen :: a -> State S R
