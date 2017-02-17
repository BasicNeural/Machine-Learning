module ML.Math where

data Operator = Log | Exp | Plus | Minus | Mul | Div
    deriving Show

data Formula a = Func Operator (Formula a) (Formula a) | Number Double
    deriving Show
