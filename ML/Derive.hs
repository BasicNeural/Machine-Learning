module ML.Derive where

derive f x = (f (x + delta) - f(x - delta)) / ((x + delta) - (x - delta))
    where delta = 1.0e-6