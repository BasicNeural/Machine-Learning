{-
미분을 하는 함수
미분은 이산적인 계산이 아니기 때문에 아주 작은 변화량의 평균변화율을 구하여 근삿값을 구한다
-}

module ML.Derive where

import Prelude as P
import Data.Matrix as M
import Data.Sequence as S
import Data.Foldable as F

derive f x = (f (x + delta) - f(x - delta)) / 2.0e-6
    where delta = 1.0e-6

deriveMatrix f w = M.fromList row col $ map (\(x, y)-> (x - y) / 2.0e-6 ) $ P.zip fxh fx
    where
        col         = M.ncols w
        row         = M.nrows w
        vw          = P.zip delta $ P.replicate (row * col) w
        delta       = map (M.fromList row col) . P.reverse . variate (col * row - 1) $ P.replicate (col * row) 0
        fxh         = map (\(v, w) -> f(w + v)) vw
        fx          = map (\(v, w) -> f(w - v)) vw
        -- 행렬에 대한 변화량을 생성해주는 함수
        variate 0 x = [1.0e-6 : tail x]
        variate n x = result : variate (n-1) x
            where result = F.toList $ update n 1.0e-6 $ S.fromList x
