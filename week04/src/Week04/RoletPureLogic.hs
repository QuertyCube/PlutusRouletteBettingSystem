module Week04.RoletPureLogic where


import qualified Prelude as Haskell

import Data.Char
import Data.List
import Data.Tuple.Utils
import System.Random
import Data.String (String)


-- hasil :: String -> IO()
-- hasil a = do
--     pro <- inputBetValue a
--     putStrLn ("Hasil pro : "++ pro)
--     if "Win" == pro
--        then putStrLn "Benar"
--        else if "Salah" == pro
--         then putStrLn "Salah"
--         else putStrLn pro


doLookUp a [] z = z
doLookUp a (l:ls) z
    | Haskell.fst l Haskell.== a = Haskell.snd l
    | Haskell.otherwise  = doLookUp a ls z

inputBetValue betValue
    | betValue Haskell.== ""                      = Haskell.return Haskell.False
    | inl                                         = genNum x Haskell.$ triTimeLookup betValue bets snd3 []
    | checkNumbers betValue Haskell.&& listChecker betValue (map Haskell.fst value) = genNum x s
    | Haskell.otherwise                           = Haskell.return Haskell.False
    where
        inl = onList betValue bets
        s = toInt betValue
        x
            | inl       = triTimeLookup betValue bets thd3 0
            | Haskell.otherwise = doLookUp (length s) value 0


genNum betValue l = do
    w <- Haskell.fmap (`Haskell.mod` 37) randomIO

    Haskell.putStr Haskell.$ "Number won: " ++ Haskell.show w ++ " "

    if w Haskell.== 0
        then Haskell.putStr ""
        else do
            if w `elem` reds
                then Haskell.putStr "red"
                else Haskell.putStr "black"
            if Haskell.odd w 
                then Haskell.putStrLn " odd" 
                else Haskell.putStrLn " even"

    if w `elem` l
        then do
            Haskell.return Haskell.True
        else do
            Haskell.return Haskell.False

reds   = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]
blacks = [1..36] \\ reds

bets = [
        ("odd",    [1,3..35],  2),
        ("even",   [2, 4..36], 2),
        ("red",    reds,       2),
        ("black",  blacks,     2),
        ("1..18",  [1..18],    2),
        ("19..36", [19..36],   2),
        ("1..12",  [1..12],    3),
        ("13..24", [13..24],   3),
        ("25..36", [25..36],   3)
    ]

value = [
        (1, 36),
        (3, 12),
        (6, 6)
    ]

onList = any Haskell.. (Haskell.. fst3) Haskell.. (Haskell.==)

toInt = map (\x -> Haskell.read x :: Haskell.Int) Haskell.. splitBy ','

checkNumbers = all (all isDigit) Haskell.. splitBy ','

triTimeLookup _ [] _ z = z
triTimeLookup a (l:ls) f z
    | fst3 l Haskell.== a = f l
    | Haskell.otherwise   = triTimeLookup a ls f z

listChecker l lengths = length (toInt l) `elem` lengths Haskell.&& all (`elem` [0..36]) (toInt l)


splitBy delimiter = foldr f [[]]
    where
        f c l@(x:xs)
            | c Haskell.== delimiter = [] : l
            | Haskell.otherwise      = (c:x) : xs