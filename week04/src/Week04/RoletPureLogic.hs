module Week04.RoletPureLogic where


import qualified Prelude as Haskell

import Data.Char
import Data.List
import Data.Tuple.Utils
import System.Random
import Data.String (String)
import System.IO.Unsafe

hasil :: String -> Haskell.Bool
hasil a = unsafePerformIO Haskell.$ do
    pro <- inputBetValue a
    if "1" Haskell.== pro
       then Haskell.return Haskell.True
       else Haskell.return Haskell.False




doLookUp :: Haskell.Int -> [(Haskell.Int,Haskell.Int)] -> Haskell.Int -> Haskell.Int
doLookUp a [] z = z
doLookUp a (l:ls) z
    | Haskell.fst l Haskell.== a = Haskell.snd l
    | Haskell.otherwise  = doLookUp a ls z


inputBetValue :: String -> Haskell.IO String
inputBetValue betValue
    | betValue Haskell.== ""                      = Haskell.return "2"
    | inl                                         = genNum x Haskell.$ triTimeLookup betValue bets snd3 []
    | checkNumbers betValue Haskell.&& listChecker betValue (map Haskell.fst value) = genNum x s
    | Haskell.otherwise                           = Haskell.return "2"
    where
        inl = onList betValue bets
        s = toInt betValue
        x
            | inl       = triTimeLookup betValue bets thd3 0
            | Haskell.otherwise = doLookUp (length s) value 0



genNum betValue l = do
    w <- Haskell.fmap (`Haskell.mod` 37) randomIO

    if w `elem` l
        then do
            Haskell.return "1"
        else do
            Haskell.return "2"

reds :: [Haskell.Int]
reds   = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]

blacks :: [Haskell.Int]
blacks = [2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35]

bets :: [(String,[Haskell.Int],Haskell.Int)]
bets = [
        ("odd",    [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35],  2),
        ("even",   [2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36], 2),
        ("red",    reds,       2),
        ("black",  blacks,     2),
        ("1..18",  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18],            2),
        ("19..36", [19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36],   2),
        ("1..12",  [1,2,3,4,5,6,7,8,9,10,11,12],            3),
        ("13..24", [13,14,15,16,17,18,19,20,21,22,23,24],   3),
        ("25..36", [25,26,27,28,29,30,31,32,33,34,35,36],   3)
    ]

value :: [(Haskell.Int,Haskell.Int)]
value = [(1,36),(3,12),(6,6)]

onList :: String -> [(String,[Haskell.Int],Haskell.Int)] -> Haskell.Bool
onList = any Haskell.. (Haskell.. fst3) Haskell.. (Haskell.==)

toInt :: String -> [Haskell.Int]
toInt = map (\x -> Haskell.read x :: Haskell.Int) Haskell.. splitBy ','

checkNumbers :: String -> Haskell.Bool
checkNumbers = all (all isDigit) Haskell.. splitBy ','

triTimeLookup _ [] _ z = z
triTimeLookup a (l:ls) f z
    | fst3 l Haskell.== a = f l
    | Haskell.otherwise   = triTimeLookup a ls f z

listChecker :: String -> [Haskell.Int] -> Haskell.Bool
listChecker l lengths = length (toInt l) `elem` lengths Haskell.&& all (`elem` [0..36]) (toInt l)

splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]]
    where
        f c l@(x:xs)
            | c Haskell.== delimiter = [] : l
            | Haskell.otherwise      = (c:x) : xs
