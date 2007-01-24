module Text.Regex.TDFA.Live where

import Data.Array
import Data.List
import qualified Data.Map as Map

import Text.Regex.TDFA
import Text.Regex.TDFA.Common
import Text.Regex.TDFA.Pattern
import Text.Regex.TDFA.ReadRegex
import Text.Regex.TDFA.CorePattern
import Text.Regex.TDFA.TNFA
import Text.Regex.TDFA.TDFA
import Text.Regex.TDFA.Wrap
import Text.Regex.TDFA.Run

import qualified Text.Regex.TRE as TRE

toP = either (error.show) id . parseRegex 
toQ = patternToQ defaultCompOpt . toP
toNFA = patternToNFA defaultCompOpt . toP
display_NFA = mapM_ print . elems . (\(x,_,_) -> snd x) . toNFA 

toDFA = nfaToDFA . toNFA


testOne s op r = 
  let foo ::  String
      foo = concatMap (\(o,l) -> show (o,(o+l))) (elems (op s r :: Array Int (Int,Int)))
  in foo

testOne' s op r =
  let bar ::  (String,String,String,[String])
      bar = op s r
  in bar


toTest :: String -> (Int,String,String,String)
toTest line = let [n,regex,input,output] = words line
                  noQ [] = []
                  noQ ('?':xs) = '-':'1':noQ xs
                  noQ (x:xs) = x:noQ xs
              in (read n,regex,input,noQ output)

load = do
  text <- readFile "totest.txt"
  let tests = map toTest (lines text)
  -- mapM_ print tests
  return tests

checkTest op (n,regex,input,output) =
  let output' = testOne input op regex
  in if output == output'
       then do putStrLn ("Pass #"++show n)
               return []
       else do putStrLn ("Fail #"++show n)
               putStrLn input
               putStrLn regex
               putStrLn output
               putStrLn output'
               return [n]

checkTests op = fmap concat (mapM (checkTest op) =<< load)

tdfa x r = let q :: Text.Regex.TDFA.Wrap.Regex
               q = makeRegexOpts (defaultCompOpt { rightAssoc = True, lastStarGreedy = True }) defaultExecOpt r
           in match q x

regressOP op =
  let mult = ["","?","*"]
      test = [ "((x"++a++")"++b++"x)"++c | a <- mult, b <- mult, c <- mult ]
      source = inits "xxxxx"
      results :: [(MatchArray,String,String)]
      results = [ (s `op` r,s,r) | s <- source, r <- test ]
  in putStr . unlines . map show $ results

regress = regressOP (tdfa)

regressTRE = regressOP (TRE.=~)