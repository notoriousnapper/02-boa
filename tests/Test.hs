{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Common
import Test.Tasty
import Text.Printf
import Control.Exception
import System.Exit

main :: IO ()
main = do
  sc <- initScore
  defaultMain (tests sc) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- getTotal sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

tests :: Score -> TestTree
tests sc = testGroup "Tests"
  [ testGroup "Normalizer"      (anfTests     sc)
  , testGroup "Adder"           (adderTests   sc)
  , testGroup "Boa"             (boaTests     sc)
  , testGroup "Your-Tests"      (yourTests    sc)
  ]

anfTests sc =
  [ anfTest sc "prim1"
      "add1(add1(add1(add1(x))))"
      "(let anf0 = add1(x), anf1 = add1(anf0), anf2 = add1(anf1) in add1(anf2))"

  , anfTest sc "prim2"
      "((2 + 3) * (12 - 4)) * (7 + 8)"
      "(let anf0 = 2 + 3, anf1 = 12 - 4, anf2 = anf0 * anf1, anf3 = 7 + 8 in anf2 * anf3)"

  , anfTest sc "let-1"
      "(let x = 10 in x + 5) + (let y = 20 in y - 5)"
      "(let anf0 = (let x = 10 in x + 5), anf1 = (let y = 20 in y - 5) in anf0 + anf1)"

  , anfTest sc "if-1"
      "(if x: y + 1 else: z + 1) + 12"
      "(let anf0 = (if x: y + 1 else: z + 1) in anf0 + 12)"
  ]

adderTests sc =
  [ mkTest sc "forty_one"  (Code "41")               (Right "41")
  , mkTest sc "nyi"        (Code "let x = 10 in x")  (Right "10")
  , mkTest sc "five"        File                     (Right "5")
  , mkTest sc "adds"        File                     (Right "8")
  , mkTest sc "subs"        File                     (Right "8")
  , mkTest sc "lets"        File                     (Right "14")
  , mkTest sc "expr0"       File                     (Right "600")
  ]

boaTests   sc =
  [ mkTest sc "expr1"       File      (Right "30")
  , mkTest sc "expr2"       File      (Right "20")
  , mkTest sc "expr3"       File      (Right "20")
  , mkTest sc "expr4"       File      (Right "-8")
  , mkTest sc "exp00"       File      (Right "65536")
  ]

yourTests sc =
  [ -- fill in your tests here
  ]



