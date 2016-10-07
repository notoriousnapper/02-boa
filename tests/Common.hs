{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf
import Control.Exception
import System.FilePath                  ((</>), (<.>))
import System.IO                        (withFile)
import System.Exit
import Data.List                        (isInfixOf)
import Data.Char (toLower)
import Data.IORef
import Language.Boa.Utils
import Language.Boa.Normalizer
import Language.Boa.Types  hiding     (Result)
import Language.Boa.Parser

import Debug.Trace (trace)


overflowError  = Left "Error: arithmetic overflow"
rLines         = Right . unlines
dynamicError t = Left ("Error: expected a " ++ pprint t)
staticError    = Left


--------------------------------------------------------------------------------
run :: FilePath -> Program -> IO Result
--------------------------------------------------------------------------------
run name pgm = do
  _ <- generateSource name pgm                 -- generate source file
  r <- executeShellCommand logF cmd timeLimit  -- compile & run
  readResult resF logF r
  where
    cmd  = printf "make %s"     resF
    resF = dirExt "output" name Res
    logF = dirExt "output" name Log

-- | `timeLimit` for each test is 15 seconds
timeLimit :: Int
timeLimit = 15 * (10 ^ 6)

--------------------------------------------------------------------------------
generateSource :: FilePath -> Program -> IO ()
--------------------------------------------------------------------------------
generateSource _    File       = return ()
generateSource name (Code pgm) = writeFile srcF pgm
  where
    srcF                       = dirExt "input"  name Src

--------------------------------------------------------------------------------
readResult :: FilePath -> FilePath -> ExitCode -> IO Result
--------------------------------------------------------------------------------
readResult resF _     ExitSuccess      = Right <$> readFile resF
readResult _    _    (ExitFailure 100) = Left  <$> return "TIMEOUT!"
readResult _    logF (ExitFailure _  ) = Left  <$> readFile logF

dirExt :: FilePath -> FilePath -> Ext -> FilePath
dirExt dir name e = "tests" </> dir </> name `ext` e

--------------------------------------------------------------------------------
-- | A test program is either a filename or a text representation of source
--------------------------------------------------------------------------------
data Program = File | Code Text
type Result  = Either Text Text

--------------------------------------------------------------------------------
-- | Construct a single compiler test case from a `Program`
--------------------------------------------------------------------------------
mkTest :: Score -> String -> Program -> Result -> TestTree
mkTest sc name pgm = mkTest' sc 1 name (run name pgm)

mkTest' :: Score -> Int -> String -> IO Result -> Result -> TestTree
mkTest' sc n name act expect = testCase name $ do
  updateTotal sc n
  res <- act
  check sc n res expect

anfTest :: Score -> String -> Text -> Text -> TestTree
anfTest sc name inS expS = mkTest' sc 1 name (return $ anfRun inS) (Right expS)

anfRun :: Text -> Result
anfRun = Right . pprint . anormal . parse ""


--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> (a -> b, a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    if f x == expR
      then updateCurrent sc points
      else assertFailure "Wrong Result"

-- check :: Result -> Result -> TestTree
check sc n (Right resV) (Right expectV)
  | trim expectV == trim resV          = updateCurrent sc n
  | otherwise                          = assertFailure "Wrong result"
check sc n (Left resE)  (Left  expectE)
  | matchError expectE resE            = updateCurrent sc n
  | otherwise                          = assertFailure "Wrong error"

check _ _ (Left resE)  (Right expectV) = assertEqual "Unexpected error"   ("Value " ++ expectV) ("Error " ++ resE)
check _ _ (Right resV) (Left  expectE) = assertEqual "Unexpected result"  ("Error " ++ expectE) ("Value " ++ resV)

matchError expectE resE = tx expectE `isInfixOf` tx resE
  where
      tx = map toLower
--------------------------------------------------------------------------------
type Score = IORef (Int, Int)
--------------------------------------------------------------------------------

getTotal :: Score -> IO (Int, Int)
getTotal = readIORef

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)
