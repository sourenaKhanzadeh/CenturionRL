{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Verify where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Data.List (intersect)
import Data.SBV hiding (prove)
import Data.SBV.Trans (prove)
import System.Environment (getArgs)
import Text.Regex.TDFA ((=~))

--------------------------------------------------------------------------------
-- Internal DSL for representing function semantics

data Expr = Var String
          | Lit Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | IfThenElse Expr Expr Expr  -- condition, then, else
          deriving (Show, Eq)

-- A simple interpreter for our DSL.
-- We assume that all variables are symbolic integers.
evalExpr :: [(String, SInteger)] -> Expr -> SInteger
evalExpr env (Var s) =
  case lookup s env of
    Just v  -> v
    Nothing -> error ("Variable " ++ s ++ " not found")
evalExpr _   (Lit n)     = literal n
evalExpr env (Add a b)   = evalExpr env a + evalExpr env b
evalExpr env (Sub a b)   = evalExpr env a - evalExpr env b
evalExpr env (Mul a b)   = evalExpr env a * evalExpr env b
evalExpr env (IfThenElse cond e1 e2) =
  ite (evalExpr env cond ./= 0)
      (evalExpr env e1)
      (evalExpr env e2)

--------------------------------------------------------------------------------
-- A model for a contract function

data FunctionModel = FunctionModel {
  funcName   :: String,    -- The function name
  inputs     :: [String],  -- Parameter names (we assume they are all SInteger)
  funcOutput :: Expr,      -- A symbolic expression representing the function's output
  isPure     :: Bool       -- Whether the function is pure (view)
} deriving (Show)

--------------------------------------------------------------------------------
-- Naïve extraction of function definitions from a Solidity source file.
--
-- This function reads the source file and uses simple regex-based heuristics to
-- locate function definitions and collect their bodies.
--
-- We return a list of (function name, function body) pairs.
extractFunctionsFromSource :: FilePath -> IO [(Text, Text)]
extractFunctionsFromSource filePath = do
  content <- TIO.readFile filePath
  let ls = T.lines content
  return (extractFunctionsFromLines ls)

-- | Scan lines and whenever a line looks like a function definition, extract the name and body.
extractFunctionsFromLines :: [Text] -> [(Text, Text)]
extractFunctionsFromLines [] = []
extractFunctionsFromLines (l:ls) =
  case matchFunctionDef l of
    Just fname ->
      let (bodyLines, rest) = collectFunctionBody ls
          funcBody = T.unlines (l : bodyLines)
      in (fname, funcBody) : extractFunctionsFromLines rest
    Nothing -> extractFunctionsFromLines ls

-- | Try to match a function definition using a regular expression.
-- We look for lines like: function <name>(...
matchFunctionDef :: Text -> Maybe Text
matchFunctionDef line =
  let pattern = "function[[:space:]]+([A-Za-z0-9_]+)\\(" :: String
      res = T.unpack line =~ pattern :: [[String]]
  in case res of
       ((_:fname:_):_) -> Just (T.pack fname)
       _ -> Nothing

-- | Collect lines until a line containing "}" is encountered (naïvely).
collectFunctionBody :: [Text] -> ([Text], [Text])
collectFunctionBody [] = ([], [])
collectFunctionBody (l:ls)
  | T.isInfixOf "}" l = ([l], ls)
  | otherwise = let (body, rest) = collectFunctionBody ls
                in (l:body, rest)

--------------------------------------------------------------------------------
-- Convert a (name, body) pair to a FunctionModel using simple heuristics.
convertToFunctionModel :: (Text, Text) -> FunctionModel
convertToFunctionModel (fname, body) =
  let nameStr = T.unpack fname
      bodyStr = T.unpack body
      -- For demonstration, we use the following heuristics:
      -- * "getCount" returns the state variable "count".
      -- * "increment": if the body contains "count += 1", we assume it means count + 1;
      --                if it contains "count -= 1", assume count - 1.
      -- * "decrement": similarly.
      model | nameStr == "getCount" = FunctionModel "getCount" [] (Var "count") True
            | nameStr == "increment" && "count += 1" `T.isInfixOf` T.pack bodyStr
                      = FunctionModel "increment" [] (Add (Var "count") (Lit 1)) False
            | nameStr == "increment" && "count -= 1" `T.isInfixOf` T.pack bodyStr
                      = FunctionModel "increment" [] (Sub (Var "count") (Lit 1)) False
            | nameStr == "decrement" && "count += 1" `T.isInfixOf` T.pack bodyStr
                      = FunctionModel "decrement" [] (Add (Var "count") (Lit 1)) False
            | nameStr == "decrement" && "count -= 1" `T.isInfixOf` T.pack bodyStr
                      = FunctionModel "decrement" [] (Sub (Var "count") (Lit 1)) False
            | otherwise = FunctionModel nameStr [] (Var "unknown") True
  in model

-- | Extract a list of FunctionModels from a Solidity source file.
extractFunctionModels :: FilePath -> IO [FunctionModel]
extractFunctionModels filePath = do
  funcPairs <- extractFunctionsFromSource filePath
  return $ map convertToFunctionModel funcPairs

--------------------------------------------------------------------------------
-- A contract model: a collection of function models extracted from one Solidity file.
data ContractModel = ContractModel {
  contractFile   :: FilePath,
  functionModels :: [FunctionModel]
} deriving (Show)

loadContractModel :: FilePath -> IO ContractModel
loadContractModel filePath = do
  funcs <- extractFunctionModels filePath
  return $ ContractModel filePath funcs

--------------------------------------------------------------------------------
-- Verification: Compare two function models symbolically.
--
-- For a pure (or view) function, we assume it maps from the global state (here,
-- the symbolic variable "count") and its inputs to an output.
verifyFunctionModel :: FunctionModel -> FunctionModel -> Symbolic SBool
verifyFunctionModel fm1 fm2 = do
  inputSyms <- mapM free (inputs fm1)
  countSym <- free "count" :: Symbolic SInteger
  let env = [("count", countSym)] ++ zip (inputs fm1) inputSyms
      out1 = evalExpr env (funcOutput fm1)
      out2 = evalExpr env (funcOutput fm2)
  return $ out1 .== out2

--------------------------------------------------------------------------------
-- Verify that every common function (by name) between two contracts behaves equivalently.
verifyContracts :: ContractModel -> ContractModel -> Symbolic SBool
verifyContracts cm1 cm2 = do
  let names1 = map funcName (functionModels cm1)
      names2 = map funcName (functionModels cm2)
      common = names1 `intersect` names2
  props <- mapM (\fname -> do
                     let f1 = head [fm | fm <- functionModels cm1, funcName fm == fname]
                         f2 = head [fm | fm <- functionModels cm2, funcName fm == fname]
                     liftIO $ putStrLn $ "Verifying function: " ++ fname
                     verifyFunctionModel f1 f2
                 ) common
  return $ foldr (.&&) sTrue props

--------------------------------------------------------------------------------
-- Main: Run the verification.
--
-- Usage: solidity-verification <Contract1.sol> <Contract2.sol>
main :: IO ()
main = do
  args <- getArgs
  case args of
    [file1, file2] -> do
      cm1 <- loadContractModel file1
      cm2 <- loadContractModel file2
      result <- prove $ verifyContracts cm1 cm2
      print result
    _ -> putStrLn "Usage: solidity-verification <contract1.sol> <contract2.sol>"
