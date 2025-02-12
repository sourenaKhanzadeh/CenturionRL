{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module SBV where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import GHC.Generics
import System.Environment (getArgs)
import System.Process (readProcess)
import Data.Maybe (fromMaybe)
import Data.SBV hiding (prove)  -- Hide SBV's own 'prove' to avoid ambiguity.
import Data.SBV.Trans (prove)

--------------------------------------------------------------------------------
-- Our internal DSL for arithmetic expressions

data Expr = Var String
          | Lit Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Contract Definition

data Contract = Contract
  { contractName :: String
  , logic        :: Expr
  } deriving (Show, Generic)

--------------------------------------------------------------------------------
-- Evaluator for our DSL

type Env = [(String, SInteger)]

evalExpr :: Env -> Expr -> SInteger
evalExpr env (Var s) =
  case lookup s env of
    Just v  -> v
    Nothing -> error ("Variable " ++ s ++ " not found in environment")
evalExpr _   (Lit n)     = literal n
evalExpr env (Add a b)   = evalExpr env a + evalExpr env b
evalExpr env (Sub a b)   = evalExpr env a - evalExpr env b
evalExpr env (Mul a b)   = evalExpr env a * evalExpr env b

contractFun :: Contract -> SInteger -> SInteger -> SInteger
contractFun contract x y = evalExpr [("x", x), ("y", y)] (logic contract)

--------------------------------------------------------------------------------
-- Extracting the AST from Solidity and building our Expr

-- | Run solc on a Solidity file to obtain the compact JSON AST.
-- Make sure that solc is in your PATH.
extractAST :: FilePath -> IO Value
extractAST filePath = do
    rawOutput <- readProcess "solc" ["--ast-compact-json", filePath] ""
    -- Extract the JSON part: drop any leading text until the first '{'
    let jsonPart = dropWhile (/= '{') rawOutput
    case eitherDecode (BLC.pack jsonPart) of
      Left err  -> error ("Error decoding AST JSON from " ++ filePath ++ ": " ++ err)
      Right ast -> return ast

-- | Given the AST (as a JSON Value), extract the expression representing the
-- body of the `compute` function.
-- NOTE: This is a placeholder. In a real tool you would parse the AST to find
-- the FunctionDefinition node for `compute` and convert its return expression.
parseComputeFromAST :: Value -> Maybe Expr
parseComputeFromAST _ =
  -- For demonstration, assume the compute function is:
  --    return x + y + 1;
  Just (Add (Add (Var "x") (Var "y")) (Lit 1))

-- | Load a Contract by extracting its compute function’s semantics from the AST.
loadContract :: FilePath -> IO Contract
loadContract filePath = do
    ast <- extractAST filePath
    let maybeExpr = parseComputeFromAST ast
    expr <- case maybeExpr of
              Nothing   -> error $ "Could not extract compute function from AST of " ++ filePath
              Just e    -> return e
    return $ Contract { contractName = filePath, logic = expr }
--------------------------------------------------------------------------------
-- Main: Verify Equivalence Using SBV

main :: IO ()
main = do
  -- args <- getArgs
  args <- return ["contracts/Contract1.sol", "contracts/Contract2.sol"]
  case args of
    [path1, path2] -> do
      contract1 <- loadContract path1
      contract2 <- loadContract path2
      putStrLn $ "Loaded contracts: " ++ contractName contract1 ++ " and " ++ contractName contract2
      putStrLn "Proving that for all x and y the contracts compute the same result..."
      result <- prove $ \(x :: SInteger) (y :: SInteger) ->
                  contractFun contract1 x y .== contractFun contract2 x y
      print result
    _ -> putStrLn "Usage: solidity-verification <Contract1.sol> <Contract2.sol>"
