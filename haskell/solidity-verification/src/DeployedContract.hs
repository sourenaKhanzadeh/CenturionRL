{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module DeployedContract where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics
import System.Environment (getArgs)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

-- Web3 libraries
import Network.Ethereum.Web3
import Network.Ethereum.Web3.Provider
import Network.Ethereum.Web3.Types

-- * Data types representing the contract artifact and ABI

-- | A parameter in the ABI.
data AbiParam = AbiParam {
  paramName :: String,
  paramType :: String
} deriving (Show, Generic)

instance FromJSON AbiParam where
  parseJSON = withObject "AbiParam" $ \v -> AbiParam
      <$> v .: "name"
      <*> v .: "type"

-- | One “entry” in the ABI. (This may be a function, event, constructor, etc.)
data AbiEntry = AbiEntry {
  abiName         :: Maybe String,  -- not all entries (e.g. constructors) have a name
  abiType         :: String,        -- e.g. "function", "constructor", "event", etc.
  abiInputs       :: [AbiParam],
  abiOutputs      :: [AbiParam],
  stateMutability :: Maybe String   -- e.g. "view", "nonpayable", etc.
} deriving (Show, Generic)

instance FromJSON AbiEntry where
  parseJSON = withObject "AbiEntry" $ \v -> AbiEntry
      <$> v .:? "name"
      <*> v .:  "type"
      <*> v .:? "inputs"  .!= []
      <*> v .:? "outputs" .!= []
      <*> v .:? "stateMutability"

-- | A contract artifact as output by many Solidity compilers.
data ContractArtifact = ContractArtifact {
  abi      :: [AbiEntry],
  bytecode :: String
} deriving (Show, Generic)

instance FromJSON ContractArtifact where
  parseJSON = withObject "ContractArtifact" $ \v -> ContractArtifact
      <$> v .: "abi"
      <*> v .: "bytecode"

-- * Utility: Load a contract artifact from a JSON file

loadArtifact :: FilePath -> IO ContractArtifact
loadArtifact path = do
  content <- B.readFile path
  case eitherDecode content of
    Left err       -> error ("Failed to decode JSON: " ++ err)
    Right artifact -> return artifact

-- * Deployment and testing functions

-- | Deploy a contract to the network.
--   (In a real system you’d want to supply a from–address and proper gas settings.)
deployContract :: ContractArtifact -> Web3 Address
deployContract artifact = do
  -- Remove the “0x” prefix if present
  let code = if take 2 (bytecode artifact) == "0x"
                then drop 2 (bytecode artifact)
                else bytecode artifact
  -- Send a transaction with no “to” field (i.e. a contract creation)
  txHash <- sendTx { to = Nothing, data' = Just ("0x" <> T.pack code) }
  liftIO $ putStrLn $ "Deploying contract, tx: " ++ show txHash
  -- Wait for the receipt (this is a simplified “polling” version)
  receipt <- awaitReceipt txHash
  case receipt of
    Just r  -> return (contractAddress r)
    Nothing -> error "Deployment failed: no receipt received."

-- | For each function ABI entry, we generate a “default” argument list based on the type.
--   (You may wish to make this more sophisticated.)
defaultArgs :: [AbiParam] -> [Value]
defaultArgs = map (\p -> defaultArg (paramType p))

-- | Produce a default value (as an Aeson Value) for a given Solidity type.
defaultArg :: String -> Value
defaultArg t
  | "uint" `elem` words t = Number 1
  | "int"  `elem` words t = Number 1
  | t == "address"        = String "0x0000000000000000000000000000000000000000"
  | t == "bool"           = Bool True
  | t == "string"         = String "test"
  | otherwise             = Null  -- unknown types

-- | A “placeholder” function that calls a contract function.
--
-- In a complete solution you would use an ABI–encoding library (such as Network.Ethereum.ABI)
-- to construct the call data from the function signature and arguments and then use 'eth_call'
-- to execute the function without a transaction.
--
-- Here we simply show a dummy definition.
callFunction :: Address -> String -> [Value] -> Web3 Value
callFunction addr fname args = do
  liftIO $ putStrLn $ "Calling " ++ fname ++ " on " ++ show addr ++ " with args " ++ show args
  -- Replace the following with proper ABI encoding/decoding logic.
  return Null

-- | For a given ABI function entry (if it has a name), call it with default arguments.
testFunction :: Address -> AbiEntry -> Web3 (Maybe Value)
testFunction addr entry =
  case abiName entry of
    Nothing    -> return Nothing  -- Skip constructors, etc.
    Just fname -> do
      let args = defaultArgs (abiInputs entry)
      result <- callFunction addr fname args
      return (Just result)

-- | Compare the outputs of all functions (from the ABI) when called on the two deployed contracts.
--
-- Here we assume that both contracts share the same ABI.
compareContracts :: Address -> Address -> [AbiEntry] -> Web3 Bool
compareContracts addr1 addr2 entries = do
  results <- forM entries $ \entry -> do
    res1 <- testFunction addr1 entry
    res2 <- testFunction addr2 entry
    let eq = res1 == res2
    liftIO $ putStrLn $ "Function " ++ show (abiName entry)
                     ++ " outputs equal? " ++ show eq
    return eq
  return (and results)

