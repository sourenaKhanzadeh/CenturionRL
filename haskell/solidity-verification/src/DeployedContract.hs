-- src/DeployContracts.hs
{-# LANGUAGE OverloadedStrings #-}

module DeployedContract where

import System.Process (readProcess)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, removeDirectoryRecursive, doesDirectoryExist)
import Control.Monad (when)

-- Function to compile a Solidity contract using solc
compileContract :: FilePath -> IO (Maybe (BS.ByteString, BS.ByteString))
compileContract contractPath = do
    let c_name = reverse . takeWhile (/= '.') . reverse $ takeWhile (/= '/') contractPath
    let outputDir = "build/" ++ c_name
    -- clean the output directory if it exists
    exists <- doesDirectoryExist outputDir
    when exists $ removeDirectoryRecursive outputDir
    let solcCmd = ["--abi", "--bin", contractPath, "-o", outputDir]
    _ <- readProcess "solc" solcCmd ""

    let binPath = outputDir ++ "/" ++ c_name ++ ".bin"
    let abiPath = outputDir ++ "/" ++ c_name ++ ".abi"

    binExists <- doesFileExist binPath
    abiExists <- doesFileExist abiPath

    if binExists && abiExists
        then do
            bytecode <- BS.readFile binPath
            abi <- BS.readFile abiPath
            return $ Just (bytecode, abi)
        else
            return Nothing

-- Function to compare compiled bytecode of two contracts
compareBytecode :: BS.ByteString -> BS.ByteString -> Correctness
compareBytecode bytecode1 bytecode2 = do
    return $ if bytecode1 == bytecode2 then Just True else Just False

-- Function to compare the contracts
compareContracts :: FilePath -> FilePath -> Correctness
compareContracts contractPath1 contractPath2 = do
    compiled1 <- compileContract contractPath1
    compiled2 <- compileContract contractPath2
    bytecode1 <- case compiled1 of
        Just (bytecode, _) -> return bytecode
        Nothing -> return BS.empty
    bytecode2 <- case compiled2 of
        Just (bytecode, _) -> return bytecode
        Nothing -> return BS.empty
    compareBytecode bytecode1 bytecode2

type Correctness = IO (Maybe Bool)
