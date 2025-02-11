-- src/DeployContracts.hs
{-# LANGUAGE OverloadedStrings #-}

module DeployedContract where

import System.Process (readProcess)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Directory (removeDirectoryRecursive)
-- Function to compile a Solidity contract using solc
compileContract :: FilePath -> IO (Maybe (BS.ByteString, BS.ByteString))
compileContract contractPath = do
    let c_name = reverse . takeWhile (/= '.') . reverse $ takeWhile (/= '/') contractPath
    let outputDir = "build/" ++ c_name
    -- clean the output directory
    removeDirectoryRecursive outputDir
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
compareBytecode :: BS.ByteString -> BS.ByteString -> Bool
compareBytecode bytecode1 bytecode2 = bytecode1 == bytecode2

-- Function to compare the contracts
compareContracts :: FilePath -> FilePath -> IO ()
compareContracts contractPath1 contractPath2 = do
    compiled1 <- compileContract contractPath1
    compiled2 <- compileContract contractPath2

    case (compiled1, compiled2) of
        (Just (bytecode1, _), Just (bytecode2, _)) ->
            if compareBytecode bytecode1 bytecode2
                then putStrLn "Contracts are functionally equivalent."
                else putStrLn "Contracts differ in functionality."
        _ -> putStrLn "Error: Failed to compile one or both contracts."
