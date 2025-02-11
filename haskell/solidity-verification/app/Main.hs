module Main where

import qualified DeployedContract as DC

main :: IO ()
main = do
    let contract1Path = "contracts/Contract1.sol"
    let contract2Path = "contracts/Contract2.sol"
    result <- DC.compareContracts contract1Path contract2Path
    case result of
        Just True -> putStrLn "Contracts are equivalent"
        Just False -> putStrLn "Contracts are different"
        Nothing -> putStrLn "Error comparing contracts"
