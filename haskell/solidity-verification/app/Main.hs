module Main where

import DeployedContract

main :: IO ()
main = do
    let contract1Path = "contracts/Contract1.sol"
    let contract2Path = "contracts/Contract2.sol"
    compareContracts contract1Path contract2Path
