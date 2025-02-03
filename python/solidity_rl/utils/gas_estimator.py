from web3 import Web3
import json
import os

def estimate_gas(contract_path: str, function_name: str, args=None, rpc_url="http://127.0.0.1:8545"):
    """
    Estimates the gas usage of a given function in a Solidity contract.

    Args:
        contract_path (str): Path to the compiled Solidity contract ABI & Bytecode.
        function_name (str): Name of the function to estimate gas for.
        args (tuple, optional): Arguments for the contract function. Defaults to None.
        rpc_url (str): RPC URL of the Ethereum node (Ganache, Hardhat, or real network).

    Returns:
        int: Estimated gas usage.
    """
    if not os.path.exists(contract_path):
        raise FileNotFoundError(f"Compiled contract JSON not found: {contract_path}")

    # Load compiled contract
    with open(contract_path, "r", encoding="utf-8") as file:
        contract_data = json.load(file)

    bytecode = contract_data["bytecode"]
    abi = contract_data["abi"]

    # Connect to Web3
    w3 = Web3(Web3.HTTPProvider(rpc_url))
    if not w3.is_connected():
        raise ConnectionError("Failed to connect to Ethereum node.")

    # Deploy contract
    Contract = w3.eth.contract(abi=abi, bytecode=bytecode)
    tx_hash = Contract.constructor().transact({"from": w3.eth.accounts[0]})
    tx_receipt = w3.eth.wait_for_transaction_receipt(tx_hash)
    contract_instance = w3.eth.contract(address=tx_receipt.contractAddress, abi=abi)

    # Estimate gas for function call
    gas_estimate = contract_instance.functions[function_name](*args).estimate_gas({"from": w3.eth.accounts[0]})
    
    return gas_estimate

# Example Usage
if __name__ == "__main__":
    compiled_contract_path = "contracts/compiled/SampleContract.json"
    
    try:
        gas_used = estimate_gas(compiled_contract_path, "increment")
        print(f"Estimated Gas Usage: {gas_used}")
    except Exception as e:
        print(f"Gas estimation failed: {e}")
