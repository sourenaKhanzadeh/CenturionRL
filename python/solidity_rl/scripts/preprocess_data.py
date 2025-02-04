import json
from solidity_rl.utils.contract_parser import parse_contract

def preprocess_data(contract_path, output_path="data/processed/contract.json"):
    """Parses and processes a Solidity contract for training."""
    ast = parse_contract(contract_path)
    with open(output_path, "w") as file:
        json.dump(ast, file, indent=4)
    print(f"Contract processed and saved to {output_path}")

if __name__ == "__main__":
    contract_path = "contracts/sample.sol"
    preprocess_data(contract_path)
