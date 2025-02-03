import os
import json
from solcx import compile_standard

def parse_contract(contract_path: str):
    """
    Parses a Solidity contract and returns its Abstract Syntax Tree (AST).

    Args:
        contract_path (str): Path to the Solidity contract (.sol file).

    Returns:
        dict: Parsed AST of the Solidity contract.
    """
    if not os.path.exists(contract_path):
        raise FileNotFoundError(f"Contract file not found: {contract_path}")

    with open(contract_path, "r", encoding="utf-8") as file:
        contract_source = file.read()

    try:
        compiled_contract = compile_standard(
            {
                "language": "Solidity",
                "sources": {os.path.basename(contract_path): {"content": contract_source}},
                "settings": {
                    "outputSelection": {
                        "*": {
                            "*": ["ast"]
                        }
                    }
                },
            },
            solc_version="0.8.0",  # You might want to adjust this version
        )
        ast = compiled_contract["sources"][os.path.basename(contract_path)]["ast"]
        return ast

    except Exception as e:
        raise ValueError(f"Error parsing contract: {e}")

def save_ast_to_json(ast: dict, output_path: str):
    """
    Saves the AST of a Solidity contract to a JSON file.

    Args:
        ast (dict): The AST to save.
        output_path (str): Path to save the JSON file.
    """
    with open(output_path, "w", encoding="utf-8") as file:
        json.dump(ast, file, indent=4)

# Example Usage
if __name__ == "__main__":
    # Ensure you have a sample Solidity contract
    sample_contract_path = "contracts/SampleContract.sol"  # Adjust this path
    output_json_path = "contracts/SampleContract_AST.json"

    try:
        ast = parse_contract(sample_contract_path)
        print("Parsed AST:", json.dumps(ast, indent=2))

        save_ast_to_json(ast, output_json_path)
        print(f"AST saved to {output_json_path}")

    except Exception as e:
        print(f"Failed to parse contract: {e}")
