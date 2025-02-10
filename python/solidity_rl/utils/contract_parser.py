import os
import json
import re
from solcx import compile_standard, install_solc, get_installed_solc_versions

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

    version_match = re.search(r"pragma solidity\s+([^\s]+);", contract_source)
    version = version_match.group(1) if version_match else "0.8.0"
    version = re.sub("[^0-9.]", "", version)

    if version not in get_installed_solc_versions():
        install_solc(version)

    try:
        compiled_contract = compile_standard(
            {
                "language": "Solidity",
                "sources": {os.path.basename(contract_path): {"content": contract_source}},
                "settings": {"outputSelection": {"*": {"": ["ast"]}}},
            },
            solc_version=version,
        )

        if "sources" not in compiled_contract or os.path.basename(contract_path) not in compiled_contract["sources"]:
            raise ValueError("Compilation output does not contain expected AST data.")

        ast = compiled_contract["sources"][os.path.basename(contract_path)]["ast"]
        return ast

    except Exception as e:
        print(f"Error parsing contract: {e}")
        raise e  # Re-raise the exception after logging to prevent UnboundLocalError


# Example Usage
if __name__ == "__main__":
    sample_contract_path = "python/solidity_rl/data/raw_contracts/sample.sol"
    output_json_path = "python/solidity_rl/data/processed/SampleContract_AST.json"

    try:
        ast = parse_contract(sample_contract_path)
        print("Parsed AST:", json.dumps(ast, indent=2))

        with open(output_json_path, "w", encoding="utf-8") as file:
            json.dump(ast, file, indent=4)

        print(f"AST saved to {output_json_path}")

    except Exception as e:
        print(f"Failed to parse contract: {e}")
