import os
import json
import re
from solcx import compile_standard, install_solc, get_installed_solc_versions

def parse_contract(contract_pth: str):
    """
    Parse the Solidity contract and return AST, ABI & Bytecode.
    """
    with open(contract_pth, "r", encoding="utf-8") as file:
        contract_source = file.read()

    # Extract Solidity version from pragma
    version_match = re.search(r"pragma solidity\s+([^\s]+);", contract_source)
    version = version_match.group(1) if version_match else "0.8.0"
    version = re.sub("[^0-9.]", "", version)

    if version not in get_installed_solc_versions():
        install_solc(version)

    try:
        compiled_contract = compile_standard(
            {
                "language": "Solidity",
                "sources": {os.path.basename(contract_pth): {"content": contract_source}},
                "settings": {
                    "outputSelection": {
                        "*": {
                            "*": ["abi", "evm.bytecode.object", "metadata"],
                            "": ["ast"]
                        }
                    }
                },
            },
            solc_version=version,
        )


        # Attempt to retrieve AST from 'sources'
        ast = compiled_contract.get("sources", {}).get(os.path.basename(contract_pth), {}).get("ast")
        contract_data = None
        contract_name = None

        # Fallback if AST is not found in 'sources'
    
        contract_data = compiled_contract["contracts"][os.path.basename(contract_pth)]
        contract_name = next(iter(contract_data))

        if not ast:
            raise ValueError("AST not found in the compiled contract output.")

        abi = contract_data[contract_name]["abi"]
        bytecode = contract_data[contract_name]["evm"]["bytecode"]["object"]

        return ast, abi, bytecode

    except Exception as e:
        print(f"Error parsing contract: {e}")
        raise e


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
