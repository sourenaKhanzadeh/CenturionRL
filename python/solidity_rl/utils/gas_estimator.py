from typing import Dict, List, Any
from web3 import Web3


def estimate_gas(contract_address:str, contract_abi:list, w3:Web3) -> Dict[str, int]:
    """
    Estimates the gas usage of a given function in a Solidity contract.

    Args:
        contract_address (str): Address of the deployed contract.
        contract_abi (list): ABI of the deployed contract.
        w3 (Web3): Web3 instance.

    Returns:
        int: Estimated gas usage.
    """
    contract = w3.eth.contract(address=contract_address, abi=contract_abi)
    gas_usage = {}

    for func_abi in contract_abi:
        if func_abi.get('type') != 'function':
            continue
        
        func_name = func_abi['name']
        func_instance = getattr(contract.functions, func_name)
        
        try:
            args = generate_default_args(func_abi.get('inputs', []))
            gas_estimate = func_instance(*args).estimate_gas({'from': w3.eth.default_account})
            gas_usage[func_name] = gas_estimate
        except Exception as e:
            gas_usage[func_name] = f"Error estimating gas: {e}"
        
    return gas_usage


def generate_default_args(abi_inputs: list) -> List[Any]:
    """
    Generates default arguments for a given ABI.

    Args:
        abi_inputs (list): ABI inputs.

    Returns:
        list: Default arguments.
    """
    args = []
    for input_arg in abi_inputs:
        if not isinstance(input_arg, dict):
            continue  # Skip if input_arg is not a dict
        arg_type = input_arg.get('type', '')
        if arg_type.startswith('uint'):
            args.append(0)
        elif arg_type == 'string':
            args.append("")
        elif arg_type == 'address':
            args.append('0x0000000000000000000000000000000000000000')
        elif arg_type.startswith('bool'):
            args.append(False)
        elif arg_type.endswith('[]'):
            args.append([])
        else:
            args.append(None)
    return args


