import os
import pathlib
from solidity_rl.utils.contract_parser import parse_contract
from solidity_rl.utils.sol_to_cfg import convert_sol_to_cfg
from web3 import Web3

class Contract:
    def __init__(self, contract_pth: str, constructor_args: list = []):
        self._contract_pth = contract_pth
        self.constructor_args = constructor_args

        self.w3 = Web3(Web3.HTTPProvider('http://127.0.0.1:7545'))
        if not self.w3.is_connected():
            raise ConnectionError("Failed to connect to the Ethereum node.")

        self.w3.eth.default_account = self.w3.eth.accounts[0]
        self.contract_ast, self.contract_abi, self.contract_bin = self.parse_contract()
        self.contract_address = self.deploy_contract()
        self.contract_gas_usage = self.estimate_gas_for_all_functions()

    @property
    def contract_pth(self):
        if not os.path.exists(self._contract_pth):
            raise FileNotFoundError(f"Contract file not found: {self._contract_pth}")
        return self._contract_pth

    @contract_pth.setter
    def contract_pth(self, value):
        self._contract_pth = value

    def read_contract(self):
        with open(self._contract_pth, 'r', encoding='utf-8') as f:
            return f.read()

    def parse_contract(self):
        ast, abi, bytecode = parse_contract(self._contract_pth)
        return ast, abi, bytecode

    def estimate_gas_for_all_functions(self):
        contract = self.w3.eth.contract(address=self.contract_address, abi=self.contract_abi)
        gas_usage = {}

        for func_abi in self.contract_abi:
            if func_abi.get('type') != 'function':
                continue
            
            func_name = func_abi['name']
            func_instance = getattr(contract.functions, func_name)
            
            try:
                args = self.generate_default_args(func_abi.get('inputs', []))
                gas_estimate = func_instance(*args).estimate_gas({'from': self.w3.eth.default_account})
                gas_usage[func_name] = gas_estimate
            except Exception as e:
                gas_usage[func_name] = f"Error estimating gas: {e}"

        return gas_usage

    def generate_default_args(self, abi_inputs):
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

    def deploy_contract(self):
        contract = self.w3.eth.contract(abi=self.contract_abi, bytecode=self.contract_bin)

        tx_hash = contract.constructor(*self.constructor_args).transact({'from': self.w3.eth.default_account})
        tx_receipt = self.w3.eth.wait_for_transaction_receipt(tx_hash)

        return tx_receipt.contractAddress

    def __str__(self):
        return f"Contract(address={self.contract_address}, gas_usage={self.contract_gas_usage})"

    def __repr__(self):
        return self.__str__()

if __name__ == "__main__":
    contract = Contract(pathlib.Path(__file__).parent.parent / "data" / "raw_contracts" / "sample.sol")
    print(contract)
