import os
import pathlib
from solidity_rl.utils.contract_parser import parse_contract
from solidity_rl.utils.gas_estimator import estimate_gas, generate_default_args
from solidity_rl.utils.metrics import get_execution_time_and_size, extract_contract_metrics
from web3 import Web3

class Contract:
    def __init__(self, contract_pth: str):
        self._contract_pth = contract_pth
        self.constructor_args = []

        self.w3 = Web3(Web3.HTTPProvider('http://127.0.0.1:7545'))
        if not self.w3.is_connected():
            raise ConnectionError("Failed to connect to the Ethereum node.")

        self.w3.eth.default_account = self.w3.eth.accounts[0]
        self.contract_ast, self.contract_abi, self.contract_bin = self.parse_contract()
        self.deployed_gas_usage = 0
        self.execution_time, self.contract_size, self.contract_address = get_execution_time_and_size(self.read_contract(), self.deploy_contract)
        self.contract_gas_usage = estimate_gas(self.contract_address, self.contract_abi, self.w3)
        self.function_count, self.loop_count, self.storage_vars = extract_contract_metrics(self.contract_ast)
        # calculate total gas usage
        self.total_gas_usage = sum(x[1] for x in self.contract_gas_usage.items() if isinstance(x[1], int))

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

    

    def deploy_contract(self):
        contract = self.w3.eth.contract(abi=self.contract_abi, bytecode=self.contract_bin)

        constructor_abi = next((item for item in self.contract_abi if item.get('type') == 'constructor'), None)
        if constructor_abi and not self.constructor_args:
            self.constructor_args = generate_default_args(constructor_abi.get('inputs', []))

        tx_hash = contract.constructor(*self.constructor_args).transact({'from': self.w3.eth.default_account})
        tx_receipt = self.w3.eth.wait_for_transaction_receipt(tx_hash)
        self.deployed_gas_usage = tx_receipt.gasUsed

        return tx_receipt.contractAddress
    
    def compare_with_another_contract(self, other_contract_path: str):
        other_contract = Contract(other_contract_path)
        
        if set(self.contract_gas_usage.keys()) != set(other_contract.contract_gas_usage.keys()):
            return 1  # Different functions present

        for func in self.contract_gas_usage.keys():
            try:
                result_self = self.invoke_function(func)
                result_other = other_contract.invoke_function(func)
                if result_self != result_other:
                    return 1  # Functional difference detected
            except Exception as e:
                print(f"Error invoking function {func}: {e}")
                return 1  # Error indicates potential functional difference

        return 0  # Contracts produce the same outputs for all functions

    def invoke_function(self, func_name):
        contract = self.w3.eth.contract(address=self.contract_address, abi=self.contract_abi)
        func_instance = getattr(contract.functions, func_name)
        args = generate_default_args(func_instance.abi.get('inputs', []))
        return func_instance(*args).call({'from': self.w3.eth.default_account})

    def __str__(self):
        return f"Contract(address={self.contract_address}, execution_time={self.execution_time}, contract_size={self.contract_size}, \n" \
               f"gas_usage={self.contract_gas_usage}, function_count={self.function_count}, loop_count={self.loop_count}, storage_vars={self.storage_vars}, \n" \
               f"total_gas_usage={self.total_gas_usage}, deployed_gas_usage={self.deployed_gas_usage})"

    def __repr__(self):
        return self.__str__()

if __name__ == "__main__":
    contract = Contract(pathlib.Path(__file__).parent.parent / "data" / "raw_contracts" / "medium.sol")
    print(contract)
