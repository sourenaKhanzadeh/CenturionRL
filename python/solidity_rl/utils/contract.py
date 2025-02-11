import os
import pathlib
from solidity_rl.utils.contract_parser import parse_contract
from solidity_rl.utils.sol_to_cfg import convert_sol_to_cfg
import web3

class Contract:
    def __init__(self, contract_pth: str, constructor_args: list = []):
        self._contract_pth = contract_pth
        self.constructor_args = constructor_args

        self.contract_ast = self.get_contract_ast()
        # self.contract_cfg = self.get_contract_cfg()
        self.contract_abi = self.get_contract_abi()
        self.contract_bin = self.get_contract_bin()
        self.contract_address = self.deploy_contract()
        # self.contract_gas_price = self.get_contract_gas_price()
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

    def get_contract_ast(self):
        return parse_contract(self._contract_pth)[0]

    def get_contract_cfg(self):
        return convert_sol_to_cfg(self._contract_pth)
    
    def estimate_gas_for_all_functions(self):
        """
        Automatically estimate gas usage for all functions in the contract.
        """
        w3 = web3.Web3(web3.HTTPProvider('http://127.0.0.1:7545'))
        contract = w3.eth.contract(address=self.contract_address, abi=self.contract_abi)

        gas_usage = {}

        for func in contract.abi:
            if func.get('type') == 'function':
                func_name = func['name']
                inputs = func['inputs']

                # Generate default arguments for the function based on input types
                args = []
                for input_arg in inputs:
                    if input_arg['type'].startswith('uint'):
                        args.append(0)
                    elif input_arg['type'] == 'string':
                        args.append("")
                    elif input_arg['type'] == 'address':
                        args.append('0x0000000000000000000000000000000000000000')
                    elif input_arg['type'].startswith('bool'):
                        args.append(False)
                    elif input_arg['type'].endswith('[]'):
                        args.append([])
                    else:
                        args.append(None)

                try:
                    txn = getattr(contract.functions, func_name)(*args).build_transaction({
                        'from': w3.eth.default_account,
                        'gas': 2000000,
                        'gasPrice': w3.eth.gas_price,
                        'nonce': w3.eth.get_transaction_count(w3.eth.default_account)
                    })
                    gas_estimate = w3.eth.estimate_gas(txn)
                    gas_usage[func_name] = gas_estimate
                except Exception as e:
                    gas_usage[func_name] = f"Error estimating gas: {e}"

        return gas_usage

    def deploy_contract(self):
        """
        Deploy the contract to Ganache and return the contract address.
        Automatically handles constructor arguments if required.
        """
        w3 = web3.Web3(web3.HTTPProvider('http://127.0.0.1:7545'))  # Connect to Ganache
        if not w3.is_connected():
            raise ConnectionError("Failed to connect to Ganache")

        w3.eth.default_account = w3.eth.accounts[0]  # Use the first Ganache account
        contract = w3.eth.contract(abi=self.contract_abi, bytecode=self.contract_bin)

        # Detect constructor inputs from ABI
        constructor_inputs = next((item['inputs'] for item in self.contract_abi if item.get('type') == 'constructor'), [])

        if constructor_inputs and not self.constructor_args:
            # Automatically handle constructor arguments with default values
            default_args = []
            for arg in constructor_inputs:
                if arg['type'].startswith('uint'):
                    default_args.append(0)
                elif arg['type'] == 'string':
                    default_args.append("")
                elif arg['type'] == 'address':
                    default_args.append('0x0000000000000000000000000000000000000000')
                elif arg['type'].startswith('bool'):
                    default_args.append(False)
                elif arg['type'].endswith('[]'):
                    default_args.append([])
                else:
                    raise ValueError(f"Unsupported constructor argument type: {arg['type']}")
            self.constructor_args = default_args

        # Deploy the contract with or without constructor arguments
        tx_hash = contract.constructor(*self.constructor_args).transact({'from': w3.eth.default_account, 'gas': 2000000})
        tx_receipt = w3.eth.wait_for_transaction_receipt(tx_hash)

        return tx_receipt['contractAddress']
    
    def get_contract_abi(self):
        """
        Get the ABI of the contract.
        """
        return parse_contract(self._contract_pth)[1]

    def get_contract_bin(self):
        """
        Get the BIN of the contract.
        """
        return parse_contract(self._contract_pth)[2]
    

    def __str__(self):
        return f"Contract(address={self.contract_address}, gas_usage={self.contract_gas_usage})"

    def __repr__(self):
        return self.__str__()


if __name__ == "__main__":
    contract = Contract(pathlib.Path(__file__).parent.parent / "data" / "raw_contracts" / "sample.sol")
    print(contract)

