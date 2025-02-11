import time
import sys
from typing import Tuple, Callable, Dict

def get_execution_time_and_size(contract_code: str, func: Callable) -> Tuple[float, int]:
    contract_size = sys.getsizeof(contract_code)
    start_time = time.time()
    res = func()
    execution_time = time.time() - start_time
    return execution_time, contract_size, res

def extract_contract_metrics(ast: Dict) -> Tuple[int, int, int]:
    function_count, loop_count, storage_vars = 0, 0, 0

    def recursive_search(node):
        nonlocal function_count, loop_count, storage_vars
        if isinstance(node, dict):
            if node.get('nodeType') == 'FunctionDefinition':
                function_count += 1
            elif node.get('nodeType') in ['ForStatement', 'WhileStatement', 'DoWhileStatement']:
                loop_count += 1
            elif node.get('nodeType') == 'VariableDeclaration' and node.get('storageLocation') == 'storage':
                storage_vars += 1

            for _, value in node.items():
                if isinstance(value, (dict, list)):
                    recursive_search(value)
        elif isinstance(node, list):
            for item in node:
                recursive_search(item)

    recursive_search(ast)
    return function_count, loop_count, storage_vars


