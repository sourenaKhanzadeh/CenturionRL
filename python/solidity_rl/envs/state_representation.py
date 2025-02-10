import numpy as np
from typing import Dict

class StateRepresentation:
    """
    Converts Solidity smart contract metrics into a structured numerical state representation for RL.
    """

    def __init__(self):
        """
        Defines the state representation structure.
        """
        self.state_keys = ["gas_cost", "execution_time", "contract_size", "function_count", "loop_count", "storage_vars", "correctness"]
        self.num_features = len(self.state_keys)

    def encode(self, contract_metrics: Dict[str, float]) -> np.ndarray:
        """
        Encodes Solidity contract metrics into a numerical feature vector.

        Args:
            contract_metrics (dict): Solidity contract statistics.

        Returns:
            np.ndarray: Encoded state vector.
        """
        state_vector = np.array([
            contract_metrics.get("gas_cost", 0) / 1e6,  # Normalize gas cost
            contract_metrics.get("execution_time", 0) / 1000,  # Normalize execution time
            contract_metrics.get("contract_size", 0) / 10000,  # Normalize contract size
            contract_metrics.get("function_count", 0) / 100,  # Approximate function count range
            contract_metrics.get("loop_count", 0) / 50,  # Approximate loop count range
            contract_metrics.get("storage_vars", 0) / 100,  # Normalize storage variable count
            contract_metrics.get("correctness", 0)  # 0 = correct, 1 = incorrect
        ], dtype=np.float32)

        return state_vector

    def decode(self, state_vector: np.ndarray) -> Dict[str, float]:
        """
        Decodes a state vector back into Solidity contract metrics.

        Args:
            state_vector (np.ndarray): Encoded state vector.

        Returns:
            dict: Decoded contract metrics.
        """
        return {
            "gas_cost": state_vector[0] * 1e6,
            "execution_time": state_vector[1] * 1000,
            "contract_size": state_vector[2] * 10000,
            "function_count": state_vector[3] * 100,
            "loop_count": state_vector[4] * 50,
            "storage_vars": state_vector[5] * 100,
            "correctness": state_vector[6]
        }

    def get_state_shape(self) -> int:
        """
        Returns the number of features in the state representation.

        Returns:
            int: Number of state features.
        """
        return self.num_features

    def extract_metrics_from_ast(self, contract_ast: dict) -> dict:
        """
        Extracts metrics from the Solidity AST to initialize the environment state.

        Args:
            contract_ast (dict): The parsed AST of a Solidity contract.

        Returns:
            dict: Extracted metrics including gas cost, execution time, contract size, etc.
        """
        # Example logic for extracting metrics (this should be adapted to your specific AST structure)
        gas_cost = contract_ast.get('gas_cost', 100000)
        execution_time = contract_ast.get('execution_time', 500)
        contract_size = contract_ast.get('contract_size', 3000)
        function_count = len(contract_ast.get('nodes', []))
        loop_count = sum(1 for node in contract_ast.get('nodes', []) if node.get('name') == 'ForStatement')
        storage_vars = sum(1 for node in contract_ast.get('nodes', []) if node.get('name') == 'StateVariableDeclaration')
        correctness = 0  # Default to correct until proven otherwise

        return {
            "gas_cost": gas_cost,
            "execution_time": execution_time,
            "contract_size": contract_size,
            "function_count": function_count,
            "loop_count": loop_count,
            "storage_vars": storage_vars,
            "correctness": correctness
        }
