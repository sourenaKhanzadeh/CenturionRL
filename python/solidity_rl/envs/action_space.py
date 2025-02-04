import numpy as np
from typing import List, Dict, Any, Tuple
import random

class ActionSpace:
    def __init__(self):
        """
        Defines the set of optimization actions that the RL agent can take.
        Actions include various Solidity-level transformations.
        """
        self.actions = [
            "inline_function",
            "remove_unused_variable",
            "constant_fold_expression",
            "optimize_loop",
            "replace_require_with_revert",
            "short_circuit_logic",
            "pack_storage_variables",
            "remove_redundant_operations",
            "use_memory_instead_of_storage",
            "simplify_expression",
            "merge_redundant_if",
            "inline_modifier",
            "use_immutable",
            "convert_state_variable_to_constant",
            "optimize_function_visibility",
            "remove_dead_code",
        ]
        self.action_space_size = len(self.actions)

    def sample(self) -> str:
        """
        Randomly selects an action from the action space.
        
        Returns:
            str: A randomly chosen action.
        """
        return random.choice(self.actions)

    def get_action_by_index(self, index: int) -> str:
        """
        Retrieves an action by its index.

        Args:
            index (int): Index of the action.

        Returns:
            str: Corresponding action.
        """
        if 0 <= index < self.action_space_size:
            return self.actions[index]
        raise IndexError(f"Invalid action index: {index}")

    def get_index_by_action(self, action: str) -> int:
        """
        Retrieves the index of an action.

        Args:
            action (str): The action name.

        Returns:
            int: Index of the action.
        """
        if action in self.actions:
            return self.actions.index(action)
        raise ValueError(f"Invalid action: {action}")

    def __len__(self):
        return self.action_space_size

    def get_action_space(self) -> List[str]:
        """
        Returns the complete list of actions.

        Returns:
            List[str]: List of all possible actions.
        """
        return self.actions


class ContinuousActionSpace:
    def __init__(self):
        """
        Defines a continuous action space for Solidity optimization.
        Actions are represented as numerical values within a bounded range.
        """
        # Action categories (High-Level HRL decisions)
        self.action_categories = {
            0: "storage_optimization",
            1: "loop_optimization",
            2: "function_inline",
            3: "require_replacement",
            4: "expression_simplification"
        }
        
        # Parameter ranges for continuous action values
        self.action_bounds = {
            "storage_optimization": (-1.0, 1.0),  # Negative: Reduce storage, Positive: Merge vars
            "loop_optimization": (0.1, 2.0),  # Adjust loop unrolling factor
            "function_inline": (0, 1),  # Probability of inlining
            "require_replacement": (0, 1),  # Probability of replacement with revert
            "expression_simplification": (-1.0, 1.0)  # Adjust simplification level
        }

    def sample(self) -> Tuple[int, float]:
        """
        Samples a random continuous action.
        
        Returns:
            Tuple[int, float]: (Selected category index, continuous parameter)
        """
        category = np.random.choice(list(self.action_categories.keys()))
        bounds = self.action_bounds[self.action_categories[category]]
        action_value = np.random.uniform(bounds[0], bounds[1])
        return category, action_value

    def normalize_action(self, category: int, value: float) -> float:
        """
        Normalizes a given action value within its respective bounds.

        Args:
            category (int): Index of the action category.
            value (float): The raw action value.

        Returns:
            float: Normalized action value within its valid range.
        """
        bounds = self.action_bounds[self.action_categories[category]]
        return np.clip(value, bounds[0], bounds[1])

    def get_action_by_index(self, index: int) -> str:
        """
        Retrieves the action category by its index.

        Args:
            index (int): The category index.

        Returns:
            str: Corresponding action category.
        """
        return self.action_categories.get(index, "Invalid category")

    def __len__(self):
        return len(self.action_categories)

    def get_action_space(self) -> Dict[int, str]:
        """
        Returns all available action categories.

        Returns:
            Dict[int, str]: Mapping of category indices to category names.
        """
        return self.action_categories