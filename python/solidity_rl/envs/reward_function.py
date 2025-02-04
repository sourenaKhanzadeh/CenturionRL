import numpy as np

class RewardFunction:
    def __init__(self):
        """
        Defines the reward function for Solidity optimization in a continuous space.
        The goal is to reduce gas usage, maintain correctness, and improve execution efficiency.
        """
        # Weight factors for different optimization metrics
        self.weights = {
            "gas_reduction": 1.0,  # Main objective: Lower gas cost
            "execution_speed": 0.5,  # Secondary: Improve efficiency
            "contract_size": -0.2,  # Smaller contracts get a bonus
            "correctness_penalty": -10.0  # Strong penalty for breaking functionality
        }

    def compute_reward(self, before_metrics: dict, after_metrics: dict) -> float:
        """
        Computes the reward based on Solidity optimization impact.

        Args:
            before_metrics (dict): Solidity contract metrics before applying an action.
            after_metrics (dict): Solidity contract metrics after applying an action.

        Returns:
            float: Calculated reward.
        """
        gas_saved = before_metrics["gas_cost"] - after_metrics["gas_cost"]
        execution_gain = before_metrics["execution_time"] - after_metrics["execution_time"]
        size_change = after_metrics["contract_size"] - before_metrics["contract_size"]
        correctness_violation = after_metrics["correctness"]  # 1 if incorrect, 0 if correct

        # Compute weighted reward
        reward = (
            self.weights["gas_reduction"] * gas_saved +
            self.weights["execution_speed"] * execution_gain +
            self.weights["contract_size"] * size_change +
            self.weights["correctness_penalty"] * correctness_violation
        )

        return reward

    def normalize_reward(self, reward: float) -> float:
        """
        Normalizes the reward to a fixed range (e.g., -1 to 1).

        Args:
            reward (float): Raw reward.

        Returns:
            float: Normalized reward.
        """
        return np.tanh(reward)  # Keeps reward bounded within [-1, 1]

    def evaluate_termination(self, after_metrics: dict) -> bool:
        """
        Determines if the optimization process should terminate.

        Args:
            after_metrics (dict): Solidity contract metrics after optimization.

        Returns:
            bool: True if optimization should stop (no more improvements), else False.
        """
        return after_metrics["gas_cost"] <= 0 or after_metrics["correctness"] == 1

