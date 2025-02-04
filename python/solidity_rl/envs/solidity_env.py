import numpy as np
import gym
from gym import spaces
from typing import Dict, Tuple
from solidity_rl.envs.action_space import ContinuousActionSpace
from solidity_rl.envs.reward_function import RewardFunction
from solidity_rl.envs.state_representation import StateRepresentation

class SolidityOptimizationEnv(gym.Env):
    """
    OpenAI Gym-compatible environment for Solidity smart contract optimization.
    Uses a continuous action space and rewards based on gas efficiency.
    """

    def __init__(self):
        super(SolidityOptimizationEnv, self).__init__()

        # Define action space (continuous)
        self.action_space = ContinuousActionSpace()

        # Define observation space (Solidity contract metrics)
        self.observation_space = spaces.Box(
            low=np.array([0, 0, 0, 0, 0, 0, 0]),  # Min values for [gas cost, execution time, contract size, function_count, loop_count, storage_vars, correctness]
            high=np.array([1e6, 1e6, 1e6, 100, 50, 100, 1]),  # Max values
            dtype=np.float32
        )

        # Reward function and state representation
        self.reward_function = RewardFunction()
        self.state_representation = StateRepresentation()

        # Initialize Solidity contract state (metrics before optimization)
        self.state = {
            "gas_cost": np.random.uniform(10000, 100000),  # Initial gas cost
            "execution_time": np.random.uniform(10, 1000),  # Execution time in ms
            "contract_size": np.random.uniform(1000, 10000),  # Contract size in bytes
            "function_count": np.random.randint(1, 50),  # Number of functions
            "loop_count": np.random.randint(0, 20),  # Number of loops
            "storage_vars": np.random.randint(1, 50),  # Number of storage variables
            "correctness": 0  # 0 = correct, 1 = incorrect (breaks logic)
        }

    def reset(self) -> np.ndarray:
        """
        Resets the environment to an initial state.

        Returns:
            np.ndarray: Initial observation (encoded Solidity contract metrics).
        """
        self.state = {
            "gas_cost": np.random.uniform(10000, 100000),
            "execution_time": np.random.uniform(10, 1000),
            "contract_size": np.random.uniform(1000, 10000),
            "function_count": np.random.randint(1, 50),
            "loop_count": np.random.randint(0, 20),
            "storage_vars": np.random.randint(1, 50),
            "correctness": 0
        }
        return self.state_representation.encode(self.state)

    def step(self, action: Tuple[int, float]) -> Tuple[np.ndarray, float, bool, Dict]:
        """
        Applies an optimization action and returns the new state, reward, and termination flag.

        Args:
            action (Tuple[int, float]): (Category index, continuous parameter).

        Returns:
            Tuple[np.ndarray, float, bool, Dict]: New state, reward, done flag, and extra info.
        """
        category_idx, action_value = action
        category = self.action_space.get_action_by_index(category_idx)

        # Apply optimization effects based on the action category
        if category == "storage_optimization":
            self.state["gas_cost"] -= action_value * 500  # Reduce gas by up to 500 units
            self.state["contract_size"] += action_value * 10  # Slight contract size increase
        elif category == "loop_optimization":
            self.state["execution_time"] *= (1 - action_value / 2)  # Reduce execution time
        elif category == "function_inline":
            self.state["gas_cost"] *= (1 - action_value / 4)  # Moderate gas reduction
        elif category == "require_replacement":
            self.state["gas_cost"] -= action_value * 300  # Reduce gas cost slightly
        elif category == "expression_simplification":
            self.state["gas_cost"] *= (1 - action_value / 3)  # Reduce gas by up to 33%

        # Ensure values stay within realistic bounds
        self.state["gas_cost"] = max(self.state["gas_cost"], 0)
        self.state["execution_time"] = max(self.state["execution_time"], 1)
        self.state["contract_size"] = max(self.state["contract_size"], 500)

        # Random chance of breaking correctness (5% probability)
        if np.random.uniform(0, 1) < 0.05:
            self.state["correctness"] = 1  # Invalid contract state

        # Compute reward
        reward = self.reward_function.compute_reward(
            before_metrics=self.state,
            after_metrics=self.state
        )
        reward = self.reward_function.normalize_reward(reward)

        # Check termination condition
        done = self.reward_function.evaluate_termination(self.state)

        return self.state_representation.encode(self.state), reward, done, {}

    def render(self, mode="human"):
        """
        Displays the current environment state.
        """
        print(f"Gas Cost: {self.state['gas_cost']:.2f}, "
              f"Execution Time: {self.state['execution_time']:.2f} ms, "
              f"Contract Size: {self.state['contract_size']} bytes, "
              f"Function Count: {self.state['function_count']}, "
              f"Loop Count: {self.state['loop_count']}, "
              f"Storage Vars: {self.state['storage_vars']}, "
              f"Correctness: {'✅' if self.state['correctness'] == 0 else '❌'}")

    def close(self):
        """
        Cleanup resources if needed.
        """
        pass
