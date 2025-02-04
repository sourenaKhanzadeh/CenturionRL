import unittest
import numpy as np
from solidity_rl.envs.reward_function import RewardFunction

class TestRewardFunction(unittest.TestCase):
    def setUp(self):
        """Initialize the reward function."""
        self.reward_fn = RewardFunction()

    def test_reward_computation(self):
        """Ensure reward is computed correctly."""
        before_metrics = {
            "gas_cost": 50000,
            "execution_time": 500,
            "contract_size": 5000,
            "correctness": 0
        }

        after_metrics = {
            "gas_cost": 45000,
            "execution_time": 480,
            "contract_size": 5000,
            "correctness": 0
        }

        reward = self.reward_fn.compute_reward(before_metrics, after_metrics)
        self.assertIsInstance(reward, float)
        self.assertGreater(reward, 0)

    def test_correctness_penalty(self):
        """Ensure correctness penalty is applied when an optimization breaks the contract."""
        before_metrics = {"gas_cost": 50000, "execution_time": 500, "contract_size": 5000, "correctness": 0}
        after_metrics = {"gas_cost": 45000, "execution_time": 480, "contract_size": 5000, "correctness": 1}

        reward = self.reward_fn.compute_reward(before_metrics, after_metrics)
        self.assertLess(reward, -5)  # Expect strong penalty

if __name__ == "__main__":
    unittest.main()
