import unittest
import numpy as np
from solidity_rl.envs.solidity_env import SolidityOptimizationEnv

class TestSolidityEnv(unittest.TestCase):
    def setUp(self):
        """Initialize the Solidity optimization environment."""
        self.env = SolidityOptimizationEnv()

    def test_env_initialization(self):
        """Check if the environment initializes correctly."""
        self.assertIsNotNone(self.env)

    def test_env_reset(self):
        """Check if environment resets properly."""
        state = self.env.reset()
        self.assertIsInstance(state, np.ndarray)
        self.assertEqual(state.shape[0], self.env.observation_space.shape[0])

    def test_env_step(self):
        """Check if stepping through the environment works correctly."""
        self.env.reset()
        action = self.env.action_space.sample()
        next_state, reward, done, _ = self.env.step(action)
        
        self.assertIsInstance(next_state, np.ndarray)
        self.assertIsInstance(reward, float)
        self.assertIsInstance(done, bool)

if __name__ == "__main__":
    unittest.main()
