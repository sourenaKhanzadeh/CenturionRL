import unittest
import torch
import numpy as np
from solidity_rl.envs.solidity_env import SolidityOptimizationEnv
from solidity_rl.agents.ppo_agent import PPOAgent

class TestTraining(unittest.TestCase):
    def setUp(self):
        """Initialize environment and agent for training tests."""
        self.env = SolidityOptimizationEnv()
        self.state_shape = self.env.observation_space.shape[0]
        self.agent = PPOAgent(self.state_shape, self.env.action_space)

    def test_train_one_episode(self):
        """Ensure an agent can complete at least one training episode."""
        state = self.env.reset()
        total_reward = 0
        done = False

        while not done:
            action = self.agent.select_action(state)
            next_state, reward, done, _ = self.env.step(action)
            self.agent.train_step([(state, action, reward, next_state, done)])
            total_reward += reward
            state = next_state

        self.assertIsInstance(total_reward, float)

    def test_save_and_load_model(self):
        """Ensure the trained model can be saved and reloaded."""
        model_path = "test_model.pth"
        self.agent.save_model(model_path)
        self.agent.load_model(model_path)
        self.assertTrue(torch.load(model_path))

if __name__ == "__main__":
    unittest.main()
