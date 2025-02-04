import unittest
import numpy as np
from solidity_rl.envs.solidity_env import SolidityOptimizationEnv
from solidity_rl.agents.dqn_agent import DQNAgent
from solidity_rl.agents.ppo_agent import PPOAgent
from solidity_rl.agents.sac_agent import SACAgent
from solidity_rl.agents.td3_agent import TD3Agent
from solidity_rl.agents.a2c_agent import A2CAgent

class TestAgents(unittest.TestCase):
    def setUp(self):
        """Initialize environment and agents before each test."""
        self.env = SolidityOptimizationEnv()
        self.state_shape = self.env.observation_space.shape[0]
        self.action_space = self.env.action_space

        self.agents = {
            "DQN": DQNAgent(self.state_shape, self.action_space),
            "PPO": PPOAgent(self.state_shape, self.action_space),
            "SAC": SACAgent(self.state_shape, self.action_space),
            "TD3": TD3Agent(self.state_shape, self.action_space),
            "A2C": A2CAgent(self.state_shape, self.action_space)
        }

    def test_agent_initialization(self):
        """Ensure agents are initialized correctly."""
        for name, agent in self.agents.items():
            self.assertIsNotNone(agent, f"{name} agent failed to initialize.")

    def test_agent_action_selection(self):
        """Ensure agents return valid actions."""
        state = self.env.reset()
        for name, agent in self.agents.items():
            action = agent.select_action(state)
            self.assertIsInstance(action, (np.ndarray, int, float), f"{name} returned invalid action.")

    def test_agent_training_step(self):
        """Ensure agents can process a training step."""
        state = self.env.reset()
        action = self.agents["DQN"].select_action(state)
        next_state, reward, done, _ = self.env.step(action)
        sample_batch = [(state, action, reward, next_state, done)]

        for name, agent in self.agents.items():
            agent.train_step(sample_batch)

if __name__ == "__main__":
    unittest.main()
