import gym
import torch
import numpy as np
from solidity_rl.envs.solidity_env import SolidityOptimizationEnv
from solidity_rl.agents.ppo_agent import PPOAgent  # Change this to your selected agent
from solidity_rl.utils.logger import Logger

# Hyperparameters
EPISODES = 500
BATCH_SIZE = 64
GAMMA = 0.99
LEARNING_RATE = 3e-4

# Initialize environment and agent
env = SolidityOptimizationEnv()
state_shape = env.observation_space.shape[0]
action_space = env.action_space
agent = PPOAgent(state_shape, action_space, lr=LEARNING_RATE, gamma=GAMMA)

logger = Logger(log_dir="logs")

# Training loop
for episode in range(EPISODES):
    state = env.reset()
    total_reward = 0
    done = False
    episode_data = []

    while not done:
        action = agent.select_action(state)
        next_state, reward, done, _ = env.step(action)
        agent.train_step([(state, action, reward, next_state, done)])
        total_reward += reward
        state = next_state

    logger.log_metric("episode_reward", total_reward, episode)
    print(f"Episode {episode + 1}/{EPISODES}: Total Reward = {total_reward:.2f}")

# Save trained model
agent.save_model("models/ppo_solidity_optimized.pth")

# Cleanup
env.close()
