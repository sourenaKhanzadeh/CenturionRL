import torch
import gym
import numpy as np
from solidity_rl.envs.solidity_env import SolidityOptimizationEnv
from solidity_rl.agents.ppo_agent import PPOAgent  # Change this to desired agent

# Load trained model
MODEL_PATH = "models/ppo_solidity_optimized.pth"

# Initialize environment and agent
env = SolidityOptimizationEnv()
state_shape = env.observation_space.shape[0]
agent = PPOAgent(state_shape, env.action_space)
agent.load_model(MODEL_PATH)

# Evaluation loop
EPISODES = 50
total_rewards = []

for episode in range(EPISODES):
    state = env.reset()
    total_reward = 0
    done = False

    while not done:
        action = agent.select_action(state)
        state, reward, done, _ = env.step(action)
        total_reward += reward

    total_rewards.append(total_reward)
    print(f"Evaluation Episode {episode + 1}/{EPISODES}: Reward = {total_reward:.2f}")

print(f"Average Reward: {np.mean(total_rewards):.2f}")

env.close()
