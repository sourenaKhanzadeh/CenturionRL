import gym
import torch
import numpy as np
from solidity_rl.envs.solidity_env import SolidityOptimizationEnv
from solidity_rl.agents.ppo_agent import PPOAgent
from solidity_rl.agents.dqn_agent import DQNAgent
from solidity_rl.agents.sac_agent import SACAgent
from solidity_rl.utils.logger import Logger

# Hyperparameters
EPISODES = 300
AGENTS = {
    "PPO": PPOAgent,
    "DQN": DQNAgent,
    "SAC": SACAgent
}

# Initialize environment and logger
env = SolidityOptimizationEnv()
logger = Logger(log_dir="logs")

# Train and compare agents
for agent_name, AgentClass in AGENTS.items():
    print(f"\nTraining {agent_name}...\n")
    agent = AgentClass(env.observation_space.shape[0], env.action_space)
    
    for episode in range(EPISODES):
        state = env.reset()
        total_reward = 0
        done = False

        while not done:
            action = agent.select_action(state)
            next_state, reward, done, _ = env.step(action)
            agent.train_step([(state, action, reward, next_state, done)])
            total_reward += reward
            state = next_state

        logger.log_metric(f"{agent_name}_reward", total_reward, episode)
        print(f"{agent_name} - Episode {episode + 1}/{EPISODES}: Reward = {total_reward:.2f}")

    agent.save_model(f"models/{agent_name.lower()}_solidity_optimized.pth")

env.close()
