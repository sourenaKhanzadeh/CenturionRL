import torch
import gym
import numpy as np
from solidity_rl.envs.solidity_env import SolidityOptimizationEnv
from solidity_rl.agents.ppo_agent import PPOAgent  # Change agent if needed
from solidity_rl.utils.contract_parser import parse_contract

# Load trained model
MODEL_PATH = "models/ppo_solidity_optimized.pth"

# Initialize environment and agent
env = SolidityOptimizationEnv()
state_shape = env.observation_space.shape[0]
agent = PPOAgent(state_shape, env.action_space)
agent.load_model(MODEL_PATH)

# Load Solidity contract
CONTRACT_PATH = "contracts/sample.sol"
contract_metrics = parse_contract(CONTRACT_PATH)
state = np.array([contract_metrics["gas_cost"], contract_metrics["execution_time"], contract_metrics["contract_size"]])

# Run inference
print("Running Solidity Optimization...")
action = agent.select_action(state)
new_state, reward, _, _ = env.step(action)

print(f"Original Gas Cost: {contract_metrics['gas_cost']}")
print(f"Optimized Gas Cost: {new_state[0]}")
print(f"Reward: {reward:.2f}")

env.close()
