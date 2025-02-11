import argparse
import os
import torch
import numpy as np
from solidity_rl.agents import PPOAgent, DQNAgent, A2CAgent, SACAgent, TD3Agent
from solidity_rl.envs.solidity_env import SolidityOptimizationEnv
from solidity_rl.utils.logger import setup_logger
from solidity_rl.utils.config_loader import load_config
from solidity_rl.utils.contract_parser import parse_contract


def set_seed(seed):
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)


def select_agent(agent_name, env, config):
    agents = {
        'ppo': PPOAgent,
        'dqn': DQNAgent,
        'a2c': A2CAgent,
        'sac': SACAgent,
        'td3': TD3Agent
    }
    state_shape = env.observation_space.shape[0]
    action_space = env.action_space

    # Filter out 'type' from agent config
    agent_config = {k: v for k, v in config['agent'].items() if k != 'type'}

    return agents[agent_name.lower()](state_shape, action_space, **agent_config)


def train(agent, env, config, logger):
    num_episodes = config['training']['num_episodes']
    max_steps = config['training']['max_steps']

    contract_dir = config['paths']['contracts']
    contract_files = [f for f in os.listdir(contract_dir) if f.endswith('.sol')]

    for episode in range(num_episodes):
        contract_file = np.random.choice(contract_files)
        contract_path = os.path.join(contract_dir, contract_file)

        # Parse contract to AST and load into environment
        contract_ast = parse_contract(contract_path)
        env.load_contract_ast(contract_ast)

        state = env.reset()
        total_reward = 0
        episode_transitions = []

        for _ in range(max_steps):
            action = agent.select_action(state)

            # Ensure action is a tuple of two elements
            if not isinstance(action, (tuple, list)) or len(action) != 2:
                action = (action, 0)  # Default second value if missing

            # Convert category_idx to integer if it's a numpy array
            category_idx = int(action[0][0]) if isinstance(action[0], np.ndarray) else int(action[0])
            action_value = action[1]
            formatted_action = (category_idx, action_value)

            next_state, reward, done, _ = env.step(formatted_action)

            episode_transitions.append((state, formatted_action, reward, next_state, done))

            state = next_state
            total_reward += reward

            if done:
                break

        agent.train_step(episode_transitions)

        logger.info(f"Episode {episode + 1}/{num_episodes} - Total Reward: {total_reward}")

        if episode % config['training']['save_interval'] == 0:
            agent.save_model(f"{config['paths']['checkpoints']}/model_episode_{episode}.pth")
        

    logger.info("Training completed.")


def main():
    parser = argparse.ArgumentParser(description='Train RL agent for Solidity optimization.')
    parser.add_argument('--config', type=str, default='python/solidity_rl/config/rl_setting.yaml', help='Path to config file.')
    args = parser.parse_args()

    config = load_config(args.config)
    set_seed(config['training']['seed'])

    env = SolidityOptimizationEnv()
    agent = select_agent(config['agent']['type'], env, config)
    logger = setup_logger(config['paths']['log'])

    train(agent, env, config, logger)


if __name__ == '__main__':
    main()
