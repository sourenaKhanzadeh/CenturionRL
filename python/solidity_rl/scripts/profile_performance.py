import numpy as np
import matplotlib.pyplot as plt
from solidity_rl.utils.visualization import plot_rewards

def profile_performance(model, env, episodes=100):
    """Profiles the performance of an RL model."""
    total_rewards = []
    for episode in range(episodes):
        state = env.reset()
        total_reward = 0
        done = False
        while not done:
            action = model.select_action(state)
            state, reward, done, _ = env.step(action)
            total_reward += reward
        total_rewards.append(total_reward)
    plot_rewards(total_rewards, title="Model Performance")
    print("Performance profiling completed.")

if __name__ == "__main__":
    from solidity_rl.models.rl_policy import RLPolicy
    env = None  # Replace with actual environment
    model = RLPolicy()
    profile_performance(model, env)