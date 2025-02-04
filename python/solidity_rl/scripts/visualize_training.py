import matplotlib.pyplot as plt
from solidity_rl.utils.visualization import plot_rewards

def visualize_training(reward_history, save_path="results/training_plot.png"):
    """Generates a training progress visualization."""
    plot_rewards(reward_history, save_path=save_path)
    print(f"Training visualization saved to {save_path}")

if __name__ == "__main__":
    import numpy as np
    reward_history = np.random.randint(-50, 100, 100)
    visualize_training(reward_history)
