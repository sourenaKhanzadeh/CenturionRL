import matplotlib.pyplot as plt
import numpy as np

def plot_rewards(reward_history, title="Training Rewards", save_path=None):
    """
    Plots the reward history of an RL agent over episodes.

    Args:
        reward_history (list): List of rewards per episode.
        title (str): Title of the plot.
        save_path (str, optional): Path to save the plot. Defaults to None.
    """
    plt.figure(figsize=(10, 5))
    plt.plot(reward_history, label="Rewards", color="blue", alpha=0.7)
    plt.xlabel("Episode")
    plt.ylabel("Reward")
    plt.title(title)
    plt.legend()
    plt.grid()

    if save_path:
        plt.savefig(save_path)
    else:
        plt.show()

def plot_gas_savings(original_gas, optimized_gas, save_path=None):
    """
    Plots gas savings before and after optimization.

    Args:
        original_gas (list): List of gas usage before optimization.
        optimized_gas (list): List of gas usage after optimization.
        save_path (str, optional): Path to save the plot. Defaults to None.
    """
    episodes = np.arange(len(original_gas))

    plt.figure(figsize=(10, 5))
    plt.plot(episodes, original_gas, label="Original Gas", color="red", linestyle="dashed")
    plt.plot(episodes, optimized_gas, label="Optimized Gas", color="green", linestyle="solid")
    plt.xlabel("Episode")
    plt.ylabel("Gas Consumption")
    plt.title("Gas Savings Over Episodes")
    plt.legend()
    plt.grid()

    if save_path:
        plt.savefig(save_path)
    else:
        plt.show()

def plot_benchmark(results, metric="gas_savings", save_path=None):
    """
    Plots benchmarking results for different RL agents.

    Args:
        results (dict): Dictionary containing agent names as keys and performance metrics as values.
        metric (str): Performance metric to plot (e.g., "gas_savings").
        save_path (str, optional): Path to save the plot. Defaults to None.
    """
    agents = list(results.keys())
    values = [results[agent][metric] for agent in agents]

    plt.figure(figsize=(10, 5))
    plt.bar(agents, values, color=["blue", "orange", "green", "red", "purple"])
    plt.xlabel("RL Agents")
    plt.ylabel(metric.replace("_", " ").title())
    plt.title(f"Benchmarking: {metric.replace('_', ' ').title()} Comparison")
    plt.xticks(rotation=30)
    plt.grid(axis="y")

    if save_path:
        plt.savefig(save_path)
    else:
        plt.show()

# Example Usage
if __name__ == "__main__":
    # Simulated data
    reward_history = np.random.randint(-50, 100, 100)
    original_gas = np.random.randint(5000, 20000, 50)
    optimized_gas = original_gas * np.random.uniform(0.5, 0.9, len(original_gas))

    benchmark_results = {
        "PPO": {"gas_savings": 30},
        "DQN": {"gas_savings": 25},
        "A2C": {"gas_savings": 20},
        "SAC": {"gas_savings": 35},
        "TD3": {"gas_savings": 28},
    }

    plot_rewards(reward_history)
    plot_gas_savings(original_gas, optimized_gas)
    plot_benchmark(benchmark_results)
