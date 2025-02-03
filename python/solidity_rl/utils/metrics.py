import numpy as np

def moving_average(data, window_size=10):
    """
    Computes a moving average over the given data.

    Args:
        data (list or np.array): List of values (e.g., rewards, gas savings).
        window_size (int): Number of elements to average.

    Returns:
        np.array: Smoothed moving average.
    """
    if len(data) < window_size:
        return np.array(data)  # Return original data if not enough points
    
    return np.convolve(data, np.ones(window_size)/window_size, mode='valid')

def compute_gas_savings_metrics(original_gas, optimized_gas):
    """
    Computes gas savings metrics.

    Args:
        original_gas (list or np.array): Gas usage before optimization.
        optimized_gas (list or np.array): Gas usage after optimization.

    Returns:
        dict: Dictionary containing min, max, avg gas savings.
    """
    savings = np.maximum(((np.array(original_gas) - np.array(optimized_gas)) / np.array(original_gas)) * 100, 0)

    return {
        "min_savings": np.min(savings),
        "max_savings": np.max(savings),
        "avg_savings": np.mean(savings),
    }

def compute_training_performance(reward_history):
    """
    Computes training performance metrics.

    Args:
        reward_history (list or np.array): List of rewards over episodes.

    Returns:
        dict: Dictionary containing avg, min, max rewards.
    """
    reward_array = np.array(reward_history)

    return {
        "avg_reward": np.mean(reward_array),
        "max_reward": np.max(reward_array),
        "min_reward": np.min(reward_array),
        "std_reward": np.std(reward_array),
    }

# Example Usage
if __name__ == "__main__":
    rewards = np.random.randint(-50, 100, 50)
    original_gas = np.random.randint(5000, 20000, 50)
    optimized_gas = original_gas * np.random.uniform(0.5, 0.9, len(original_gas))

    smoothed_rewards = moving_average(rewards)
    gas_metrics = compute_gas_savings_metrics(original_gas, optimized_gas)
    training_metrics = compute_training_performance(rewards)

    print("Smoothed Rewards:", smoothed_rewards)
    print("Gas Savings Metrics:", gas_metrics)
    print("Training Metrics:", training_metrics)
