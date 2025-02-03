def compute_gas_savings(original_gas: int, optimized_gas: int):
    """
    Computes the percentage of gas saved after optimization.

    Args:
        original_gas (int): Gas consumption before optimization.
        optimized_gas (int): Gas consumption after optimization.

    Returns:
        float: Percentage of gas saved.
    """
    if original_gas == 0:
        return 0.0  # Prevent division by zero

    savings = ((original_gas - optimized_gas) / original_gas) * 100
    return max(savings, 0)  # Ensure non-negative savings


def compute_reward(original_gas: int, optimized_gas: int, penalty_factor=0.1):
    """
    Computes the RL reward based on gas savings.

    Args:
        original_gas (int): Gas usage before optimization.
        optimized_gas (int): Gas usage after optimization.
        penalty_factor (float): Penalizes cases where optimization fails.

    Returns:
        float: Reward value.
    """
    gas_savings = compute_gas_savings(original_gas, optimized_gas)

    if gas_savings > 0:
        reward = gas_savings  # Reward proportional to gas savings
    else:
        reward = -penalty_factor * abs(gas_savings)  # Penalize for failed optimizations

    return reward


# Example Usage
if __name__ == "__main__":
    original_gas = 10000  # Example gas before optimization
    optimized_gas = 7500  # Example gas after optimization

    gas_savings = compute_gas_savings(original_gas, optimized_gas)
    reward = compute_reward(original_gas, optimized_gas)

    print(f"Gas Savings: {gas_savings:.2f}%")
    print(f"Reward: {reward:.2f}")
