import numpy as np

def moving_average(data, window_size=10):
    """Computes a moving average over the reward function."""
    return np.convolve(data, np.ones(window_size)/window_size, mode='valid')

def compute_gas_savings(original_gas, optimized_gas):
    """Calculates percentage gas savings from optimization."""
    if original_gas == 0:
        return 0
    return ((original_gas - optimized_gas) / original_gas) * 100
