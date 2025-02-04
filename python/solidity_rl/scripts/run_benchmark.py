import numpy as np
from solidity_rl.utils.metrics import compute_gas_savings_metrics

def run_benchmark(original_gas, optimized_gas):
    """Runs benchmarking on optimized Solidity contracts."""
    metrics = compute_gas_savings_metrics(original_gas, optimized_gas)
    print("Benchmarking Results:", metrics)

if __name__ == "__main__":
    original_gas = np.random.randint(5000, 20000, 50)
    optimized_gas = original_gas * np.random.uniform(0.5, 0.9, len(original_gas))
    run_benchmark(original_gas, optimized_gas)