import json
import torch
import os
import sys

from solidity_rl.utils.contract_parser import parse_contract
from solidity_rl.utils.gas_estimator import estimate_gas
from solidity_rl.models.rl_policy import RLPolicy  

def load_trained_model(model_path: str):
    """
    Loads a trained RL model.

    Args:
        model_path (str): Path to the saved PyTorch model.

    Returns:
        RLPolicy: Loaded RL model.
    """
    if not os.path.exists(model_path):
        raise FileNotFoundError(f"Trained model not found: {model_path}")

    model = RLPolicy()
    model.load_state_dict(torch.load(model_path))
    model.eval()
    return model

def optimize_contract(contract_path: str, model: RLPolicy):
    """
    Optimizes a Solidity contract using a trained RL model.

    Args:
        contract_path (str): Path to the Solidity contract (.sol file).
        model (RLPolicy): Trained RL model.

    Returns:
        str: Optimized Solidity contract code.
    """
    ast = parse_contract(contract_path)
    state = extract_features_from_ast(ast)

    # RL model selects optimization actions
    optimized_state = model.optimize(state)

    # Convert optimized state back to Solidity contract
    optimized_contract = reconstruct_contract_from_state(optimized_state)
    return optimized_contract

def extract_features_from_ast(ast: dict):
    """
    Extracts RL-friendly features from Solidity AST.

    Args:
        ast (dict): Solidity Abstract Syntax Tree.

    Returns:
        torch.Tensor: State representation.
    """
    # Convert AST to RL input (feature engineering required)
    return torch.tensor([len(ast)])  # Placeholder implementation

def reconstruct_contract_from_state(state):
    """
    Converts an optimized RL state back to Solidity code.

    Args:
        state (torch.Tensor): Optimized RL state.

    Returns:
        str: Solidity contract code.
    """
    # Placeholder: Convert state back to Solidity syntax
    return "pragma solidity ^0.8.0;\ncontract OptimizedContract {}"

def save_optimized_contract(contract_code: str, output_path: str):
    """
    Saves the optimized Solidity contract to a file.

    Args:
        contract_code (str): Optimized Solidity contract code.
        output_path (str): Path to save the contract.
    """
    with open(output_path, "w", encoding="utf-8") as file:
        file.write(contract_code)

# Example Usage
if __name__ == "__main__":
    contract_path = "contracts/SampleContract.sol"
    optimized_contract_path = "contracts/OptimizedContract.sol"
    model_path = "models/checkpoints/trained_rl_model.pth"

    try:
        model = load_trained_model(model_path)
        optimized_code = optimize_contract(contract_path, model)

        save_optimized_contract(optimized_code, optimized_contract_path)
        print(f"Optimized contract saved to {optimized_contract_path}")

        # Estimate gas savings
        original_gas = estimate_gas("contracts/compiled/SampleContract.json", "increment")
        optimized_gas = estimate_gas("contracts/compiled/OptimizedContract.json", "increment")
        gas_savings = ((original_gas - optimized_gas) / original_gas) * 100
        print(f"Gas Savings: {gas_savings:.2f}%")

    except Exception as e:
        print(f"Optimization failed: {e}")
