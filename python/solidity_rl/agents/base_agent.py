import abc
import numpy as np
import torch

class BaseAgent(abc.ABC):
    """
    Abstract base class for RL agents in Solidity optimization.
    All agents must implement train_step() and select_action().
    """

    def __init__(self, action_space, state_shape, device="cpu", model=None):
        self.action_space = action_space
        self.state_shape = state_shape
        self.device = torch.device(device)
        self.model = model

    @abc.abstractmethod
    def select_action(self, state: np.ndarray) -> np.ndarray:
        """
        Selects an action based on the current state.

        Args:
            state (np.ndarray): The current environment state.

        Returns:
            np.ndarray: Selected action.
        """
        pass

    @abc.abstractmethod
    def train_step(self, batch):
        """
        Performs a training step on a batch of experiences.

        Args:
            batch (dict): Batch of experiences.
        """
        pass

    def save_model(self, path):
        """
        Saves the trained model.

        Args:
            path (str): Path to save the model.
        """
        if type(self.model) is not dict:
            torch.save(self.model.state_dict(), path)
        else:
            torch.save(self.model, path)

    def load_model(self, path):
        """
        Loads a trained model.

        Args:
            path (str): Path to the saved model.
        """
        self.model.load_state_dict(torch.load(path, map_location=self.device))
