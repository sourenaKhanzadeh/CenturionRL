class BaseAgent:
    """Base class for RL agents."""

    def __init__(self, state_dim, action_dim):
        self.state_dim = state_dim
        self.action_dim = action_dim

    def select_action(self, state):
        raise NotImplementedError("This method should be overridden by subclasses.")

    def train(self, experiences):
        raise NotImplementedError("This method should be overridden by subclasses.")
