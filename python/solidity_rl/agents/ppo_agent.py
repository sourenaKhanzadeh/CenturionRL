import torch
import torch.nn as nn
import torch.optim as optim
from .base_agent import BaseAgent

class PPOAgent(BaseAgent):
    def __init__(self, state_shape, action_space, lr=3e-4, gamma=0.99, clip_epsilon=0.2):
        super().__init__(action_space, state_shape)
        self.gamma = gamma
        self.clip_epsilon = clip_epsilon

        # Policy network
        self.policy = nn.Sequential(
            nn.Linear(state_shape, 128),
            nn.ReLU(),
            nn.Linear(128, len(action_space)),
            nn.Tanh()
        ).to(self.device)

        # Value function network
        self.value_function = nn.Sequential(
            nn.Linear(state_shape, 128),
            nn.ReLU(),
            nn.Linear(128, 1)
        ).to(self.device)

        # Combined model for saving
        self.model = {'policy': self.policy, 'value_function': self.value_function}

        self.optimizer = optim.Adam(list(self.policy.parameters()) + list(self.value_function.parameters()), lr=lr)


    def select_action(self, state):
        state_tensor = torch.FloatTensor(state).to(self.device)
        action = self.policy(state_tensor).cpu().detach().numpy()
        return action

    def train_step(self, batch):
        states, actions, rewards, next_states, dones = zip(*batch)

        states = torch.FloatTensor(states).to(self.device)
        actions = torch.FloatTensor(actions).to(self.device)
        rewards = torch.FloatTensor(rewards).to(self.device)
        next_states = torch.FloatTensor(next_states).to(self.device)
        dones = torch.FloatTensor(dones).to(self.device)

        # Compute advantage
        values = self.value_function(states).squeeze()
        next_values = self.value_function(next_states).squeeze()
        targets = rewards + self.gamma * next_values * (1 - dones)
        advantages = targets - values

        # Compute loss
        old_probs = self.policy(states).detach()
        new_probs = self.policy(states)
        ratio = (new_probs / old_probs).sum(dim=1)

        clipped_ratio = torch.clamp(ratio, 1 - self.clip_epsilon, 1 + self.clip_epsilon)
        policy_loss = -torch.min(ratio * advantages, clipped_ratio * advantages).mean()
        value_loss = nn.MSELoss()(values, targets)

        # Optimize networks
        self.optimizer.zero_grad()
        (policy_loss + value_loss).backward()
        self.optimizer.step()
