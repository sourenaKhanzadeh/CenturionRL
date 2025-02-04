import torch
import torch.nn as nn
import torch.optim as optim
import numpy as np
from .base_agent import BaseAgent

class TD3Agent(BaseAgent):
    def __init__(self, state_shape, action_space, lr=3e-4, gamma=0.99, tau=0.005):
        super().__init__(action_space, state_shape)
        self.gamma = gamma
        self.tau = tau

        # Actor network
        self.actor = nn.Sequential(
            nn.Linear(state_shape, 128),
            nn.ReLU(),
            nn.Linear(128, len(action_space)),
            nn.Tanh()
        ).to(self.device)

        # Twin Critics
        self.critic_1 = nn.Sequential(
            nn.Linear(state_shape + len(action_space), 128),
            nn.ReLU(),
            nn.Linear(128, 1)
        ).to(self.device)

        self.critic_2 = nn.Sequential(
            nn.Linear(state_shape + len(action_space), 128),
            nn.ReLU(),
            nn.Linear(128, 1)
        ).to(self.device)

        self.actor_optimizer = optim.Adam(self.actor.parameters(), lr=lr)
        self.critic_optimizer = optim.Adam(list(self.critic_1.parameters()) + list(self.critic_2.parameters()), lr=lr)

    def select_action(self, state):
        state_tensor = torch.FloatTensor(state).to(self.device)
        action = self.actor(state_tensor).cpu().detach().numpy()
        return action

    def train_step(self, batch):
        states, actions, rewards, next_states, dones = zip(*batch)

        states = torch.FloatTensor(states).to(self.device)
        actions = torch.FloatTensor(actions).to(self.device)
        rewards = torch.FloatTensor(rewards).to(self.device)
        next_states = torch.FloatTensor(next_states).to(self.device)
        dones = torch.FloatTensor(dones).to(self.device)

        # Compute Q-value targets
        next_actions = self.actor(next_states)
        next_q = torch.min(self.critic_1(torch.cat([next_states, next_actions], dim=-1)),
                           self.critic_2(torch.cat([next_states, next_actions], dim=-1)))

        q_targets = rewards + self.gamma * next_q * (1 - dones)

        # Compute loss and update
        critic_loss = nn.MSELoss()(self.critic_1(torch.cat([states, actions], dim=-1)), q_targets) + \
                      nn.MSELoss()(self.critic_2(torch.cat([states, actions], dim=-1)), q_targets)

        self.critic_optimizer.zero_grad()
        critic_loss.backward()
        self.critic_optimizer.step()
