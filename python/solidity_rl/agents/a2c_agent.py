import torch
import torch.nn as nn
import torch.optim as optim
from .base_agent import BaseAgent

class A2CAgent(BaseAgent):
    def __init__(self, state_shape, action_space, lr=3e-4, gamma=0.99, entropy_coef=0.01):
        super().__init__(action_space, state_shape)
        self.gamma = gamma
        self.entropy_coef = entropy_coef

        # Actor network
        self.actor = nn.Sequential(
            nn.Linear(state_shape, 128),
            nn.ReLU(),
            nn.Linear(128, len(action_space)),
            nn.Tanh()
        ).to(self.device)

        # Critic network
        self.critic = nn.Sequential(
            nn.Linear(state_shape, 128),
            nn.ReLU(),
            nn.Linear(128, 1)
        ).to(self.device)

        self.optimizer = optim.Adam(list(self.actor.parameters()) + list(self.critic.parameters()), lr=lr)

    def select_action(self, state):
        state_tensor = torch.FloatTensor(state).to(self.device)
        action_probs = self.actor(state_tensor)
        action = torch.distributions.Categorical(torch.softmax(action_probs, dim=-1)).sample()
        return action.cpu().numpy()

    def train_step(self, batch):
        states, actions, rewards, next_states, dones = zip(*batch)

        states = torch.FloatTensor(states).to(self.device)
        actions = torch.LongTensor(actions).to(self.device)
        rewards = torch.FloatTensor(rewards).to(self.device)
        next_states = torch.FloatTensor(next_states).to(self.device)
        dones = torch.FloatTensor(dones).to(self.device)

        # Compute advantage
        values = self.critic(states).squeeze()
        next_values = self.critic(next_states).squeeze()
        targets = rewards + self.gamma * next_values * (1 - dones)
        advantages = targets - values

        # Actor loss
        action_probs = self.actor(states)
        log_probs = torch.log_softmax(action_probs, dim=-1)
        selected_log_probs = log_probs.gather(1, actions.unsqueeze(1)).squeeze()
        entropy = -torch.sum(torch.exp(log_probs) * log_probs, dim=-1)
        actor_loss = -(selected_log_probs * advantages).mean() - self.entropy_coef * entropy.mean()

        # Critic loss
        critic_loss = nn.MSELoss()(values, targets)

        # Optimize
        self.optimizer.zero_grad()
        (actor_loss + critic_loss).backward()
        self.optimizer.step()
