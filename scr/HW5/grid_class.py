import numpy as np
import pandas as pd
import json


class grid_class():

    def __init__(self):

        grid = []
        for line in open('./inputs/grid.txt'):
            grid.append(np.array([val for val in line.rstrip('\n').split(',') if val != '']))
        self.grid = np.array(grid)

        self.grid_size = len(grid)

        with open('./inputs/hw5_parameters.json') as fp:
            _param = json.load(fp)

        # Model and input parameters
        self.epsilon = _param["epsilon"]
        self.gamma = _param["gamma"]
        self.pr = _param["pr"]

        # Rewards
        self.reward = {'F': _param["F-Reward"],
                       'R': _param["R-Reward"],
                       'C': _param["C-Reward"],
                       'BROKEN': _param["BROKEN-Reward"]}

        # set initial utility grid
        self.utility = np.zeros((self.grid_size, self.grid_size))

        # set initial policy grid
        self.policy = np.empty((self.grid_size, self.grid_size), dtype=str)
        for i in range(self.grid_size):
            for j in range(self.grid_size):
                self.policy[i, j] = 'R'


    def set_actions_grid(self):

        self.actions = np.empty((self.grid_size, self.grid_size), dtype=list)

        for i in range(self.grid_size):
            for j in range(self.grid_size):
                self.actions[i, j] = ['R']

        for i in range(self.grid_size-1):
            for j in range(self.grid_size):
                self.actions[i, j] += ['N']

        for i in range(self.grid_size):
            for j in range(self.grid_size-1):
                self.actions[i, j] += ['E']

        for i in range(self.grid_size):
            for j in range(1, self.grid_size):
                self.actions[i, j] += ['W']

        for i in range(1, self.grid_size):
            for j in range(self.grid_size):
                self.actions[i, j] += ['S']

        for i in range(self.grid_size):
            for j in range(self.grid_size):
                if self.grid[i, j] == 'C':
                    self.actions[i, j] = ['R']

    def visualize(self):
        """
        for i in range(self.grid_size):
            for j in range(self.grid_size):
                print i, j, self.grid[i, j], self.utility[i, j], self.actions[i, j]
        """
        for i in range(self.grid_size-1, -1, -1):
            for j in range(self.grid_size):
                print('[%s,%0.2f,%s] '%(self.grid[i, j], self.utility[i, j], self.policy[i, j])),
            print
        print

    def generate_latex_table(self):

        print('\\begin{table}[h]')
        print('\\centering')
        print('\\caption{Mycaption}')
        print('\\label{my - label}')
        print('\\begin')
        print('{tabular}{ | c | c | c | c | }')
        print('\\hline')

        for i in range(self.grid_size-1, -1, -1):
            for j in range(self.grid_size):
                print('\\begin{tabular}[c]{ @ {}c @ {}}%0.2f\\\\ $[~$%s$~]$\end{tabular}'
                      %(self.utility[i, j], self.policy[i, j])),
                if j < self.grid_size - 1:
                    print('  &  ')
                else:
                    print('\\\\ \\hline')
        print('\\end{tabular}')
        print('\\end{table}')


    def action_next_move(self, action, i, j):
        if action == 'R':
            return i, j
        elif action == 'N':
            return i+1, j
        elif action == 'E':
            return i, j+1
        elif action == 'S':
            return i-1, j
        elif action == 'W':
            return i, j-1

    def update_utility_grid_value_iteration(self):

        new_utility = self.utility.copy()
        for i in range(self.grid_size):
            for j in range(self.grid_size):

                # add reward
                r = self.reward[self.grid[i, j]]

                # add utilities from previous batch
                u = []
                for iaction in self.actions[i, j]:
                    inew, jnew = self.action_next_move(iaction, i, j)
                    self.utility[inew, jnew]

                if self.grid[i, j] == 'R':
                    u *= self.pr
                u /= len(self.actions[i, j])

                new_utility[i, j] = r + self.gamma * u

        self.utility = new_utility

    def update_utility_grid(self):

        def best_policy(i, j):
            u = []
            for iaction in self.actions[i, j]:
                inew, jnew = self.action_next_move(iaction, i, j)
                u += [self.utility[inew, jnew]]
            ind = np.argmax(u)
            return self.actions[i, j][ind], u[ind]

        new_utility = self.utility.copy()
        for i in range(self.grid_size):
            for j in range(self.grid_size):

                # add reward
                r = self.reward[self.grid[i, j]]

                # find the best policy and add utilities from previous batch
                p, u = best_policy(i, j)

                if self.grid[i, j] == 'R':
                    u *= self.pr

                new_utility[i, j] = r + self.gamma * u
                self.policy[i, j] = p

        convergence_status = np.max(np.abs(self.utility - new_utility)) < self.epsilon * (1.0 - self.gamma) / self.gamma
        self.utility = new_utility

        return convergence_status