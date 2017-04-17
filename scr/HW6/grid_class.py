import numpy as np
import numpy.random as npr
import json

npr.seed(1483)

class grid_class():

    def __init__(self):

        grid = []
        for line in open('./inputs/grid.txt'):
            for val in line.rstrip('\n').split(','):
                grid.append(val)
        self.grid = np.array(grid)

        self.grid_size = int(np.sqrt(len(grid)))
        self.n_states = self.grid_size * self.grid_size + 1

        with open('./inputs/hw6_parameters.json') as fp:
            _param = json.load(fp)

        # Model and input parameters
        self.explorationchance = _param["explorationchance"]
        self.gamma = _param["gamma"]
        self.alpha = _param["alpha"]
        self.pr = _param["pr"]

        # Rewards
        self.reward = {'F': _param["F-Reward"],
                       'R': _param["R-Reward"],
                       'C': _param["C-Reward"],
                       'BROKEN': _param["BROKEN-Reward"]}

        # Action Number to Action Character and vice versa.
        self.action_num_to_char = {0:'N', 1:'S', 2:'E', 3:'W', 4:'R'}
        self.action_char_to_num = {'N':0, 'S':1, 'E':2, 'W':3, 'R':4}

        # set initial utility grid
        self.utility = np.zeros(self.n_states)

        # set initial policy grid
        self.policy = np.empty(self.n_states, dtype=str)
        for i in range(self.n_states):
            self.policy[i] = 'R'

        # set initial Q - This should include a Q value for each action-state pair.
        self.Q = np.zeros([self.n_states, 5])

        self.trials = 0
        self.iterations = 0
        self.action = None
        self.learning = True
        self.state = 0

        self.valid_action = self.set_actions_grid()

    def index_to_xy(self, index):
        x = index % 4
        y = index / 4
        return x, y

    def xy_to_index(self, x, y):
        return x + 4*y

    def simulator(self, action, state):

        if state == 16:
            return 16, 0

        new_state = state

        valid = action in self.valid_action[state]
        broken = False

        if self.grid[state] == 'R':
            if npr.random() > self.pr:
                new_state = 16
                broken = True

        if valid and not broken:
            if action == 'N':
                new_state = state + 4
            elif action == 'S':
                new_state = state - 4
            elif action == 'E':
                new_state = state + 1
            elif action == 'W':
                new_state = state - 1

        reward = self.reward[self.grid[state]]

        return new_state, reward

    def set_actions_grid(self):

        valid_actions = np.empty(self.n_states, dtype=list)

        for i in range(self.grid_size):
            for j in range(self.grid_size):
                ind = self.xy_to_index(i, j)
                valid_actions[ind] = ['R']

        for i in range(self.grid_size):
            for j in range(self.grid_size-1):
                ind = self.xy_to_index(i, j)
                valid_actions[ind] += ['N']

        for i in range(self.grid_size-1):
            for j in range(self.grid_size):
                ind = self.xy_to_index(i, j)
                valid_actions[ind] += ['E']

        for i in range(1, self.grid_size):
            for j in range(self.grid_size):
                ind = self.xy_to_index(i, j)
                valid_actions[ind] += ['W']

        for i in range(self.grid_size):
            for j in range(1, self.grid_size):
                ind = self.xy_to_index(i, j)
                valid_actions[ind] += ['S']

        for i in range(self.grid_size):
            for j in range(self.grid_size):
                ind = self.xy_to_index(i, j)
                if self.grid[ind] == 'C':
                    valid_actions[ind] = ['R']

        valid_actions[self.n_states-1] = ['R']

        return valid_actions

    def visualize(self):
        print " i, j, Env, Q, Policy"
        for ind in range(self.n_states-1):
            print self.index_to_xy(ind), self.grid[ind], self.Q[ind], self.action_num_to_char[self.Q[ind].argmax()]
        print

    def generate_latex_table(self):

        print('\\begin{table}[h]')
        print('\\centering')
        print('\\caption{Mycaption}')
        print('\\label{my - label}')
        print('\\begin')
        print('{tabular}{ | c | c | c | c | }')
        print('\\hline')

        for j in range(self.grid_size-1, -1, -1):
            for i in range(self.grid_size):
                ind = self.xy_to_index(i, j)
                q_ind = self.Q[ind].argmax()
                print('\\begin{tabular}[c]{ @ {}c @ {}}%0.2f\\\\ $[~$%s$~]$\end{tabular}'
                      %(self.Q[ind, q_ind], self.action_num_to_char[q_ind])),
                if i < self.grid_size - 1:
                    print('  &  ')
                else:
                    print('\\\\ \\hline')
        print('\\end{tabular}')
        print('\\end{table}')

    def Q_learning(self):

        from numpy.random import random, randint

        total_reward = 0
        total_reward_array =[]

        while self.learning:

            # Read newstate and reward(which can be garbage if first round)

            if self.action in ["N", "S", "E", "W", "R"]:
                action_ind = self.action_char_to_num[self.action]
                # Find the Q value for state and action, called q here.
                q = self.Q[self.state][action_ind]
                # Find the Q value for newstate with its action with the maximal Q value, q'
                qp = max(self.Q[newstate])
                # Update q according to: q = q + alpha * (reward + (gamma * q') - q)
                q = q + self.alpha * (reward + (self.gamma * qp) - q)
                self.Q[self.state][action_ind] = q
            elif self.action is None:
                newstate = npr.randint(0, 16)
                total_reward = 0

            self.state = newstate

            if self.trials == 1000:
                self.learning = False

            if random() < self.explorationchance:
                exploration = True
                exploitation = False
            else:
                exploration = False
                exploitation = True

            if exploitation:
                # set action as action with the highest Q value for newstate
                self.action = self.action_num_to_char[self.Q[self.state].argmax()]

            if exploration:
                # set action to one of "N", "S", "E", "W", or "R" randomly.
                self.action = self.action_num_to_char[randint(0, 5)]

            if not self.learning:
                self.action = "DONE_SIGNAL"
            elif newstate == 16:
                self.action = "RESTART_SIGNAL"
                self.action = None
                self.iterations = 0
                self.trials += 1
                # self.explorationchance = np.power(self.trials, -0.1) # PART 1
                # self.explorationchance = np.power(self.trials, -0.01) # PART 2
                self.explorationchance = np.power(self.trials, -0.5) # PART 3
            elif self.iterations > 14:
                self.action = "RESTART_SIGNAL"
                self.action = None
                self.iterations = 0
                self.trials += 1
                # self.explorationchance = np.power(self.trials, -0.1) # PART 1
                # self.explorationchance = np.power(self.trials, -0.01) # PART 2
                self.explorationchance = np.power(self.trials, -0.5) # PART 3

            self.iterations += 1

            # commit to the action, compute information on result and reward
            newstate, reward = self.simulator(self.action, self.state)

            if self.action is not None:
                total_reward += reward

            if self.trials > 990 and self.action is None:
                print total_reward,
                total_reward_array.append(total_reward)

        print
        print "Mean Total Rewards :", np.mean(total_reward_array)
        # output Q values learned.You may also automatically find policies, etc.
        return self.Q