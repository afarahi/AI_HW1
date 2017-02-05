
import numpy.random as npr

def cost_function(state):

    cost = 0

    for i in range(8):

        queen1 = state[i]

        for j in range(i+1, 8):

            queen2 = state[j]

            # check whether they are on the same row
            if queen1 == queen2:
                cost += 1

            # check whether they are on the diagonal line
            if abs(queen1 - queen2) == abs(i - j):
                cost += 1

    return cost


class steepest_descent:

    def __init__(self):

        pass

    def _move(self, state):

        new_state = state[:]
        cost = cost_function(state)

        for i in range(8):
            for j in range(1, 9):

                alt_state = state[:]
                alt_state[i] = j
                alt_cost = cost_function(alt_state)

                if alt_cost < cost:
                    new_state = alt_state[:]
                    cost = alt_cost

        return new_state

    def run(self, initial_state):

        self.state = initial_state
        self.moves = [self.state[:]]

        state = [0, 0, 0, 0, 0, 0, 0, 0]

        while self.state != state:

            state = self.state[:]
            self.state = self._move(self.state)

            self.moves += [self.state[:]]

        self.moves = self.moves[:-1]

        return self.state


def hw2_task3_pipeline():

    print "Cost function of initial state: ", cost_function([1, 1, 1, 1, 1, 1, 1, 1])
    print "Cost function of solution state: ", cost_function([7, 5, 3, 1, 6, 8, 2, 4])

    queens_problem = steepest_descent()
    print "steepest descent after termination : ", queens_problem.run([1, 1, 1, 1, 1, 1, 1, 1])
    print "Moves: ", queens_problem.moves
    print "number of moves : ", len(queens_problem.moves)-1


    npr.seed(148)

    # number of trials
    num_trials = 10

    for i in range(5):

        counter = 0
        tot_moves = 0
        final_state = [1, 1, 1, 1, 1, 1, 1, 1]

        print "Run = ", i+1

        while cost_function(final_state) != 0 and counter < num_trials:

            # setup a random initial state:
            initial_state = list(npr.randint(1, 9, size=8))
            final_state = queens_problem.run(initial_state)

            counter += 1
            tot_moves += len(queens_problem.moves) - 1

            # print out results
            print "  Trial number ", counter
            print "     Initial State : ", initial_state
            print "     Final State : ", final_state
            print "     Cost of the finale State : ", cost_function(final_state)
            print "     Number of Moves : ", len(queens_problem.moves) - 1

        print "Total Moves : ", tot_moves
        print "---------------------------------"