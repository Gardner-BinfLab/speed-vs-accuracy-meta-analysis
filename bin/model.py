"""
model.py

Copyright 2017  <jmp197@uclive.ac.nz>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
MA 02110-1301, USA.
"""

import random
import math
import heapq
import decimal
import copy

# Number of times to run the simulation
TRIALS = 1000
# Number of agents in each simulation
NUMBEROFAGENTS = 10
# Should agents publish (and stop working) as soon as possible?
PUBLISHING = True
# Should agents have a tendancy towards a goal?
# (some combination of speed and accuracy)
FOCUSING = True
# Version:
#  1 - All agents start with 0 speed and 0 accuracy
#  2 - All agents start with some speed and 0 accuracy
#  3 - All agents start with 0 speed and some accuracy
#  4 - All agents start with some speed or some accuracy, 0 otherwise
#  5 - All agents start with a linear trade off of speed and accuracy
VERSION = 1


DEFAULTCODINGTIME = 0.25 # 6 hrs

ENDDATE = 260

# All four are represented as percentage increases
SPEEDDISTRIBUTIONMEAN = 0.02
SPEEDDISTRIBUTIONSTD = 0.02
ACCDISTRIBUTIONMEAN = 0.1
ACCDISTRIBUTIONSTD = 0.1

# The details of the biological problem to solve
PROBLEMRANGE = 6
NUMSEQUENCES = 32
LENSEQUENCES = 20000

MONTHDAYS = 22
WEEKSECONDS = 60*60*24*5
OPERATIONSPERSECOND = 10**9
MAXRUNOPS = WEEKSECONDS*OPERATIONSPERSECOND

COUNTER = 0

BINS = [[0 for j in range(10)] for i in range(10)]

class Problem():
    """
    Class to represent the problem that the algorithm must work to solve.
    This is a binary classification problem, ie the solution can be
    correct or incorrect.
    The task is to align a set number of DNA sequences of a given length,
    however, as algorithms may not initially be able to handle the
    entire problem in a reasonable amount of time, only a subset of the
    sequences are used.
    """
    def __init__(self, sequence_count=NUMSEQUENCES, sequence_length=LENSEQUENCES):
        """
        Initialises the number and length of sequences in the problem.
        Also sets the number of sequences to use in the first run of the
        algorithm.
        """
        self.sequences = sequence_count
        self.length = sequence_length

        self.sequences_to_use = max(2, self.sequences//10)

    def get_seqs(self):
        """
        Return the total number of sequences in the problem set
        """
        return self.sequences

    def get_len(self):
        """
        Return the length of thhe seqeuences in the problem
        """
        return self.length

    def solve_time(self, algorithm_complexity):
        """
        Runs the current problem through the supplied algorithm and
        makes sure that we don't overflow. (happens at about )
        """

        ret_val = 0
        try:
            ret_val = int(algorithm_complexity(self.sequences_to_use, self.length))
        except (decimal.Overflow, OverflowError):
            print("An error occured")
            ret_val = MAXRUNOPS

        if ret_val < MAXRUNOPS and self.sequences_to_use < self.sequences:
            self.sequences_to_use += max(1, self.sequences//10)
            self.sequences_to_use = min(self.sequences_to_use, self.sequences)
        return ret_val

class Algorithm():
    """
    Represents the algorithm that an agent develops to solve a binary
    classification problem, which is to align a set of DNA sequences.
    As the algorithm is initially very slow, it only tackles part of
    each problem before completeing it entirely and moving on to the
    next.
    Algorithm speed is based on the formula:

    steps = sequenceCount^(sequenceLength+expon*(
       log(sequnceLength(base sequenceCount)) + 1 - sequenceLength))

    Hence as expon moves from 0 to 1, sequenceLength is removed from
    the exponent and replaced with 1 plus its log, base sequence
    Count.
    This makes the algorithm linear in the power of the
    sequenceCount.
    """
    def __init__(self, default_acc=(0, 0), default_speed=0.0):
        """
        Initialises a new binary classification algorithm for an agent.
        All parameters have defaults
        speed must be in the range [0,1]
        accuracy is an ordered pair (true, false) and represents the
        means of two gaussian distributions which themselves represent
        the true and false positive positive population distributions.
        Only the distance between the means matter as if the true mean
        is less than the false mean, then the algorithm classifications
        will be inverted, swaping the effective locations of the
        distributions.
        """
        self.expon = default_speed

        (self.true_mean, self.false_mean) = default_acc
        self.true_sd = 2
        self.false_sd = 2

        self.thresholds = []

    def generate_thresholds(self):
        """
        Goes from -10 to 10 in steps of 0.1 and calculates the
        sensitivity and specificity at each one, storing them for use
        in ROC curve generation
        """
        self.thresholds = []
        scale_factor = 10

        low = int(min(self.true_mean, self.false_mean)*scale_factor)
        high = int(max(self.true_mean, self.false_mean)*scale_factor)
        diff = high - low
        low = min(-100, low - 3*diff)
        high = max(100, high + 3*diff)
        step = 1

        for threshold in range(low, high, step):

            sens = 1 - cumulative_normal_distribution(
                threshold/scale_factor, self.true_mean, self.true_sd)

            spec = cumulative_normal_distribution(
                threshold/scale_factor, self.false_mean, self.false_sd)

            self.thresholds.append((sens, spec))

    def get_specificity(self, threshold):
        """
        Calculates the specificity of the algorithm at the specified
        threshold, by calculating the ratio of true negatives generated
        by the algorithm
        """
        return cumulative_normal_distribution(threshold, self.false_mean, self.false_sd)

    def get_threshold_from_true_mean(self, sds):
        """
        Gets the x value at the point (sds) standard deviations away
        from the true mean
        """
        return self.true_mean - sds*self.true_sd

    def get_speed(self):
        """
        Returns the speed of the algorithm represented as a value in the
        range [0,1], which corresponds to the range
        [seqLen^seqCount,seqLen*seqCount], and is linear in the power of
        seqLen
        """
        return self.expon

    def get_accuracy(self):
        """
        Generate an approximation of the area under the ROC curve,
        using Riemann integrals on each adjacent pair of points.
        The sum will tend toward 0.5, either from below or above,
        due to the rounding that occurs with the method.
        """
        self.generate_thresholds()

        area = 0

        first = self.thresholds[0]

        for second in self.thresholds[1:]:
            width = second[1] - first[1]
            height_diff = abs(second[0] - first[0])
            rect_area = width * (min(first[0], second[0]) + height_diff)

            area += rect_area
            first = second
        area = (max(area, 1-area)-0.5)*2

        return area

    def get_threshold_points(self):
        """
        Generates the ROC points if they do not yet exist, and returns
        them
        """
        if not self.thresholds:
            self.generate_thresholds()
        return self.thresholds

    def get_means(self):
        """
        Return an ordered pair (True, False) of the means used to
        determine the ROC curve of the algorithm
        """
        return (self.true_mean, self.false_mean)

    def formula_packer(self):
        """
        Creates the formula used to determine the number of actions the
        algorithm takes to finish a problem
        """
        return lambda n, m: pow(n, 1+decimal.Decimal(math.log(m, n))+
                                decimal.Decimal((1-self.expon))*(m -
                                                                 (1+
                                                                  decimal.Decimal(math.log(m, n)))))

    def change_speed(self, change):
        """
        Updates the exponent of the algorithm complexity formula,
        ensuring that it remains within the [0,1] bounds
        """
        self.expon = max_min(0.0, self.expon+change, 1.0)

    def change_accuracy(self, change, change_true=True):
        """
        Adjusts the true mean by the given amount
        This works as the distance between the means is the only
        relevant statistic, not where the means actually are
        """
        if change_true:
            self.true_mean += change
        else:
            self.false_mean -= change

class Agent():
    """
    Represents an agent that is attempting to develop abinary
    classification algorithm to align a set of DNA sequences.
    Each agent is responsible for improving its algorithm and reporting
    the time their algorithm will finish with the current problem.
    """
    def __init__(self, default_speed=0.0, default_acc=(0.0, 0.0),
                 coding_time=0.1, problem_set=[Problem()],
                 focus=(1, 1)):
        """
        Initialises the agent, all parameters have a default value.
        No values should be outside the range [0,1], except coding time,
        which must be non-negative.
        default_speed should be a (seqs_level,len_level,op_level) tuple
        default_acc should be a (true_mean, false_mean) tuple
        focus = (speed, accuracy)
        """

        self.start_specs = Algorithm(default_acc, default_speed)

        self.prog_alg = Algorithm(default_acc, default_speed)
        self.problem_set = problem_set
        self.problem_level = 0
        self.prog_prob = self.problem_set[self.problem_level]

        self.curr_date = 0
        self.iterations = 0
        self.wasted_cycles = 0
        self.coding_time = coding_time
        self.published = False
        self.focus = focus

        self.improvement_range = [0, 1/2]
        self.improvement_range[0] += math.sqrt(self.focus[1])/4
        self.improvement_range[1] -= math.sqrt(self.focus[0])/4

    def __repr__(self):
        """
        Returns a string representation of the student.
        Currently set to return a stringified tuple containing its
        speed and accuracy
        """
        return str((self.get_speed(), self.get_accuracy()))
        #return "I'm a agent! I've made "+str(self.iterations)+\
        #       " changes to my program, but "+str(self.wasted_cycles)+\
        #       " of them were wasted on bad changes."

    def iterate(self):
        """
        Performs one iteration of the agents development cycle.
        Returns the date after the iteration has finished running, and
        the agent will next be available to work on the algorithm
        """

        # Work out which direction our development will attempt to head
        # in for this iteration, based on our focus'
        improvement_dir = math.pi*random.random()*(\
                          self.improvement_range[1] -\
                          self.improvement_range[0]) +\
                          self.improvement_range[0]

        # Get the horizontal and vertical components of the direction,
        # which represent speed and accuracy respectively, transforming
        # them to enhance the effect of the focus
        speed_improve = math.pow(math.cos(improvement_dir), 3)
        acc_improve = math.pow(math.sin(improvement_dir), 3)

        # Determine the change in speed and accuracy to make, based on
        # the global distribution values
        speed_change = speed_distribution()
        acc_change = acc_distribution()

        # Record the current score in preparation for a change in the
        # style of 'it is better to seek forgiveness than permission'
        score = self.get_accuracy() + self.get_speed()

        change_true = random.choice([True, False])
        # Change the speed and accuracy
        self.prog_alg.change_accuracy(acc_improve*acc_change, change_true)
        self.prog_alg.change_speed(speed_improve*speed_change)

        # See if we need to seek forgiveness
        if self.get_accuracy() + self.get_speed() < score:
            # Revert the changes if we have a bad update
            self.prog_alg.change_accuracy(-acc_improve*acc_change, change_true)
            self.prog_alg.change_speed(-speed_improve*speed_change)
            self.wasted_cycles += 1
        # Otherwise we're good to go

        # Time still passes either way
        self.curr_date += self.coding_time
        # See how long it took us to run this iteration
        run_operations = self.get_solve_time()
        if run_operations < WEEKSECONDS*OPERATIONSPERSECOND:
            # The algorithm finishes in less than a week
            self.curr_date += run_operations/(OPERATIONSPERSECOND*60*60*24)
            if run_operations < (OPERATIONSPERSECOND*60*60*24)\
            and (self.prog_prob.get_seqs() == self.prog_prob.sequences_to_use\
            or run_operations < (OPERATIONSPERSECOND*60*60))\
            and self.problem_level+1 < len(self.problem_set):
                # If we can run the program in less than a day, and
                # have a more difficult problem to work on then move
                # up to the next problem, provided that either we can
                # complete the current problem in less than an hour or
                # the current problem set uses all available sequences
                self.problem_level += 1
                self.prog_prob = self.problem_set[self.problem_level]
        else:
            # Only let the program run for a week and make a note to
            # focus on speeding the algorithm up next time
            self.curr_date += 7

        # Increment the number of iterations the agent has taken
        self.iterations += 1

        return self.curr_date

    def get_date(self):
        """
        Returns the current date for the agent, as agents are ordered by
        the time their program will end, and they can work on the next
        iteration (after analysing the results)
        """
        return self.curr_date

    def get_solve_time(self):
        """
        Determines how long it takes to run the algorithm with the
        current problem set
        """
        return self.prog_prob.solve_time(self.prog_alg.formula_packer())

    def get_accuracy(self):
        """
        Returns the accuracy of the agents algorithm, as determined by
        doubling the area under the computed ROC graph above 0.5
        This is because we are using a binary clasification system, so
        we can reverse the responses if the algorithm is consistantly
        wrong
        """
        return self.prog_alg.get_accuracy()

    def get_speed(self):
        """
        Returns the speed of the agents algorithm represented as a float
        between 0 and 1, representing seqLen^seqCount and
        seqLen*seqCount respectively. With the net speed linear in the
        power
        """
        return self.prog_alg.get_speed()

    def get_means(self):
        """
        Returns an ordered pair (True, False) of the means
        The second will always be the same as it was originally set to
        """
        return self.prog_alg.get_means()

    def get_thresholds(self):
        """
        Returns the list of specificty/sensitivity points for the algorithms ROC curve
        """
        return self.prog_alg.get_threshold_points()

    def get_problem_subset_size(self):
        """
        Returns the number of sequences being used in the current
        problem set
        """
        return self.prog_prob.sequences_to_use

    def get_algorithm(self):
        """
        Returns the algorithm the agent has developed
        """
        return self.prog_alg

    def get_start_speed(self):
        """
        Returns the speed of the algorithm as it was first created
        """
        return self.start_specs.get_speed()

    def get_start_accuracy(self):
        """
        Returns the accuracy of the algorithm as it was first created
        """
        return self.start_specs.get_accuracy()

    def get_specificity(self, sds):
        """
        Computes and returns the specificity of the algorithm at
        the given threshold
        sds - the number of standard deviations from the true mean the
        threshold is
        """
        return self.prog_alg.get_specificity(self.prog_alg.get_threshold_from_true_mean(sds))

def compute_mean(matrix2d):
    """
    Computes the mean of a supplied 2d array
    """
    total = 0
    count = 0
    for row in matrix2d:
        for cell in row:
            total += cell
            count += 1
    return total/count

def compute_sd(matrix2d, mean=None):
    """
    Takes a 2d array and computes the standard deviation, calculating
    the mean if this is not supplied
    """
    if mean is None:
        mean = compute_mean(matrix2d)
    total = 0
    count = 0
    for row in matrix2d:
        for cell in row:
            total += (cell - mean)**2
            count += 1
    return math.sqrt(total/count)

def max_min(left, measured, right):
    """
    If the given value (measured) is between the left and right values,
    then it is returned, else the closest of left and right to measured
    is returned.
    """
    if left > right:
        raise ValueError('Left greater than right. Right must be at least equal to left.')
    return max(left, min(measured, right))

def speed_distribution():
    """
    Returns a random value from a gaussian (normal) distribution to be
    used as the speed value.
    The distribution characteristics are entirely determined by the
    globals SPEEDDISTRIBUTIONMEAN and SPEEDDISTRIBUTIONSTD
    """
    return random.gauss(SPEEDDISTRIBUTIONMEAN, SPEEDDISTRIBUTIONSTD)

def acc_distribution():
    """
    Returns a random value from a gaussian (normal) distribution to be
    used as the accuracy value.
    The distribution characteristics are entirely determined by the
    globals ACCDISTRIBUTIONMEAN and ACCDISTRIBUTIONSTD
    """
    return random.gauss(ACCDISTRIBUTIONMEAN, ACCDISTRIBUTIONSTD)

def cumulative_normal_distribution(X, mu, sigma):
    """
    Only ever returns values in (0,1).
    A transformed error function is the same as the cumulative
    distribution function of a gaussian or normal distribution.
    """
    return (1 + math.erf((X - mu)/(sigma * math.sqrt(2))))/2

def main():
    """
    Runs the main simulation the stated number of times, and prints a
    progress meter to the console.
    """
    global COUNTER

    number_of_agents = NUMBEROFAGENTS
    final_date = ENDDATE

    problem_set = [
        Problem(sequence_length=2*10**i) for i in range(0, PROBLEMRANGE)
    ]
    while COUNTER < TRIALS:
        if COUNTER % 100 == 0:
            print(COUNTER)
        run_priority_queue_simulation(number_of_agents, final_date, PUBLISHING, problem_set)
        COUNTER += 1

    pprint(BINS)

def pprint(list2d):
    """
    Prints the BINS array nicely to the console, and to a file.
    Output is given as the counts of each bin, with the slowest and
    least accurate in the top left corner, with speed increasing
    horizontally and accuracy increasing downward.
    The file name is dependant on the settings set at the top of this
    file.
    """
    biggest = 0
    smallest = NUMBEROFAGENTS*TRIALS
    outstr = ""
    for row in list2d:
        for cell in row:
            outstr = outstr + str(cell).zfill(math.ceil(
                math.log(TRIALS, 10))) + '\t'
            biggest = max(biggest, cell)
            smallest = min(smallest, cell)
            #print(str(cell).zfill(math.ceil(math.log(TRIALS,10))), end=' ')
        outstr = outstr + '\n'
        #print('\n')

    mean = compute_mean(list2d)
    sd = compute_sd(list2d, mean)

    print(outstr)
    print((smallest, biggest))
    print((mean, sd))
    f_name = "ranks_V"+str(VERSION)+"_"+str(TRIALS//1000)+"k_"+\
    ["unpub", "pub"][int(PUBLISHING)]+"_"+\
    ["nofocus", "focus"][int(FOCUSING)]+".tsv"
    print("Saving output to "+f_name)
    with open(f_name, 'w') as out_file:
        out_file.write(outstr)

def follow_one_simulation(last_date, problems_to_use):
    """
    Follows one agent as it runs through the simulation.
    This can be used to show the path taken by an agent.
    Each turn the speed and accuracy are saved to a tsv file
    """
    default_acc = (0, 0)
    coding_time = DEFAULTCODINGTIME

    stud = Agent(default_acc, coding_time, problems_to_use)

    prog_points = []

    while stud.get_date() < last_date:
        stud.iterate()
        prog_points.append((stud.get_speed(), stud.get_accuracy()))

    best_speed = 0
    best_acc = 0

    for i in prog_points:
        best_speed = max(best_speed, i[0])
        best_acc = max(best_acc, i[1])

    with open("followOne.tsv", 'w') as out_file:
        for i, point in enumerate(prog_points):
            line_out = "min/avg/max cyles used\t{}\t{:.4}\t{:.4}"\
                       .format(i, float(point[0]), float(point[1]))
            write_out = "{}\t{}\n".format(point[0], point[1])
            #print(line_out)
            out_file.write(write_out+"\n")

    write_out = "{}\t{}\t{}\t{}\t{}\n".format(
        i, stud.get_speed()/best_speed, stud.get_accuracy()/best_acc,
        stud.iterations, stud.get_date())

def run_priority_queue_simulation(number_of_agents, last_date,
                                  publishing=False,
                                  problems_to_use=[Problem()]):

    """
    Runs the simulation with the given initial variables.
    Each agent is given an iteration until it reaches the final date.
    Will need to be changed for the publish early model.
    A priority queue for each one would work, based on when the next
    one is due to come up.
    """
    start_speed = 0.0
    start_acc = (0, 0)
    coding_time = DEFAULTCODINGTIME

    accuracies = [
        (0, 0),
        (0.45, -0.45),
        (0.95, -0.95),
        (1.625, -1.625),
        (3.305, -3.305)
    ]

    focus = (1, 1)

    agents = []

    for i in range(number_of_agents):

        if VERSION in [2, 5]:
            start_speed = (i%5)/4
        if VERSION in [3, 5]:
            start_acc = accuracies[4-i%5]
        elif VERSION == 4:
            if i%2:
                start_speed = (i%5)/4
            else:
                start_acc = accuracies[4-i%5]

        if FOCUSING:
            focus = (3*i/number_of_agents, 1,
                     3*(1-i/number_of_agents))

        agents.append((0, i, Agent(default_speed=start_speed,
                                   default_acc=start_acc,
                                   coding_time=coding_time,
                                   problem_set=problems_to_use,
                                   focus=focus)))

    if publishing:
        published_speed = Algorithm((0.36, -0.36))
        published_acc = Algorithm((0.36, -0.36))
    to_rank = []

    # A priority queue is used so that the list of agents will remain
    # in choronological order in an efficient way
    heapq.heapify(agents)

    active_agents = len(agents)
    while active_agents:
        (curr_date, agent_number, next_agent) = heapq.heappop(agents)
        if curr_date >= last_date:
            #We shouldn't get in here, but just in case we do
            print("We got an ENDDATE and broke everything")
            break

        next_date = next_agent.iterate()

        if publishing:
            #We must close the gap to perfection by at least 10% at each publication
            min_acc_acc = 1-((1-published_acc.get_accuracy())*9/10)
            min_acc_speed = 1-((1-published_acc.get_speed())*9/10)
            min_speed_speed = 1-((1-published_speed.get_speed())*9/10)
            min_speed_acc = 1-((1-published_speed.get_accuracy())*9/10)

            if next_agent.get_accuracy() > min_acc_acc or \
            (next_agent.get_accuracy() >= published_acc.get_accuracy() and\
            next_agent.get_speed() > min_acc_speed) and\
            next_agent.get_accuracy() > 0.2:
                # The algorithm is either more accurate than the last
                # most accurate, or it is faster and just as accurate
                published_acc = copy.deepcopy(next_agent.get_algorithm())
                next_agent.published = True

            if next_agent.get_speed() > min_speed_speed or \
            (next_agent.get_speed() >= published_speed.get_speed() and \
            next_agent.get_accuracy() > min_speed_acc) and\
            next_agent.get_accuracy() > 0.2:
                # The algorithm is either faster than the last fastest,
                # or it is more accurate and just as fast
                published_speed = copy.deepcopy(next_agent.get_algorithm())
                next_agent.published = True

            if next_agent.published:
                # We have published so we discontinue development of the
                # algorithm
                active_agents -= 1
                heapq.heappush(agents, (last_date, agent_number, next_agent))
                to_rank.append(next_agent)
                if len(to_rank) >= 10:
                    extract_ranks(to_rank)
                    to_rank = []

        if not next_agent.published:
            # We haven't published our work, so we wait till the program
            # stops running
            if next_date >= last_date:
                active_agents -= 1
            heapq.heappush(agents, (next_date, agent_number,
                                    next_agent))

    published_agents = [stud for (date, number, stud) in agents if publishing == stud.published]
    #print("Finished sim")
    extract_data(published_agents)
    for stud in agents:
        if stud[2].published is False and stud[2] not in to_rank:
            to_rank.append(stud[2])
            if len(to_rank) >= 10:
                extract_ranks(to_rank)
                to_rank = []
    #assemble_meta_tools(published_agents)

def extract_ranks(agents):
    """
    Ranks the agents against each other in terms of speed and accuracy,
    and increases the count of each relevant bin.
    Therefore after each round the sum of each row and column must
    increase by exactly one, and all rows and columns must have the same
    sum at the end
    """
    speed_ranked = agents.copy()
    speed_ranked.sort(key=lambda x: x.get_speed())
    accuracy_ranked = agents.copy()
    accuracy_ranked.sort(key=lambda x: x.get_accuracy())
    for stud in agents:
        BINS[accuracy_ranked.index(stud)][speed_ranked.index(stud)] += 1

def extract_data(agents):
    """
    Extracts the iteration, wasted iteration, speed and accuracy value
    from each agent and writes them to disk (for speed and accuracy)
    or prints them to the terminal (for used and wasted iterations)
    """
    with open("results"+str(PUBLISHING)+".tsv", 'w') as out_file:
        for i, stud in enumerate(agents):
            write_out = "{}\t{}\t{}\t{}\t{}\n"\
            .format(i, stud.get_start_speed(), stud.get_start_accuracy()
                    , stud.get_speed(), stud.get_accuracy())
            out_file.write(write_out)


def extract_ROC(stud):
    """
    Takes the specificity and sensetivity at various thresholds, and
    places them (sequentially) in a tsv file for graphing externally.
    """
    acc_points = stud.get_thresholds()

    with open("results.tsv", 'w') as out_file:
        for i in acc_points:
            line_out = "{:.4}\t{:.4}".format(i[0], i[1])
            out_file.write(line_out+"\n")

if __name__ == "__main__":
    main()
    #extract_ROC(Agent())
