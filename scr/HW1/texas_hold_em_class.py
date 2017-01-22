

class card_class():

    def __init__(self):
        pass

    def int_to_card(self, i):
        suit = i/13 + 1
        number = i%13 + 2
        return suit, number

    def card_to_int(self, card):
        return (card[0]-1)*13 + card[1] - 2

    def card_to_name(self, card):
        if card[0] == 1:
            return "c%i"%card[1]
        elif card[0] == 2:
            return "d%i"%card[1]
        elif card[0] == 3:
            return "h%i"%card[1]
        elif card[0] == 4:
            return "s%i"%card[1]




class texas_hold_em_agent_class():

    def __init__(self, card_1, card_2):
        """
        Jack=11, Queen=12, King=13, Ace=14
        Initialize the agent
        :param card_1: two int value, the second value is the card number
         the first value is the suit
        :param card_2: two int value, the second value is the card number
         the first value is the suit
        """

        self.card_1 = card_1
        self.card_2 = card_2

    def pair(self):
        """
        true if the hole cards are a pair
        :return: boolean
        """
        return self.card_1[1] == self.card_2[1]

    def same_suit(self):
        """
        true if the hole cards have the same suit
        :return: boolean
        """
        return self.card_1[0] == self.card_2[0]

    def distance_between(self):
        """
        returns the distance between the cards
        useful for detecting possible straights
        :return: integer
        """
        return abs(self.card_1[1] - self.card_2[1])

    def high_card_value(self):
        """
        returns the value of the high card
        :return: integer
        """
        return max(self.card_1[1], self.card_2[1])

    def call(self):
        return "call"

    def fold(self):
        return "fold"


class agent1_class():
    """
    agent that only calls on pairs
    """

    def __init__(self):
        pass

    def assign(self, card_1, card_2):
        """
        Jack=11, Queen=12, King=13, Ace=14
        Initialize the agent
        :param card_1: two int value, the first value is the card number
         the second value is the suit
        :param card_2: two int value, the first value is the card number
         the second value is the suit
        """
        self.agent = texas_hold_em_agent_class(card_1, card_2)

    def action(self):

        if self.agent.pair():
            return self.agent.call()
        else:
            return self.agent.fold()


class agent2_class():
    """
    agent that only calls on pairs
    """

    def __init__(self):
        pass

    def assign(self, card_1, card_2):
        """
        Jack=11, Queen=12, King=13, Ace=14
        Initialize the agent
        :param card_1: two int value, the first value is the card number
         the second value is the suit
        :param card_2: two int value, the first value is the card number
         the second value is the suit
        """
        self.agent = texas_hold_em_agent_class(card_1, card_2)

    def action(self):

        if self.agent.pair():
            return self.agent.call()
        elif self.agent.same_suit():
            return self.agent.call()
        else:
            return self.agent.fold()


class agent3_class():
    """
    agent that only calls on pairs
    """

    def __init__(self):
        pass

    def assign(self, card_1, card_2):
        """
        Jack=11, Queen=12, King=13, Ace=14
        Initialize the agent
        :param card_1: two int value, the first value is the card number
         the second value is the suit
        :param card_2: two int value, the first value is the card number
         the second value is the suit
        """
        self.agent = texas_hold_em_agent_class(card_1, card_2)

    def action(self):

        if self.agent.high_card_value() > 10:
            return self.agent.call()
        else:
            return self.agent.fold()


class agent4_class():
    """
    agent that only calls on pairs
    """

    def __init__(self):
        pass

    def assign(self, card_1, card_2):
        """
        Jack=11, Queen=12, King=13, Ace=14
        Initialize the agent
        :param card_1: two int value, the first value is the card number
         the second value is the suit
        :param card_2: two int value, the first value is the card number
         the second value is the suit
        """
        self.agent = texas_hold_em_agent_class(card_1, card_2)

    def action(self):

        if self.agent.pair() and self.agent.high_card_value() > 10:
            return self.agent.call()
        # elif (self.agent.same_suit() and self.agent.distance_between() < 2
        #      and self.agent.high_card_value() > 9):
        #    return self.agent.call()
        else:
            return self.agent.fold()
