import sys
from HW1 import agent1_class, agent2_class, agent3_class, agent4_class, card_class

def print_tabular_rules():

    try:
        agent_id = int(sys.argv[2])
    except IndexError:
        print "Error, Please provide agent_id"
        sys.exit(-1)

    if agent_id == 1:
        agent = agent1_class()
    if agent_id == 2:
        agent = agent2_class()
    if agent_id == 3:
        agent = agent3_class()
    if agent_id == 4:
        agent = agent4_class()
    card = card_class()

    counter = 0

    for i in range(51):

        card_1 = card.int_to_card(i)

        for j in range(i+1, 52):

            card_2 = card.int_to_card(j)

            agent.assign(card_1, card_2)

            # counter += 1
            # print counter, card.card_to_name(card_1), card.card_to_name(card_2), agent.action()
            # counter += 1
            # print counter, card.card_to_name(card_2), card.card_to_name(card_1), agent.action()

            counter += 1
            print "%s,%s,%s"%(card.card_to_name(card_1), card.card_to_name(card_2), agent.action())
            counter += 1
            print "%s,%s,%s"%(card.card_to_name(card_2), card.card_to_name(card_1), agent.action())

