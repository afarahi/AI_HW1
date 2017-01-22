import numpy as np

def main_pipeline():

    import sys

    try:
        argument = sys.argv[1]
    except IndexError:
        sys.exit(-1)

    # generate a new catalogs
    if argument == 'HW1':

        from HW1 import print_tabular_rules
        print_tabular_rules()

    elif argument == 'HW2':

        pass

    elif argument == 'HW3':

        pass
