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

        from HW2 import hw2_task3_pipeline
        hw2_task3_pipeline()

    elif argument == 'HW5':

        from HW5 import task5_pipeline
        task5_pipeline()
        pass
