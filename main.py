#! /usr/bin/env python
import sys
import warnings
warnings.filterwarnings('ignore')


fdir = '/Users/aryaf/Desktop/codes/PyCharms/AI_HW/'

sys.path.insert(0, fdir+'scr')
sys.path.insert(0, fdir+'scr/HW1')
sys.path.insert(0, fdir+'scr/HW2')
sys.path.insert(0, fdir+'data')
sys.path.insert(0, fdir+'scr/libraries')


def main():

    from main_pipeline import main_pipeline
    main_pipeline()

if __name__ == "__main__":

    main()


