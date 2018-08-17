#!/usr/bin/env python

"""
This tool creates dummy standard costs and associated SEND model need_setting_costs.csv input file for each state
observed in the input transitions.csv file.

This script can be run from the command line with two arguments. The first is the path to the transitions
history input file, and the second is the path where the output costs file should be written.
e.g:
    ./generate_dummy_costs.py path/to/transitions.csv path/to/dummy_costs.csv

"""

import sys
import pandas as pd
from generate_valid_states import validate_input

if sys.version_info[0] < 3:
    raise Exception("Python 3 is required.")

TRANSITIONS_PATH = sys.argv[1]
DUMMY_COSTS_PATH = sys.argv[2]
FIXED_COST = 1000


def generate_dummy_costs(transitions):
    states = set()
    for row in transitions.iterrows():
        setting_1 = row[1][1]
        need_1 = row[1][2]
        setting_2 = row[1][4]
        need_2 = row[1][5]

        states.add((setting_1, need_1))
        states.add((setting_2, need_2))

    states.discard(('NONSEND', 'NONSEND'))
    costs = pd.DataFrame()

    for state in states:
        setting, need = state
        costs = costs.append({'need': need, 'setting': setting, 'cost': FIXED_COST}, ignore_index=True)

    return costs


if __name__ == '__main__':
    print("reading", TRANSITIONS_PATH)
    in_df = pd.read_csv(TRANSITIONS_PATH)
    validate_input(in_df)
    out_df = generate_dummy_costs(in_df)
    print("writing", DUMMY_COSTS_PATH)
    out_df.to_csv(DUMMY_COSTS_PATH, index=False, columns=['need', 'setting', 'cost'])
