#!/usr/bin/env python

"""
This tool processes a SEND transitions history CSV file and writes a valid_settings_academic_years CSV file
based on the needs, settings and academic years observed in the input transitions.

This script can be run from the command line with two arguments. The first is the path to the transitions
history input file, and the second is the path where the output valid settings file should be written.
e.g:
    ./generate_valid_states.py path/to/transitions.csv path/to/new_valid_settings.csv

"""

import sys
import pandas as pd

if sys.version_info[0] < 3:
    raise Exception("Python 3 is required.")

TRANSITIONS_PATH = sys.argv[1]
VALID_STATES_PATH = sys.argv[2]


def validate_input(transitions):
    expected_order = ['calendar-year', 'setting-1', 'need-1', 'academic-year-1',
                      'setting-2', 'need-2', 'academic-year-2']
    if not (transitions.columns == expected_order).all():
        raise ValueError("input columns did not match expected names or order:", str(expected_order))


def generate_valid_states(transitions):
    settings = pd.unique(transitions[['setting-1', 'setting-2']].values.ravel())
    obs_next_setting, obs_ay, obs_need = [{key: set() for key in settings} for _ in range(3)]

    for row in transitions.iterrows():
        setting_1 = row[1][1]
        need_1 = row[1][2]
        ay_1 = row[1][3]
        setting_2 = row[1][4]
        need_2 = row[1][5]
        ay_2 = row[1][6]

        obs_next_setting[setting_1].add(setting_2)
        obs_need[setting_1].add(need_1)
        obs_need[setting_2].add(need_2)
        obs_ay[setting_1].add(ay_1)
        obs_ay[setting_2].add(ay_2)

    valid_states = pd.DataFrame()
    for setting in settings:
        if setting == 'NONSEND':
            continue

        obs_next_setting[setting].discard('NONSEND')

        valid_states = valid_states.append({'setting': setting,
                                            'setting-group': 'ignore',
                                            'min-academic-year': min(obs_ay[setting]),
                                            'max-academic-year': max(obs_ay[setting]),
                                            'needs': ','.join(obs_need[setting]),
                                            'setting->setting': ','.join(obs_next_setting[setting])},
                                           ignore_index=True)

    int_cols = ['min-academic-year', 'max-academic-year']
    valid_states[int_cols] = valid_states[int_cols].astype(int)
    return valid_states


if __name__ == "__main__":
    print("reading", TRANSITIONS_PATH)
    in_df = pd.read_csv(TRANSITIONS_PATH)
    validate_input(in_df)
    out_df = generate_valid_states(in_df)
    print("writing", VALID_STATES_PATH)
    out_df.to_csv(VALID_STATES_PATH, index=False, columns=['setting', 'setting-group', 'min-academic-year',
                                                           'max-academic-year', 'needs', 'setting->setting'])

