import pandas as pd

def dataframe_from_transitions(edn_path, n_sims):
    """Takes transitions.edn file and returns all information contained in the file as a dataframe. 
    
    Args:
        edn_path: string, path to transitions.edn file
        n_sims: int, the number of simulations used
    
    TODO: infer n_sims from config file in results dir"""    
    with open(edn_path) as in_file: 
        transitions = in_file.read()    
    transitions = transitions.replace("{","").replace("}","").replace(":", "").split(", ")
    
    df = pd.DataFrame()
    for i in transitions:
        l = i.replace("[","").replace("]","").split(" ")
        need_1 = l[2].split("-")[0]
        need_2 = l[3].split("-")[0]

        if need_1 == 'NONSEND':
            setting_1 = 'NONSEND'
        else:
            setting_1 = l[2].split("-")[1]        
        if need_2 == 'NONSEND':
            setting_2 = 'NONSEND'
        else:
            setting_2 = l[3].split("-")[1]

        df = df.append({'year':int(l[0]), 'ay':int(l[1]), 'state_1':l[2], 'state_2':l[3], 'count':int(l[4])/n_sims,
                       'need_1':need_1, 'setting_1':setting_1, 'need_2':need_2, 'setting_2':setting_2}, ignore_index=True)
    return df


def heatmap_pivot(df, x, y):
# WIP
    """Takes transitions dataframe and returns format required for seaborn heatmap
    
    Args:
        df: transitions dataframe, such as produced by dataframe_from_transitions()
        x: name of ay_1 column
        y: name of ay_2 columns
    
    Example use: sns.heatmap(heatmap_pivot(transitions_df, 'need_1', 'need_2'))
    """
    x_vals = df[x].unique()
    y_vals = df[y].unique()

    p = pd.DataFrame()

    for i in x_vals:
        probs = []
        sub = df[df[x] == i]
        total = sub['count'].sum()
        for j in y_vals:
            probs.append(sub[sub[y] == j]['count'].sum()/total)
        p[i] = pd.Series(probs, index=x_vals)
    return p.T
