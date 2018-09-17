import json
import pandas as pd
import numpy as np

def unpackDataFrame(jsonTxt):
    """
    Loads a pandas DataFrame object from a JSON file that is assumed to store
    named columns, for example '{"col1":[1,2,3],"col2":[2,3,4]}'.
    """
    return(pd.DataFrame.from_dict(json.loads(jsonTxt)))

def packDataFrame(x):
    """
    Serializes a pandas DataFrame to a JSON file with named column vectors.
    """
    return(json.dumps(pd.DataFrame.to_dict(x, orient="list")))

def unpackMatrix(jsonTxt):
    """
    Loads a numpy matrix from a JSON file.
    """
    return(np.matrix(json.loads(jsonTxt)))

def packMatrix(x):
    """
    Serializes a numpy matrix to a JSON file.
    """
    return(json.dumps(np.matrix.tolist(x)))
