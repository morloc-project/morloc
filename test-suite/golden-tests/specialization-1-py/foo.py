import numpy as np
from typing import List, Union, TypeVar

T = TypeVar('T', int, float, bool)

def numpy2list(xs: np.ndarray) -> List[Union[int, float, bool]]:
    """
    Convert numpy array xs to a normal python list
    """
    return xs.tolist()

def list2numpy(xs: List[T]) -> np.ndarray:
    """
    Convert pure python list xs to numpy array
    """
    return np.array(xs)

def square(x: Union[int, float, np.ndarray]) -> Union[int, float, np.ndarray]:
    """
    Square the input value or array
    """
    return x ** 2
