import numpy as np
def load(path):
    return np.zeros((2, 3, 3), dtype=np.uint8)
def sp(n, img):
    return np.zeros(img.shape[:2], dtype=np.int64)
def graph(img, labels):
    return int(labels.size)
