import os
from setuptools import setup, Extension
import numpy as np  # Import numpy

# Ensure numpy is installed before running setup
try:
    np_include_path = np.get_include()
except AttributeError:
    raise RuntimeError("Numpy is required to build this extension")


module = Extension(
    'pymorloc',
    sources=['pymorloc.c'],
    include_dirs=[
        os.path.expanduser('~/.local/share/morloc/include'),
        np_include_path  # Add numpy include directory
    ],
    library_dirs=[os.path.expanduser('~/.local/share/morloc/lib')],
    runtime_library_dirs=[os.path.expanduser('~/.local/share/morloc/lib')],
    libraries=['morloc']
)

setup(
    name='pymorloc',
    version='0.1',
    ext_modules=[module],
    extras_require={
        'numpy': ['numpy']
    }
)
