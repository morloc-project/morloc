import os
from setuptools import setup, Extension
import numpy as np

try:
    np_include_path = np.get_include()
except AttributeError:
    raise RuntimeError("Numpy is required to build this extension")

morloc_home = os.environ.get(
    'MORLOC_HOME',
    os.path.expanduser('~/.local/share/morloc')
)

module = Extension(
    'pymorloc',
    sources=['pymorloc.c'],
    include_dirs=[
        os.path.join(morloc_home, 'include'),
        np_include_path
    ],
    library_dirs=[os.path.join(morloc_home, 'lib')],
    runtime_library_dirs=[os.path.join(morloc_home, 'lib')],
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
