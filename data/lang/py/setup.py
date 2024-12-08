import os
from distutils.core import setup, Extension

module = Extension('pymorloc',
                   sources=['pymorloc.c'],
                   include_dirs=[os.path.expanduser('~/.morloc/include')],
                   library_dirs=[os.path.expanduser('~/.morloc/lib')],
                   libraries=['morloc'])

setup(name='pymorloc', version='0.1', ext_modules=[module])
