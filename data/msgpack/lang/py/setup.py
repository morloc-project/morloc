import os
from distutils.core import setup, Extension

module = Extension('pympack',
                   sources=['pympack.c'],
                   include_dirs=[os.path.expanduser('~/.morloc/include')],
                   library_dirs=[os.path.expanduser('~/.morloc/lib')],
                   libraries=['mlcmpack'])

setup(name='pympack', version='0.1', ext_modules=[module])
