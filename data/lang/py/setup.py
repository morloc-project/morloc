import os
import sys
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
    # Resolve libmorloc relative to this extension's own location at load time
    # (origin = .../morloc/opt, so origin/../lib = .../morloc/lib), so the
    # binding is not tied to the MORLOC_HOME used at build time. This mirrors
    # the nexus rpath (data/rust/morloc-nexus/build.rs). The loader's origin
    # token is platform-specific: ELF expands $ORIGIN, Mach-O (macOS) expands
    # @loader_path. The absolute runtime dir (runtime_library_dirs) is kept as
    # a fallback for non-standard layouts.
    runtime_library_dirs=[os.path.join(morloc_home, 'lib')],
    extra_link_args=[
        '-Wl,-rpath,'
        + ('@loader_path' if sys.platform == 'darwin' else '$ORIGIN')
        + '/../lib'
    ],
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
