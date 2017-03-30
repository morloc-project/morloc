# Core Libraries

This folder contains the source code for the core functions that all supported
languages should define.  When Morloc is installed, these libraries will be copied
to the `~/.loc/lib/core` directory. They will usually be included under the
"include" section in Morloc scripts. The frontend will load these files and the
source will be included in the resultant LIL. Therefore, the backends do not
need to interact with these libraries (or know where they are).

The libraries are all written in Morloc, with native code in the `source`
sections. This allows all typing, documentation, and default argument setting
to be done in the background.
