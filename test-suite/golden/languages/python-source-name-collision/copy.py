import base64

def copy_kind(x):
    # a stdlib import from inside a source file whose own name is a
    # stdlib module name must still reach the real stdlib
    return "copy:" + base64.b64encode(x.encode()).decode()
