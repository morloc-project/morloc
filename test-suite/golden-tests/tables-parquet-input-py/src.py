def py_identity(t):
    """Pass-through that forces the table through the python pool.

    The intent is to exercise the JSON->Arrow SHM->pyarrow->Arrow SHM->JSON
    round trip; pyarrow refuses to operate on an empty table, so we touch
    the schema (via num_rows) to ensure the import side works.
    """
    _ = t.num_rows
    return t
