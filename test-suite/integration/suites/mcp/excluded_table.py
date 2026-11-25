"""Pool-side implementations for excluded_table.loc (MCP test suite).

Requires pyarrow (the morloc Arrow Table representation). The runner skips
this program's group if it fails to compile (e.g. pyarrow not installed).
"""

import pyarrow as pa


def add2(x, y):
    return x + y


def read_data(n):
    return pa.RecordBatch.from_pydict({
        "id": list(range(n)),
        "value": [float(i) * 0.5 for i in range(n)],
    })


def row_count(t):
    return t.num_rows
