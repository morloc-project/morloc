"""Build the Parquet fixture this golden test reads.

Writes a 3-row, 2-column Parquet file (PAR1 magic) into input.parquet. The
test then has the nexus read it back and round-trip through the python pool,
asserting it lands on stdout as JSON.
"""
import pyarrow as pa
import pyarrow.parquet as pq


def main():
    table = pa.Table.from_pydict({
        "x": [42, 17, 99],
        "y": ["alpha", "beta", "gamma"],
    })
    pq.write_table(table, "input.parquet", compression="none")


if __name__ == "__main__":
    main()
