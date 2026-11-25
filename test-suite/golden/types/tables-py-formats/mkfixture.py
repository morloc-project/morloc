"""Build Parquet and Arrow IPC fixtures for the format I/O round-trip test.

input.parquet -- 3-row Parquet file (PAR1 magic), used by the parquet input path.
input.arrow   -- 3-row Arrow IPC file (ARROW1 magic), used by the IPC input path.
"""
import pyarrow as pa
import pyarrow.parquet as pq


def main():
    batch = pa.RecordBatch.from_pydict({
        "x": [42, 17, 99],
        "y": ["alpha", "beta", "gamma"],
    })
    pq.write_table(pa.Table.from_batches([batch]), "input.parquet",
                   compression="none")
    with pa.OSFile("input.arrow", "wb") as sink:
        with pa.ipc.new_file(sink, batch.schema) as writer:
            writer.write_batch(batch)


if __name__ == "__main__":
    main()
