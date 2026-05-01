"""Build the Arrow IPC fixture this golden test reads.

Writes a 3-row, 2-column Arrow IPC file (file format with ARROW1 magic) into
input.arrow. The test then has the nexus read it back and round-trip through
the python pool, asserting it lands on stdout as JSON.
"""
import pyarrow as pa


def main():
    batch = pa.RecordBatch.from_pydict({
        "x": [42, 17, 99],
        "y": ["alpha", "beta", "gamma"],
    })
    with pa.OSFile("input.arrow", "wb") as sink:
        with pa.ipc.new_file(sink, batch.schema) as writer:
            writer.write_batch(batch)


if __name__ == "__main__":
    main()
