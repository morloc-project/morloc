import decimal
import pyarrow as pa


def _pad(n):
    return pa.array(["x" * n], pa.string())


def mk(pad):
    return pa.RecordBatch.from_arrays(
        [_pad(pad), pa.array([decimal.Decimal("1.25")], pa.decimal128(10, 2))],
        names=["pad", "price"])


def mkViewed(pad):
    return pa.RecordBatch.from_arrays(
        [_pad(pad), pa.array(["viewed"], pa.string_view())],
        names=["pad", "label"])


def price(t):
    return "%s %s" % (t.column("price")[0].as_py(), t.schema.field("price").type)


def label(t):
    return "%s %s" % (t.column("label")[0].as_py(), t.schema.field("label").type)
