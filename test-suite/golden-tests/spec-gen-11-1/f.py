from mlc_types._local.main import Person, Inner
def kindOf(p):
    return f"{type(p).__name__} {isinstance(p, Person)} {type(p.inner).__name__} {p.inner.model_dump()} {p.name}"
def bump(x):
    return x + 1
