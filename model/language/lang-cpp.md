# C++

What sourced C++ code and mapped C++ types must do, and what morloc does
for them. These are the C++ forms of rules stated once for every language;
each item cites its rule. Prefix: CPP.

Rules for C++ stated elsewhere: NATIVE-3 and NATIVE-4 (a record needs a
user struct), NUM-1 (`Int` is the native default integer), NUM-6 (integer
division), OWN (how generated C++ borrows, copies and moves).

### CPP-1 A sourced C++ function takes each argument by value or by `const T&`
Intent: ruled 2026-10-18
Tests: spec-cpp-1-1
Code: conforms 2026-10-19

A parameter of a sourced C++ function is `T`, `const T&`, or a template
parameter that deduces one of these. A parameter of type `T&` is rejected at
build. A function that needs to change a value takes it by value and
changes its own copy. C++ code that calls a function morloc passed to it
calls it the same way, never with a non-const reference. This is EVAL-7's
rule that sourced code never changes an argument, and it holds until morloc
has a way to say where a value may be changed.

### CPP-2 A sourced C++ function returns a value that owns its data
Intent: ruled 2026-10-18
Code: conforms 2026-10-21

A sourced C++ function's result refers into none of its arguments: it is
not, and does not hold, a reference, pointer, `std::string_view`,
`std::span` or other view into an argument, and it is not a lambda that
captures an argument by reference. A function may return `const T&` to
data that outlives every call, such as a static; morloc copies the result
into a value it owns. A program that breaks this rule has no defined
result (EVAL-7).

### CPP-3 A mapped C++ type is a value type
Intent: ruled 2026-10-18
Tests: spec-cpp-3-1
Code: conforms 2026-10-19

A C++ type that a morloc type maps to (`type Cpp => T = "..."`, `record
Cpp => R = "..."`) is not a pointer, a reference, a view or a
`std::weak_ptr`, and is move constructible; neither is any field of a
mapped record. A `std::shared_ptr<const T>` is a value. A mapping that
breaks this rule is rejected at build, for each mapping the program's C++
code uses; a struct that hides a pointer cannot be detected and is the
program's error (EVAL-7).

### CPP-4 A mapped C++ record accepts moved fields
Intent: ruled 2026-10-18
Tests: spec-cpp-4-1
Code: conforms 2026-10-19

Morloc may build a mapped record from moved field values, and move a
record into a function or another value. A mapped record's constructor
therefore takes no field by non-const lvalue reference, and no field type
deletes its move constructor (EVAL-7).

### CPP-5 A mapped C++ type may be read from several threads at once
Intent: ruled 2026-10-18
Code: unaudited

Calling a `const` member function of a mapped type, or reading it through
a `const T&`, is safe while other threads do the same with the same object
(EVAL-7). Morloc may share one object between parallel workers instead of
copying it.

### CPP-6 A C++ `data` value and a recursive record field are shared and never changed
Intent: ruled 2026-11-15
Tests: spec-cpp-6-1, spec-cpp-6-2, spec-cpp-6-3
Code: conforms 2026-11-21

A `data` type with payloads is a struct holding
`std::variant<std::shared_ptr<const T_C>...> v`, one alternative per
constructor in declaration order, where `T_C` is the struct for constructor
`C`. Morloc generates this form, and a mapped `data` type is written the
same way. A `?R` field that leads back to its own record is a
`std::shared_ptr<const R>`, with `nullptr` meaning absent; a value read
from such a field is the `std::optional<R>` every other `?R` value is, so
the pointer is storage only. A copy shares the
arm, and nothing changes an arm after it is built: to change a value, copy
the arm, change the copy, and wrap it in a new `std::make_shared<T_C>`,
which converts to the const holder. An arm is never itself a const object:
one made by `std::make_shared<const T_C>` has no defined result, since
morloc may move a field out of an arm it holds alone. Code that is still
building a value may hold a `std::shared_ptr<T>` to a non-const `T` and
store it into a const field. A raw pointer, a `std::unique_ptr`, or a
`std::shared_ptr` to a non-const type in these positions is rejected at
build (EVAL-7, CPP-3).

Morloc moves a field out of an arm it owns when no other holder shares the
arm, deciding at run time from the arm's count of owners, and copies the
field otherwise (OWN-5, OWN-7); whoever built the arm. A `std::weak_ptr`, a
raw pointer or a reference is not an owner and is not counted, so the
decision rests on EVAL-7: sourced code keeps none of them to a value past
the call that gave or returned it (ruled 2026-11-21). When nothing in the
frame names the value after the last field taken from its arm, morloc
releases the arm at once, so a `std::weak_ptr` kept against EVAL-7 finds it
expired rather than reading a field that was moved out. A `std::weak_ptr`
locked on another thread while morloc takes a field has no defined result:
C++ cannot test that a `std::shared_ptr` has one owner and take it in one
step, as Rust's `Arc::try_unwrap` does, so the count can rise between the
test and the move. An arm built with
`mlc::make_arm` (from `mlc_rec.hpp`) is released in bounded stack (STATE-8);
one built with `std::make_shared` is released recursively.

A record morloc generates that refers to itself, anywhere in its fields, is
a handle to its fields instead (GEN-1, ruled 2026-11-15): `struct R {
std::shared_ptr<const R_fields> p; ... }`, read with `r->k`, beside
`struct R_fields` holding the fields. Its `?R` is then the ordinary
`std::optional<R>`, so a field read borrows the field and a copy of `R`
shares the node. A handle is null only after it is moved from; its default
constructor holds the one default node of its type. A source builds one as
`R{{...}}` or `R(R_fields{...})`, whose node, like those pool code builds,
comes from `mlc::make_arm` wherever `mlc_rec.hpp` can be included.

Why: reversed 2026-11-15 for generated records, whose `?R` was a pointer read as a copy.

### CPP-7 A C++ function value takes a non-scalar parameter by `const A&`
Intent: ruled 2026-11-21
Tests: spec-cpp-7-1
Code: conforms 2026-11-21

A morloc function value in C++ is a `std::function<R(P...)>` whose
parameter `P` is `A` when `A` is a scalar (`std::is_scalar_v<A>`: an
integer, floating-point, `bool` or enum type) and `const A&` otherwise,
wherever morloc spells the type, a template parameter included: a
let, a parameter or result of a generated function, a field, and what a
sourced function receives or returns. Applying a function value to a value
then copies nothing, as applying a Rust function value does (HOST-8); a
function value that keeps its argument copies it. A
sourced function written against `std::function<R(A)>` for a non-scalar
`A` still receives a value it can call, through a conversion, but a
template that deduces `A` from that spelling does not match.
