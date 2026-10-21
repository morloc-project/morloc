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
Intent: ruled 2026-10-19
Tests: spec-cpp-6-1
Code: conforms 2026-10-19

A `data` type with payloads is a struct holding
`std::variant<std::shared_ptr<const T_C>...> v`, one alternative per
constructor in declaration order, where `T_C` is the struct for constructor
`C`. Morloc generates this form, and a mapped `data` type is written the
same way. A `?R` field that leads back to its own record is a
`std::shared_ptr<const R>`, with `nullptr` meaning absent. A copy shares the
arm, and nothing changes an arm after it is built: to change a value, copy
the arm, change the copy, and wrap it in a new `std::make_shared<T_C>`,
which converts to the const holder. An arm is never itself a const object:
one made by `std::make_shared<const T_C>` has no defined result, since
morloc may move a field out of an arm it holds alone. Code that is still
building a value may hold a `std::shared_ptr<T>` to a non-const `T` and
store it into a const field. A raw pointer, a `std::unique_ptr`, or a
`std::shared_ptr` to a non-const type in these positions is rejected at
build (EVAL-7, CPP-3).

Morloc moves a field out of an arm only when it built the arm itself and no
other holder shares it, and copies it otherwise (OWN-5). An arm built with
`mlc::make_arm` (from `mlc_rec.hpp`) is released in bounded stack (STATE-8);
one built with `std::make_shared` is released recursively.
