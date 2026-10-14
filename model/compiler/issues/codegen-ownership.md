# Ownership in generated pool code

Where a pool takes a value instead of copying it, and a later read of the
same name sees what was taken.

- **C++ `@show` of a `@try` result after its payload was taken prints the
  emptied payload.** [reproduced] Breaks EVAL-3 (a named value has one
  value in its scope). `consumableProjectionLets`
  (library/Morloc/CodeGenerator/Grammars/Translator/Imperative.hs) does not
  count a mention inside an arm that ends in a throw, on the grounds that
  nothing follows a throw. But the throw's own argument can read the
  subject after the projection on the same path, and C++ has already
  `std::move`d the payload out (Cpp.hs `BindConsume`). Repro, C++ pool,
  `mk n` = n bytes of 7:

      f n =
        let t = @try (mkCpp n)
        in match t
           | (Ok v) = (? lenCpp v > 3 = lenCpp v : @throw (@show t))
           | (Err _) = 0

  `./nexus f 2` fails with `{"Ok":[[]]}`; it should be `{"Ok":[[7,7]]}`.
  The exemption is sound only for a mention evaluated before the projection
  or on another branch. A tag test evaluated after the projection (a second
  `match t` that does not project) is harmless in C++ (the tag survives
  the move) but would be a use-after-move in any member that moves the
  whole subject.
