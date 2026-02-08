# Debugging

## Quick Tips

**Inspect intermediate representations:**
```bash
morloc dump script.loc
```

**Run individual golden test:**
```bash
cd test-suite/golden-tests/native-morloc-1
make
diff exp.txt obs.txt
make clean
```

**Verbose compilation:**
```bash
morloc make -v script.loc
```

## Understanding Generated Code

**Nexus:** Always named `nexus` in current directory or output dir
**Pools:** Named `pool_<lang>_<id>.<ext>` (e.g., `pool_py_0.py`)

Read generated code to understand:
- How nexus dispatches calls
- How pools deserialize arguments
- Serialization format

## Common Errors

**"openBinaryFile: does not exist"**
- Cause: Morloc not initialized
- Fix: `morloc init -f`

**Missing module errors**
- Cause: Core libraries not installed
- Fix: `morloc install internal root root-py root-cpp root-r math`

**Type mismatch errors**
- Compiler shows source location and expected vs actual types
- Check function signatures and call sites

**Serialization errors**
- Check type compatibility between languages
- Verify msgpack library is installed
- Inspect generated serialization code

## Configuration

**Location:** `~/.local/share/morloc/config.yaml`

Check:
- Module paths
- Compiler settings
- Library locations

## Debug Mode

Use `morloc dump` to see:
- Parsed AST
- Type-checked expressions
- Resolved modules
- Generated intermediate code

Output shows each compilation stage.

## Golden Test Debugging

If golden test fails:
1. Navigate to test directory
2. Run `make` to compile and execute
3. Check `obs.txt` (observed) vs `exp.txt` (expected)
4. Examine generated `nexus`, pool files
5. Run nexus manually: `./nexus <command> <args>`
6. Check for compilation warnings/errors

---
*See also: [[TESTS.md]], [[ARCHITECTURE.md]]*
