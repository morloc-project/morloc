def _morloc_foreign_call(cmds, args):
    arg_filenames = []
    for (i, arg) in enumerate(args):
        temp = tempfile.NamedTemporaryFile(prefix="morloc_py_", delete=False)
        with open(temp.name, "w") as fh:
            print(arg, file=fh)
            arg_filenames.append(temp.name)
    try:
        sysObj = subprocess.run(
            cmds + arg_filenames,
            stdout=subprocess.PIPE,
            check=True
        )
    except subprocess.CalledProcessError as e:
        raise MorlocForeignCallError(f"python foreign call error: {str(e)}")
    finally:
        for arg_filename in arg_filenames:
            try:
                os.unlink(arg_filename)
            except:
                pass

    return(sysObj.stdout.decode("ascii"))
