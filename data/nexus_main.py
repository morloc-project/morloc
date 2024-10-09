def dispatch(cmd, args):
    if cmd in ["-h", "--help", "-?", "?"]:
        usage()
    else:
        command_table[cmd](args)


if __name__ == "__main__":
    if len(sys.argv) == 1:
        usage()
    else:
        cmd = sys.argv[1]
        args = sys.argv[2:]
        dispatch(cmd, args)

