import sys
text = b"batch 1\n" + b"x" * (20 << 20) + b"batch 2\n" + b"batch 3\n"
if sys.argv[1] == "text":
    sys.stdout.buffer.write(text)
else:
    sys.stdout.buffer.write(b"\0" + b"batch 1\n\0" + b"x" * (20 << 20) + b"batch 2\n\0" + b"batch 3\n\0")
