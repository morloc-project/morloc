import sys
n = int(sys.argv[1])
sys.stdout.write('{"label":"ab","next":' * (n - 1))
sys.stdout.write('{"label":"ab\\u0000ab","next":null}')
sys.stdout.write('}' * (n - 1))
