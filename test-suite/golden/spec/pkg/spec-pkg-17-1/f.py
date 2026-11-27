import subprocess


def runTool(x):
    try:
        return subprocess.run(["fakebin"], capture_output=True, text=True).stdout.strip()
    except FileNotFoundError:
        return "missing"
