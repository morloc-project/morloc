import sys

def procId():
    print("PID_QUERY")
    sys.stdout.flush()
    return 1234

def envHome():
    print("ENV_QUERY")
    sys.stdout.flush()
    return "/home/user"

def calMonth(year, month):
    months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    idx = (month - 1) % 12
    return months[idx] + "-" + str(year)
