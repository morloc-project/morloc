def createMarketParams(spot, strike, volatility, riskFreeRate, timeToMaturity):
    """Create market parameters record"""
    return {
        "spot": spot,
        "strike": strike,
        "volatility": volatility,
        "riskFreeRate": riskFreeRate,
        "timeToMaturity": timeToMaturity
    }

def formatResults(price, stdError):
    """Format pricing results as a string"""
    return f"Option Price: ${price:.4f} ± ${stdError:.4f}"

def extractFinalPrices(paths):
    """Extract the final price from each path"""
    return [path[-1] for path in paths]
