# Claude-Test-9: Monte Carlo Option Pricer

## Overview
Demonstrates quantitative finance workflows with C++ for performance-critical Monte Carlo simulation, R for statistical analysis, and Python for I/O.

## Features Tested
- **Numerical computation**: Monte Carlo simulation with Geometric Brownian Motion
- **2D arrays**: `[[Real]]` for price paths
- **Records with reals**: MarketParams, SimulationResult, PriceDistribution
- **Tuples in records**: `confidenceInterval :: (Real, Real)`
- **Map with lambdas**: Extract final prices from paths
- **Helper functions**: `last` to get final element
- **Deterministic random numbers**: Seeded RNG for reproducible results

## Architecture

### Python (`market_io.py`)
- `createMarketParams(...)`: Build market parameter record
- `formatResults(price, stdError)`: Format output string

### C++ (`monte_carlo.hpp`)
- `simulatePaths(params, nPaths, nSteps)`: Generate stock price paths using GBM
- `priceCallOption(params, paths)`: Calculate discounted expected payoff
- `calculateStdError(values)`: Compute standard error

### R (`finance_stats.R`)
- `analyzeDistribution(prices)`: Comprehensive distribution analysis
- `confidenceInterval(values, alpha)`: Calculate confidence intervals
- `percentiles(values, probs)`: Compute percentiles

## Mathematical Background

**Geometric Brownian Motion:**
```
dS = μS dt + σS dW
```

Where:
- S = stock price
- μ = drift (risk-free rate in risk-neutral measure)
- σ = volatility
- dW = Wiener process increment

**Option Pricing:**
```
Call Price = e^(-rT) * E[max(S_T - K, 0)]
```

## Data Flow

1. **priceOption**: Create params → C++ simulates paths → C++ prices option
2. **analyzeDistribution**: Create params → C++ simulates → Extract final prices → R analyzes
3. **generatePaths**: Create params → C++ simulates → Return raw paths

## Example Usage

```bash
# Price option with spot=100, strike=105, vol=0.2, 1000 paths
./nexus price --spot 100 --strike 105 --volatility 0.2 --paths 1000

# Analyze price distribution
./nexus analyze-dist --spot 100 --paths 5000

# Generate 20 paths with 100 steps
./nexus paths --paths 20 --steps 100
```

## What This Tests

1. **Large numeric arrays**: Thousands of price paths
2. **2D array serialization**: `[[Real]]`
3. **Records with multiple real fields**
4. **Tuples in records**: `(Real, Real)` for confidence intervals
5. **Statistical functions** in R
6. **Performance-critical C++ code**
7. **Map with lambda**: `map (\path -> last path) paths`
8. **Deterministic randomness**: Seed 42 for reproducible tests

## Default Parameters

- Spot price: $100
- Strike price: $105
- Volatility: 20% (0.2)
- Risk-free rate: 5% (0.05)
- Time to maturity: 1 year
- Paths: 1000
- Steps: 50
