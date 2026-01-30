# Claude-Test-7: CSV Data Analysis Dashboard

## Overview
Demonstrates data science workflows with Python for I/O, R for statistics, and C++ for fast aggregations. Tests records, lists of records, and field accessors.

## Features Tested
- **Records**: SalesRecord, Summary, CategorySummary
- **Lists of records**: `[SalesRecord]`, `[CategorySummary]`
- **Field accessors**: `.price`, `.category` to extract record fields
- **Map with lambdas**: `map (\s -> .price s) sales`
- **Cross-language data pipeline**: Python → C++ → Python
- **String generation**: Formatted reports from Python

## Architecture

### Python (`data_io.py`)
- `generateSalesData(n)`: Create random sales records (deterministic seed)
- `formatReport(summaries)`: Format category summaries as text

### C++ (`aggregations.hpp`)
- `calculateRevenue(sales)`: Fast sum of all revenues
- `fastGroupBy(sales)`: Group by category and aggregate

### R (`statistics.R`)
- `computeSummary(values)`: Compute count, mean, stddev, min, max
- `correlationAnalysis(x, y)`: Pearson correlation coefficient

## Data Flow

1. **analyzeSales**: Python generates data → C++ calculates total revenue
2. **summarizeStats**: Python generates → Extract prices with map → R computes statistics
3. **groupByCategory**: Python generates → C++ groups and aggregates → Python formats report

## Example Usage

```bash
# Calculate total revenue from 100 sales
./nexus analyze --records 100

# Get summary statistics
./nexus summary --records 50

# Group by category and format report
./nexus group-by --records 200
```

## What This Tests

1. **Records with multiple fields** (strings, ints, reals)
2. **Lists of records** serialization
3. **Field accessor syntax** (`.price`, `.category`)
4. **Map with lambda and field access**: `map (\s -> .price s) sales`
5. **Cross-language aggregation** patterns
6. **String formatting** and text generation
7. **Deterministic random data** for reproducible tests
