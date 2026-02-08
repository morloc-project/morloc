# Claude-Test-6: Image Filter Pipeline

## Overview

Demonstrates cross-language image processing with Python for I/O, C++ for
computationally intensive filtering, and R for statistical analysis.

## Features Tested

- **Records**: Multiple nested records (ImageMetadata, ColorStats, FilteredResult)
- **Cross-language composition**: Python → C++ → R
- **2D arrays**: Image pixel data as `[[Real]]`
- **Where bindings**: Multi-step computations with intermediate variables
- **Struct/dict mapping**: Records map to different native types per language

## Architecture

### Python (`image_io.py`)
- `createTestImage(width, height)`: Generate random test images
- `pixelsToDict(pixels, metadata, stats)`: Combine results into FilteredResult record

### C++ (`filters.hpp`)
- `blurFilter(pixels)`: Apply 3x3 box blur
- `edgeDetect(pixels)`: Sobel-like edge detection
- `getMetadata(pixels)`: Extract image dimensions

### R (`stats.R`)
- `computeColorStats(pixels)`: Calculate mean and standard deviation per channel

## Data Flow

1. **analyzeImage**: Python creates image → R computes statistics
2. **applyBlur**: Python creates image → C++ blurs → R stats → Python assembles result
3. **detectEdges**: Python creates image → C++ edge detection → R stats → Python assembles result

## Example Usage

```bash
# Analyze 8x8 test image
./nexus analyzeImage 8 8

# Apply blur to 10x10 image
./nexus blur --width 10 --height 10

# Detect edges
./nexus edges --width 12 --height 12
```

## What This Tests

1. **Record serialization** across 3 languages
2. **Nested records** (FilteredResult contains ImageMetadata and ColorStats)
3. **2D array serialization** (pixel matrices)
4. **Let bindings** for multi-step pipelines
5. **Language-specific type mappings** (struct in C++, dict in Python, list in R)
