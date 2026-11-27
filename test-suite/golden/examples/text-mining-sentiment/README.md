# Claude-Test-8: Text Mining Pipeline

## Overview
Demonstrates text processing and NLP workflows with Python for tokenization, C++ for fast string operations, and R for sentiment analysis.

## Features Tested
- **String processing**: Tokenization, word counting, n-grams
- **Lists of strings**: `[Str]` for tokens
- **Tuples**: `(Str, Int)` for word-count pairs
- **Helper functions**: `take`, `reverse` for list manipulation
- **Pattern matching**: Case expressions on lists
- **Records with strings**: Document, WordFreq, Sentiment records
- **Cross-language text analysis**: Python → C++ → R

## Architecture

### Python (`text_processing.py`)
- `getSampleText(id)`: Retrieve sample texts for testing
- `tokenizeText(text)`: Tokenize into lowercase words
- `createDocument(id, text)`: Create Document record

### C++ (`string_ops.hpp`)
- `countWords(tokens)`: Count word frequencies, return sorted list
- `findNgrams(tokens, n)`: Generate n-grams
- `calculateTfidf(word, tokens)`: Simple TF-IDF calculation

### R (`text_stats.R`)
- `computeSentiment(tokens)`: Sentiment analysis with word lists
- `wordDiversity(tokens)`: Type-token ratio

## Data Flow

1. **analyzeText**: Python gets text → tokenizes → creates Document record
2. **findTopWords**: Python tokenizes → C++ counts and sorts → morloc takes top N
3. **computeSentiment**: Python tokenizes → R analyzes sentiment

## Example Usage

```bash
# Analyze sample text 1
./nexus analyze --sample 1

# Find top 10 words from sample 3
./nexus top-words --sample 3 --count 10

# Compute sentiment for sample 2 (positive text)
./nexus sentiment --sample 2
```

## What This Tests

1. **String operations** across languages
2. **Tuples**: `(Str, Int)` for word frequencies
3. **Pattern matching**: Case expressions on lists (`case xs of`)
4. **Helper functions**: `take`, `reverse` implemented in morloc
5. **Lists of tuples** serialization
6. **Records with string fields**
7. **Cross-language text processing** pipelines

## Sample Texts

1. "The quick brown fox..." (neutral)
2. "This is a wonderful day!..." (positive)
3. "The weather is terrible..." (negative)
4. "Machine learning..." (neutral/technical)
5. "The cat sat on the mat..." (neutral)
