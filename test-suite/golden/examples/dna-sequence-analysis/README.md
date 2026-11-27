# Claude-Test-10: DNA Sequence Analyzer

## Overview
Demonstrates bioinformatics workflows with Python for sequence I/O, C++ for alignment algorithms, and R for statistical analysis of nucleotide sequences.

## Features Tested
- **String algorithms**: DNA sequence analysis, alignment, motif finding
- **Record field access**: `.nucleotides`, `.seqId` accessor syntax
- **Lists of strings**: K-mer lists
- **Records with strings**: Sequence, Alignment, KmerProfile records
- **String validation**: C++ validates DNA sequences
- **Cross-language biology pipelines**: Python → C++ → R

## Architecture

### Python (`sequence_io.py`)
- `getSampleSequence(id)`: Retrieve sample DNA sequences
- `createSequence(id, desc, nucleotides)`: Create Sequence record
- `formatAlignment(alignment)`: Format alignment results

### C++ (`sequence_align.hpp`)
- `alignSmithWaterman(seq1, seq2)`: Local sequence alignment
- `findKmers(seq, k)`: Extract all k-mers of length k
- `countKmer(seq, kmer)`: Count k-mer occurrences
- `validateSequence(seq)`: Validate DNA alphabet (ATGC only)

### R (`sequence_stats.R`)
- `calculateGC(seq)`: Compute GC content percentage
- `sequenceComplexity(seq)`: Measure sequence complexity (unique k-mers ratio)
- `analyzeComposition(seq)`: Full composition analysis

## Biological Background

**GC Content:** Percentage of guanine (G) and cytosine (C) bases. Important for:
- Gene prediction
- Species classification
- Thermal stability

**Smith-Waterman Alignment:** Finds optimal local alignment between sequences
- Match: +2
- Mismatch: -1
- Gap: -1

**K-mers:** Substrings of length k used for:
- Motif discovery
- Sequence assembly
- Pattern recognition

## Data Flow

1. **analyzeSequence**: Python gets sequence → R analyzes composition
2. **findMotifs**: Python gets sequence → C++ finds all k-mers
3. **alignSequences**: Python gets 2 sequences → C++ aligns them
4. **gcContent**: Python gets sequence → R calculates GC%

## Sample Sequences

1. **SEQ001**: Sample E. coli sequence (mixed content)
2. **SEQ002**: Sample human sequence (mixed content)
3. **SEQ003**: High GC content (all GC)
4. **SEQ004**: Low complexity (ATAT repeat)
5. **SEQ005**: Random sequence

## Example Usage

```bash
# Analyze sequence composition
./nexus analyze --sample 1

# Find all 3-mers
./nexus motifs --sample 1 --kmer-length 3

# Align sequences 1 and 2
./nexus align --seq1 1 --seq2 2

# Calculate GC content
./nexus gc-content --sample 3
```

## What This Tests

1. **String-heavy operations** across languages
2. **Record field accessors**: `.nucleotides`, `.seqId`
3. **Lists of strings** serialization
4. **Algorithm implementation**: Smith-Waterman in C++
5. **String validation**: DNA alphabet checking
6. **Statistical string analysis** in R
7. **Domain-specific types**: DNA sequences as strings
8. **Pattern matching** and motif finding

## Expected Results

- **GC content (SEQ001)**: ~47.5%
- **GC content (SEQ003)**: 100%
- **GC content (SEQ004)**: 0%
- **Alignment SEQ001 vs SEQ002**: High similarity
- **3-mers in SEQ001**: Multiple overlapping k-mers
