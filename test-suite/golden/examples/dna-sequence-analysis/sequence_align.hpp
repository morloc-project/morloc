#include <vector>
#include <string>
#include <algorithm>
#include <cctype>

struct Alignment {
    double score;
    std::string seq1;
    std::string seq2;
    std::string alignedSeq1;
    std::string alignedSeq2;
    double identity;
};

// Simple Smith-Waterman local alignment
Alignment alignSmithWaterman(std::string seq1, std::string seq2) {
    int m = seq1.length();
    int n = seq2.length();

    // Scoring scheme
    const int match = 2;
    const int mismatch = -1;
    const int gap = -1;

    // Initialize matrix
    std::vector<std::vector<int>> H(m + 1, std::vector<int>(n + 1, 0));

    int maxScore = 0;
    int maxI = 0, maxJ = 0;

    // Fill matrix
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int matchScore = (seq1[i-1] == seq2[j-1]) ? match : mismatch;
            int score = std::max({
                0,
                H[i-1][j-1] + matchScore,
                H[i-1][j] + gap,
                H[i][j-1] + gap
            });
            H[i][j] = score;

            if (score > maxScore) {
                maxScore = score;
                maxI = i;
                maxJ = j;
            }
        }
    }

    // Traceback (simplified - just return simple alignment)
    std::string aligned1 = seq1.substr(0, std::min(m, n));
    std::string aligned2 = seq2.substr(0, std::min(m, n));

    // Calculate identity
    int matches = 0;
    int len = std::min(aligned1.length(), aligned2.length());
    for (int i = 0; i < len; i++) {
        if (aligned1[i] == aligned2[i]) matches++;
    }
    double identity = (len > 0) ? static_cast<double>(matches) / len : 0.0;

    Alignment result;
    result.score = maxScore;
    result.seq1 = seq1;
    result.seq2 = seq2;
    result.alignedSeq1 = aligned1;
    result.alignedSeq2 = aligned2;
    result.identity = identity;

    return result;
}

// Find all k-mers in a sequence
std::vector<std::string> findKmers(std::string seq, int k) {
    std::vector<std::string> kmers;

    for (size_t i = 0; i + k <= seq.length(); i++) {
        kmers.push_back(seq.substr(i, k));
    }

    return kmers;
}

// Count occurrences of a k-mer in sequence
int countKmer(std::string seq, std::string kmer) {
    int count = 0;
    size_t pos = 0;

    while ((pos = seq.find(kmer, pos)) != std::string::npos) {
        count++;
        pos++;
    }

    return count;
}

// Validate DNA sequence (only A, T, G, C)
bool validateSequence(std::string seq) {
    for (char c : seq) {
        char upper = std::toupper(c);
        if (upper != 'A' && upper != 'T' && upper != 'G' && upper != 'C') {
            return false;
        }
    }
    return true;
}
