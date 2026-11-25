#include <vector>
#include <string>
#include <map>
#include <algorithm>
#include <cmath>

struct WordFreq {
    std::string word;
    int count;
};

// Count word frequencies and return sorted list
std::vector<WordFreq> countWords(std::vector<std::string> tokens) {
    std::map<std::string, int> wordCounts;

    // Count occurrences
    for (const auto& token : tokens) {
        wordCounts[token]++;
    }

    // Convert to vector and sort by count (descending)
    std::vector<WordFreq> result;
    for (const auto& entry : wordCounts) {
        result.push_back({entry.first, entry.second});
    }

    std::sort(result.begin(), result.end(),
              [](const auto& a, const auto& b) { return a.count > b.count; });

    return result;
}

// Generate n-grams from tokens
std::vector<std::string> findNgrams(std::vector<std::string> tokens, int n) {
    std::vector<std::string> ngrams;

    for (size_t i = 0; i + n <= tokens.size(); i++) {
        std::string ngram;
        for (int j = 0; j < n; j++) {
            if (j > 0) ngram += " ";
            ngram += tokens[i + j];
        }
        ngrams.push_back(ngram);
    }

    return ngrams;
}

// Calculate simple TF-IDF score for a word in a document
double calculateTfidf(std::string word, std::vector<std::string> tokens) {
    // Term frequency
    int termCount = 0;
    for (const auto& token : tokens) {
        if (token == word) termCount++;
    }

    if (termCount == 0) return 0.0;

    double tf = static_cast<double>(termCount) / tokens.size();

    // Simple IDF (assuming single document, so just use log of doc length)
    double idf = std::log(static_cast<double>(tokens.size()) / (1.0 + termCount));

    return tf * idf;
}
