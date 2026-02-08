#include <vector>
#include <cmath>
#include <random>

struct MarketParams {
    double spot;
    double strike;
    double volatility;
    double riskFreeRate;
    double timeToMaturity;
};

struct PriceDistribution {
    double mean;
    double median;
    double stddev;
    double percentile5;
    double percentile95;
    double ciLower;
    double ciUpper;
};

// Simulate stock price paths using Geometric Brownian Motion
std::vector<std::vector<double>> simulatePaths(MarketParams params, int nPaths, int nSteps) {
    std::mt19937 gen(42);  // Deterministic seed
    std::normal_distribution<> dist(0.0, 1.0);

    double dt = params.timeToMaturity / nSteps;
    double drift = (params.riskFreeRate - 0.5 * params.volatility * params.volatility) * dt;
    double diffusion = params.volatility * std::sqrt(dt);

    std::vector<std::vector<double>> paths(nPaths, std::vector<double>(nSteps + 1));

    for (int i = 0; i < nPaths; i++) {
        paths[i][0] = params.spot;

        for (int j = 1; j <= nSteps; j++) {
            double z = dist(gen);
            double S_prev = paths[i][j-1];
            paths[i][j] = S_prev * std::exp(drift + diffusion * z);
        }
    }

    return paths;
}

// Price a European call option using simulated paths
double priceCallOption(MarketParams params, std::vector<std::vector<double>> paths) {
    int nPaths = paths.size();
    double payoffSum = 0.0;

    for (int i = 0; i < nPaths; i++) {
        double finalPrice = paths[i].back();
        double payoff = std::max(finalPrice - params.strike, 0.0);
        payoffSum += payoff;
    }

    double avgPayoff = payoffSum / nPaths;
    double discountedPrice = avgPayoff * std::exp(-params.riskFreeRate * params.timeToMaturity);

    return discountedPrice;
}

// Calculate standard error of a sample
double calculateStdError(std::vector<double> values) {
    int n = values.size();
    if (n == 0) return 0.0;

    double mean = 0.0;
    for (double v : values) mean += v;
    mean /= n;

    double variance = 0.0;
    for (double v : values) {
        double diff = v - mean;
        variance += diff * diff;
    }
    variance /= (n - 1);

    return std::sqrt(variance / n);
}
