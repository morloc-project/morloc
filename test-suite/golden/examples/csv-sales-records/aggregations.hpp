#include <vector>
#include <string>
#include <map>
#include <algorithm>

struct SalesRecord {
    std::string product;
    std::string category;
    int quantity;
    double price;
    std::string region;
};

struct CategorySummary {
    std::string category;
    double totalSales;
    int itemCount;
    double avgPrice;
};

// Calculate total revenue from all sales
double calculateRevenue(std::vector<SalesRecord> sales) {
    double total = 0.0;
    for (const auto& sale : sales) {
        total += sale.quantity * sale.price;
    }
    return total;
}

// Group sales by category and compute aggregates
std::vector<CategorySummary> fastGroupBy(std::vector<SalesRecord> sales) {
    std::map<std::string, std::pair<double, int>> categoryData;

    // Aggregate by category
    for (const auto& sale : sales) {
        double revenue = sale.quantity * sale.price;
        categoryData[sale.category].first += revenue;
        categoryData[sale.category].second += 1;
    }

    // Convert to result vector
    std::vector<CategorySummary> results;
    for (const auto& entry : categoryData) {
        CategorySummary summary;
        summary.category = entry.first;
        summary.totalSales = entry.second.first;
        summary.itemCount = entry.second.second;
        summary.avgPrice = entry.second.first / entry.second.second;
        results.push_back(summary);
    }

    return results;
}
