#include <vector>
#include <cmath>
#include <algorithm>

// Simple box blur filter
std::vector<std::vector<double>> blurFilter(std::vector<std::vector<double>> pixels) {
    int height = pixels.size();
    if (height == 0) return pixels;
    int width = pixels[0].size();

    std::vector<std::vector<double>> result(height, std::vector<double>(width));

    // Simple 3x3 box blur
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            double sum = 0;
            int count = 0;

            for (int di = -1; di <= 1; di++) {
                for (int dj = -1; dj <= 1; dj++) {
                    int ni = i + di;
                    int nj = j + dj;
                    if (ni >= 0 && ni < height && nj >= 0 && nj < width) {
                        sum += pixels[ni][nj];
                        count++;
                    }
                }
            }
            result[i][j] = sum / count;
        }
    }
    return result;
}

// Simple edge detection (Sobel-like)
std::vector<std::vector<double>> edgeDetect(std::vector<std::vector<double>> pixels) {
    int height = pixels.size();
    if (height == 0) return pixels;
    int width = pixels[0].size();

    std::vector<std::vector<double>> result(height, std::vector<double>(width));

    for (int i = 1; i < height - 1; i++) {
        for (int j = 1; j < width - 1; j++) {
            // Horizontal gradient
            double gx = pixels[i-1][j+1] + 2*pixels[i][j+1] + pixels[i+1][j+1]
                      - pixels[i-1][j-1] - 2*pixels[i][j-1] - pixels[i+1][j-1];

            // Vertical gradient
            double gy = pixels[i+1][j-1] + 2*pixels[i+1][j] + pixels[i+1][j+1]
                      - pixels[i-1][j-1] - 2*pixels[i-1][j] - pixels[i-1][j+1];

            result[i][j] = std::sqrt(gx*gx + gy*gy);
        }
    }
    return result;
}

// Get image metadata
struct ImageMetadata {
    int width;
    int height;
    int channels;
};

ImageMetadata getMetadata(std::vector<std::vector<double>> pixels) {
    ImageMetadata meta;
    meta.height = pixels.size();
    meta.width = (meta.height > 0) ? pixels[0].size() : 0;
    meta.channels = 1;  // Grayscale
    return meta;
}
