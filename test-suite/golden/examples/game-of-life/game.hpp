#include <vector>
#include <random>

// Initialize a random Game of Life board
std::vector<std::vector<int>> initGame(int seed, int width, int height) {
    std::mt19937 gen(seed);
    std::uniform_int_distribution<> dis(0, 1);

    std::vector<std::vector<int>> board(height, std::vector<int>(width));
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            board[i][j] = dis(gen);
        }
    }
    return board;
}

// Apply one step of Conway's Game of Life
std::vector<std::vector<int>> updateGame(std::vector<std::vector<int>> board) {
    int height = board.size();
    if (height == 0) return board;
    int width = board[0].size();

    std::vector<std::vector<int>> newBoard(height, std::vector<int>(width));

    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            int neighbors = 0;

            // Count neighbors (wrap around edges)
            for (int di = -1; di <= 1; di++) {
                for (int dj = -1; dj <= 1; dj++) {
                    if (di == 0 && dj == 0) continue;
                    int ni = (i + di + height) % height;
                    int nj = (j + dj + width) % width;
                    neighbors += board[ni][nj];
                }
            }

            // Apply Game of Life rules
            if (board[i][j] == 1) {
                // Live cell survives with 2-3 neighbors
                newBoard[i][j] = (neighbors == 2 || neighbors == 3) ? 1 : 0;
            } else {
                // Dead cell becomes alive with exactly 3 neighbors
                newBoard[i][j] = (neighbors == 3) ? 1 : 0;
            }
        }
    }
    return newBoard;
}
