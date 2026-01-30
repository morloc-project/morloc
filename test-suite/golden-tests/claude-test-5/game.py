import time
import os

def drawGame(board):
    """Draw the game board and return it (for composition)"""
    # Clear screen (works on Unix-like systems)
    print("\033[2J\033[H", end="")  # ANSI escape codes

    # Draw board
    for row in board:
        print(''.join(['█' if cell else '·' for cell in row]))
    print()  # Extra newline

    return board

def doSleep(board, duration):
    """Sleep for duration seconds and return board (for composition)"""
    time.sleep(duration)
    return board

def gameLoop(initBoard, updateFn, sleepTime, steps):
    """Main game loop - applies draw, sleep, update for N steps"""
    board = initBoard

    for i in range(steps):
        # Draw current state
        print("\033[2J\033[H", end="")
        for row in board:
            print(''.join(['█' if cell else '·' for cell in row]))
        print(f"Step: {i+1}/{steps}")

        # Sleep
        time.sleep(sleepTime)

        # Update
        board = updateFn(board)

    # Draw final state
    print("\033[2J\033[H", end="")
    for row in board:
        print(''.join(['█' if cell else '·' for cell in row]))
    print(f"Step: {steps}/{steps} (Final)")

    return board
