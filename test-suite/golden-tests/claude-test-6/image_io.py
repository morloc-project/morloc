import random

def createTestImage(width, height):
    """Create a test image with random grayscale values"""
    random.seed(42)  # Deterministic for testing
    # Create 2D array of random values between 0 and 255
    pixels = [[random.uniform(0, 255) for _ in range(width)] for _ in range(height)]
    return pixels

def pixelsToDict(pixels, metadata, stats):
    """Combine pixels, metadata, and stats into a result dictionary"""
    return {
        "pixels": pixels,
        "metadata": metadata,
        "stats": stats
    }
