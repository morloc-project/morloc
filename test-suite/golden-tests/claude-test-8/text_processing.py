import re

SAMPLE_TEXTS = {
    1: "The quick brown fox jumps over the lazy dog. The dog was very lazy indeed.",
    2: "This is a wonderful day! I feel great and everything is amazing.",
    3: "The weather is terrible today. It's cold, rainy, and miserable.",
    4: "Machine learning and artificial intelligence are transforming technology.",
    5: "The cat sat on the mat. The mat was soft and comfortable for the cat."
}

def getSampleText(sampleId):
    """Get a sample text by ID"""
    return SAMPLE_TEXTS.get(sampleId, SAMPLE_TEXTS[1])

def tokenizeText(text):
    """Tokenize text into words (lowercase, alphanumeric only)"""
    # Convert to lowercase and split on non-alphanumeric
    words = re.findall(r'\b[a-z]+\b', text.lower())
    return words

def createDocument(docId, text):
    """Create a Document record from text"""
    tokens = tokenizeText(text)
    return {
        "docId": docId,
        "text": text,
        "tokens": tokens,
        "wordCount": len(tokens)
    }
