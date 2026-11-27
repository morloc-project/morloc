SAMPLE_SEQUENCES = {
    1: ("SEQ001", "Sample E. coli sequence", "ATGCGATCGATCGATCGATCGTAGCTAGCTAGCTAGCTAG"),
    2: ("SEQ002", "Sample human sequence", "ATGCGATCGTAGCTAGCTAGCTGCATGCTAGCTAGCTAG"),
    3: ("SEQ003", "High GC content", "GCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGCGC"),
    4: ("SEQ004", "Low complexity", "ATATATATATATATATATATATATATATATATAT"),
    5: ("SEQ005", "Random sequence", "ACGTACGTTAGCTAGCTAGCTAGCTACGTACGTACGT")
}

def getSampleSequence(sampleId):
    """Get a sample DNA sequence by ID"""
    if sampleId in SAMPLE_SEQUENCES:
        seqId, desc, nucs = SAMPLE_SEQUENCES[sampleId]
    else:
        seqId, desc, nucs = SAMPLE_SEQUENCES[1]

    return {
        "seqId": seqId,
        "description": desc,
        "nucleotides": nucs,
        "length": len(nucs)
    }

def createSequence(seqId, description, nucleotides):
    """Create a Sequence record"""
    return {
        "seqId": seqId,
        "description": description,
        "nucleotides": nucleotides,
        "length": len(nucleotides)
    }

def formatAlignment(alignment):
    """Format alignment result as a string"""
    lines = [
        f"Alignment Score: {alignment['score']:.2f}",
        f"Identity: {alignment['identity']*100:.1f}%",
        "",
        alignment['alignedSeq1'],
        alignment['alignedSeq2']
    ]
    return "\n".join(lines)
