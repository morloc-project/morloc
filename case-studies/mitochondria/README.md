# Find top hits against mitochondrial proteins

This is a Morloc implementation of my bcb660-project repo's code.

The core workflow does the following:

 1. Retrieve all mitochondrial genomes

 2. Based on the NCBI common tree, extract all mitochondria underneath
    a given taxon

 3. extract proteins from the mitochondria based on a pattern

 4. Prepare a blast database from an input fasta file

 5. blast selected mitochondrial proteins against database

 6. map the reference to species name (by fasta header parsing)

 7. merge blast result and ref2species maps, for final table

 8. aggregate the results on species and bitscore

 9. throw away the results

The orthogonal pathways

 1. plot the final result

 2. assert that the references in ref2name and blast result table match 

 3. assert that the bitscore column is positvie integer

 4. assert that the bitscore column is reasonably distributed
