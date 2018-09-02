CDF - Common Data Format

The STS can be partioned into 4 main groups (GRIM model).

 * General (G) - A structured type that can often map directly to the machine
    type of specific languages, though this may be ambiguous when a given type
    of data can map to multiple language-specific types. G maps uniquely to
    a CDF. This is the lowest level type that generalizes across languages.

    Examples:
      * Integer
      * ProteinSeq, ProteinStructure
      * Table {col1 : Integer, col2 : String}

 * Resources (R) - Types that describe resources. This may be data outside of
    the CDF. It may be databases.

    Examples:
      * URL, Filename, Folder
      * SQLDB
      * FastaFile, FastaFile UniProt, FastaFile GeneBank
      * Markdown, Markdown GitHub
      * Music, MP3

 * Ideal (I) - A high-level idea that often does not contain enough information to
    unambiguously map to a machine type.

    Examples:
      * Protein
      * Shoe, House
      * Tree
      * Symbol
      * Music

 * Machine (M) - The data type within a specific language (e.g. AAStringSet or
   double). The Machine type will closely follow type format for the specific
   language. Part of building the binding for a new language is mapping
   `M<-->G`.

    Examples:
      * (C) int, double, float, bool
      * (R) integer, numeric, logical
      * (R) AAStringSet


Functions may have roles. There are a few roles that are builtin to the compiler:

 * load - R -> ?G | R -> ?M
   
   Load an external resource into a general form (i.e. conversion to CDF) or
   a machine type. Loading can fail.

 * write - G -> ?R | M -> ?R

   Write to the given resource (generally not to the CDF). Writing can fail.
   
   Examples:
     * upload data to a database
     * write a file
     * send an email

 * convert - M -> M | M -> G | G -> M

   A reversible conversion from x to y. 

   Examples:
     * (R) data.frame -> data.table
     * (a, b, c) -> (b, c, a)
     * Foot -> Meter

 * downcast - M -> M | G -> G | G -> M | R -> R

   Convert one type to another descendent type, possibly irreversible

   Examples:
     * FullName -> GivenName  -- both are names
     * Double -> Integer      -- both are numbers
     * Integer -> String

 * upcast - M -> ?M | G -> ?G | G -> ?M | R -> ?R

   Attempts to upcast, this may fail

   Examples:
     * String -> ?Integer

 * specialize - M -> M | M -> G | G -> M

   Like convert, but may not be reversible, however the data should be the
   same, in some sense. Formally, they share a common ancestor.

   Examples:
     * (a, b, c) -> (a, b)    -- both are tuples

 * transform - M -> M | M -> G | G -> M 
    
   This role is used when there is one natural transformation between two
   types. But no concept of specialization. The things on both sides are
   different.

   Examples:
     * Person -> Age
     * Graph -> Size
     * sorting given a comparator
