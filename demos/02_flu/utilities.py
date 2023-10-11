import sys

#  parseRecord :: Str -> (Meta, Sequence)
def parseRecord(xmlstr):
    # STUB
    return (dict(accession = "x24601", clade=""), "GATTACA")

#  labelRef :: Map Accession Clade -> Meta -> Meta
def labelRef(cladeMap, meta):
    if meta["accession"] in cladeMap:
        meta["clade"] = cladeMap[meta["accession"]]
    else:
        meta["clade"] = ""
    return meta
