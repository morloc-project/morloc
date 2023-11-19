#  parseRecord :: JsonObj -> (Meta, Sequence)
def parseRecord(jsonObj):
    meta = dict(accession = jsonObj["GBSeq_primary-accession"], clade="")
    sequence = jsonObj["GBSeq_sequence"].upper() 
    return (meta, sequence)

#  labelRef :: Map Accession Clade -> Meta -> Meta
def labelRef(cladeMap, meta):
    if meta["accession"] in cladeMap:
        meta["clade"] = cladeMap[meta["accession"]]
    else:
        meta["clade"] = ""
    return meta
