#  parseRecord :: JsonObj -> (JsonObj, Sequence)
def parseRecord(jsonObj):
    sequence = jsonObj["GBSeq_sequence"].upper() 
    del jsonObj["GBSeq_sequence"]
    return (jsonObj, sequence)

#  labelRef :: Map Accession Clade -> JsonObj -> (JsonObj, Clade)
def labelRef(cladeMap, jsonObj):
    accession = jsonObj["GBSeq_primary-accession"]
    if accession in cladeMap:
        return (jsonObj, cladeMap[accession])
    else:
        return (jsonObj, "")

# setLeafNames :: (JsonObj, Clade) -> Str
def setLeafNames(meta):
    (jsonObj, clade) = meta
    return (clade + "|" + jsonObj["GBSeq_primary-accession"])
