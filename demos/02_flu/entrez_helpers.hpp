#ifndef __morloc_flucase_entrez_helpers_hpp__
#define __morloc_flucase_entrez_helpers_hpp__

#include <tuple>
#include <string>
#include <nlohmann/json.hpp>

using json = nlohmann::ordered_json;

std::string setLeafName(std::tuple<json, std::string> meta) {
    json jsonObj;
    std::string clade;
    std::tie(jsonObj, clade) = meta;
    
    return clade + "|" + jsonObj["GBSeq_primary-accession"].get<std::string>() + "|" + jsonObj["GBSeq_length"].get<std::string>();
}

#endif
