#ifndef __FASTAIO_HPP__
#define __FASTAIO_HPP__

#include <vector>
#include <tuple>
#include <string>
#include <iostream>
#include <fstream>

std::vector<std::tuple<std::string,std::string>> readFasta(std::string filename){
    std::vector<std::string> seq;
    std::vector<std::string> def;
    std::string line;
    std::ifstream fastaFile(filename);
    bool begin = false;
    if (fastaFile.is_open()) {
        while (std::getline(fastaFile,line)) {
            if (line.size() == 0) {
                continue;
            }
            if (line[0] == '>') {
                def.push_back(line.substr(1,line.size()-1));
                begin = true;
            } else {
                if (begin){
                    seq.push_back(line.substr(0, line.size()));
                } else {
                    seq[seq.size()-1].append(line);
                }
                begin = false;
            }
        }
        fastaFile.close();
    }
    std::vector<std::tuple<std::string,std::string>> out;
    for (size_t i = 0; i < seq.size(); i++){
        out.push_back(std::make_tuple(def[i], seq[i]));
    }
    return out;
}

std::string writeFasta(std::vector<std::tuple<std::string,std::string>> bioseq){
    std::ostringstream fasta;
    for(size_t i = 0; i < bioseq.size(); i++){
        fasta << ">" << std::get<0>(bioseq[i]) << '\n';
        fasta << std::get<1>(bioseq[i]) << '\n';
    }
    return(fasta.str());
}

#endif
