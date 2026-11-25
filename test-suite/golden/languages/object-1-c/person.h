#ifndef __PERSON_H__
#define __PERSON_H__

#include <vector>
#include <tuple>
#include <utility>

template <class T>
class PersonObj{
    public:
        std::string name;
        T info; 

        PersonObj(std::string name_i, T info_i){
           name = name_i; 
           info = info_i; 
        }

        PersonObj(T info_i){
           name = "anonymous"; 
           info = info_i; 
        }

        PersonObj(){ }

        std::string initials(){
            std::string n = "";
            bool in = false;
            for(size_t i = 0; i < name.size(); i++){
                if (in && name[i] == ' ') {
                    in = false;
                }
                else if (!in && name[i] != ' ') {
                    n.push_back(name[i]);
                    in = true;
                }
            }
            return n;
        }
};

#endif
