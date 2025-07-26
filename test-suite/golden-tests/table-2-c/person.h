#ifndef __PERSON_H__
#define __PERSON_H__

template <class T>
class PersonYay{
    public:
        std::vector<std::string> name;
        std::vector<T> info; 

        PersonYay(std::vector<std::string> names_i, std::vector<T> infos_i){
            name = names_i;
            info = infos_i;
        }
        PersonYay(){
            name = {};
            info = {};
        }
};

template <class T>
T id(T x){
    return x;
}

#endif
