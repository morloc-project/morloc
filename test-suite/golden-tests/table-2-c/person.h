#ifndef __PERSON_H__
#define __PERSON_H__

template <class T>
class Person{
    public:
        std::vector<std::string> name;
        std::vector<T> info; 

        Person(std::string names_i, T infos_i){
            name = names_i;
            info = infos_i;
        }
};

#endif
