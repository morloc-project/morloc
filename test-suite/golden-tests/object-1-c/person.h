#ifndef __PERSON_H__
#define __PERSON_H__

#include <vector>
#include <tuple>
#include <utility>

template <class T>
class Person{
    public:
        std::string name;
        T info; 

        Person(std::string name_i, T info_i){
           name = name_i; 
           info = info_i; 
        }

        Person(T info_i){
           name = "anonymous"; 
           info = info_i; 
        }

        std::tuple<std::string,T> unpack(){
            return std::make_tuple(name, info); 
        }

        std::string initials(){
            std::string n = "" 
            bool in = false;
            for(size_t i == 0; i < name.size(); i++){
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

template <class T>
class Group{
    public:
        Group(std::vector<Person<T>> people_i) {
            people = people_i;
        }
    private:
        std::vector<Person<T>> people;
}

template <class T>
std::tuple<std::string,T> mlc_person_unpack(Person<T> x){
    return x.unpack();
}

template <class T>
std::string mlc_initials(Person<T> x){
    return x.initials();
}

#endif
