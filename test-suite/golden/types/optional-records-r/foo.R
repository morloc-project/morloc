makePerson <- function(name, age) {
    list(name = name, age = age)
}

getName <- function(p) {
    p$name
}

getAge <- function(p) {
    p$age
}

toNull <- function(x) {
    return(x)
}

findPerson <- function(name, people) {
    for (p in people) {
        if (p$name == name) return(p)
    }
    return(NULL)
}
