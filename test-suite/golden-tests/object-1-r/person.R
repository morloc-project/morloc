# OK, this isn't really a proper class constructor and eventually I will need
# to deal with the wonkey variation in field accessors, but for now this will
# allow testing of the passing to the right constructor.
personObj <- function(name, info){
  list(name=name, info=info)
}
