.make_temporary_file <- function(x) {
  temp_filename <- tempfile(pattern = "morloc_r_", tmpdir = "/tmp", fileext = "")
  writeLines(x, temp_filename)
  return(temp_filename)
}

.morloc_foreign_call <- function(cmd, cmd_args, args, .pool, .name){
  # write the input arguments to temporary files
  arg_files <- lapply(args, .make_temporary_file)

  # try to run the foreign pool, passing the serialized arguments as tmp files
  result <- .morloc_try(f=system2, args=list(cmd, args=c(cmd_args, arg_files), stdout=TRUE), .pool=.pool, .name=.name)

  # clean up temp files
  on.exit(unlink(arg_files))

  return(result)
}
