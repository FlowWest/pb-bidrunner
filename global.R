injection_source <- function(file, is_remote, overwrite_with = list()) {
  env <- new.env()
  
  sys.source(file, envir = env)
  
  if (is_remote) {
    
    for (name in names(overwrite_with)) {
      env[[name]] <- overwrite_with[[name]]
    }
  }
  
  list2env(as.list(env, all.names = TRUE), envir = .GlobalEnv)
}
