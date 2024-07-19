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

appender_sqs <- function(bid_name, sqs_url,
                         access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
                         secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                         session_token = Sys.getenv("AWS_SESSION_TOKEN")) {

  logger::fail_on_missing_package("paws")
  force(bid_name)
  force(sqs_url)
  force(access_key_id)
  force(secret_access_key)
  force(session_token)

  aws_credentials <- list(
    creds = list(
      access_key_id = access_key_id,
      secret_access_key = secret_access_key,
      session_token = session_token
    ))

  sqs_client <- paws::sqs(region="us-west-2",
                          credentials = aws_credentials)

  structure(
    function(lines) {
      for (line in lines) {
        sqs_client$send_message(
          QueueUrl = sqs_url,
          MessageBody = line,
          MessageAttributes = setNames(
            list(
              list(
                DataType = "String",
                StringValue = bid_name
              )
            ),
            "bid_name"
          )
        )
      }
    },
    generator = deparse(match.call())
  )
}


get_computing_backend <- function() {
  x <- Sys.getenv("COMPUTE_BACKEND") 
  if (x == "AWS") {
    "aws"
  } else {
    "local"
  }
}
