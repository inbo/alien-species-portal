
#' Setup connection to S3 bucket based on AWS file
#' 
#' INFO: https://www.gormanalysis.com/blog/connecting-to-aws-s3-with-r/
#' @param awsFile path to AWS file
#' @param inboUserName set NULL if not a inbo user, for inbo user, provide your user name firstName_lastName
#' @return no return value, ENV variables are set correctly
#' 
#' @author mvarewyck
#' @importFrom aws.signature read_credentials
#' @export
setupS3 <- function(awsFile = "~/.aws/credentials", inboUserName = NULL){
  
  #"sander_devisscher"
  profile <- if (is.null(inboUserName)) 
      "inbo-alien" else 
      sprintf("inbo-uat-%s", gsub("_", "-", inboUserName))
  
  # for inbo user on their PC
  userProfile <- Sys.getenv("USERPROFILE", unset = NA)

  if (!is.na(userProfile))
    awsFile <- normalizePath(file.path(userProfile, ".aws", "credentials"))
  

  # credentials are in ~/.aws/credentials OR manually copy/paste OR using aws.signature::
  x <- aws.signature::read_credentials(file = awsFile)[[profile]]

  Sys.setenv(
    AWS_DEFAULT_REGION = eval(parse(text = config::get("credentials", file = system.file("config.yml", package = "alienSpecies"))$region)),
    AWS_ACCESS_KEY_ID = x$AWS_ACCESS_KEY_ID, 
    AWS_SECRET_ACCESS_KEY = x$AWS_SECRET_ACCESS_KEY,
    AWS_SESSION_TOKEN = x$AWS_SESSION_TOKEN 
  )
}


#' Download all files from the S3 bucket for local use
#' @param dataDir path to folder where to save all the files from S3
#' @param files character vector, specific files to be downloaded;
#' if NULL all files in the bucket will be downloaded
#' @inheritParams readS3
#' 
#' @return TRUE, if all downloads succeeded
#' 
#' @author mvarewyck
#' @importFrom aws.s3 get_bucket_df save_object
#' @export
downloadS3 <- function(
    dataDir = file.path("~/git/alien-species-portal/dataS3"),
    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
    files = NULL) {
  
  if (!dir.exists(dataDir))
    dir.create(dataDir)
  
  # List all available files on the S3 bucket
  if (is.null(files))
    files <- aws.s3::get_bucket_df(bucket = bucket)$Key
  
  for (iFile in files)
    aws.s3::save_object(object = iFile, bucket = bucket, file = file.path(dataDir, iFile))
  
  return(TRUE)
  
}


#' Quick check for valid user credentials to make connection with S3 bucket
#' @return no return value
#' 
#' @author mvarewyck
#' @importFrom aws.ec2metadata is_ec2 metadata
#' @export
checkS3 <- function() {
  
  if (is_ec2()) {
    # Try to retrieve metadata from the instance
    metadata$instance_id()
  } else {
    credentials <- Sys.getenv(c("AWS_DEFAULT_REGION", "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN"))
    if (any(credentials == ""))
      stop("Please specify 'Sys.setenv()' for ", 
           paste(names(credentials)[which(credentials == "")], collapse = ", "))
  }
  
}


#' Test all data files before launching the app
#' @return no return value; if any of the tests failed, stop with error message
#' 
#' @author mvarewyck
#' @importFrom testthat test_file
#' @importFrom methods is
#' @export
testS3 <- function() {
  
  cat("Test Data in S3 bucket\n")
  testResult <- test_file(system.file("tests/testData/testData.R", package = "alienSpecies"), reporter = "minimal")
  
  # Remove objects loaded globally during tests
  allResults <- ls(envir = .GlobalEnv)
  rm(list = allResults, envir = .GlobalEnv)
  gc()
  
  warningMessage <- NULL
  isWarning <- as.data.frame(testResult)$warning > 0
  for (i in which(isWarning)) {
    toPrint <- as.data.frame(testResult)$test[i]
    tmp <- testResult[[i]]$results
    warningMessage <- paste("<h4>Test:", toPrint, "</h4>Warning message:</br>", 
                            gsub("\n", "</br>", tmp[sapply(tmp, function(x)
                              is(x, "expectation_warning"))][[1]]$message), "</br>")
  }
  
  isFailed <- as.data.frame(testResult)$failed > 0 | (!as.data.frame(testResult)$skipped & !as.data.frame(testResult)$passed > 0)
  if (any(isFailed)) {
    # Message will be in HTML
    errorMessage <- paste("</br>Please check data in S3 bucket:",
                          config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
                          "</br>Following tests in tests/testData.R failed. Please fix in order of occurrence.</br>")
    for (i in which(isFailed)) {
      toPrint <- as.data.frame(testResult)$test[i]
      tmp <- testResult[[i]]$results
      errorMessage <- paste(errorMessage, "<h4>Test:", toPrint, "</h4>Error message:</br>", 
                            gsub("\n", "</br>", tmp[sapply(tmp, function(x)
                              is(x, "expectation_failure") | is(x, "expectation_error"))][[1]]$message), "</br>")
    }
    stop(paste(errorMessage, warningMessage))
    
    
  } else if (!is.null(warningMessage))
    cat(gsub("</br>", "\n", warningMessage)) else 
      cat("Finished successfully\n")
  
}


#' Download files from the S3 bucket
#' @param FUN function, which function should be called to download the data;
#' if .RData file this argument can be empty 
#' @param ... additional arguments to \code{FUN}
#' @param file character, name of the file to be downloaded
#' @param forceDownload boolean, true s3 data will be downloaded to a temporary file and then supplied to \code{FUN}, this apprach
#' is faster than \code{s3read_using}
#' @param bucket character, name of the S3 bucket as specified in the config.yml file;
#' default value is "inbo-exotenportaal-uat-eu-west-1-default"
#' @param envir environment, where to load the data; default is \code{.GlobalEnv}
#' @return depending on the input file
#' \itemize{
#' \item{rdata}{no return value,
#' the R object (data.frame or list) loaded from the \code{file} is assigned to
#' the \code{.GlobalEnv}}
#' \item{other files}{R object (data.frame or list) is returned}
#' }
#' @author mvarewyck
#' @importFrom aws.s3 s3load s3read_using get_object
#' @export


readS3 <- function(FUN = read.csv, ..., file, 
                   bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies")),
                   envir = .GlobalEnv,
                   forceDownload = FALSE) {
  
  if(forceDownload){
    args <- list(...)
    destFile <- file.path(tempdir(), file)
    writeBin(get_object(file, bucket = bucket),   con = destFile)
    args <- c(destFile, args)
    toReturn <- do.call(FUN, args)
    on.exit(unlink(destFile))
    toReturn 
  }else{

  if (tolower(tools::file_ext(file)) == "rdata") {
    
    s3load(bucket = bucket, object = file, envir = envir)
    
  } else {
    
    s3read_using(FUN = FUN, ..., bucket = bucket, object = basename(file))
    
   }
  }
}



#' Write files to the S3 bucket
#' @param dataFiles character vector, path to files to be uploaded
#' @inheritParams readS3
#' @return boolean vector, TRUE for each dataFile that was uploaded correctly
#' 
#' @author mvarewyck
#' @importFrom aws.s3 put_object
#' @importFrom config get
#' @export
writeS3 <- function(dataFiles,
                    bucket = config::get("bucket", file = system.file("config.yml", package = "alienSpecies"))) {
  
  response <- sapply(dataFiles, function(iFile) {
    put_object(file = iFile, object = basename(iFile), 
               bucket = bucket, multipart = TRUE)
  })
  
  response
  
}

