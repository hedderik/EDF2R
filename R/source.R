
readEDF <- function(name, checkConsistency=FALSE) {
    retVal <- NULL
    if (file.exists(name)) {
      retVal <- parseEDF(name);

      ## Convert factors to strings:
      retVal$events$msg <- as.character(retVal$events$msg)
      ## Remove empty events:
      retVal$events <- retVal$events[retVal$events$msg != "",]
    } else {
        warning(paste0("Specified file (",name,") not found."))
    }
    return(retVal)
}
