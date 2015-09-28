
readEDF <- function(name) {
    retVal <- NULL
    if (file.exists(name)) {
        samples <- readEDFsamples(name);
        if (nrow(samples) > 0) {
            events <- readEDFevents(name);
            ## Convert factors to strings:
            events$msg <- as.character(events$msg)
            ## Remove empty events:
            events <- events[events$msg != "",]
            retVal <- list(samples=samples,events=events)
        } else {
            warning(paste0("Specified file (",name,") is either empty or not a valid EDF "))
        }            
    } else {
        warning(paste0("Specified file (",name,") not found."))
    }
    return(retVal)
}
