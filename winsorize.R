winsorize <- function(x, probs = NULL, cutpoints = NULL , replace = c(cutpoints[1], cutpoints[2]), verbose = TRUE){
  dummy = is.integer(x)
  if (!is.null(probs)){
    stopifnot(is.null(cutpoints))
    stopifnot(length(probs)==2)
    cutpoints <- quantile(x, probs, type = 1, na.rm = TRUE)
  } else if (is.null(cutpoints)){
    l <- quantile(x, c(0.25, 0.50, 0.75), type = 1, na.rm = TRUE) 
    cutpoints <- c(l[2]-5*(l[3]-l[1]), l[2]+5*(l[3]-l[1]))
  } else{
    stopifnot(length(cutpoints)==2)
  }
  if (is.integer(x)) cutpoints <- round(cutpoints)
  bottom <- x < cutpoints[1]
  top <- x > cutpoints[2]
  if (verbose){
    length <- length(x)
    message(paste(100*sum(bottom, na.rm = TRUE)/length,"% observations replaced at the bottom"))
    message(paste(100*sum(top, na.rm = TRUE)/length,"% observations replaced at the top"))
    
    #rm: added this to save
    #cat(paste0('\n',sum(bottom, na.rm = TRUE),' \t',sum(top, na.rm = TRUE)),file = filename, append = TRUE)             
    
  }
  x[bottom] <- replace[1]
  x[top] <- replace[2]
  if (dummy){
    x <- as.integer(x)
  }
  x
}
