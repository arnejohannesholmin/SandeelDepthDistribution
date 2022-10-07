
# Pad numbers by zeros to obtain equal number of chcaracters:
zeropad<-function(x, n = NULL){
	
	numberOfCharactersX <- nchar(x)
	if(!length(n)) {
		n <- max(numberOfCharactersX)
	}
	
	
	if(!all(numberOfCharactersX == numberOfCharactersX[1])){
		formatC(x, width = n, format = "d", flag = "0")
	}
	else {
		x
	}
}

##################################################
#' Plot the mixture gamma/normal fit
#' 
#' @inheritParams estimateDepthDistribution
#' @param dateVariable The variable defining the intervals.
#' @param length The number of intervals.
#' @param start,end Optional start and end points of the interval span.
#' @param unit Set this to "days" if you need day intervals.
#' @param truncate An optinal two element vector of trucation limits for the time variable. Values below the first element are grouped into one interval, and values above the second element are grouped into one interval.
#' 
#' @export
#' 
addIntervals <- function(x, variable, length = 1, start = NULL, end = NULL, unit = NULL, truncate = NULL) {
	
	# Generate day intervals:
	if(!length(start)) {
		start <- x[, min(get(variable))]
	}
	if(!length(end)) {
		end <- x[, max(get(variable))]
	}
	
	if(!length(truncate)) {
		truncate <- c(start, end)
	}
	
	totalLength <- diff(truncate)
	steps <- ceiling(totalLength / length - 1e-10)
	
	
	intervalSeq <- seq(truncate[1], length.out = steps + 1, by = if(length(unit)) paste(length, unit) else length)
	
	if(start < utils::head(intervalSeq, 1)) {
		intervalSeq <- c(start, intervalSeq)
	}
	if(end > utils::tail(intervalSeq, 1)) {
		intervalSeq <- c(intervalSeq, end)
	}
	
	
	#intervalSeq <- seq(start, length.out = steps + 1, by = if(length(unit)) paste(length, unit) else length)
	
	if("Date" %in% class(intervalSeq) || (is.numeric(intervalSeq) && all(intervalSeq %% 1 == 0))) {
		intervalSeqString <- paste(zeropad(intervalSeq[- length(intervalSeq)]), zeropad(intervalSeq[-1]), sep = " - ")
	}
	else {
		intervalSeqString <- paste(round(intervalSeq[- length(intervalSeq)], digits = 4), round(intervalSeq[-1], digits = 4), sep = " - ")
	}
	
	
	intervalName <- paste0(variable, "Interval")
	intervalStringName <- paste0(variable, "IntervalString")
	intervalLengthName <- paste0(variable, "IntervalLength")
	
	x[, eval(intervalName) := findInterval(get(variable), intervalSeq, rightmost.closed = TRUE)]
	x[, eval(intervalStringName) := eval(intervalSeqString)[get(intervalName)]]
	x[, eval(intervalLengthName) := ..length]
}
