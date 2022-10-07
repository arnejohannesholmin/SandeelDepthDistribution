##################################################
#' Plot the mixture gamma/normal fit
#' 
#' @param data A table of sandeel depth data.
#' @param variable The vvariable of interest, i.e. the variable that is fitted to the model (e.g. "distanceToBottom").
#' @param dateVariable The variable defining the date, defaulted to "Date".
#' @param timeVariable The variable defining the (diurnal) time, defaulted to "Hour".
#' @param dateIntervalLength The length of the date intervals.
#' @param timeIntervalLength The length of the time intervals.
#' @param timeTruncate An optinal two element vector of trucation limits for the time variable. Values below the first element are grouped into one interval, and values above the second element are grouped into one interval.
#' @param p0 Initial value for the probability that a school is feeding.
#' @param lower,upper The lower and upper limits for the optimization of p.
#' @param minLength The minimum number of schools required for fitting, defaulted to 20.
#' 
#' @import data.table
#'
#' @export
#' 
estimateDepthDistribution <- function(data, variable = "distanceToBottom", dateVariable = "Date", timeVariable = "Hour", dateIntervalLength = 10, timeIntervalLength = 2, timeTruncate = NULL, p0 = 0.5, lower = 0.01, upper = 0.99, minLength = 20) {
	
	dateIntervalVariable <- paste0(dateVariable, "Interval")
	timeIntervalVariable = paste0(timeVariable, "Interval")
	dateIntervalStringVariable <- paste0(dateVariable, "IntervalString")
	timeIntervalStringVariable = paste0(timeVariable, "IntervalString")
	
	data <- addDateAndTimeIntervals(
		data, 
		dateVariable = dateVariable,
		timeVariable = timeVariable,
		dateIntervalLength = dateIntervalLength,
		timeIntervalLength = timeIntervalLength,
		timeTruncate = timeTruncate
	) 
	
	
	splitBy <- c(dateIntervalVariable, timeIntervalVariable)
	
	
	data_splitted <- split(data, by = splitBy)
	
	par <- list()
	
	for(ind in seq_along(data_splitted)) {
		par[[ind]] <- estimateDepthDistributionIteratively(
			data_splitted[[ind]], 
			variable = variable, 
			p0 = p0, 
			lower = lower, 
			upper = upper, 
			minLength = minLength
		)
		par[[ind]]$par[[dateIntervalVariable]] <- data_splitted[[ind]][[dateIntervalVariable]][[1]]
		par[[ind]]$par[[timeIntervalVariable]] <- data_splitted[[ind]][[timeIntervalVariable]][[1]]
		par[[ind]]$par[[dateIntervalStringVariable]] <- data_splitted[[ind]][[dateIntervalStringVariable]][[1]]
		par[[ind]]$par[[timeIntervalStringVariable]] <- data_splitted[[ind]][[timeIntervalStringVariable]][[1]]
	}
	
	parTable <- data.table::rbindlist(lapply(par, "[[", "par"), fill = TRUE)
	
	data.table::setorderv(parTable, splitBy)
	
	list(
		par = par, 
		data = data, 
		parTable = parTable
	)
}



addDateAndTimeIntervals <- function(data, dateVariable = "Date", timeVariable = "Hour", dateIntervalLength = 10, timeIntervalLength = 2, timeTruncate = NULL) {
	
	dateIntervalVariable <- paste0(dateVariable, "Interval")
	timeIntervalVariable = paste0(timeVariable, "Interval")
	
	data <- data.table::copy(data)
	
	addIntervals(
		data, 
		variable = dateVariable, 
		length = dateIntervalLength, 
		unit = "days"
	)
	
	addIntervals(
		data, 
		variable = timeVariable, 
		length = timeIntervalLength, 
		truncate = timeTruncate
	)
	
	# expand to full grid of date and time interval:
	data <- expandDateAndTimeIntervalGrid(data, dateIntervalVariable = dateIntervalVariable, timeIntervalVariable = timeIntervalVariable)
	
	return(data)
} 

estimateDepthDistributionIteratively <- function(data, variable, p0, minLength = 20, interval = c(0, 1), lower = 0.05, upper = 0.95) {
	
	if(nrow(data) > minLength) {
		
		diff <- Inf
		oldp <- p0
		
		parList <- list()
		
		
		n <- 0
		while(diff > 0.001 && n < 100) {
			parNew <- estimateDepthDistributionOne(data, variable, oldp, interval = interval, lower = lower, upper = upper)
			
			diff <- abs(oldp - parNew$p)
			n <- n + 1
			
			parList[[n]] <- parNew
			
			oldp <- parNew$p
			
		}
		
		out <- list(
			par = parList[[n]], 
			parList = parList, 
			data = data
		)		
	}
	else {
		
		emptyPar <- structure(as.list(rep(NA_real_, 7)), names = c("shape", "rate", "mu", "sigma", "p", "N", "MaxLogLikelihood"))
		
		out <- list(
			par = emptyPar, 
			parList = list(emptyPar), 
			data = data
		)
	}
	
	return(out)
}

estimateDepthDistributionOne <- function(data, variable, p0, interval = c(0, 1), lower = 0.05, upper = 0.95) {
	
	par <- estimateGammaAndNormalGivenP(data, variable, p0)
	
	# Estimate the p by Nelder–Mead:
	verticalPDF <- function(x, p, par) {
		(1 - p) * dgamma(x, shape = par$shape, rate = par$rate) + p * dnorm(x, mean = par$mu, sd = par$sigma)
	}
	
	
	l <- function(p) {
		# The minus sign is becuse optim performs minimization:
		-sum(log(verticalPDF(data[[variable]], p, par)))
	}
	
	pnew <- optim(p0, fn = l, method = "Brent", lower = lower, upper = upper)
	
	parNew <- estimateGammaAndNormalGivenP(data, variable, p0)
	
	
	parNew$p <- pnew$par
	
	parNew$N <- nrow(data)
	
	parNew$MeanMaxLogLikelihood <- -pnew$value / nrow(data)

	
	return(parNew)
}

estimateGammaAndNormalGivenP <- function(data, variable, p) {
	
	# First divide into feeding and resting:
	x <- data[[variable]]
	numberOfResting <- ceiling(length(x) * (1 - p))
	orderInd <- order(x)
	xresting <- x[orderInd[seq_len(numberOfResting)]]	
	xfeeding <- x[orderInd[seq(numberOfResting + 1, length(x))]]
	
	# Fit the Gamma distribution to the resting data:
	par = MASS::fitdistr(xresting/mean(xresting), densfun = dgamma, list(shape = 1, rate = 1))$est
	par[2] = par[2]/mean(xresting)
	
	# Fit the normal distribution to the feeding data:
	mu = mean(xfeeding, na.rm = TRUE)
	sigma = sd(xfeeding, na.rm = TRUE)
	
	out <- list(
		shape = par[1], 
		rate = par[2], 
		mu = mu, 
		sigma = sigma
	)
}

expandDateAndTimeIntervalGrid <- function(x, dateIntervalVariable = "DateInterval", timeIntervalVariable = "HourInterval") {
	
	x[, thisXVariable := as.character(get(dateIntervalVariable))]
	x[, thisYVariable := as.character(get(timeIntervalVariable))]
	
	suffixes <- c("", "String", "Length")
	varToMergeBackInDate <- paste0(dateIntervalVariable, suffixes)
	toMergeBackInDate <- unique(x[, c("thisXVariable", varToMergeBackInDate), with = FALSE])
	varToMergeBackInTime <- paste0(timeIntervalVariable, suffixes)
	toMergeBackInTime <- unique(x[, c("thisYVariable", varToMergeBackInTime), with = FALSE])
	
	x <- x[
		CJ(
			thisXVariable = sort(unique(x$thisXVariable)), 
			thisYVariable = sort(unique(x$thisYVariable)), unique = TRUE
		),
		on = c("thisXVariable", "thisYVariable"), nomatch = NA
	]
	
	x[, c(varToMergeBackInDate, varToMergeBackInTime) := NULL]
	
	x <- merge(x, toMergeBackInDate, by = "thisXVariable", sort = FALSE)
	x <- merge(x, toMergeBackInTime, by = "thisYVariable", sort = FALSE)
	x[, c("thisXVariable", "thisYVariable") := NULL]
	
	return(x)
}


##################################################
#' Read sandeel school depth data
#' 
#' @param filePath The path to the sandeel school depth data file.
#'
#' @export
#' 
readSandeelData <- function(filePath) {
	
	# Read the sandeel data to a data.table:
	d <- load(filePath)
	sandeel.dt <- data.table::setDT(sandeel.dt)
	
	# Add BottomDepth and distance from the bottom i meters:
	sandeel.dt[, BottomDepth := weighted_meanDepth / nor_Depth]
	sandeel.dt[, distanceToBottom_weighted := BottomDepth - weighted_meanDepth]
	sandeel.dt[, distanceToBottom := BottomDepth - meanDepth]
	
	# Convert to proper Date and Time:
	sandeel.dt[, DateTime := as.POSIXct(YMD_time, tz = "UTC")]
	setnames(sandeel.dt, "Date", "DateNumeric")
	sandeel.dt[, Date := as.Date(DateTime)]
	
	sandeel.dt[, Hour := as.POSIXlt(DateTime)$hour]
	
	# Calculate time during the day relative to sun rise (0) and sun set (1):
	latlondate <- subset(sandeel.dt, select=c(Latitude, Longitude, Date))
	colnames(latlondate) <- c("lat", "lon", "date")
	sun.df <- suncalc::getSunlightTimes(data = latlondate, keep = c("sunrise", "sunset"))
	
	# Merge with the data:
	sandeel.dt <- cbind(sandeel.dt, sun.df[, c("sunrise", "sunset")])
	
	
	sandeel.dt[, hour_from_sunrise := as.numeric(difftime(DateTime, sunrise), units="hours")]
	sandeel.dt[, sunset_sunrise := as.numeric(difftime(sunset, sunrise), units="hours")]
	sandeel.dt[, relative_time := hour_from_sunrise / sunset_sunrise]
	
	
	return(sandeel.dt)
}

##################################################
#' dcast a parTable
#' 
#' @param parTable The parTable returned from estimateDepthDistribution().
#' @inheritParams estimateDepthDistribution
#' @param variables A vector of parameters to display in the plots.
#'
#' @export
#' 
dcastParTable <- function(parTable, dateVariable, timeVariable, variables = c("shape", "rate", "mu", "sigma", "p", "MeanMaxLogLikelihood")) {
	
	dcastOne <- function(variable, parTable, dateVariable, timeVariable) {
		dcast(parTable, get(dateVariable) ~ get(timeVariable), value.var =variable, sum, fill =  NA)
	}
	
	dcastTables <- lapply(variables, dcastOne, parTable, dateVariable, timeVariable)
	names(dcastTables) <- variables
	
	return(dcastTables)
}

cropParTable <- function(parTable, dateVariable, timeVariable) {
	
	dateRange <- subset(parTable, !is.na(p))[, range(get(dateVariable))]
	timeRange <- subset(parTable, !is.na(p))[, range(get(timeVariable))]
	
	parTable <- subset(parTable, get(dateVariable) >= dateRange[1] & get(dateVariable) <= dateRange[2] )
	parTable <- subset(parTable, get(timeVariable) >= timeRange[1] & get(timeVariable) <= timeRange[2] )
	
	return(parTable)
}
