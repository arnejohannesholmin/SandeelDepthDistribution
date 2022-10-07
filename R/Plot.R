##################################################
#' Plot the mixture gamma/normal fit
#' 
#' @param fit The output of \code{estimateDepthDistribution}.
#' @param variable The vvariable of interest, i.e. the variable that is fitted to the model (e.g. "distanceToBottom").
#' @param showAllFits Logical: If TRUE, plot all fits in the iterative method, with the final fit highlighted.
#' @param dateIntevalVariable The variable defining the date intervals, defaulted to "DateInterval",
#' @param timeIntevalVariable The variable defining the (diurnal) time intervals, defaulted to "HourInterval",
#' @param arrange Logical: If TRUE, arrange the plots using ggpubr::ggarrange.
#' @param adds Optional ggplot object to add to each plot.
#' @param crop Logical: If TRUE, crop the plots of the date and time intervals with no valid fits.
#'
#' @return
#' A list of ggplots or an arranged ggplot if arrange = TRUE.
#' 
#' @import ggplot2
#' 
#' @export
#' 
plotModelFit <- function(fit, variable, showFit = TRUE, showAllFits = FALSE, dateIntevalVariable = "DateInterval", timeIntevalVariable = "HourInterval", arrange = FALSE, adds = NULL, crop = FALSE, byrow = TRUE)  {
	if(crop) {
		fit$parTable <- cropParTable(fit$parTable, dateVariable = dateIntevalVariable, timeVariable = timeIntevalVariable)
	}
	
	if(byrow) {
		orderBy <- c(timeIntevalVariable, dateIntevalVariable)
		data.table::setorderv(fit$parTable, orderBy)
	}
	
	indices <- seq_len(nrow(fit$parTable))
	plots <- mapply(
		plotModelFitOne, 
		indices, 
		showFit = showFit, 
		MoreArgs = list(
			fit = fit, 
			variable = variable, 
			showAllFits = showAllFits, 
			dateIntevalVariable = dateIntevalVariable, 
			timeIntevalVariable = timeIntevalVariable
		), 
		SIMPLIFY = FALSE
	)
	
	if(length(adds)) {
		plots <- lapply(plots, "+", adds)
	}
	
	if(arrange) {
		plots <- ggpubr::ggarrange(
			plotlist  = plots, 
			nrow = length(unique(fit$parTable[[timeIntevalVariable]])),
			ncol = length(unique(fit$parTable[[dateIntevalVariable]]))
		)
	}
	
	return(plots)
}

plotModelFitOne <- function(ind, fit, variable, showFit = TRUE, showAllFits = FALSE, dateIntevalVariable = "DateInterval", timeIntevalVariable = "HourInterval") {
	
	thisPar <- as.list(fit$parTable[ind, ])
	thisParList <- fit$par[[ind]]$parList
	thisData <- subset(fit$data, get(dateIntevalVariable) == thisPar[[dateIntevalVariable]] & get(timeIntevalVariable) == thisPar[[timeIntevalVariable]])
	
	
	main <- paste0(dateIntevalVariable, ": ", thisPar[[dateIntevalVariable]], "\n", timeIntevalVariable,":  ", thisPar[[timeIntevalVariable]])
	main <- gsub("IntervalString", "", main)
	
	
	if(!is.na(thisPar[[1]])) {
		ggplotObject <- ggplot2::ggplot(data = thisData, ggplot2::aes_string(x = variable)) + 
			ggplot2::geom_histogram(ggplot2::aes(y = ..density..), colour = "black", fill = "pink") + 
			ggplot2::ggtitle(main) + 
			ggplot2::xlim(0, NA)
		
		
		if(showAllFits) {
			for(ind2 in seq_along(thisParList)) {
				
				thisPar2 <- thisParList[[ind2]]
				ggplotObject <- addLine(
					ggplotObject = ggplotObject, 
					data = thisData, 
					variable = variable, 
					par = thisPar2, 
					plotNr = ind2
				)
			}		
		}
		
		if(showFit) {
			valuesToPrint <- c("shape", "rate", "mu", "sigma", "p", "N")
			toPrint <- paste0(
				valuesToPrint, 
				" = ", 
				round(unlist(thisPar[valuesToPrint]), 3), 
				collapse = "\n"
			)
			
			
			ggplotObject <- addLine(
				ggplotObject = ggplotObject, 
				data = thisData, 
				variable = variable, 
				par = thisPar, 
				plotNr = 1, 
				size = 1
			) + 
			ggplot2::theme(legend.position = "none") + 
			ggplot2::annotate("text",  x = Inf, y = Inf, label = toPrint, vjust = 1, hjust = 1, size = 3)		
		}
	}
	else {
		ggplotObject <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::ggtitle(main)
	}	
	
	return(ggplotObject)	
}


addLine <- function(ggplotObject, data, variable, par, plotNr, ...) {
	maxx <- max(data[[variable]], na.rm = TRUE)
	x_seq <- seq(0, maxx, by = maxx * 0.001)
	y <- (1 - par$p) * dgamma(x_seq, shape =  par$shape, rate =  par$rate) + par$p * dnorm(x_seq, mean = par$mu, sd = par$sigma)
	thisData <- data.table(
		x = x_seq, 
		y = y
	)
	ggplotObject <- ggplotObject + 
		ggplot2::geom_path(data = thisData, ggplot2::aes_string(x = "x", y = "y", color = plotNr), ...)
	
	return(ggplotObject)
}
