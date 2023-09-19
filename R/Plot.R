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
#' @param byrow Logical: Used when arrange = TRUE. If TRUE (the default) arrange the plots by  row first and then column.
#' @param valuesToPrint A vector of strings naming the estimated parameters to print in the plot. Possible values are "N", "p", "shape", "rate", "scale", "mu" and "sigma".
#' @param addMain Logical: If TRUE, add the automatically generated main title.
#' @param distToBottomLim A vector of min and max of the limit of the disance to the bottom NULL (the default) implies individual automatic limits from 0 to the maximum distance for each plot, and NA implies 0 to the maximum across all plots.
#' @param flip Logical: If TRUE, flip the axes so that distance to bottom is on the x axis.
#'
#' @return
#' A list of ggplots or an arranged ggplot if arrange = TRUE.
#' 
#' @import ggplot2
#' 
#' @export
#' 
plotModelFit <- function(fit, variable, showFit = TRUE, showAllFits = FALSE, dateIntevalVariable = "DateInterval", timeIntevalVariable = "HourInterval", arrange = FALSE, adds = NULL, crop = FALSE, byrow = TRUE, valuesToPrint = c("N", "p", "shape", "rate", "mu", "sigma"), addMain = TRUE, distToBottomLim = NULL, densityLim = NULL, flip = FALSE, transpose = FALSE, size = list(main = 3, info = 3, line = 1, axis.text = 10, axis.title = 14), axis.text.angle = list(x = 0, y = 0), ...)  {
	
	# Apply sizes specified by the user:
	defaultSize <- formals()$size
	newSizes <- intersect(names(size), names(defaultSize))
	if(length(defaultSize)) {
		defaultSize[newSizes] <- size[newSizes]
		size <- defaultSize
		print(size)
	}
	
	if(crop) {
		fit$parTable <- cropParTable(fit$parTable, dateVariable = dateIntevalVariable, timeVariable = timeIntevalVariable)
	}
	
	if(byrow) {
		orderBy <- c(timeIntevalVariable, dateIntevalVariable)
		data.table::setorderv(fit$parTable, orderBy)
	}
	
	if(length(distToBottomLim) == 1 && is.na(distToBottomLim)) {
		distToBottomLim <- c(0, max(fit$data[[variable]], na.rm   =  TRUE))
	}
	# This did not work, as the density is first caluclated inside the plotModelFitOne using geom_histogram():
	#if(length(densityLim) == 1 && is.na(densityLim)) {
	#	densityLim <- c(0, max(fit$data[["density"]], na.rm   =  TRUE))
	#}
	
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
			timeIntevalVariable = timeIntevalVariable, 
			valuesToPrint = valuesToPrint, 
			addMain = addMain,
			distToBottomLim = distToBottomLim, 
			densityLim = densityLim, 
			flip = flip, 
			size = size, 
			axis.text.angle = axis.text.angle
		), 
		SIMPLIFY = FALSE
	)
	
	nrows <- length(unique(fit$parTable[[timeIntevalVariable]]))
	ncols <- length(unique(fit$parTable[[dateIntevalVariable]]))
	
	if(transpose) {
		nrowsOld <- nrows
		nrows  <- ncols
		ncols <- nrowsOld
		dim(plots) <- c(nrows, ncols)
		plots <- c(t(plots))
	}
	
	# Remove axes for inner plots:
	plotseq <- seq_along(plots)
	innerX <- which((plotseq %% ncols) != 1)
	#innerY <- which(ceiling(plotseq / ncols) != nrows)
	
	# Remove y axis for inner plots:
	for(ind in innerX) {
		plots[[ind]] <- plots[[ind]] + ggplot2::theme(
			axis.text.y = ggplot2::element_blank(),
			axis.ticks.y = ggplot2::element_blank()
		)
	}
	
	
	# Remove all x lab:
	for(ind in plotseq) {
		plots[[ind]] <- plots[[ind]] + ggplot2::theme(
			axis.title.x = ggplot2::element_blank(),
			axis.title.y = ggplot2::element_blank()
		)
	}
	
	
	#for(ind in innerY) {
	#	plots[[ind]] <- plots[[ind]] + ggplot2::theme(
	#		axis.text.x = ggplot2::element_blank(),
	#		axis.ticks.x = ggplot2::element_blank(),
	#		axis.title.x = ggplot2::element_blank()
	#	)
	#}
	
	
	if(length(adds)) {
		plots <- lapply(plots, "+", adds)
	}
	
	if(arrange) {
		plots <- ggpubr::ggarrange(
			plotlist  = plots, 
			nrow = nrows,
			ncol = ncols, 
			...
		)
		
		plots <-ggpubr::annotate_figure(
			plots, 
			left = grid::textGrob(variable, rot = 90, vjust = 1, gp = grid::gpar(cex = 1.3)),
			bottom = grid::textGrob("density", gp = grid::gpar(cex = 1.3))
		)
	}
	
	return(plots)
}




plotModelFitOne <- function(ind, fit, variable, showFit = TRUE, showAllFits = FALSE, dateIntevalVariable = "DateInterval", timeIntevalVariable = "HourInterval", valuesToPrint = c("N", "p", "shape", "rate", "mu", "sigma"), addMain = TRUE, distToBottomLim = NULL, densityLim = NULL, flip = FALSE, size = list(main = 3, info = 3, line = 1, axis.text = 10, axis.title = 14), axis.text.angle = list(x = 0, y = 0)) {
	
	
	
	thisPar <- as.list(fit$parTable[ind, ])
	thisParList <- fit$par[[ind]]$parList
	thisData <- subset(fit$data, get(dateIntevalVariable) == thisPar[[dateIntevalVariable]] & get(timeIntevalVariable) == thisPar[[timeIntevalVariable]])
	
	
	
	main <- paste0(dateIntevalVariable, ": [", thisPar[[dateIntevalVariable]], ")\n", timeIntevalVariable,": [", thisPar[[timeIntevalVariable]], ")")
	main <- gsub("IntervalString", "", main)
	
	
	if(NROW(thisData)) {
		# Plot the histogram:
		ggplotObject <- ggplot2::ggplot(data = thisData, ggplot2::aes_string(x = variable)) + 
			ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), colour = "black", fill = "pink")
		
		if(length(distToBottomLim)) {
			ggplotObject <- ggplotObject + ggplot2::xlim(distToBottomLim[1], distToBottomLim[2])
		}
		else {
			ggplotObject <- ggplotObject + ggplot2::xlim(0, NA)
		}
		
		if(length(densityLim)) {
			ggplotObject <- ggplotObject + ggplot2::ylim(densityLim[1], densityLim[2])
		}
		else {
			ggplotObject <- ggplotObject + ggplot2::ylim(0, NA)
		}
		
		
		# Get the parameter values to print in the plot:
		if(length(valuesToPrint)) {
			toPrint <- paste0(
				valuesToPrint, 
				" = ", 
				round(unlist(thisPar[valuesToPrint]), 3), 
				collapse = "\n"
			)
		}
		else {
			toPrint <- NULL
		}
		
		if(length(toPrint)) {
			ggplotObject <- ggplotObject + ggplot2::annotate("text",  x = Inf, y = Inf, label = toPrint, vjust = 1.05, hjust = 1.05, size = size$info)
		}
		
		
		if(!is.na(thisPar[[1]])) {
			
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
				
				ggplotObject <- addLine(
					ggplotObject = ggplotObject, 
					data = thisData, 
					variable = variable, 
					par = thisPar, 
					plotNr = 1, 
					size = size$line
				) + 
					ggplot2::theme(legend.position = "none")
				
			}
		}
		
	}
	else {
		ggplotObject <- ggplot2::ggplot() + ggplot2::theme_void()
	}
	
	
	
	if(addMain) {
		ggplotObject <- ggplotObject + ggplot2::ggtitle(main)
	}
	
	if(!flip) {
		ggplotObject <- ggplotObject + ggplot2::coord_flip()
	}
	
	ggplotObject <- ggplotObject + ggplot2::theme(
		axis.text.x = ggplot2::element_text(size = size$axis.text, angle = axis.text.angle$x, vjust = 0.5, hjust = 0.5),
		axis.title.x = ggplot2::element_text(size = size$axis.title, face = "bold"),
		axis.text.y = ggplot2::element_text(size = size$axis.text),
		axis.title.y = ggplot2::element_text(size = size$axis.title, face = "bold"), 
		plot.title = ggplot2::element_text(size = size$main)
	)
	
	
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
