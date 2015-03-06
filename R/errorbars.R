#This is based off the "bxplot" function included in the beeswarm package by Aron Ecklund

errorbars=function (x, ...) { UseMethod("errorbars") }

errorbars.formula = function (formula, data = NULL, ..., subset, na.action = NULL) 
{
    if (missing(formula) || (length(formula) != 3L)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE) 
    ##store the call used to invoke this function in m, keep ... collapsed
    
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- NULL 
    ## don't include extraneous crap in the model frame call
    
    m$na.action <- na.action
    require(stats, quietly = TRUE)
    m[[1L]] <- as.name("model.frame")
    ## substitute model.frame for the initial function name in the call, thereby allowing us to construct a model frame with the stuff to plot, using all terms in the formula
    
    mf <- eval(m, parent.frame())
    ##ok evaluate that new function to actually create the model frame
    
    response <- attr(attr(mf, "terms"), "response")
    #get the index position of the response term dictated by the formula from within the model.frame object
    
    errorbars(split(mf[[response]], mf[-response]), ...)
    #split the response variable by all other variables in the formula, returning a list of lists, pass that to the errorbars function with the ... parameters
}
# processes the formula notation, splitting the data apart appropriately, and calls the generic errorbars.default with the results.

error.method.sem = function (x)
{
	if (length(x) < 2) {return(c(NA,NA))}
	mx = mean(x, na.rm=T)
	semx = sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))
	return(c(mx+semx,mx-semx))
}

error.method.ci = function (x,level)
{
	if (length(x) < 2) {return(c(NA,NA))}
	alpha = 1-level
	mx = mean(x, na.rm=T)
	semx = sd(x, na.rm=T)/sqrt(length(x[!is.na(x)]))
	tvalue = qt(1-alpha/2,length(x[!is.na(x)])-1)
	return(c(mx+semx*tvalue,mx-semx*tvalue))
}

errorbars.default = function (x, vertical = TRUE, horizontal = !vertical, 
    add = FALSE,
    error.method = c('sem','ci','none'), ci.level = 0.95,
    upperbound.manual = NULL, lowerbound.manual = NULL, center.manual = NULL,
    center.method = c('mean','median','mode','none'),
    bar.col = NULL, bar.lty = NULL, bar.lwd = NULL, bar.offset = 0,
    cap.col = NULL, cap.lty = NULL, cap.lwd = NULL, cap.offset = 0,
    center.col = NULL, center.lty = NULL, center.lwd = NULL, center.offset = 0,
    col = par("col"), lty = par("lty"), lwd = par("lwd"), offset = 0,
    at = NULL, cap.width = 0.5, center.width = 0.25, ...) 
{
    if (is.null(bar.col)) bar.col=col
    if (is.null(bar.lty)) bar.lty=lty
    if (is.null(bar.lwd)) bar.lwy=lwd
    if (is.null(cap.col)) cap.col=col
    if (is.null(cap.lty)) cap.lty=lty
    if (is.null(cap.lwd)) cap.lwy=lwd
    if (is.null(center.col)) center.col=col
    if (is.null(center.lty)) center.lty=lty
    if (is.null(center.lwd)) center.lwy=lwd
    
    if (is.numeric(x)) {
        x <- list(x)
    }
    # turn everything into a list so we can process all of it the same way
    
    
    n <- length(x)
    
    # calculate the error bars for each group unless manually specified

	if (is.null(upperbound.manual) & is.null(lowerbound.manual)) {
		y = switch(match.arg(error.method),
			sem = lapply(x, error.method.sem),
			ci = lapply(x, error.method.ci, level=ci.level),
			none = NULL
		)
	} else { 
				# construct a list using manually provided values.
				if (is.null(upperbound.manual)) {
					upperbound.manual = rep(NA,length(lowerbound.manual))
				}
				if (is.null(lowerbound.manual)) {
					lowerbound.manual = rep(NA,length(upperbound.manual))
				}
				y = mapply(c,upperbound.manual,lowerbound.manual,SIMPLIFY=FALSE)
			}
	
    # calculate the measure of central tendency for each group unless manually specified
    
    if (is.null(center.manual)) {
    	   y.center = switch(match.arg(center.method),
		mean = sapply(x, mean),
		median = sapply(x, median),
		mode = sapply(x, function(x) {dn=density(x);return(dn$x[which.max(dn$y)])}), #mode using kernel density estimate
		none = NULL)
	} else { 
				y.center = center.manual
			}
			 
	y.top = sapply(y,'[[',1) #pull first & second values from list to new vector 
	y.bottom = sapply(y,'[[',2)
			    
    if (is.null(at)) 
        at <- 1:n
    # if we don't explicitly specify which independent axis coordinates to draw at, use defaults.    
    
    if (!add) {
        boxplot(x, horizontal = horizontal, at = at, pars = list(whisklty = 0, 
            staplelty = 0, outpch = NA, boxlty = 0, medlty = 0), 
            ...)
    #This draws an empty plot.
    }

    hw <- cap.width/2
    if (horizontal & !is.null(y)) {
    	#horizontal graph's error bars
        
        #vectorized, should handle colors as expected

        #dependent axis bar
        segments(y0 = at+offset+bar.offset,
        	x0 = y.top,
        	x1 = y.bottom,
        	col = bar.col, lwd = bar.lwd, lty = bar.lty)
        #top cap
        segments(y0 = at+offset+cap.offset - hw,
        	y1 = at+offset+cap.offset + hw,
        	x0 = y.top, 
            col = cap.col, lwd = cap.lwd, lty = cap.lty)
        #bottom cap
        segments(y0 = at+offset+cap.offset - hw,
        	y1 = at+offset+cap.offset + hw,
        	x0 = y.bottom, 
            col = cap.col, lwd = cap.lwd, lty = cap.lty)		
        		
        
        
    }
    else if (!is.null(y)){
    	#vertical graph's error bars
        #vectorized, should handle colors as expected

        #dependent axis bar
        segments(x0 = at+offset+bar.offset,
        	y0 = y.top,
        	y1 = y.bottom,
        	col = bar.col, lwd = bar.lwd, lty = bar.lty)
        #top cap
        segments(x0 = at+offset+cap.offset - hw,
        	x1 = at+offset+cap.offset + hw,
        	y0 = y.top, 
            col = cap.col, lwd = cap.lwd, lty = cap.lty)
        #bottom cap
        segments(x0 = at+offset+cap.offset - hw,
        	x1 = at+offset+cap.offset + hw,
        	y0 = y.bottom, 
            col = cap.col, lwd = cap.lwd, lty = cap.lty)
    }
    
    hw <- center.width/2
    if (horizontal & !is.null(y.center)) {
    	#horizontal graph's marker of central tendency
		#vectorized
        segments(y0 = at+offset+center.offset - hw,
        	y1 = at+offset+center.offset + hw,
        	x0 = y.center, 
            col = center.col, lwd = center.lwd, lty = center.lty)
        
    }
    else if (!is.null(y.center)){
    	#vertical graph's marker of central tendency      
        #vectorized
        segments(x0 = at+offset+center.offset - hw, x1 = at+offset+center.offset + hw, y0 = y.center, 
                col = center.col, lwd = center.lwd, lty = center.lty)
    }
    
    
}










# # require(beeswarm)
# testdata = rbind(
					# data.frame(group="a", value = rnorm(20)*2+12),
					# data.frame(group="b", value = rnorm(20)*2+8),
					# data.frame(group="c", value = rnorm(20)*1+6)
				# )
# testdata = cbind(
					# testdata,
					# data.frame(subgroup = rep(c(rep("x",10),rep("y",10)),3))
# )


#tests to notrun
#require(beeswarm)
#beeswarm(value~group+subgroup,testdata,ylim=c(1,20),log=F,horizontal=F,pch=16,cex=0.75,col="gray60",bty="L")
#errorbars(value~group+subgroup,testdata,add=T,horizontal=F,bar.lwd=2,center.lwd=2,cap.lwd=1,cap.width=.125, error.method="sem",ci.level=.95,)

#manual bars
#errorbars(value~group+subgroup,testdata,add=T,horizontal=F,bar.lwd=2,center.lwd=2,cap.lwd=1,cap.width=.25, error.method="sem",ci.level=.95,upperbound.manual=c(5,6,7,8),lowerbound.manual=c(NA,NA,NA,1),center.manual=c(1,1,1,1))

#only draw error bars
#errorbars(value~group+subgroup,testdata,add=T,horizontal=F,bar.lwd=2,center.lwd=1,center.method="mode",center.width=1,cap.lwd=1,cap.width=.25, error.method="ci",ci.level=.95,offset=.125,col=c("red","green","blue"))


#errorbars(value~group+subgroup,testdata,add=T,horizontal=F,bar.lwd=2,center.lwd=0,cap.lwd=1,cap.width=.25, error.method="ci",ci.level=.95,offset=.125)

#errorbars(value~group+subgroup,testdata,add=T,horizontal=F,bar.lwd=2,center.lwd=0,cap.lwd=1,cap.width=.25, error.method="ci",ci.level=.99,offset=.25)

