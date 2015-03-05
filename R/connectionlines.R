#Draw pairwise conection lines

u2inx = function(x) {
	out = grconvertX(x,from="user",to="inches")
	return(out)
	
}

u2iny = function(y) {
	out = grconvertY(y,from="user",to="inches")
	return(out)
	
}

in2ux = function(x) {
	out = grconvertX(x,from="inches",to="user")
	return(out)
	
}

in2uy = function(y) {
	out = grconvertY(y,from="inches",to="user")
	return(out)
	
}

connectionlines = function (x, ... ) { UseMethod("connectionlines")}

connectionlines.formula = function (formula, data = NULL, ..., subset, na.action = NULL) 
{
    if (missing(formula) || (length(formula) != 3L)) 
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE) 
    
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- NULL 
    
    m$na.action <- na.action
    require(stats, quietly = TRUE)
    m[[1L]] <- as.name("model.frame")
    
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    
    connectionlines(split(mf[[response]], mf[-response]), ...)
}

connectionlines.default = function (x,
	comparisons = "all", labels = NULL, 
	vertical = TRUE, horizontal = !vertical, 
    line.position = c('above.max','below.min','top','bottom'),
    col = par("col"), lty = par("lty"), lwd = par("lwd"),
    draw.ticks=TRUE, drop.ticks=TRUE, tick.length = NULL, tick.spacing = NULL,
    offset = NULL, spacing = NULL, units=c('inches','cm'), labels.offset=NULL, labels.rotate=T,
    at = NULL, xpd = NULL, ...) 
{
        
    if (is.numeric(x)) {
        x <- list(x)
    }
        
    #set intelligent defaults for spacing in inches
    if (is.null(offset)) {
    	offset=0
    } else {
    	#convert to inches for ease
    	offset = switch(match.arg(units),
    		inches = offset,
    		cm = offset/2.54)
    }
    
    if (is.null(spacing)){
    	spacing =0.125
    	} else {
    	#convert to inches for ease
		spacing = switch(match.arg(units),
    		inches = spacing,
    		cm = spacing/2.54)
    }
    
    if (is.null(tick.length)){
    	tick.length =0.0625
    	} else {
    	#convert to inches for ease
		tick.length = switch(match.arg(units),
    		inches = tick.length,
    		cm = tick.length/2.54)
    }
    
    if (is.null(tick.spacing)){
    	tick.spacing =0.0625
    	} else {
    	#convert to inches for ease
		tick.spacing = switch(match.arg(units),
    		inches = tick.spacing,
    		cm = tick.spacing/2.54)
    }
    
    if (is.null(labels.offset)){
    	labels.offset = 0
    	} else {
    	#convert to inches for ease
		labels.offset = switch(match.arg(units),
    		inches = labels.offset,
    		cm = labels.offset/2.54)
    }
    
    if (!is.null(xpd)) {
    	xpd.old=par(xpd)
    	par(xpd=xpd)
    } else {
    	xpd = TRUE
    	xpd.old=par(xpd)
    	par(xpd=xpd)
    }
    
    if (!is.null(at)) {
    	 if (length(at) != length(x)) {stop(paste("at needs to be a numeric vector of the same length as the number of groups, which is",length(x)))}
     if (anyDuplicated(at)) {stop(paste("at can't have duplicate values. That makes the brains hurt"))}

    } else {at = seq(from=1,to=length(x))}
        
    # turn everything into a list so we can process all of it the same way

	if (is.character(comparisons) & comparisons[1] == "all") {
        # deal with the special case of all pairwise comparisons
        comparisons = matrix(rep(NA,length(x)*length(x)),nrow=length(x),ncol=length(x))
        comparisons = (row(comparisons)<col(comparisons)) 
        # create a strict upper triangular matrix of the comparisons to do.
        # its very important that it be strict upper triangular - this makes the column coordinate
        # of a TRUE cell always greater than the row coordinate. makes downstream
        # functions simpler
    } else {
    	if(nrow(comparisons) !=ncol(comparisons)) {stop('comparisons must be square matrix, strict upper triangular')}
    }
    
    drawfrom = NULL #coordinates to draw to and from are not neccesarily indexes any more
    drawto = NULL
    indexfrom = NULL #original indices of each group, used to apply the correct label from label matrix regardless of where the group has been moved to
    indexto = NULL

	#remember, we freak out if there's anything in the lower triangle of the matrix!
    for (i in (1:ncol(comparisons))[order(at)]) { #reorder i & j, keeping the same values, so these vectors remain in drawing order regardless of how we rearrange "at"
		for (j in (1:ncol(comparisons))[order(at)]) {
			if (comparisons[i,j]==TRUE) {
				if (at[i]<=at[j]) { #make sure draw/indexfrom is always less than draw/indexto, regardless of how we mangle 'at'
					drawfrom = c(drawfrom,at[i])
					indexfrom = c(indexfrom,i)
					drawto = c(drawto,at[j])
					indexto = c(indexto,j)
				} else {
					drawfrom = c(drawfrom,at[j])
					indexfrom = c(indexfrom,j)
					drawto = c(drawto,at[i])
					indexto = c(indexto,i)
				}
				
				if (i>j) {stop("comparisons must be a matrix without anything in the lower triangle!")}
			}
		}
    
    }    
    
    newindexfrom = match(at[indexfrom],sort(at)) #sort at (gives us new order), then look up the index of the value fetched from the unsorted list 
    newindexto = match(at[indexto],sort(at)) #this gives us the new index for calculating overlap and collisions!
        
    #stack upward & point ticks downward or do the converse?			
	drawdirection = switch(match.arg(line.position),
					above.max = 1,
					below.min = -1,
					top = 1, 
					bottom = -1
				)
				
	#automatically set the offset if not specified based on direction:
	if (is.null(offset)) {
		offset = drawdirection*0.5
	}
    
    # data boundary, where points stop and the lines can pass, per group
    groupbounds = switch(match.arg(line.position),
						above.max = sapply(x, max),
						below.min = sapply(x, min),
						#top is the top of the plot region, which we use grconvert to pull out
						top = switch(horizontal+1,
							rep(grconvertY(1,from="npc",to="user"),length(x)), #horizontal is false or zero, +1 gets to index 1
							rep(grconvertX(1,from="npc",to="user"),length(x))), #horizontal is true , +1 gets to index 2
						bottom = switch(horizontal+1,
							rep(grconvertY(0,from="npc",to="user"),length(x)), #horizontal is false or zero, +1 gets to index 1
							rep(grconvertX(0,from="npc",to="user"),length(x))), #horizontal is true , +1 gets to index 2
				)
				
	#reorder the groupbounds based on the true 'at' coordinates, so the drawing logic pulls the correct bounds
	groupbounds = groupbounds[order(at)]
		
	#how long is each line?					
    drawlength = abs(drawfrom-drawto)
    
    #find the closest data boundary for each line, by finding the max/min value the line passes
    #fill with zeros for the ones not drawing next to data
    linebounds = switch(match.arg(line.position),
						top = ,
						above.max = mapply(function(a,b) {max(groupbounds[a:b])},newindexfrom,newindexto),
						bottom = ,
						below.min = mapply(function(a,b) {min(groupbounds[a:b])},newindexfrom,newindexto)
				)

	#lines with smallest/largest bounds (if applicable), then shortest, then left to right
    draworder = switch(match.arg(line.position),
					above.max = order(linebounds,drawlength,drawfrom,decreasing=FALSE),
					below.min = order(-linebounds,drawlength,drawfrom,decreasing=FALSE),
					top = , #inherits next
					bottom = order(drawlength,drawfrom,decreasing=FALSE)
				)
	

 	
 	# current bounds will be used to "grow" the bounding rectangle not to draw inside
 	currentgroupbounds = groupbounds
 	currentlinebounds = linebounds
 	
 	
	linepositions = rep(NA,length(draworder))		
 	 
 		for (i in draworder) {
 			#Sys.sleep(0.25)
 			linepositions[i] = switch(horizontal + 1,
 				in2uy(u2iny(currentlinebounds[i])+drawdirection*spacing), #horizontal is false, false + 1 = 1
 				in2ux(u2inx(currentlinebounds[i])+drawdirection*spacing)) #horizontal is true, true + 1 = 2

			if (horizontal == FALSE) { 
 				segments(x0=drawfrom[i],x1=drawto[i],y0=in2uy(u2iny(linepositions[i])+offset),lty=lty,col=col,lwd=lwd)
 			} else {
 				segments(y0=drawfrom[i],y1=drawto[i],x0=in2ux(u2inx(linepositions[i])+offset),lty=lty,col=col,lwd=lwd)
 			}
 			if (draw.ticks == TRUE) {
 				if (drop.ticks == TRUE) {
 					if (horizontal == FALSE) { #drop ticks vertical
			 			segments(x0=drawfrom[i],
			 						y0=in2uy(u2iny(linepositions[i])+offset),
			 						y1=in2uy(u2iny(currentgroupbounds[newindexfrom[i]]) +
			 						drawdirection*tick.spacing+offset),
			 						lty=lty,col=col,lwd=lwd)
			 			segments(x0=drawto[i],
			 						y0=in2uy(u2iny(linepositions[i])+offset),
			 						y1=in2uy(u2iny(currentgroupbounds[newindexto[i]]) +
			 						drawdirection*tick.spacing+offset),
			 						lty=lty,col=col,lwd=lwd)
		 						} else { #drop ticks horizontal
			 					segments(y0=drawfrom[i],
			 						x0=in2ux(u2inx(linepositions[i])+offset),
			 						x1=in2ux(u2inx(currentgroupbounds[newindexfrom[i]]) +
			 						drawdirection*tick.spacing+offset),
			 						lty=lty,col=col,lwd=lwd)
			 			segments(y0=drawto[i],
			 						x0=in2ux(u2inx(linepositions[i])+offset),
			 						x1=in2ux(u2inx(currentgroupbounds[newindexto[i]]) +
			 						drawdirection*tick.spacing+offset),
			 						lty=lty,col=col,lwd=lwd)
		 						}
				} else {
		 			if (horizontal == FALSE) { #non-dropped ticks vertical
			 			segments(x0=drawfrom[i],
			 						y0=in2uy(u2iny(linepositions[i])+offset),
			 						y1=in2uy(u2iny(linepositions[i])-drawdirection*tick.length+offset),
			 						lty=lty,col=col,lwd=lwd)
			 			segments(x0=drawto[i],
			 						y0=in2uy(u2iny(linepositions[i])+offset),
			 						y1=in2uy(u2iny(linepositions[i])-drawdirection*tick.length+offset),
			 						lty=lty,col=col,lwd=lwd)	
					} else { #non-dropped ticks horizontal
			 			segments(y0=drawfrom[i],
			 						x0=in2ux(u2inx(linepositions[i])+offset),
			 						x1=in2ux(u2inx(linepositions[i])-drawdirection*tick.length+offset),
			 						lty=lty,col=col,lwd=lwd)
			 			segments(y0=drawto[i],
			 						x0=in2ux(u2inx(linepositions[i])+offset),
			 						x1=in2ux(u2inx(linepositions[i])-drawdirection*tick.length+offset),
			 						lty=lty,col=col,lwd=lwd)	
					}
				}
 			}
  			
 			# these coordinates are now occupado!
 			currentgroupbounds[newindexfrom[i]:newindexto[i]] = linepositions[i]
 			
 			
 			currentlinebounds = switch(match.arg(line.position),
						top = ,
						above.max = mapply(function(a,b) {max(currentgroupbounds[a:b])},newindexfrom,newindexto),
						bottom = ,
						below.min = mapply(function(a,b) {min(currentgroupbounds[a:b])},newindexfrom,newindexto)
						)
			
 			
		}
		
		if (!is.null(labels)) {
			for (i in draworder) {
				currlabel = labels[min(indexfrom[i],indexto[i]), max(indexfrom[i],indexto[i])] #make sure row index is smaller than column, swap if needed 
				if (!is.na(currlabel) & (currlabel !=FALSE)) { #not NA or FALSE
					if (horizontal == FALSE) { #labels vertical
						text(x=(drawfrom[i]+drawto[i])/2,
						y=in2uy(u2iny(linepositions[i])+offset+tick.spacing*-drawdirection+labels.offset),
						labels=currlabel)
					} else {  #labels horizontal
						if (labels.rotate == TRUE) { # text vertical
							text(y=(drawfrom[i]+drawto[i])/2,
							x=in2ux(u2inx(linepositions[i])+offset+tick.spacing*-drawdirection+labels.offset),
							labels=currlabel)
						} else { # text horizontal
							text(y=(drawfrom[i]+drawto[i])/2,
							x=in2ux(u2inx(linepositions[i])+offset+tick.spacing*-drawdirection+labels.offset),
							labels=currlabel)
						}
						
					}
					
				}
			}
		}

#reset xpd if neccesary
    if (!is.null(xpd)){
    	par(xpd=xpd.old)
    }

}

#test commands
#require(beeswarm)
#beeswarm(value~group+subgroup,testdata,ylim=c(1,30),log=T,horizontal=F,pch=16,cex=0.75,col="gray60",bty="L")
#beeswarm(value~group+subgroup,testdata,xlim=c(1,30),log=T,horizontal=T,pch=16,cex=0.75,col="gray60",bty="L")


#connectionlines(value~group+subgroup,testdata,line.position="below.min",draw.ticks=T,drop.ticks=T,xpd=T)

#comps = matrix(data=FALSE,nrow=6,ncol=6)
#comps[1,2]=TRUE
#comps[1,3]=TRUE
#comps[2,4]=TRUE
#comps[3,4]=TRUE

#plabels=matrix(data=FALSE,nrow=6,ncol=6)
#plabels[1,2]='ns'
#plabels[1,3]="*"
#plabels[2,4]="**"
#plabels[3,4]="***"

#connectionlines(value~group+subgroup,testdata,spacing=.1,comparisons="all",labels=plabels,line.position="above.max",at=c(3.5,2:6),drop.ticks=T,lwd=2)
#connectionlines(value~group+subgroup,testdata,comparisons=comps,labels=plabels,line.position="top",horizontal=T,draw.ticks=T,drop.ticks=T,labels.rotate=F,xpd=T)



#connectionlines(value~group+subgroup,testdata,spacing=.2,comparisons=comps,labels=plabels,line.position="top",
#labels.offset=-1/32,offset=-1/4)



### TO DO
#DONE - use real life coordinates to draw, allowing for log scales
#DONE add horizontal
#DONE - add labeling
#DONE draw above/below axes
#DONE add back processing for "at" parameter - this may cause collisions if things get _reordered_!
# vectorize drawing commands
#
#WONTFIX allow specification of labels and comparisons by name not position
