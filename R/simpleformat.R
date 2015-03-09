# A simple plain text markup translator that outputs R plotmath expressions.
# Does some common things people shouldn't need to learn plotmath to do
# Accepts a string, returns a string that can be parse()'d to make the expression.

# this is obviously not intended for math, it's intended to make it easier to hijack the plotmath engine
# to do inintended things.

# Input Syntax:
# *italics*
# **bold**
# ***bolditalic***
# ^super^
# ..sub..
# _underline_

# accepts a single instance of \n for newline (remember to double up the \ in scripts or command window)
# it can only handle one, since this is a hack that uses atop()
# if you provide a list as input it will interpret each item as a seperate line. A character vector will produce a vector of formatted lines.

# special characters *, _, \, and ^, need escaping with \ (remember R will eat one of your \s so you'll need
# to double it in scripts or the command window). three . in succession are reserved symbols, 
# break them up as so .\\.\\.\\.


riffle <- function (a,b) { 
 	n <- min(length(a),length(b)) 
 	p1 <- as.vector(rbind(a[1:n],b[1:n])) 
 	p2 <- c(a[-(1:n)],b[-(1:n)]) 
 	c(p1,p2) 
} 

add.between <- function(stringvector,addbetween) {
	addbetween = rep(addbetween,length(stringvector)-1)
	return(riffle(stringvector,addbetween))
}

split.and.add.between <- function(stringvector,remove,replacement,fixed) {
	
	listofsplits = strsplit(stringvector,remove,fixed)
	if (length(unlist(listofsplits)) == length(stringvector)) {
		return(stringvector) #we didn't do anything!
	} else {
		return(unlist(lapply(listofsplits,add.between,replacement)))
	}	
}

split.before.index <- function(string,index) {
	if (index==1){
		return(string)
	} else {
		return(c(
			substring(string,1,index-1),
			substring(string,index,nchar(string))
		))
	}

}

split.after.index <- function(string,index) {
	if (index==length(string)){
		return(string)
	} else {
		return(c(
			substring(string,1,index),
			substring(string,index+1,nchar(string))
		))
	}

}

split.by.gregexpr.output <- function(string,matches) {
	
	startcoordinates <- matches
	attributes(startcoordinates) <- NULL
	
	if (matches[1]==-1) {return(string)} #bailout if no match
	
	finishcoordinates <- startcoordinates+attr(matches,"match.length")
	breakpoints <- riffle(startcoordinates,finishcoordinates)
	
	breakpoints <- breakpoints[(breakpoints>1) & (breakpoints<=nchar(string))]
	
	out <- NULL
	
	currstring <- string
	while (length(breakpoints > 0)) {
		out <- c(out,split.before.index(currstring,breakpoints[1])[1])
		currstring <- split.before.index(currstring,breakpoints[1])[2]
		breakpoints <- breakpoints - breakpoints[1] + 1
		breakpoints <- breakpoints[-1]
	}
	out <- c(out, currstring)
	
	
	return(na.omit(out))
	
}

split.before.and.after <- function(stringvector,pattern) {
	listofmatches <- gregexpr(pattern,stringvector)
	
	
	splits <- mapply(split.by.gregexpr.output,stringvector,listofmatches)
		
	return(as.vector(unlist(splits)))

}

#' @export
simpleFormat <- function(string) {
	chunks = string
	if(is.list(chunks)) {
		chunks<-lapply(chunks,simpleFormat)
		out = ""
		for (i in chunks[1:length(chunks)-1]) {
			out = paste0(out,"atop(",i[[1]],",")
		}
		closeparens = paste(rep(")",length(chunks)-1),collapse="")
		out = paste0(out,chunks[[length(chunks)]],closeparens)
		return(out)
	}	

	#if not a list but length >1
	if(length(chunks)>1) {return(as.vector(sapply(chunks,simpleFormat)))}

	chunks <- unlist(strsplit(chunks,'\\n',fixed=T))
	if(length(chunks)>1) {
			return(simpleFormat(as.list(chunks)))} # handle that newline
	
	chunks <- split.and.add.between(chunks,'\\*','*', fixed=T)
	chunks <- split.and.add.between(chunks,'\\_','_', fixed=T)
	chunks <- split.and.add.between(chunks,'\\.','\\.', fixed=T) #. isnt special but .. & ... are, this takes care of it		chunks <- split.and.add.between(chunks,'\\^','^', fixed=T)

	
	
	


	
	chunks <- split.before.and.after(chunks,'(\\.\\.)(.*?)(\\.\\.)')

	chunks <- gsub('^(\\.\\.)(.*?)(\\.\\.)$','...phantom\\(\\)\\[~~~...\\2...\\~~~]...', chunks) #use triple dots to mark non-quoted or already quoted and triple tildes to mark non-quoted without need for concatenation marker

	chunks <- paste(chunks,collapse="")



	#after this we ignore ..., adding it to nonquoted fragments.  
	
	chunks <- split.before.and.after(chunks,'(\\*\\*\\*)(.*?)(\\*\\*\\*)')
	chunks <- gsub('^(\\*\\*\\*)(.*?)(\\*\\*\\*)$','...bolditalic\\(~~~...\\2...~~~)...', chunks)
	chunks <- paste(chunks,collapse="")
	

	chunks <- split.before.and.after(chunks,'(\\*\\*)(.*?)(\\*\\*)')
	chunks <- gsub('^(\\*\\*)(.*?)(\\*\\*)$','...bold\\(~~~...\\2...~~~)...', chunks)
	chunks <- paste(chunks,collapse="")


#	cat(rbind(chunks,rep("\n",length(chunks))))
	
	
	chunks <- split.before.and.after(chunks,'(\\*)(.*?)(\\*)')
	chunks <- gsub('^(\\*)(.*?)(\\*)$','...italic\\(~~~...\\2...~~~)...', chunks)
	chunks <- paste(chunks,collapse="")


	chunks <- split.before.and.after(chunks,'(\\^)(.*?)(\\^)')
	chunks <- gsub('^(\\^)(.*?)(\\^)$','...phantom\\(\\)\\^\\{~~~...\\2...~~~}...', chunks)
	chunks <- paste(chunks,collapse="")

	chunks <- split.before.and.after(chunks,'(\\_\\_)(.*?)(\\_\\_)')
	chunks <- gsub('^(\\_\\_)(.*?)(\\_\\_)$','...underline\\(~~~...\\2...~~~)...', chunks)
	chunks <- paste(chunks,collapse="")


	priorlength = -1
	while (length(chunks) != priorlength) {
		priorlength <-length(chunks)
		chunks <- split.before.and.after(chunks,'(\\.\\.\\.){1}?(.+?)(\\.\\.\\.){1}?') #break everything apart by ... pairs
		#for some reason this fails to break it apart completely. we need to run multiple times...
	}

	#	cat(rbind(chunks,rep("\n",length(chunks))))

	
	hasdots <- grepl('^(\\.\\.\\.){1}?+(.*?)(\\.\\.\\.){1}?+$',chunks)
	
	#finally strip dots
	chunks <- gsub('^(\\.\\.\\.){1}?+(.*?)(\\.\\.\\.){1}?+$','\\2', chunks)

	#quote the ones that didnt have dots
	chunks[!hasdots] <- paste0('"',chunks[!hasdots],'"')
	
	hasleadingtildes <- grepl('^(\\~\\~\\~)+(.*?)$',chunks)
	hastrailingtildes <- grepl('^(.*?)(\\~\\~\\~)+$',chunks)
	isfirst <- c(TRUE,rep(FALSE,length(chunks)-1))
	islast <- c(rep(FALSE,length(chunks)-1),TRUE)
	followedbytildes <- c(hasleadingtildes[2:length(chunks)],FALSE)
	precededbytildes <- c(FALSE,hastrailingtildes[1:length(chunks)-1])

	#add * to guys who need concatenation markers
	leadingstars <- !(isfirst | hasleadingtildes | precededbytildes)
	trailingstars <- !(islast | hastrailingtildes | followedbytildes)

	#suppress multiple stars
	precededbystar <- c(FALSE, trailingstars[1:length(chunks)-1])
	leadingstars = leadingstars & !precededbystar

	
	if(length(chunks)>1) { #suppress if we didn't split the string at all
		chunks[leadingstars] <- paste0("*", chunks[leadingstars]) 
		chunks[trailingstars] <- paste0(chunks[trailingstars],"*")
	}


	#strip tildes
	chunks <- gsub('^(\\~\\~\\~)*(.*?)(\\~\\~\\~)*$','\\2', chunks)
	
	#unescape .s since R won't allows escaped dots...
	chunks <- gsub('\\\\\\.','.', chunks) #wow that's a lot of backslashes to send one backslash...

	
	line = paste(chunks,collapse="")
	return(line)

}

# a demo
# x = simpleformat("*This simplifies^some of **the** ^*problems with plotmath. in ***..tr..__u__^th^*** we can do **most** of the ***useful*** things\\.\\.\\. \\nWe can also do a __single__ newline. symbols εηθλθ can be unicode")
# plot(c(-1,1),c(-1,1),type="n")
# text(0,0,parse(text=x),cex=0.75)

#>> cat(x)
#> atop(italic("This simplifies"*phantom()^{"some of "*bold("the")*" "})*"problems with plotmath. in "*bolditalic(phantom()["tr"]*underline("u")*phantom()^{"th"})*" we can do "*bold("most")*" of the "*bolditalic("useful")*" things... ","We can also do a "*underline("single")*" newline. symbols εηθλθ can be unicode")