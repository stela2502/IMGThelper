#' @name stack
#' @aliases stack,IMGThelper-method
#' @rdname stack-methods
#' @docType methods
#' @description Create the data structure required for the stacked_boxplot
#' @param x the IMGThelper object
#' @param fname the file for the input Junctions default file.path(x$path, '6_Junction.txt')
#' @param splitAt separate the plot at splitAt (ask Stijn of how to phrase that) default= 5
#' @param ... additional option passed to the tree function
#' @title description of function stack
#' @export 
setGeneric('stack', ## Name
	function ( x, fname= '6_Junction.txt', splitAt = 5, ... ) { ## Argumente der generischen Funktion
		standardGeneric('stack') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('stack', signature = c ('IMGThelper'),
	definition = function ( x, fname= '6_Junction.txt', splitAt = 5, ... ) {

    tabout<-tree( x, splitAt = splitAt, ... )
    plo<-tabout[order(tabout$Freq, decreasing=TRUE),]
    plo$Freq<-plo$Freq/sum(plo$Freq)
    plo
  
} )
