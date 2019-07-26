#' @name stacked_bargraph
#' @aliases stacked_bargraph,IMGThelper-method
#' @rdname stacked_bargraph-methods
#' @docType methods
#' @description create a stacked_bargraph based on the 6_Juntions.txt file
#' @param x the IMGThelper object
#' @param ofile the outfile (pdf)
#' @param name the name of this sample
#' @param ... options for the stack function
#' @title description of function stacked_bargraph
#' @export 
setGeneric('stacked_bargraph', ## Name
	function (x, ofile, name, ... ) { ## Argumente der generischen Funktion
		standardGeneric('stacked_bargraph') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('stacked_bargraph', signature = c ('IMGThelper'),
	definition = function (x, ofile, name, ... ) {
  	out <- stack( x, ... )
    figure.fname<-name 
  
  	pdf(ofile, width=7, height=7)
  	barplot(as.matrix(out[,2]), col=as.vector(out[,'color']))
  	dev.off()
  	invisible(x)
} )
