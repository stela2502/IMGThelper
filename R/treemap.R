#' @name treemap
#' @aliases treemap,IMGThelper-method
#' @rdname treemap-methods
#' @docType methods
#' @description draw a treemap of clone sizes
#' @param x the IMGThelper object
#' @param name the experiment this VDJ analysis came from
#' @param ofile the pdf file name
#' @title description of function treemap
#' @export 
setGeneric('treemap', ## Name
	function ( x, name, ofile ) { ## Argumente der generischen Funktion
		standardGeneric('treemap') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('treemap', signature = c ('IMGThelper'),
	definition = function ( x, name, ofile ) {

	  out <- tree( x )

    clones<-nrow(out)

    out.title<-paste(name,";", clones, "in this treemap")
 
    figure.fname<-paste(name2, "list with", clones, "clones in treemap", paste= " ")
  
    pdf( ofile, width=7, height=7)
    treemap(out, index=c("CDR3"), vSize="Freq", vColor="color", type="color",
  	algorithm="squarified",fontsize.labels=10,sortID="size",title=out.title, 
  	border.lwds=0.5, drop.unused.levels = FALSE)
    dev.off()
  
} )
