#' @name tree
#' @aliases tree,IMGThelper-method
#' @rdname tree-methods
#' @docType methods
#' @description create the data table used for the treemap plots (based on 6_Junctions.txt)
#' @param x the IMGThelper object
#' @param splitAt separate the plot at splitAt (ask Stijn of how to phrase that) default= 5
#' @param color a global CDR3 vs color table ( default NULL -> rainbow colors)
#' @title description of function tree
#' @export 
setGeneric('tree', ## Name
	function ( x, splitAt = 5, color=NULL ) { ## Argumente der generischen Funktion
		standardGeneric('tree') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('tree', signature = c ('IMGThelper'),
	definition = function ( x, splitAt = 5, color=NULL ) {
    if ( ! is.null(x$usedObj$treeDat )) {
      return (x$usedObj$treeDat )
    }

    more = recurringProductives( x, cutoff= splitAt)
  
  tabmore<-as.data.frame(table(more$CDR3AA))
  colnames(tabmore)<-c("CDR3", "Freq")

  x$usedObj$CDR3elements = unique (tabmore[,'CDR3'], x$usedObj$CDR3elements)

  if ( is.null(color) ){
    color = data.frame( 'CDR3' = x$usedObj$CDR3elements, color= sample( rainbow(length(x$usedObj$CDR3elements))
      , length(x$usedObj$CDR3elements) ))
  }

  if ( is.null (x$usedObj$color)) {
    x$usedObj$color = color
  }else if( ! nrow(x$usedObj$color) == length(x$usedObj$CDR3elements)){
    x$usedObj$color = color
  }else {
    color = x$usedObj$color
  }
  
  tabout<-left_join(tabmore, color, by="CDR3")
  x$usedObj$treeDat = tabout
  tabout
} )
