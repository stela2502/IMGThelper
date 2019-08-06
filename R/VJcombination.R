#' @name VJcombination
#' @aliases VJcombination,IMGThelper-method
#' @rdname VJcombination-methods
#' @docType methods
#' @description Function to create VJ_usage.csv files used for the Chord diagrams
#' @param x the IMGThelper object
#' @param cells the cell IDs to look into (default NULL == all)
#' @param cutoff select only CDR3 elements used more than cutoff times (default=5; see recurringProductives)
#' @title Ask Stijn about this function!
#' @export 
setGeneric('VJcombination', ## Name
	function (x, cells=NULL, cutoff=5 ) { ## Argumente der generischen Funktion
		standardGeneric('VJcombination') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('VJcombination', signature = c ('IMGThelper'),
	definition = function (x,  cells=NULL , cutoff=5 ) {
  
  VHorder = read.delim( file = system.file("extdata", "IGHVorder3.txt", package = "IMGThelper") )
  
  more = recurringProductives( x, cutoff= cutoff, cells= cells)

  V = unlist( lapply( more$V , function(a) { stringr::str_split( a, ',')[[1]][1]}) ) 
  V = unlist( lapply( V , function(a) { stringr::str_replace( a, '\\*.*', '' ) }) ) 

  J = unlist( lapply( more$J , function(a) { stringr::str_split( a, ',')[[1]][1]}) ) 
  J = unlist( lapply( J , function(a) { stringr::str_replace( a, '\\*.*', '' ) }) ) 

  x$usedObj$vjCombination = table( V, J )
  x$usedObj$vjCombination

} )
