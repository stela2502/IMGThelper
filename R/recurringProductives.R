#' @name recurringProductives
#' @aliases recurringProductives,IMGThelper-method
#' @rdname recurringProductives-methods
#' @docType methods
#' @description restrict the productives to onle thoise that have been sequenced > cutoff times
#' @param x the IMGThelper object
#' @param cutoff select only CDR3 elements used more than cutoff times (default=5)
#' @title description of function recurringProductives
#' @export 
setGeneric('recurringProductives', ## Name
	function ( x, cutoff=5 ) { ## Argumente der generischen Funktion
		standardGeneric('recurringProductives') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('recurringProductives', signature = c ('IMGThelper'),
	definition = function ( x, cutoff=5 ) {

	if ( ! is.null( x$usedObj$recurring )) {
    print ( "returning the stored reocurring CDR3 elements")
    return ( x$usedObj$recurring )
  }
	print ( "accessing productive recombination events" )
	productive = productive(x)

  	tab = matrix( apply( productive[,1:5], 1, paste, collapse="&" ), ncol=1)
  	#colnames(tab)<-c( "lala")

  	plot<-table(tab)
	#  lessthan5<- which(plot <5)
  	morethan5<- which(plot > cutoff )
  
  	OK = which( is.na( match ( tab[,1] , names(morethan5))) == F)

  	if ( length( OK ) == 0 ) {
   	 stop( "This dataset does not contain a single element passing the threshold!" )
  	}
  	recurring = productive[OK ,1:5]

  	colnames(recurring)<-c("V", "D", "J", "CDR3AA", "CDR3nt")
  	x$usedObj$recurring = recurring
  	recurring
} )
