#' @name VJcombination
#' @aliases VJcombination,IMGThelper-method
#' @rdname VJcombination-methods
#' @docType methods
#' @description Function to create VJ_usage.csv files used for the Chord diagrams
#' @param x the IMGThelper object
#' @param splitAt separate the plot at splitAt (ask Stijn of how to phrase that) default= 5
#' @title Ask Stijn about this function!
#' @export 
setGeneric('VJcombination', ## Name
	function (x, splitAt = 5  ) { ## Argumente der generischen Funktion
		standardGeneric('VJcombination') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('VJcombination', signature = c ('IMGThelper'),
	definition = function (x, splitAt = 5  ) {
  
  VHorder = read.delim( file = system.file("extdata", "IGHVorder3.txt", package = "IMGThelper") )
  
   more = recurringProductives( x, cutoff= splitAt)


  a<-as.data.frame(more$V)
  b<-as.data.frame(more$J)
  proddy<-cbind(a,b)
  
  Vs <- VHorder$Vsegments
  
  Vma <- matrix(0, ncol=4, nrow=length(Vs))
  Js <- paste( 'J', 1:4, sep='')
  for ( l in 1:nrow(proddy) ) {
    co <- NULL
    for ( t in 1:4 ) {
      if ( length( grep ( Js[t], proddy[l,2] ) ) > 0 ){
        co <- t
        break
      }
    }
    if ( is.null(co) ) {
      stop( paste("the J",proddy[l,2], "could not be converted to J1-4 - why?") )
    }
    Vma[match( proddy[l,1], Vs ), co ] <- Vma[match( proddy[l,1], Vs ), co ] +1
  }
  rownames(Vma) <- Vs
  colnames(Vma) <- Js
  Vma
} )
