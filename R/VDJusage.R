#' @name VDJusage
#' @aliases VDJusage,IMGThelper-method
#' @rdname VDJusage-methods
#' @docType methods
#' @description returns the cout for each V, D and J segment in the recurringProductives
#' @param x the IMGThelper object
#' @param minCount the minimal reoccurance value for a single VDJ recombination event (default = 0)
#' @title description of function VDJusage
#' @export 
setGeneric('VDJusage', ## Name
	function ( x, minCount=0  ) { ## Argumente der generischen Funktion
		standardGeneric('VDJusage') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('VDJusage', signature = c ('IMGThelper'),
	definition = function ( x, minCount=0  ) {

  more <- recurringProductives(x,cutoff= minCount )
  # make sure we do not have more...
  more = more[, c("V", "D", "J", "CDR3AA", "CDR3nt", "N1.REGION.nt.nb","N2.REGION.nt.nb")]
  
  prodV<- as.data.frame(str_split_fixed(more$V, "\\*", n=2))
  prodD<- as.data.frame(str_split_fixed(more$D, "\\*", n=2))
  prodJ<- as.data.frame(str_split_fixed(more$J, "\\*", n=2))
  
  colnames(prodV)<-c("V","rest")
  colnames(prodD)<-c("D","rest")
  colnames(prodJ)<-c("J","rest")

  return (  
    lapply( list("V Freq" = prodV ,"D Freq" = prodD ,"J Freq" = prodJ ),
      function(dat) { table(dat[,1]) } ) 
    ) 
  }
  )

