#' @name compare
#' @aliases compare,IMGThelper-method
#' @rdname compare-methods
#' @docType methods
#' @description comapre two IMGT result datasets
#' @param x one IMGThelper object
#' @param a the other IMGThelper object
#' @title description of function compare
#' @export 
setGeneric('compare', ## Name
	function (x, a) { ## Argumente der generischen Funktion
		standardGeneric('compare') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('compare', signature = c ('IMGThelper', 'IMGThelper'),
	definition = function (x, a) {

GetMedianMutations_4_CRD3 <- function(CDR3, tab ) {
        tab 
        OK = which(is.na(match( as.vector(t(tab[,'CDR3.IMGT'])), CDR3))==F)
        vals = as.numeric(t(tab[OK, 'V.REGION.Nb.of.nonsilent.mutations.V.REGION.Nb.of.nonsilent.mutations']))
        return ( c(  median(vals, na.rm=T), length(OK) ))
}

CDR3_X<- as.vector( t(productive(x)$CDR3.IMGT))
CDR3_A <- as.vector(t(productive(a)$CDR3.IMGT))
reoccuring <- names(which( table( c( names(table(CDR3_X)), names(table(CDR3_A)) ) ) == 2))

if ( length( reoccuring ) == 0 ) {
  stop("There is no overlap between CDR3 elements in both samples")
}

X.reocurring <- t( data.frame(
        lapply( split(reoccuring, reoccuring) , GetMedianMutations_4_CRD3, productive(x) )
))

A.reocurring <- t( data.frame(
        lapply( split(reoccuring, reoccuring) , GetMedianMutations_4_CRD3, productive(a) )
))

result <- cbind ( X.reocurring, A.reocurring )
rownames(result) = reoccuring
colnames(result) = c(
  paste( 'median', x$name,  'mutations'), 
  paste( x$name, '[n]'), 
  paste('median', a$name,'mutations'), 
  paste( a$name, '[n]')
  )

result
} )
