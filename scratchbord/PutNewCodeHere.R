compare <- function(x, a) {

GetMedianMutations_4_CRD3 <- function(CDR3, obj ) {
        tab = productive(obj)
        OK = which(is.na(match( as.vector(t(tab[,'CDR3.IMGT'])), CDR3))==F)
        vals = as.numeric(t(tab[OK, 'V.REGION.Nb.of.nonsilent.mutations.V.REGION.Nb.of.nonsilent.mutations']))
        return ( c(  median(vals, na.rm=T), length(OK) ))
}

CDR3_X<- as.vector( t(productive(x)$CDR3.IMGT))
CDR3_A <- as.vector(t(productive(a)$CDR3.IMGT))
reoccuring <- names(which( table( c( names(CDR3_X), names(CDR3_A) ) ) == 2))

if ( length( reoccuring ) == 0 ) {
  stop("There is no overlap between CDR3 elements in both samples")
}

X.reocurring <- t( data.frame(
        lapply( split(reoccuring, reoccuring) , GetMedianMutations_4_CRD3, x )
))

A.reocurring <- t( data.frame(
        lapply( split(reoccuring, reoccuring) , GetMedianMutations_4_CRD3, a )
))

result <- cbind ( X.reocurring, A.reocurring )
rownames(result) = reoccuring
colnames(result) = c(
  paste( 'median' x$name,  'mutations'), 
  paste( x$name, '[n]'), 
  paste('median', a$name,'mutations'), 
  paste( a$name, '[n]')
  )

result
}

