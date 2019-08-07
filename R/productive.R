#' @name productive
#' @aliases productive,IMGThelper-method
#' @rdname productive-methods
#' @docType methods
#' @description select productively recombined VDJ recombination evens from the Junctions file
#' @param x the IMGThelper object
#' @param fname the file to read from default=file.path(x$path, '6_Junction.txt') 
#' @param OK the ids to select (default = function(data) { which(data$V.DOMAIN.Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA")} )
#' @title select only productively recombined VDJ segments
#' @export 
setGeneric('productive', ## Name
	function (x, fname=file.path(x$path, '6_Junction.txt'),  OK= function(data) {
   which( t(data[,'V.DOMAIN.Functionality']) == 'productive' & ! t(data[,'D.GENE.and.allele']) 
      == "" & ! t(data[,'CDR3.IMGT']) == "" & ! t(data[,'CDR3.IMGT'])  
    == "NA")
   } ) { ## Argumente der generischen Funktion
		standardGeneric('productive') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('productive', signature = c ('IMGThelper'),
	definition = function (x, fname='6_Junction.txt', OK = function(data) { 
    which( data[,'V.DOMAIN.Functionality'] == 'productive' & ! data[,'D.GENE.and.allele'] 
      == "" & ! data[,'CDR3.IMGT'] == "" & ! data[,'CDR3.IMGT']== "NA")} ) {
	if ( ! is.null(x$usedObj$productive) ) {
		print ("returning the previousely analyzed productive elements" )
		return ( x$usedObj$productive )
	}	
	data <- open(x, fname )
	if ( ! is.function( OK ) ) {
  		stop(paste(c( "OK needs to be a function to select a list of row ids from the read table:\n", 
        colnames( x), "\n", 
        x[1,]), collapse=", "))
  	}else {
  		ok = OK(data)
  	}
 	productive <- data[ ok ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", 
    "CDR3.IMGT..AA.", "CDR3.IMGT", 'Sequence.ID', 'V.DOMAIN.Functionality',"N1.REGION.nt.nb","N2.REGION.nt.nb" )]
 	
 	if ( nrow(productive) == 0 ){
  		stop("No productive VDJ recombination events detected" )
  	}

  ## now It would be nice to get the mutation status, too:
  data = open( x, '8_V-REGION-nt-mutation-statistics.txt')
  productive <- cbind( productive, "V.REGION.Nb.of.nonsilent.mutations" = data[ok, "V.REGION.Nb.of.nonsilent.mutations"] )

 	#prod<- as.data.frame(stringr::str_split(productive$V.GENE.and.allele, "\\*", n=2))
  #colnames(prod)<-c("V","rest")
  #productive<-cbind('V.Name' = prod$V, productive)
  browser()
  cell_ids <- table(unlist(lapply(stringr::str_split(as.vector(t(productive[,'Sequence.ID'])), '_'), function(x){x[1]})))
  x$cells$Productive = 0
  x$cells$Productive[ match( names(cell_ids), x$cells$cellID) ] = cell_ids
  x$usedObj$productive = productive
 	productive

} )


