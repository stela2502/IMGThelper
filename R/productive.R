#' @name productive
#' @aliases productive,IMGThelper-method
#' @rdname productive-methods
#' @docType methods
#' @description select productively recombined VDJ recombination evens from the Junctions file
#' @param x the IMGThelper object
#' @param fname the file to read from default=file.path(x$path, '6_Junction.txt') 
#' @title select only productively recombined VDJ segments
#' @export 
setGeneric('productive', ## Name
	function (x, fname=file.path(x$path, '6_Junction.txt') ) { ## Argumente der generischen Funktion
		standardGeneric('productive') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('productive', signature = c ('IMGThelper'),
	definition = function (x, fname=file.path(x$path, '6_Junction.txt') ) {
	data <- read.delim(file= fname )

  	OK = which(data$V.DOMAIN.Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA")
 	productive <- data[ OK ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT") ]
 	prod<- as.data.frame(str_split_fixed(productive$V.GENE.and.allele, "\\*", n=2))
  colnames(prod)<-c("V","rest")
  productive<-cbind(prod$V, productive)
  colnames(productive)<-c("a","V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT")
 	productive
} )
