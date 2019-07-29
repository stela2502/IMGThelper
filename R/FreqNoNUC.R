#' @name FreqNoNNuc
#' @aliases NoNNuc,IMGThelper-method
#' @rdname NoNNuc-methods
#' @docType methods
#' @description calculate the frequency of VDJ segments not containing N nucleotides
#' @param x the IMGThelper object
#' @param minCount the minimal occurance of one VDJ element ( default = 0)
#' @title description of function NoNNuc
#' @export 
setGeneric('FreqNoNNuc', ## Name
	function ( x, minCount=0 ) { ## Argumente der generischen Funktion
		standardGeneric('FreqNoNNuc') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('FreqNoNNuc', signature = c ('IMGThelper'),
	definition = function ( x, minCount=0 ) {

    productive <- productive(x)

    NoNNuc = 
  number<-nrow(productive)
  
  tab = apply( productive[,c("V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT", 
    "N1.REGION.nt.nb","N2.REGION.nt.nb")], 1, paste, collapse="&" )
  t=table(tab[,1])
  morethan5<-which(table(t) > minCount ) 

  tabSelected<- productive[ which(is.na(match( names(t)[morethan5], tab[,1] ))==F),]  
  
  Nonuc = which( apply( productive[tabSelected, c("N1.REGION.nt.nb","N2.REGION.nt.nb")],1,sum, na.rm = TRUE ) == 0) 
  FreqNonuc <- length(Nonuc)*100/nrow(tabSelected)
  FreqNonuc
} )
