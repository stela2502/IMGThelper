#' @name analyzeCDR3
#' @aliases analyzeCDR3,IMGThelper-method
#' @rdname analyzeCDR3-methods
#' @docType methods
#' @description Stijn - please help here!
#' @param x  the IMGThelper object
#' @param fname  the absolute position of the Junction file to analyze ( default( file.path(x$path, 6_Junction.txt')))
#' @param OK  which rows to process (default which(data$V.DOMAIN.Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA"))
#' @title description of function analyzeCDR3
#' @export 
setGeneric('analyzeCDR3', ## Name
	function (x, fname=file.path(x$path, '6_Junction.txt'), OK=NULL ) { ## Argumente der generischen Funktion
		standardGeneric('analyzeCDR3') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('analyzeCDR3', signature = c ('IMGThelper'),
	definition = function (x, fname=file.path(x$path, '6_Junction.txt'), OK=NULL ) {

  if ( ! file.exists(fname) ){
  	stop("I need a existing file loaction at start up (fname)" )
  }

  data <- read.delim(file= fname )

  if ( is.null(OK) ) {
  	OK = which(data$V.DOMAIN.Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA")
  }
  
  productive <- data[ OK ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT") ]
  
  if ( nrow(productive) == 0 ){
  	stop("No productive VDJ recombination events detected" )
  }

  prod<- as.data.frame(str_split_fixed(productive$V.GENE.and.allele, "\\*", n=2))
  colnames(prod)<-c("V","rest")
  productive<-cbind(prod$V, productive)
  colnames(productive)<-c("a","V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT")
  
  tab<-as.data.frame(paste(productive[,1],productive[,3],productive[,4],productive[,5],productive[,6], sep="&"))
  colnames(tab)<-c( "lala")
  plot<-as.data.frame(table(tab))
  
  number<-(nrow(productive)/numbers[i,2])
  
  #select on reads
  
  morethan5<-subset(plot, plot$Freq >number)
  more2<-as.data.frame(tab[which((match(tab[,1], morethan5[,1])>=1) == TRUE),c( "lala")])
  more<-as.data.frame(str_split_fixed(more2[,1], "&", n=7))
  colnames(more)<-c("V", "D", "J", "CDR3AA", "CDR3nt", "N1.REGION.nt.nb","N2.REGION.nt.nb")
  
  CDR3a <- more$CDR3AA
  tab<-table(CDR3a)
  tabsort<-sort(tab, decreasing=TRUE)
  tabsort<-as.data.frame(tabsort)
  tabsort$Freq<-(tabsort[,2]*100)/sum(tabsort[,2])
  colnames(tabsort)<-c("Reads","Freq")
  tabsort
} )
