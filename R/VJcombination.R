#' @name VJcombination
#' @aliases VJcombination,IMGThelper-method
#' @rdname VJcombination-methods
#' @docType methods
#' @description Function to create VJ_usage.csv files used for the Chord diagrams
#' @param x the IMGThelper object
#' @param fname the file to parse default=file.path(x$path, '6_Junction.txt') 
#' @title Ask Stijn about this function!
#' @export 
setGeneric('VJcombination', ## Name
	function (x, fname=file.path(x$path, '6_Junction.txt')  ) { ## Argumente der generischen Funktion
		standardGeneric('VJcombination') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('VJcombination', signature = c ('IMGThelper'),
	definition = function (x, fname=file.path(x$path, '6_Junction.txt')  ) {
  
  VHorder = read.delim(file=  system.file("extdata", "IGHVorder3.txt", package = "IMGThelper") )
  
  tab<-as.data.frame(paste(productive[,1],productive[,3],productive[,4],productive[,5],productive[,6], sep="&"))

  colnames(tab)<-c( "lala")
  plot<-as.data.frame(table(tab))
  number<-(nrow(productive)/numbers[i,2])
  #select on reads
  lessthan5<-subset(plot, plot$Freq <5)
  morethan5<-subset(plot, plot$Freq >5 & plot$Freq>number )
  
  more2<-as.data.frame(tab[which((match(tab[,1], morethan5[,1])>=1) == TRUE),c( "lala")])
  more<-as.data.frame(str_split_fixed(more2[,1], "&", n=5))
  colnames(more)<-c("V", "D", "J", "CDR3AA", "CDR3nt")
  
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
