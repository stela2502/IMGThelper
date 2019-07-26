#' @name stack
#' @aliases stack,IMGThelper-method
#' @rdname stack-methods
#' @docType methods
#' @description Create the data structure required for the stached_boxplot
#' @param x the IMGThelper object
#' @param fname the file for the input Junctions default file.path(x$path, '6_Junction.txt')
#' @param splitAt separate the plot at splitAt (ask Stijn of how to phrase that) default= 5
#' @title description of function stack
#' @export 
setGeneric('stack', ## Name
	function ( x, fname= file.path(x$path, '6_Junction.txt'), splitAt = 5 ) { ## Argumente der generischen Funktion
		standardGeneric('stack') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('stack', signature = c ('IMGThelper'),
	definition = function ( x, fname= file.path(x$path, '6_Junction.txt'), splitAt = 5 ) {

  productive <- productive(x, fname=fname )
  #unique<-unique(productive)

  tab<-as.data.frame(paste(productive[,1],productive[,2],productive[,3],productive[,4],productive[,5], sep="&"))
  colnames(tab)<-c( "lala")
  plot<-as.data.frame(table(tab))
  number<-(nrow(productive)/numbers[i,2])
  #select on reads
  lessthan5<-subset(plot, plot$Freq < splitAt)
  morethan5<-subset(plot, plot$Freq >= splitAt & plot$Freq>number )
  
  more2<-as.data.frame(tab[which((match(tab[,1], morethan5[,1])>=1) == TRUE),c( "lala")])
  more<-as.data.frame(str_split_fixed(more2[,1], "&", n=5))
  colnames(more)<-c("V", "D", "J", "CDR3AA", "CDR3nt")
  
  #tmoreVJCDR3<-paste(more$V, more$J,more$CDR3AA, sep="&")
  #tabmore<-as.data.frame(table(tmoreVJCDR3))
  tabmore<-as.data.frame(table(more$CDR3AA))
  colnames(tabmore)<-c("CDR3", "Freq")
  #treemore<-as.data.frame(str_split_fixed(tabmore[,1], "&", n=3))
  #treemoreV<-as.data.frame(treemore[,1])
  #treemoreVJ<-as.data.frame(paste(treemore[,1], treemore[,2]))
  #treemoreVJCDR3<-as.data.frame(paste(treemore[,1], treemore[,2], treemore[,3]))
  #treemore<-cbind(treemoreV, treemoreVJ, treemoreVJCDR3, treemore[,3], as.data.frame(tabmore[,2]))
  #colnames(treemore)<-c("V", "VJ", "VJCDR3", "CDR3","freq")

  #tabmore$color<-ut[which(is.na(match(ut$CDR3,tabmore$CDR3))==FALSE), c("color")]
  tabout<-left_join(tabmore, treecol, by="CDR3")
  plo<-tabout[order(tabout$Freq, tabout$color, decreasing=TRUE),]
  plo$Freq<-plo$Freq/sum(plo$Freq)
  plo
  
} )
