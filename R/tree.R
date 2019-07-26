#' @name tree
#' @aliases tree,IMGThelper-method
#' @rdname tree-methods
#' @docType methods
#' @description create the data table used for the treemap plots (based on 6_Junctions.txt)
#' @param x the IMGThelper object
#' @title description of function tree
#' @export 
setGeneric('tree', ## Name
	function ( x ) { ## Argumente der generischen Funktion
		standardGeneric('tree') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('tree', signature = c ('IMGThelper'),
	definition = function ( x ) {

	productive = productive(x)

  #unique<-unique(productive)
  tab<-as.data.frame(paste(productive[,1],productive[,2],productive[,3],productive[,4],productive[,5], sep="&"))
  colnames(tab)<-c( "lala")
  plot<-as.data.frame(table(tab))
  number<-(nrow(productive)/numbers[i,2])
  #select on reads
  lessthan5<-subset(plot, plot$Freq <5)
  morethan5<-subset(plot, plot$Freq >=5 & plot$Freq>number )
  
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
  tabout
} )
