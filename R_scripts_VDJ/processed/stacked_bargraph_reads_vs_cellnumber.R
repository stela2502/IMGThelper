library(treemap)
library(stringr)
library(dplyr)


files <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*_Junction.txt", full.names=T, recursive=FALSE)

treecol<-read.delim(file="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/treemapcolors5.csv", sep=",")
treecol<-treecol[,2:3]

stack <- function( fname=fname  ) {
  data <- read.delim(file= files[i]   )
  productive <- data[which(data$V.DOMAIN.Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA") ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT") ]
  #unique<-unique(productive)
  tab<-as.data.frame(paste(productive[,1],productive[,2],productive[,3],productive[,4],productive[,5], sep="&"))
  colnames(tab)<-c( "lala")
  plot<-as.data.frame(table(tab))
  number<-(nrow(productive)/numbers[i,2])
  #select on reads
  lessthan5<-subset(plot, plot$Freq <5)
  morethan5<-subset(plot, plot$Freq >5 & plot$Freq>number )
  
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
  
}




##run the loop

for(i in seq_along(files)){
  
  ##clonalheatmapVHJH
  out <- stack(files[i])
  woopwoop<-length(strsplit(files[i],"\\/")[[1]])
  name<-strsplit(files[i],"\\/")[[1]][woopwoop]
  name2<-strsplit(name,"\\.")[[1]][1]
 
  out.fname <- paste("/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/",name2,"_Stacked_graph_Run4_freq.pdf", sep="")
 
 
  figure.fname<-paste(name2, "with more than 5 reads- list with", clones, "clones in treemap", paste= " ")
  
  pdf(out.fname, width=7, height=7)
  barplot(as.matrix(out[,2]), col=out[,3])
  dev.off()
  
  out<-NULL
  name<-NULL
  name2<-NULL
  name3<-NULL
  out.fname<-NULL
  figure.fname<-NULL
  
}
