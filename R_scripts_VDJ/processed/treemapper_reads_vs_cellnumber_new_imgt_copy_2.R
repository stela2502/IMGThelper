library(treemap)
library(stringr)
library(dplyr)

files <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*_Junction.txt", full.names=T, recursive=FALSE)
numbers2 <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*.csv", full.names=T, recursive=FALSE)
numbers<-read.delim(numbers2, sep=",")

treecol<-read.delim(file="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/treemapcolors5.csv", sep=",")
treecol<-treecol[,2:3]

tree <- function( fname=fname  ) {
  data <- read.delim(file= fname)
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
  tabout
  
  
}


#pdf("CD5hi.pdf", width=7, height=7)
#treemap(tabout, index=c("CDR3"), vSize="Freq", vColor="color", type="color",algorithm="squarified",fontsize.labels=10,sortID="size",title="CD5hi", border.lwds=0.5, drop.unused.levels = FALSE)
#dev.off()
#pdf("CD5int.pdf", width=7, height=7)
#treemap(tabout, index=c("CDR3"), vSize="Freq", vColor="color", type="color",algorithm="squarified",fontsize.labels=10,sortID="size",title="CD5int", border.lwds=0.5, drop.unused.levels = FALSE)
#dev.off()
#pdf("CD5low.pdf", width=7, height=7)
#treemap(tabout, index=c("CDR3"), vSize="Freq", vColor="color", type="color",algorithm="squarified",fontsize.labels=10,sortID="size",title="CD5low", border.lwds=0.5, drop.unused.levels = FALSE)
#dev.off()
#pdf("CD5neg.pdf", width=7, height=7)
#treemap(tabout, index=c("CDR3"), vSize="Freq", vColor="color", type="color",algorithm="squarified",fontsize.labels=10,sortID="size",title="CD5neg", border.lwds=0.5, drop.unused.levels = FALSE)
#dev.off()
#pdf("WT2nd4.pdf", width=7, height=7)
#treemap(tabout, index=c("CDR3"), vSize="Freq", vColor="color", type="color",algorithm="squarified",fontsize.labels=10,sortID="size",title="WT2nd4", border.lwds=0.5, drop.unused.levels = FALSE)
#dev.off()
#pdf("L282nd2.pdf", width=7, height=7)
#treemap(tabout, index=c("CDR3"), vSize="Freq", vColor="color", type="color",algorithm="squarified",fontsize.labels=10,sortID="size",title="L282nd2", border.lwds=0.5, drop.unused.levels = FALSE)
#dev.off()
#pdf("Intact2nd5.pdf", width=7, height=7)
#treemap(tabout, index=c("CDR3"), vSize="Freq", vColor="color", type="color",algorithm="squarified",fontsize.labels=10,sortID="size",title="Intact2nd5", border.lwds=0.5, drop.unused.levels = FALSE)
#dev.off()



##run the loop

for(i in seq_along(files)){
  
  ##clonalheatmapVHJH
  out <- tree(files[i])
  woopwoop<-length(strsplit(files[i],"\\/")[[1]])
  name<-strsplit(files[i],"\\/")[[1]][woopwoop]
  name2<-strsplit(name,"\\.")[[1]][1]
  clones<-nrow(out)
  out.fname <- paste("/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/",name2,"_treemap.pdf", sep="")
  out.title<-paste(name2, clones, "in this treemap")
 
  figure.fname<-paste(name2, "list with", clones, "clones in treemap", paste= " ")
  
  pdf(out.fname, width=7, height=7)
  treemap(out, index=c("CDR3"), vSize="Freq", vColor="color", type="color",algorithm="squarified",fontsize.labels=10,sortID="size",title=out.title, border.lwds=0.5, drop.unused.levels = FALSE)
  dev.off()
  
  out<-NULL
  name<-NULL
  name2<-NULL
  name3<-NULL
  out.fname<-NULL
  figure.fname<-NULL
  
}
