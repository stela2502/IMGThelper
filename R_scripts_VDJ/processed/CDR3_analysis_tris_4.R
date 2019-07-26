library(stringr)
#load data
analyzeCDR3 <- function( fname ) {
  data <- read.delim(file= fname )
  productive <- data[which(data$V.DOMAIN.Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA") ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT") ]
  
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
  more<-as.data.frame(str_split_fixed(more2[,1], "&", n=5))
  colnames(more)<-c("V", "D", "J", "CDR3AA", "CDR3nt")
  
  CDR3a <- more$CDR3AA
  tab<-table(CDR3a)
  tabsort<-sort(tab, decreasing=TRUE)
  tabsort<-as.data.frame(tabsort)
  tabsort$Freq<-(tabsort[,2]*100)/sum(tabsort[,2])
  colnames(tabsort)<-c("Reads","Freq")
  tabsort
}

##select folder to loop

files <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*_Junction.txt", full.names=T, recursive=FALSE)

##run the loop

for(i in seq_along(files)){
  
  ##clonalheatmapVHJH
  out <- analyzeCDR3(files[i])
  woopwoop<-length(strsplit(files[i],"\\/")[[1]])
  name<-strsplit(files[i],"\\/")[[1]][woopwoop]
  name2<-strsplit(name,"\\.")[[1]][1]
  clones<-nrow(out)
  out.fname <- paste("/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/",name2,"_CDR3list.csv", sep="")
  write.csv(out, out.fname)
  
}
