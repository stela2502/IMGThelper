library(stringr)
library(reldist)


files <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*_Junction.txt", full.names=T, recursive=FALSE)
numbers2 <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*.csv", full.names=T, recursive=FALSE)
numbers<-read.delim(numbers2, sep=",")


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
  more<-as.data.frame(str_split_fixed(more2[,1], "&", n=7))
  colnames(more)<-c("V", "D", "J", "CDR3AA", "CDR3nt", "N1.REGION.nt.nb","N2.REGION.nt.nb")
  
  CDR3a <- more$CDR3AA
  tab<-table(CDR3a)
  tabsort<-sort(tab, decreasing=TRUE)
  tabsort<-as.data.frame(tabsort)
  tabsort$Freq<-(tabsort[,2]*100)/sum(tabsort[,2])
  colnames(tabsort)<-c("Reads","Freq")
  tabsort
}
for(i in seq_along(files)){
  
  ##clonalheatmapVHJH
  out <- analyzeCDR3(files[i])
  woopwoop<-length(strsplit(files[i],"\\/")[[1]])
  name<-strsplit(files[i],"\\/")[[1]][woopwoop]
  name2<-strsplit(name,"\\.")[[1]][1]
  
  #out.fname <- paste("/Users/med-sve/Desktop/",name2,"_violin.pdf", sep="")
  ##out.fname <- paste("/Users/med-sve/Desktop/",name2,"_dots.pdf", sep="")
  #p <- ggplot(out, aes(1, Freq)) + geom_violin() + scale_y_log10(limits = c(0.001, 100))
  #p<-ggplot(out, aes(x = 1, y = Freq)) + geom_dotplot(binaxis = "y", stackdir = "center", dotsize=7, binwidth = 0.4) + coord_cartesian(ylim = c(0, 100))
  #pdf(out.fname, width=4, height=7)
  #p<-sd(out$Freq, na.rm = FALSE)/mean(out$Freq)
  #pr<-paste("sample", name2, "has a CV off", p)
  #p<-sd(out$Freq, na.rm = FALSE)
  #pr<-paste("sample", name2, "has an SD off", p)
  p<-gini(out$Freq, weights=rep(1, length=length(out$Freq)))
  pr<-paste("sample", name2, "has a gini index off", p)
  print(pr)
  #dev.off()
  
  
}
