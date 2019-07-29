
NoNNuc <- function( fname ) {
  data <- read.delim(file=  fname ) 
  productive <- data[which(data$V.DOMAIN.Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA") ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT", "N1.REGION.nt.nb","N2.REGION.nt.nb") ]
  number<-(nrow(productive)/numbers[i,2])
  
  tab<-as.data.frame(paste(productive[,1],productive[,2],productive[,3],productive[,4],productive[,5],productive[,6],productive[,7], sep="$"))
  colnames(tab)<-c("a")
  a<-as.data.frame(table(tab[,1]))
  morethan5<-a[which(a[,2]>number), c(1,2)]
  tab5<-as.data.frame(tab[!is.na(match(tab[,1], morethan5[,1])),c(1)])
  
  
  #d<-read.table(text = as.character(tab5[,1]), sep = "$", colClasses = "character")
  N1<-as.data.frame(sapply(strsplit(as.character(tab5[,1]), '\\$'), '[', 6))
  N2<-as.data.frame(sapply(strsplit(as.character(tab5[,1]), '\\$'), '[', 7))
  nuc<-cbind(N1,N2)
  colnames(nuc)<-c("N1","N2")
  Nonuc<-as.data.frame(nuc[which(nuc$N1 == 0 & nuc$N2 == 0), c("N1","N2")])
  FreqNonuc<-(nrow(Nonuc)*100/nrow(nuc))
  FreqNonuc
}

for(i in seq_along(files)){
  woopwoop<-length(strsplit(files[i],"\\/")[[1]])
  name<-strsplit(files[i],"\\/")[[1]][woopwoop]
  name2<-strsplit(name,"\\.")[[1]][1]
  name3<-strsplit(name2,"\\_")[[1]][1]
  
  out.fname <- paste("/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/",name2,"nNuclueotides.txt")
  out<-NoNNuc(files[i])
  
  write.csv(out, out.fname)
  
}

