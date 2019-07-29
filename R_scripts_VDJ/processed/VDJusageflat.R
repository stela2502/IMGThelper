library(stringr)
library(gplots)

files <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*_Junction.txt", full.names=T, recursive=FALSE)
numbers2 <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*.csv", full.names=T, recursive=FALSE)
numbers<-read.delim(numbers2, sep=",")


VDJusage_flat <- function( fname=fname  ) {
  data <- read.delim(file= fname )
  productive <- data[which(data$Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA") ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT") ]
  
  tab<-as.data.frame(paste(productive[,1],productive[,2],productive[,3],productive[,4],productive[,5],sep="&"))
  colnames(tab)<-c( "lala")
  plot<-as.data.frame(table(tab))
  
  number<-(nrow(productive)/numbers[i,2])
  
  morethan5<-subset(plot, plot$Freq >number)
  
  more<-as.data.frame(str_split_fixed(morethan5[,1], "&", n=7))
  colnames(more)<-c("V", "D", "J", "CDR3AA", "CDR3nt", "N1.REGION.nt.nb","N2.REGION.nt.nb")
  
  
  prodV<- as.data.frame(str_split_fixed(more$V, "\\*", n=2))
  prodD<- as.data.frame(str_split_fixed(more$D, "\\*", n=2))
  prodJ<- as.data.frame(str_split_fixed(more$J, "\\*", n=2))
  
  colnames(prodV)<-c("V","rest")
  colnames(prodD)<-c("D","rest")
  colnames(prodJ)<-c("J","rest")
  
  tabV<-as.data.frame(table(prodV$V))
  
  Vout<-as.data.frame(matrix(0,16,1))
  rownames(Vout)<-seq(1,16)
  colnames(Vout)<-c("Freq V segment")
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    for(r in 1:nrow(tabV)){
      if(grepl(grx, tabV[r,1])){
        Vout[n,1]<-(Vout[n,1]+tabV[r,2])
      }
    }
  }
  
  Voutfreq<-as.data.frame(Vout[,1]*100/sum(Vout[,1]))
  colnames(Voutfreq)<-c("V Freq")
  
  tabD<-as.data.frame(table(prodD$D))
  
  Dout<-as.data.frame(matrix(0,16,1))
  rownames(Dout)<-seq(1,16)
  colnames(Dout)<-c("Freq D segment")
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHD",n,"*", sep=""))
    for(r in 1:nrow(tabD)){
      if(grepl(grx, tabD[r,1])){
        Dout[n,1]<-(Dout[n,1]+tabD[r,2])
      }
    }
  }
  
  Doutfreq<-as.data.frame(Dout[,1]*100/sum(Dout[,1]))
  colnames(Doutfreq)<-c("D Freq")
  
  tabJ<-as.data.frame(table(prodJ$J))
  
  Jout<-as.data.frame(matrix(0,16,1))
  rownames(Jout)<-seq(1,16)
  colnames(Jout)<-c("Freq J segment")
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHJ",n,"*", sep=""))
    for(r in 1:nrow(tabJ)){
      if(grepl(grx, tabJ[r,1])){
        Jout[n,1]<-(Jout[n,1]+tabJ[r,2])
      }
    }
  }
  
  Joutfreq<-as.data.frame(Jout[,1]*100/sum(Jout[,1]))
  colnames(Joutfreq)<-c("J Freq")
  
  tableout<-cbind(Voutfreq,Doutfreq,Joutfreq)
}

for(i in seq_along(files)){
  name1<-strsplit(files[i],"\\/")[[1]][9]
  name2<-strsplit(name1,"\\.")[[1]][1]
  name3<-strsplit(name2,"\\_")[[1]][1]
  name4<-strsplit(name2,"\\_")[[1]][2]
  name5<-strsplit(name2,"\\_")[[1]][3]
  
  name.out<-paste("/Users/med-sve/Desktop/",name2,"_VDJusage_flat.csv", sep="")
  out<-VDJusage_flat(files[i])
  write.csv(out, name.out)
  p<-paste(name.out, "has", numbers[i,2], "cells")
  print(p)
  
}
