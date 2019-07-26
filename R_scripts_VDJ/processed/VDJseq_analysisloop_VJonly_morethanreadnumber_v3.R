#loop through VH analysis

##load packages

library(stringr)
library(gplots)

##make functions

VJcombination <- function( fname=fname  ) {
  data <- read.delim(file= fname )
  productive <- data[which(data$V.DOMAIN.Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA") ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT") ]
  prod<- as.data.frame(str_split_fixed(productive$V.GENE.and.allele, "\\*", n=2))
  colnames(prod)<-c("V","rest")
  productive<-cbind(prod$V, productive)
  colnames(productive)<-c("a","V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT")
  VHorder<-read.delim(file= '/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/IGHVorder3.txt')
  
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
}


##select folder to loop

files <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*_Junction.txt", full.names=T, recursive=FALSE)
numbers2 <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq", pattern="*.csv", full.names=T, recursive=FALSE)
numbers<-read.delim(numbers2, sep=",")

##run the loop

for(i in seq_along(files)){
  ##heatmapVHJH
  out <- VJcombination(files[i])
  woopwoop<-length(strsplit(files[i],"\\/")[[1]])
  name<-strsplit(files[i],"\\/")[[1]][woopwoop]
  name2<-strsplit(name,"\\.")[[1]][1]
  out.csv <- paste("/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/",name2,"_VJ_usage.csv", sep="")
  outpercentage<-t(t(out*100))/(sum(out))
  write.csv(outpercentage, out.csv)
  out<-NULL
  name<-NULL
  name2<-NULL
  out.fname<-NULL
  outpercentage<-NULL
}
  
