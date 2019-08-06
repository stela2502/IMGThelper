## example analysis of the N nucleotides - hopefully...

analyzeNOW <- function( fname ) {
  data <- read.delim(file=  fname ) 
  productive <- data[which(data$Functionality == 'productive' & ! data$D.GENE.and.allele == "" & ! data$CDR3.IMGT == "" & ! data$CDR3.IMGT == "NA") ,c( "V.GENE.and.allele","D.GENE.and.allele", "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT", "N1.REGION.nt.nb","N2.REGION.nt.nb") ]

  prod<- as.data.frame(str_split_fixed(productive$V.GENE.and.allele, "\\*", n=2))
  colnames(prod)<-c("V","rest")
  productive<-cbind(prod$V, productive)
  colnames(productive)<-c("a","V.GENE.and.allele","D.GENE.and.allele", 
    "J.GENE.and.allele", "CDR3.IMGT..AA.", "CDR3.IMGT", "N1.REGION.nt.nb","N2.REGION.nt.nb")
  
  tab<-as.data.frame(paste(productive[,1],productive[,3],productive[,4],productive[,5],productive[,6], productive[,7],productive[,8],sep="&"))
  colnames(tab)<-c( "lala")
  plot<-as.data.frame(table(tab))
  
  #select on reads
  morethan5<-subset(plot, plot$Freq >5)
  more2<-as.data.frame(tab[which((match(tab[,1], morethan5[,1])>1) == TRUE),c( "lala")])
  more<-as.data.frame(str_split_fixed(more2[,1], "&", n=7))
  colnames(more)<-c("V", "D", "J", "CDR3AA", "CDR3nt", "N1.REGION.nt.nb","N2.REGION.nt.nb")
  
  outtab2<-more
  
 
  N1<- as.data.frame(table(outtab2$N1.REGION.nt.nb)*100 / sum(table(outtab2$N1.REGION.nt.nb)))
  N2<- as.data.frame(table(outtab2$N2.REGION.nt.nb)*100 / sum(table(outtab2$N2.REGION.nt.nb)))

  output <- matrix(0, ncol=4, nrow=26)
  output[,1]<-0:25
  output[,3]<-0:25
  colnames(output)<-c("N1 nucleotide", "Frequency", "N2 nucleotide", "Frequency")
  

    for ( i in seq(1:26) ) {
      R<-which(N1[,1] %in% c(output[i,1]))
      if(length(R) != 0 ){
        output[i,2]<- N1[R,2]}
    }
    
  for ( i in seq(1:26) ) {
    R<-which(N2[,1] %in% c(output[i,1]))
    if(length(R) != 0 ){
      output[i,4]<- N2[R,2]}
  }
    
  output
  
}

files <- list.files(path="/Users/med-sve/Documents/workingfolders/RunVDJ_ILL_YUAN_2016_4/done/", pattern="*.txt", full.names=T, recursive=FALSE)

for(i in seq_along(files)){
  name1<-strsplit(files[i],"\\/")[[1]][9]
  name2<-strsplit(name1,"\\.")[[1]][1]
  name3<-strsplit(name2,"\\_")[[1]][1]
  name4<-strsplit(name2,"\\_")[[1]][2]
  name5<-strsplit(name2,"\\_")[[1]][3]
  
  name.out<-paste("/Users/med-sve/Desktop/",name3, name4, name5,"_Nnuc_list_non_clonal_morethan5reads_short.csv", sep="")
  out<-analyzeNOW(files[i])
  write.csv(out, name.out)
  
}
