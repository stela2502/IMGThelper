cdr <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output", pattern="*_CDR3list.csv", full.names=T, recursive=FALSE)

table<-matrix(0,1,1)
table<-as.data.frame(table)
table[1,1]<-NA
colnames(table)<-c("CDR3")
for(i in seq_along(cdr)){
  read<-as.data.frame(read.delim(cdr[i], sep=","))
  add<-as.data.frame(read[,2])
  colnames(add)<-c("CDR3")
  table<-rbind(table, add)
}

ut<-as.data.frame(unique(table[,1]))
colnames(ut)<-c("CDR3")
ut$color<-sample(rainbow(nrow(ut)))
ut$color<-substr(ut$color,1,7)

write.csv(ut, "/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/treemapcolors5.csv")