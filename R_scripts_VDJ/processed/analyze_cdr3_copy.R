
#load data
analyzeCDR3 <- function( fname ) {
  data <- read.delim(file= fname )
  CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("CDR3.IMGT..AA.") ]
  tab<-table(CDR3a)
  tabsort<-sort(tab, decreasing=TRUE)
  tabsort<-as.data.frame(tabsort)
  tabsort
}

spleenWT1<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/spleenWT1/6_Junction.txt')
write.csv(spleenWT1, "/Users/med-sve/Desktop/Sequencing/spleenWT1CDR3.txt", sep="/t")
spleenWT2<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/spleenWT2/6_Junction.txt')
write.csv(spleenWT2, "/Users/med-sve/Desktop/Sequencing/spleenWT2CDR3.txt", sep="/t")
spleenWT3<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/spleenWT3/6_Junction.txt')
write.csv(spleenWT3, "/Users/med-sve/Desktop/Sequencing/spleenWT3CDR3.txt", sep="/t")

spleenl1<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/spleenl1/6_Junction.txt')
write.csv(spleenl1, "/Users/med-sve/Desktop/Sequencing/spleenl1CDR3.txt", sep="/t")
spleenl2<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/spleenl2/6_Junction.txt')
write.csv(spleenl2, "/Users/med-sve/Desktop/Sequencing/spleenl2CDR3.txt", sep="/t")
spleenl3<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/spleenl3/6_Junction.txt')
write.csv(spleenl3, "/Users/med-sve/Desktop/Sequencing/spleenl3CDR3.txt", sep="/t")

pcWT1<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/pcWT1/6_Junction.txt')
write.csv(pcWT1, "/Users/med-sve/Desktop/Sequencing/pcWT1CDR3.txt", sep="/t")
pcWT2<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/pcWT2/6_Junction.txt')
write.csv(pcWT2, "/Users/med-sve/Desktop/Sequencing/pcWT2CDR3.txt", sep="/t")
pcWT3<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/pcWT3/6_Junction.txt')
write.csv(pcWT3, "/Users/med-sve/Desktop/Sequencing/pcWT3CDR3.txt", sep="/t")

pcl1<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/pcl1/6_Junction.txt')
write.csv(pcl1, "/Users/med-sve/Desktop/Sequencing/pcl1CDR3_bis.txt")
pcl2<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/pcl2/6_Junction.txt')
write.csv(pcl2, "/Users/med-sve/Desktop/Sequencing/pcl2CDR3_bis.txt")
pcl3<-analyzeCDR3( '/Users/med-sve/Desktop/Sequencing/pb195_20102015/pcl3/6_Junction.txt')
write.csv(pcl3, "/Users/med-sve/Desktop/Sequencing/pcl3CDR3_bis.txt")



data <- read.delim(file= '/Users/med-sve/Desktop/pb195_20102015/spleenWT1/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
a<-as.data.frame(CDR3a)

data <- read.delim(file= '/Users/med-sve/Desktop/pb195_20102015/spleenWT2/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
b<-as.data.frame(CDR3a)

data <- read.delim(file= '/Users/med-sve/Desktop/pb195_20102015/spleenWT3/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
c<-as.data.frame(CDR3a)
abc<-table(rbind(a,b,c))
abc<-sort(abc, decreasing=TRUE)


data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/spleenl1/6_Junction.txt' )
write.csv(tabsort,'/Users/med-sve/Desktop/pb195_20102015/spleenl1_CDR3.txt')
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)

d<-as.data.frame(CDR3a)
data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/spleenl2/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
e<-as.data.frame(CDR3a)

data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/spleenl3/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
f<-as.data.frame(CDR3a)
def<-table(rbind(d,e,f))
def<-sort(def, decreasing=TRUE)


data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/pcwt1/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
g<-as.data.frame(CDR3a)

data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/pcwt2/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
h<-as.data.frame(CDR3a)

data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/pcwt3/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
i<-as.data.frame(CDR3a)
ghi<-table(rbind(g,h,i))
ghi<-sort(ghi, decreasing=TRUE)


data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/pcl1/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)

j<-as.data.frame(CDR3a)

data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/pcl2/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)

k<-as.data.frame(CDR3a)

data <- read.delim(file=  '/Users/med-sve/Desktop/pb195_20102015/pcl3/6_Junction.txt' )
CDR3a <- data[which(data$Functionality == 'productive' &! data$D.GENE.and.allele == ""),c("V.GENE.and.allele","J.GENE.and.allele","CDR3.IMGT..AA.") ]
tab<-table(CDR3a)
tabsort<-sort(tab, decreasing=TRUE)
tabsort<-as.data.frame(tabsort)
l<-as.data.frame(CDR3a)
jkl<-table(rbind(j,k,l))
jkl<-sort(jkl, decreasing=TRUE)


