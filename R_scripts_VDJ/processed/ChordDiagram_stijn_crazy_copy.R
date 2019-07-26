
##LOOP THE LOOP!

files <- list.files(path="/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output", pattern="*_VJ_usage.csv", full.names=T, recursive=FALSE)


out.col=NULL
out.col[c(paste(test1$X, seq(1:nrow(test1))),colnames(test1))]<-sample(rainbow(sum(nrow(test1),ncol(test1))))
cols<-out.col
write(cols, "/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/colscircos.txt")
for(i in seq_along(files)){
  
  ##clonalheatmapVHJH
  plota<-read.delim(file=files[i], sep=",")
  plotb<-plota[,2:5]
  rownames(plotb)<-paste(plota$X, seq(1:nrow(plota)))
  colnames(plotb)<-colnames(plota[,2:5])
  
  woopwoop<-length(strsplit(files[i],"\\/")[[1]])
  name<-strsplit(files[i],"\\/")[[1]][woopwoop]
  name2<-strsplit(name,"\\.")[[1]][1]
  out.fname <- paste("/Volumes/Seagate Backup Plus Drive/Projects/4.VDJ_sequence/TOM+VDJseq_Test1/VDJseq/Output/",name2,"_test_chorddiagram.pdf", sep="")
 
  
  pdf(out.fname, height="5", width="5")
  par(cex = 0.4, mar = c(0, 0, 0, 0))
  chordDiagram(as.matrix(plotb), annotationTrack = "grid", preAllocateTracks = 1, grid.col = cols)
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.1))
    circos.axis(h = "top", labels.cex = 0.8, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
  }, bg.border = NA)
  dev.off()
  
}

## read in the file and put data in table with first column as rownames

intact<-read.delim(file="/Users/med-sve/Desktop/IntactPercB1a3_Junction_RUN4_VJ_usage.csv", sep=",")
lin<-read.delim(file="/Users/med-sve/Desktop/L28PercB1a1_Junction_RUN4_VJ_usage.csv", sep=",")
wt<-read.delim(file="/Users/med-sve/Desktop/WTpercB1a1_Junction_RUN4_VJ_usage.csv", sep=",")

intactplot<-intact[,2:5]
rownames(intactplot)<-paste(intact$X, seq(1:nrow(intactplot)))
colnames(intactplot)<-colnames(intact[,2:5])

linplot<-lin[,2:5]
rownames(linplot)<-paste(lin$X, seq(1:nrow(linplot)))
colnames(linplot)<-colnames(lin[,2:5])


wtplot<-wt[,2:5]
rownames(wtplot)<-paste(wt$X, seq(1:nrow(wtplot)))
colnames(wtplot)<-colnames(wt[,2:5])

## In their vignette they write the following to define colour (which I cannot do manually of course) 
## grid.col =c(S1 = "red", S2 = "green", S3 = "blue",E1 = "grey", E2 = "grey", E3 = "grey", E4 = "grey", E5 ="grey", E6 = "grey")
## chordDiagram(mat, grid.col = grid.col)


## I tried to plot generate a similar structured table/matrix/however this is calles, but I guess it???s not exactly the same

out.col=NULL
out.col[c(rownames(intactplot),colnames(intactplot))]<-sample(rainbow(sum(nrow(intactplot),ncol(intactplot))))
cols<-out.col
write(cols, "/Users/med-sve/Desktop/colscircos.txt")

outeraa<-as.data.frame(matrix(0,16,1))
couterlab<-as.data.frame(sample(rainbow(16)))
outeraa[,1]<-couterlab[,1]

## then I tried to plot it like this:
pdf("intact.pdf", height="5", width="5")
par(cex = 0.4, mar = c(0, 0, 0, 0))
chordDiagram(as.matrix(intactplot), annotationTrack = "grid", preAllocateTracks = 2, grid.col = cols)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
 
  
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    if(grepl(grx, sector.name)==TRUE){
      sn<-sector.name
      circos.text(mean(get.cell.meta.data ("xlim", sector.index = sn)),get.cell.meta.data ("ylim", sector.index = sn)[1] +0.1, n, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.1))   
    
    }
  }
  
}, bg.border = NA)



circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
 
  sector.name = get.cell.meta.data("sector.index")
  
  
  
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    if(grepl(grx, sector.name)==TRUE){
      sn<-sector.name
      draw.sector( get.cell.meta.data ("cell.start.degree", sector.index = sn, track.index = 1), get.cell.meta.data ("cell.end.degree", sector.index = sn,track.index = 1),rou1 = 0.8, rou2 = 0.7, lwd=5, border= outeraa[n,1], col = outeraa[n,1])
     
    }
  }
  
  
}, bg.border = NA)

dev.off()

pdf("lin28.pdf", height="5", width="5")
par(cex = 0.4, mar = c(0, 0, 0, 0))
chordDiagram(as.matrix(linplot), annotationTrack = "grid", preAllocateTracks = 2, grid.col = cols)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  
  
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    if(grepl(grx, sector.name)==TRUE){
      sn<-sector.name
      circos.text(mean(get.cell.meta.data ("xlim", sector.index = sn)),get.cell.meta.data ("ylim", sector.index = sn)[1] +0.1, n, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.1))   
      
    }
  }
  
}, bg.border = NA)



circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  
  sector.name = get.cell.meta.data("sector.index")
  
  
  
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    if(grepl(grx, sector.name)==TRUE){
      sn<-sector.name
      draw.sector( get.cell.meta.data ("cell.start.degree", sector.index = sn, track.index = 1), get.cell.meta.data ("cell.end.degree", sector.index = sn,track.index = 1),rou1 = 0.8, rou2 = 0.7, lwd=5, border= outeraa[n,1], col = outeraa[n,1])
      
    }
  }
  
  
}, bg.border = NA)

dev.off()

pdf("wt.pdf", height="5", width="5")
par(cex = 0.4, mar = c(0, 0, 0, 0))
chordDiagram(as.matrix(wtplot), annotationTrack = "grid", preAllocateTracks = 2, grid.col = cols)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  
  
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    if(grepl(grx, sector.name)==TRUE){
      sn<-sector.name
      circos.text(mean(get.cell.meta.data ("xlim", sector.index = sn)),get.cell.meta.data ("ylim", sector.index = sn)[1] +0.1, n, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.1))   
      
    }
  }
  
}, bg.border = NA)



circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  
  sector.name = get.cell.meta.data("sector.index")
  
  
  
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    if(grepl(grx, sector.name)==TRUE){
      sn<-sector.name
      draw.sector( get.cell.meta.data ("cell.start.degree", sector.index = sn, track.index = 1), get.cell.meta.data ("cell.end.degree", sector.index = sn,track.index = 1),rou1 = 0.8, rou2 = 0.7, lwd=5, border= outeraa[n,1], col = outeraa[n,1])
      
    }
  }
  
  
}, bg.border = NA)

dev.off()