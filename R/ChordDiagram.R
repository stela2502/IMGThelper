#' @name ChordDiagram
#' @aliases ChordDiagram,IMGThelper-method
#' @rdname ChordDiagram-methods
#' @docType methods
#' @description Create a Chord diagram of V gene usage
#' @param x the IMGThelper object
#' @param ofile the pdf out file
#' @param col the colors to be used in the plot (leave NULL if unsure!)
#' @title description of function ChordDiagram
#' @export 
setGeneric('ChordDiagram', ## Name
	function ( x, ofile, col=NULL ) { ## Argumente der generischen Funktion
		standardGeneric('ChordDiagram') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('ChordDiagram', signature = c ('IMGThelper'),
	definition = function ( x, ofile, col=NULL ) {

pdf( ofile , height="5", width="5")
par(cex = 0.4, mar = c(0, 0, 0, 0))

intactplot = VJcombination(x)

if ( is.null(col) ) {
	out.col[c(rownames(intactplot),colnames(intactplot))]<-sample(rainbow(sum(nrow(intactplot),ncol(intactplot))))
}

outeraa<-as.data.frame(matrix(0,16,1))
couterlab<-as.data.frame(sample(rainbow(16)))
outeraa[,1]<-couterlab[,1]

chordDiagram(as.matrix(intactplot), annotationTrack = "grid", preAllocateTracks = 2, grid.col = cols)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
 
  
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    if(grepl(grx, sector.name)==TRUE){
      sn<-sector.name
      circos.text(mean(get.cell.meta.data ("xlim", sector.index = sn)),
      	get.cell.meta.data ("ylim", sector.index = sn)[1] +0.1, n, 
      	facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.1))   
    
    }
  }
  
}, bg.border = NA)



circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
 
  sector.name = get.cell.meta.data("sector.index")
  
  
  
  for(n in 1:16){
    grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
    if(grepl(grx, sector.name)==TRUE){
      sn<-sector.name
      draw.sector( get.cell.meta.data ("cell.start.degree", sector.index = sn, track.index = 1), 
      	get.cell.meta.data ("cell.end.degree", sector.index = sn,track.index = 1),
      	rou1 = 0.8, rou2 = 0.7, lwd=5, border= outeraa[n,1], col = outeraa[n,1])
     
    }
  }
  
  
}, bg.border = NA)

dev.off()
invisible(x)

} )
