#' @name ChordDiagram
#' @aliases ChordDiagram,IMGThelper-method
#' @rdname ChordDiagram-methods
#' @docType methods
#' @description Create a Chord diagram of V gene usage
#' @param x the IMGThelper object
#' @param ofile the pdf out file
#' @param col the colors to be used in the plot (leave NULL if unsure!)
#' @param cells the cell IDs to look into (default NULL == all)
#' @param cutoff select only re-occuring VDJ recombination events (default 5; see ?recurringProductives)
#' @param sortFun a function to sort the Ig/TCR fragments default function(x) {sort(x)}
#' @param plotType xolor  the V genes according to the family (deafult 1) or the segment (2)
#' @title description of function ChordDiagram
#' @export 
setGeneric('ChordDiagram', ## Name
	function ( x, ofile, col=NULL , cells=NULL, cutoff=5, sortFun = function(x){ sort(x) }, plotType=1 ) { ## Argumente der generischen Funktion
		standardGeneric('ChordDiagram') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('ChordDiagram', signature = c ('IMGThelper'),
	definition = function ( x, ofile, col=NULL, cells=NULL , cutoff=5, sortFun = function(x){ sort(x) } , plotType=1 ) {

circlize::circos.par(RESET=T)

pdf( ofile , height=5, width=5)
par(cex = 0.4, mar = c(0, 0, 0, 0))

intactplot = VJcombination(x, cells=cells, cutoff=cutoff )
if ( nrow( intactplot) < 1) {
  dev.off()
  stop( "Sorry, there is no data in the object regarding your request")
}
col = x$usedObj$cordCols[ c(
  rownames(intactplot),
  colnames(intactplot)
  ) ]

if ( length(which(is.na(col))) > 0 ) {
  ## damn this is a problem!
  x$usedObj$cordCols  = NULL
  x$usedObj$recurring = NULL
  tmp = VJcombination(x, cutoff=0 )
  x$usedObj$cordCols<-sample(rainbow(sum(nrow(tmp),ncol(tmp))))
  names(x$usedObj$cordCols) = c(rownames(tmp),colnames(tmp))

  allVs = unique(unlist(lapply( rownames(tmp) , function (x) {stringr::str_extract( x, '[TtiI][RrgG].?[Vv][0-9]*' )[1] })))
  add = sample(rainbow(length(allVs)))
  names(add) = allVs
  x$usedObj$cordCols = c( x$usedObj$cordCols, add)
}

#print( "The VJ combinations are not ordered here - you can manually reorder the table in slot 'x$usedObj$vjCombination'")

#outeraa<-as.data.frame(matrix(0,16,1))
#couterlab<-as.data.frame(sample(rainbow(16)))
#outeraa[,1]<-couterlab[,1]

circlize::chordDiagram(as.matrix(intactplot), annotationTrack = "grid", preAllocateTracks = 2, grid.col = col)

obj = x

circlize::circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {

  sector.name = get.cell.meta.data("sector.index")
  name = stringr::str_extract( sector.name, '[TtiI][RrgG].?[Vv][0-9]*-[0-9]*' )

  circlize::circos.text(mean(get.cell.meta.data ("xlim", sector.index = sector.name)),
        get.cell.meta.data ("ylim", sector.index = sector.name )[1] +0.1, name, 
        facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.1) ) 

  Vclass = stringr::str_extract( sector.name, '[TtiI][RrgG].?[Vv][0-9]*' )
  circlize::draw.sector( get.cell.meta.data ("cell.start.degree", sector.index = sector.name, track.index = 2), 
        get.cell.meta.data ("cell.end.degree", sector.index = sector.name,track.index = 2),
        rou1 = 0.8, rou2 = 0.7, lwd=5, col= obj$usedObj$cordCols[Vclass], border= obj$usedObj$cordCols[Vclass])

}, bg.border = NA)

if ( plotType==2) {
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
 
  sector.name = get.cell.meta.data("sector.index")
  Vclass = stringr::str_extract( sector.name, '[TtiI][RrgG].?[Vv][0-9]*' )
  circlize::draw.sector( get.cell.meta.data ("cell.start.degree", sector.index = sector.name, track.index = 1), 
        get.cell.meta.data ("cell.end.degree", sector.index = sector.name,track.index = 1),
        rou1 = 0.8, rou2 = 0.7, lwd=.2, col= col[sector.name], 
        border= col[Vclass])
       # border= col[sector.name])
  
  #for(n in 1:16){
  #  #browser() ## what are we doing here?! use the same colors over and over!
  #  grx <- glob2rx(paste("Musmus IGHV",n,"-*", sep=""))
  #  if(grepl(grx, sector.name)==TRUE){
  #    sn<-sector.name
  #    draw.sector( get.cell.meta.data ("cell.start.degree", sector.index = sn, track.index = 1), 
  #    	get.cell.meta.data ("cell.end.degree", sector.index = sn,track.index = 1),
  #    	rou1 = 0.8, rou2 = 0.7, lwd=5)
  #   
  #  }
  #}
  
  
}, bg.border = NA)
}
dev.off()
invisible(x)

} )


##   source('~/git_Projects/VDJ_Stijn/R/ChordDiagram.R')
##   ChordDiagram( Bcells_firstGuessObj$usedObj$BcellIMGT, ofile="Chord_IgM", cells=colnames(Bcells_firstGuessObj$dat)[which(Bcells_firstGuessObj$samples$Bcellsclass == 'Ighm')] , cutoff=0)
