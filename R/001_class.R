require('R6')

#' Class a simple interface to biological data (numeric) and rich annotation for both columns (samples) and rows (values)
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords IMGThelper
#' @return Object of \code{\link{R6Class}} to access IMGT data in R 
#' @format \code{\link{R6Class}} object.
#' @field path the path to the IMGT data
#' @field cells the cells table refering to the detected cells analyzed in this 10x VDJ experiment
#' @field usedObj a multi purpose list to store whichever ananlyis results do not fit in the stats list
#' @field version the version of the IMGThelper package
#' @export 
IMGThelper <- #withFormalClass(
	R6::R6Class(
		'IMGThelper',
		class=TRUE,
		public= list(
			path =NULL,
			cells=NULL,
			version=NULL,
			usedObj=NULL,
		print = function(...) {
			cat (paste("An object of class", paste(collapse="::",rev(class(self))),"\n" ) )
			cat("reading from path ",self$path,"\n")
			cat( paste("with information about" , nrow(self$cells), "cells (or DNA elements)" ,"\n" ))
		},
		initialize = function ( path ) {
			if (!dir.exists( path ) ){
				stop( "Please I need an existsing path at startup")
			}
			exp_files <- c('10_V-REGION-mutation-hotspots.txt',  '2_IMGT-gapped-nt-sequences.txt',  
					'5_AA-sequences.txt', '8_V-REGION-nt-mutation-statistics.txt', '11_Parameters.txt', 
					'3_Nt-sequences.txt', '6_Junction.txt', '9_V-REGION-AA-change-statistics.txt', 
					'1_Summary.txt', '4_IMGT-gapped-AA-sequences.txt',  '7_V-REGION-mutation-and-AA-change-table.txt')
			missing = NULL
			for ( name in  exp_files ) {
				if ( ! file.exists( file.path(path, name)))  {
					missing = c( missing, name)
				}
			}
			if ( ! is.null(missing) ){
				stop( paste( "The files", paste( missing, collapse=", "), "is/are missing from the uinput path") )
			}
			self$usedObj = list()
			self$path = path
			self$version = utils::sessionInfo('IMGThelper')$otherPkgs$IMGThelper$Version

			## now populate the cells table with as much info as you can get..
			print ( "loading initial information" )
			sum = data.table::fread( file.path( self$path, '1_Summary.txt'), sep="\t", fill=TRUE )
			colnames(sum) = make.names(colnames(sum))
			## from a 10x experiment you get Sequence.ID as e.g. AAACCTGTCTGTCTAT-1_contig_1
			## (cell_id)_contig_(conmtig_id)
			#browser()
			cell_ids <- unlist(lapply(stringr::str_split(t(sum[,'Sequence.ID']), '_'), function(x){x[1]}))
			sum  = cbind( sum, cell_ids = cell_ids )
			cell_ids <- table( cell_ids )
			self$cells = data.frame(t(cell_ids))[,c(2,3)]
			colnames(self$cells) = c('cellID', 'nContigs' )

			
			#Segemnts = Functions = vector( 'character', nrow( self$cells) )
			#l = 0
			#for ( id in self$cells[,'cellID']) {
			#	l = l +1
			#	OK =  which(is.na(match( t(sum[,'Sequence.ID']), id))==F)
			#	Segemnts[l] = c( Segemnts, paste( collapse="; ", t(sum[OK, 'V.GENE.and.allele' ])))
			#	Functions[l] = c( Functions, paste( collapse="; ", t(sum[OK, 'V.DOMAIN.Functionality' ])))
			#}
			#self$cells$V.GENE.and.allele = Segemnts
			#self$cells$V.DOMAIN.Functionality = Functions
			self
		} )
	)


## obtained from https://rappster.wordpress.com/2015/04/03/r6s3-and-s4-getting-the-best-of-both-worlds/

.onAttach <- function(libname, pkgname) {
	where <- as.environment("package:IMGThelper")
	clss <- list(
			c("IMGThelper", "R6")
	)
	## Ensure clean initial state for subsequent package loads
	## while developing //
	sapply(clss, function(cls) {
				idx <- sapply(cls, methods::isClass )
				suppressWarnings(try(sapply(cls[idx], methods::removeClass,
										where = where), silent = TRUE))
			})
	## Set formal class equivalent //
	sapply(clss, function(cls) {
				try(methods::setOldClass(cls, where = where), silent = TRUE)
			})
#	r6x::formalizeClasses()
}


##' @name show
##' @title the IMGThelper show function
##' @param object the IMGThelper object
##' @docType methods
##' @export show
#setMethod('show', signature = c ('IMGThelper'),#
		#definition = function (  object ) {
	#		object$print()
#		})