#' @name open
#' @aliases open,IMGThelper-method
#' @rdname open-methods
#' @docType methods
#' @description load one file in the path of the object
#' @param x  the IMGThelper object
#' @param name the file to load default= '6_Junction.txt'
#' @title description of function open
#' @export 
setGeneric('open', ## Name
	function ( x, name= '6_Junction.txt' ) { ## Argumente der generischen Funktion
		standardGeneric('open') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('open', signature = c ('IMGThelper'),
	definition = function ( x, name= '6_Junction.txt' ) {
	fname=file.path(x$path, name )
	read.delim(file= fname )
} )
