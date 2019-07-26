open <- function( x, name= '6_Junction.txt' ) {
	fname=file.path(x$path, name )
	read.delim(file= fname )
}