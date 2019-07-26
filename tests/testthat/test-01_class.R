path = dirname ( system.file("extdata", "1_Summary.txt", package = "IMGThelper") )

test = IMGThelper$new( path )

#test$cells

expect_equal( dim(test$cells), c(637, 4 ))

expect_equal( dim(productive( test )) , c(4,8) )

expect_equal( dim(productive( test, OK=function( x) { which(x$V.DOMAIN.Functionality == 'productive' ) } )) , c(150,8) )

