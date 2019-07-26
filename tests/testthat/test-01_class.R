path = dirname ( system.file("extdata", "1_Summary.txt", package = "IMGThelper") )

test = IMGThelper$new( path )

#test$cells

expect_equal( dim(test$cells), c(637, 4 ))



