## The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse matrix 
##4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
i<-NULL                         ## i is the Inverse of the matrix x
set<-function(y){
        x<<-y
        i<<-NULL
}
get <- function() x
seti <- function(im) i <<- im
geti <- function() i
list(set = set, get = get,
     seti = seti,
     geti = geti)
}



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$geti()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$seti(i)
        i
       
}
