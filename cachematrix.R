## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function implements a special matrix where 
## inverse of the matrix is cached.   The data are encapsulated 
## within and only accessable using the functions: set, get, 
## setenv, and getinv.
## sample usage:
## 1. define a matrix
##       x<-matrix(c(3,5,7,1,2,3,7,8,4),nrow=3, ncol=3)
##    contents
##
##          [,1] [,2] [,3]
##    [1,]    3    1    7
##    [2,]    5    2    8
##    [3,]    7    3    4
##
## 2. make it a cached matrix type
##      cached_x<-makeCacheMatrix(x)
##    contents
##      cached_x$get()
##          [,1] [,2] [,3]
##    [1,]    3    1    7
##    [2,]    5    2    8
##    [3,]    7    3    4
##
##      cached_x$getinv()
##      NULL
##
##
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
        x <<- y
        i <<- NULL
     }
     get <- function() x
     setinv <- function(solve) i <<- solve
     getinv <- function() i
     list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function takes a chached matrix as an argument,  If the
## inverse of the matrix has never been computed it is computed, 
## stored, and returned.  If the inverse has been computed already
## the stored value is retrieved and returned.
##
## sample usage:
##
##      cacheSolve(cached_x)
##           [,1] [,2] [,3]
##      [1,]  3.2 -3.4  1.2
##      [2,] -7.2  7.4 -2.2
##      [3,] -0.2  0.4 -0.2
##
##      cacheSolve(cached_x)
##      getting cached inverted matrix
##           [,1] [,2] [,3]
##      [1,]  3.2 -3.4  1.2
##      [2,] -7.2  7.4 -2.2
##      [3,] -0.2  0.4 -0.2
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getinv()
  if(!is.null(m)) {
     message('getting cached inverted matrix')
     return(m)
  }
  d <- x$get()
  m <- solve(d)
  x$setinv(m)
  m
}
