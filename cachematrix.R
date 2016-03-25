# Matrix inversion is usually a costly computation and there may be some  benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# 1. set the value of the matrix
    
     i <- NULL
     set <- function(y) {
         x <<- y
         i <<- NULL
     }

# 2. get the value of the matrix

     get <- function() x

# 3. set the value of inverse of the matrix
    
     setinverse <- function(inverse) i <<- inverse

# 4. get the value of inverse of the matrix

     getinverse <- function() i

     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The cacheSolve function returns the inverse of the matrix. 
# It will check to see if the inverse has already been computed, and use that
# computation first.  If the inverse has not been computed, it sets the value
# in cache using the setinverse function. This function assumes that the matrix
# supplied is invertible.

cacheSolve <- function(x, ...) {
#
# 1. Check to see if the inverse has already been computed
#    if exists, return the result
    
     i <- x$getinverse()

     if(!is.null(i)) {
         message("Getting cached data.")
         return(i)
     }
#
# 2. Check to see if the inverse has already been computed
#    if the inverse does not exist, compute the inverse

    data <- x$get()

    i  <- solve(data)

    x$setinverse(i)

    i
}

