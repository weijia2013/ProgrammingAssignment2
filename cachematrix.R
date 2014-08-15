
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invm <<- inverse
        getinverse <- function() invm
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        invm <- x$getinverse()
        if(!is.null(invm)) {
                message("getting cached data.")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data)
        x$setinverse(invm)
        invm
}

#evaluation
#x <- rbind(c(1,1,2), c(-1,2,0), c(1,1,3))
#y <- makeCacheMatrix(x)
#y$get()

#cacheSolve(y)
#cacheSolve(y)