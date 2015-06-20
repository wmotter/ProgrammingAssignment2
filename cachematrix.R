#create an inversible matrix and allow caching of matrix
makeCacheMatrix <- function(x = matrix()) {
        
        theCache <- NULL
        
        # cache the given argument 
        cacheInverse <- function(solve) {
          theCache <<- solve
        }
        
        # get the cached value
        getInverse <- function() {
          theCache 
        }
        
        #put new values into matrix
        setMatrix <- function(newValue) {
                #save new matrix
                x <<- newValue
                # clear the cache
                theCache <<- NULL
        }

        # returns the stored matrix
        getMatrix <- function() {
                x
        }

        # cache the given argument 
        cacheInverse <- function(solve) {
                theCache <<- solve
        }

        # get the cached value
        getInverse <- function() {
                theCache 
        }
        
        # return the list. 
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# Calculates the inverse of the matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
        # get the cached value
        inverse <- x$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- x$getMatrix()
        inverse <- solve(data)
        x$cacheInverse(inverse)
        
        # return the inverse
        inverse
}