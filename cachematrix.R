## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
		# store a matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		# returns the stored matrix
        get <- function() x
		 # cache the given argument
        setInverse <- function(inverse) inv <<- inverse
		# get the cached value
        getInverse <- function() inv
		# return a list. Each named element of the list is a function
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
	
        inv <- x$getInverse()
		# if a cached value exists return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		# otherwise get the matrix, caclulate the inverse and store it in
            # the cache
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
		 # return the inverse
        inv
}