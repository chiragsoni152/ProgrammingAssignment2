# makeCacheMatrix creates a list containing a function and does the following:
# Set and Get the value of the matrix
# Set and Get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	mt <- NULL
	set <- function(y) {
		x <<- y
		mt <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) mt <<- inverse
	getinverse <- function() mt
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function returns the inverse of the matrix. If the inverse has been computed already,
# it gets the result and skips the computation. Else, it computes the inverse and sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) {
	mt <- x$getinverse()
	if(!is.null(mt)) {		
		return(mt)
	}
	data <- x$get()
	mt <- solve(data) %*% data
	x$setinverse(mt)
	mt
}
