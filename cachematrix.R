

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
