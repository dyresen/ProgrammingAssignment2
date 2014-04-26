## Two functions that will enable you to cache and reuse the inverse of a matrix.
## Using solve function of compute the inverse. 

## Takes a matrix as argument and returns a list of functions to set and
## get the cached values on object x.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse) 
}


## Client function of makeCacheMatrix. Will query x for a cached inverse value. 
## If there is no cached value, we compute and cache if for later usage. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}