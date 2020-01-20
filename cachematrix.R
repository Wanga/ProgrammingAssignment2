## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix save a matrix's inv result, cacheSolve will 
# return the calculated result if exist

require(MASS) # for ginv

## Write a short comment describing this function
# Create a "class" that save a matrix, inv result and methods

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv  <- function(inv) inv <<- inv
	getinv  <- function() inv
	list(set = set, get = get,
		setinv  = setinv,
		getinv  = getinv)
}


## Write a short comment describing this function
# Calculate result only if it's not calculated before

cacheSolve <- function(x, ...) {
	inv<- x$getinv()
	if(!is.null(inv)) {
		message("getting cached inverse matrix")
		return(inv)
	}
	data <- x$get()
	inv<- ginv(data, ...)
	x$setinv(inv)
	inv
}


