## These functions allow costly matrix inversion operations to be cached in a
##	special object created by makeCacheMatrix which includes the matrix and its
##	inverse along with functions to get and set this data.  The inverse (inv) is
##	initialized to NULL when a new matrix is made cache'able.  The cachSolve function
##	is used to replace a direct call to the solve function and it branches on if inv
##	is NULL to determine if the inverse can be returned or if it needs to be calculated
##	first.

## Usage Example:
##	> x <- makeCacheMatrix(rnorm(4,2,2))
##	> x$get()
##	           [,1]       [,2]
##	[1,] -0.4287108 -0.8836323
##	[2,] -1.1177926  0.5346379
##	#	Test to see if inverse is cached
##	> x$getinv
##	NULL
##	#	cachSolve it once
##	> cacheSolve(x)
##	           [,1]       [,2]
##	[1,] -0.4393359 -0.7261203
##	[2,] -0.9185404  0.3522909
##	#	Confirm that the inverse is cached
##	> x$getinv
##	           [,1]       [,2]
##	[1,] -0.4393359 -0.7261203
##	[2,] -0.9185404  0.3522909
##	#	Test to see if inverse is accurate
##	> x$get() %*% x$getinv()
##	             [,1]          [,2]
##	[1,] 1.000000e+00 -1.265806e-17
##	[2,] 1.959695e-17  1.000000e+00





## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##	If the inverse has already been calculated (and the matrix has not changed),
##	then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
