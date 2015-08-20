## Caching the Inverse of a Matrix
## These functions assume that the matrix supplied is always square and invertible, i.e.
## they don't include error checking.
## The computing of the inverse of a square matrix will be done with the solve function in R. 

## This function creates a special "matrix", which is really a list containing a funcion to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverted <- function(solve) i <<- solve
	getinverted <- function() i
	list(set = set, get = get,
		setinverted = setinverted,
		getinverted = getinverted)
}


## This function calculates the inverse of the special "matrix" created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverted()
	## Check if exist cached data
	if(!is.null(i)) {
		message("getting cached inverted matrix")
		return(i) # return cached inverted matrix
	}
	## If not, cumpute inverted matrix
	mtrx <- x$get()
	i <- solve(mtrx, ...)
	## Cache inverted matrix
	x$setinverted(i)
	i ## return computed matrix
}
