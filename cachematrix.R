## Consist in a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: creates a special "matrix", wich contain 4 functions that
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

makeCacheMatrix <- function(mtrx = matrix()) {
	x <- NULL
	set <- function(newmtrx)
	{
		mtrx <<- newmtrx
		x <<- NULL
	}
	get <- function() mtrx
	setInverse <- function(i) x <<- solve(mtrx)
	getInverse <- function() x
	list(	set = set, 
			get = get, 
			setInverse = setInverse,
			getInverse = getInverse)
}


##cacheSolve: calculate the inverse of the special "matrix", created by the previous function
##Validate if has been computed before, if so, get the inverse matrix from the cache and skip the computation.
##Otherwise, calculate the inverse of the matrix and set the inverse in the cache using the setInverse function.
cacheSolve <- function(matrx, ...) {
	mtrxinv <- mtrx$getInverse()
	if(!is.null(mtrxinv))
	{
		message("getting cached data")
		return(mtrxinv)
	}
	data <- mtrx$get()
	mtrxinv <- solve(data, ...)
	mtrx$setInverse(mtrxinv)
	mtrxinv	
}
