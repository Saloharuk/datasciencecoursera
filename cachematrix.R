## Programming assignment 2 by Scott Ragland
## for R Programming class, May 21, 2014.
##
## The function makeCacheMatrix creates a list of
## functions that prepare the matrix for conversion
## to its inverse using solve().  For example, 'x' could be:
##   x <- matrix(c(-1,4,0,-1,5,1,-1,0,-3),nrow=3)
## which corresponds to the matrix:
##     -1   -1   -1
##      4    5    0
##      0    1   -3
## The cacheSolve function performs the actual solving.
## The inverse of the example above will be:
##     15    4   -5
##    -12   -3    4
##     -4   -1    1
## Assumption: The matrix 'x' is invertible.

## makeCacheMatrix sets and gets the input matrix,
## then sets and gets the mechanism to run solve()
## to produce the matrix inverse.  These items are
## placed in a list, which are then pulled and used
## by the cacheSolve function.
##
## Example of function use:
##   a <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
	## 1. Initialize the output, 'm', to NULL.
	m <- NULL
	## 2. Set and get the matrix.
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	## 3. Set and get solve() to apply to output 'm'.
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	## 4. Place the set and get functions into a list.
	list(set = set, get = get,
		setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve extracts the matrix data and solve function
## from makeCacheMatrix's list.  It runs solve() on the
## matrix and passes the matrix inverse out through variable 'm'.
##
## If being used in a loop, cacheSolve returns the stored
## matrix inverse 'm', rather than perform a redundent solve()
## on the matrix.
##
## Example of function use:
##   cacheSolve(a)

cacheSolve <- function(x, ...) {
        ## 1. Obtain either the existing matrix inverse 'm'
        ##    if it already exists from a previous iteration
        ##    of cacheSolve.
        m <- x$getinverse()
        ## 2. It 'm' contains the inverse (that is, isn't
        ##    set to its initial NULL state), return the
        ##    inverse of matrix 'x' to the user.
        if(!is.null(m)) {
        	message("Getting cached data")
        	return(m)
        }
        ## 3. If 'm' is NULL, run solve() on matrix 'x'
        ##    and return the inverse matrix 'm' to the user.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}