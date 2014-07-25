# Programming assingment 2 for https://class.coursera.org/rprog-005
# Mitch Hendrickson

# Per the assignment, we write a couple of functions to demonstrate/duplicate 
# the vector caching described in the assignemnt, but this time for matrices

## Create a "matrix" which is really a list of operation functions and has a place to 
## stash a cache of the inverse. Based on makeVector(), and modified. 

makeCacheMatrix <- function(x = matrix()) {

	# we don't have an inverse cached at first
	inv <- NULL

	## learn/wrap the new matrix (y), toss any cached inverse
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## return the wrapped matrix
	get <- function() x

	## stash a matrix assumed to be the inverse (i.e. "cache it")
	setinverse <- function(newInv) inv <<- newInv

	## retrieve the (cached inverse) matrix stored with setinverse 
	getinverse <- function() inv

	## we really return a list of these accessors/mutators
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Inverse solver for our caching matrix which will use the cached version if it can.
## Based on cachemean from the assignment, and modified

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. 

        # get and return the stashed inverse if we (x) has one
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }

        # no stash; solve it and store the result in the cache; return that
        m <- x$get()
        inv <- solve(m, ...)
        x$setinverse(inv)
        inv
    }

# testinverse <- function() {
# 	m <- matrix(c(1,0,5,2,1,6,3,4,0), nrow = 3, ncol = 3)	# from http://www.purplemath.com/modules/mtrxinvr2.htm 
# 	cm <- makeCacheMatrix(m)
# 	i <- cacheSolve(cm)
# 	print(i)
# 	i <- cacheSolve(cm)
# 	print(i)
# 	i <- cacheSolve(cm)
# 	print(i)
# 	cm <- makeCacheMatrix(i)
# 	i <- cacheSolve(cm)
# 	print(i)
# 	i <- cacheSolve(cm)
# 	print(i)
# 	i <- cacheSolve(cm)
# 	print(i)
# }