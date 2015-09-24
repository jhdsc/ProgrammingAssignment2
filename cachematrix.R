## Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# i is used to store the inverse and is intialized to NULL
	i <- NULL

	# the set function will intializie the matrix to the given matrix y
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	# the get function will return the matrix
	get <- function() x

	# the setinverse function will set the inverse of the matrix 
	# to the given inverse value
	setinverse <- function(inverse) i <<- inverse

	# the getinverse function will return the currently stored 
	# inverse value of the matrix
	getinverse <- function() i

	list(set = set, get = get, setinverse = setinverse,
	     getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## This function returns a matrix that is the inverse of 'x'

	# check to see if the inverse is already cached (ie, not NULL)
	i <- x$getinverse()
	if (!is.null(i)) {
		# the inverse is already cached so just return its value
		message("getting cached data")
		return(i)
	}

	# the inverse is not cached, so get the value of matrix and
	# compute its inverse using solve 
	data <- x$get()
	i <- solve(data, ...)

	# cache the inverse value using set
	x$setinverse(i)

	# retrun the inverse value
	i
}
