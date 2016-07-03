## Below are two functions thar are used to create a special
## object that stores a square matrix and caches 
## its inverse matrix

## makeCacheMatrix creates a special "matrix", which is really
## containing a function to:
##		1- set the value of the matrix
##		2- get the value of the matrix
##		3- set the value of the inverse matrix
##		4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	set <- function(y){ x <<- y ; m <<-NULL }

	get <- function(){ x }

	set.inverse <- function(inverse){ m <<- inverse}

	get.inverse <- function(){ m }

	list(set=set, 
		 get=get, 
		 set.inverse=set.inverse, 
		 get.inverse=get.inverse)
}


## cacheSolve calculates the inverse matrix of the special
## 'matrix' created with the above function. It first checks
## if the inverse matrix has already been calculates. If so,
## it gets the inverse matrix from the cache. Otherwise, it
## calculates the inverse matrix and sets the value of the
## inverse matrix in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$get.inverse()
	
	if(!is.null(m)){
		message("getting chached data")
		return(m)
	}

	data <- x$get()

	m <- solve(data, ...)

	x$set.inverse(m)

	m
}
