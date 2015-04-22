## functions to compute the inverse of matrix and return it from cache
## 

##  a function to return list of functions to set and get the values of matrix and its Inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y) {
		
		x <<- y
		inv <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## a function to check if inverse is in the cache or not, 
## if its computed it will set it in setinverse function else it will compute it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinverse()
	
	if(!is.null(inv)) {
		message("getting inverse matrix from cache...")
		return (inv)
	}
	message("Computing inverse Matrix...") 
	mat <- x$get()
	inv <- solve(mat)
	x$setinverse(inv)
	inv
}
