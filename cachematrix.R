## functions to compute the inverse of matrix and return it from cache

   ##---------makeCacheMatrix-------####

## this function returns a list of functions to set and get the values of matrix and its Inverse
## function set -> if input of makecahematrix is changed it assigns new value to the old variable which then be computed for inverse
## function get -> returns matrix made with input value 'x'
## function setinverse -> store the value of inverse of the matrix
## function getinverse -> returns the value of inverse of matrix
## finally makecachematrix makes a list of all above 4 functions 

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

 ##---------------------cacheSolve----------------##

## a function to check if inverse is in the cache or not, 
## if its already computed it will set it in setinverse function else it will compute it 
## when we run this function for the first time it computes the inverse of supplied matrix by function solve()
## all other times when ran it by same input value , it gets inverse from cache and returns it 

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
