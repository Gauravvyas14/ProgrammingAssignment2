## functions to compute the inverse of matrix and return it from cache

   ##---------MAKE-CACHE-MATRIX--------####

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

 ##---------------------CACHE-SOLVE----------------##

## a function to check if inverse is in the cache or not, 
## if its already computed it will return it from cache else it will compute and set it in setinverse function
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


##----------------------SAMPLE-OUTPUT------------------##
##  x <- matrix((rnorm(16)), nrow= 4, ncol = 4)
##  m <- makeCacheMatrix(x)
##  m$get() 
##   		[,1]       [,2]       [,3]       [,4]
##	[1,]  0.8509464 -0.8969362  1.5400567  0.6881839
##	[2,]  1.3769274 -0.9388900  0.1799797 -0.1291284
##	[3,] -0.5838616  1.5117345 -0.6439339  0.4995189
##	[4,] -0.7529193 -1.5797879 -0.4986466  1.5340340
##  cacheSolve(m)
##  Computing inverse Matrix...
##          [,1]         [,2]       [,3]        [,4]
##	[1,] 0.05599154  0.824421706  0.4413350 -0.09943158
##	[2,] 0.13866189  0.005901135  0.5095497 -0.22763026
##	[3,] 0.54403361 -0.555003915 -0.2430668 -0.21162859
##	[4,] 0.34712008  0.230304383  0.6623488  0.29986338

##   cacheSolve(m)
##   getting inverse matrix from cache...
##           [,1]         [,2]       [,3]        [,4]
##	[1,] 0.05599154  0.824421706  0.4413350 -0.09943158
##	[2,] 0.13866189  0.005901135  0.5095497 -0.22763026
##	[3,] 0.54403361 -0.555003915 -0.2430668 -0.21162859
##	[4,] 0.34712008  0.230304383  0.6623488  0.29986338
