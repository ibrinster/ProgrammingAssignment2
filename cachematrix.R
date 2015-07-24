## The functions cache the matrix and its inverse
## When an inverse of a matrix is requested, we first check whether the inverse has already been cached.
##If so, the cached value is returned. Otherwise, the inverse is calculated and cached.

## makeCacheMatrix(x) accepts a matrix as an argument and creates an object, a list, to
## store its contents and its inverse. The list contains setter and getter functions: set, get, setinverse, and getinverse
## set(y) caches the contents of the matrix
## get() allows to retrieve the matrix
## setinverse() sets the inverse of the matrix
##getinverse() returns the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	
	x_inverse <- NULL
	set <- function(y){
		x <<- y
		x_inverse <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(inverse) x_inverse <<- inverse
	getinverse <- function() x_inverse
	
	##return the 
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve(x,...) returns a matrix that is the inverse of a matrix stored in object x
## It accepts object "x" created by the function makeCacheMatrix as an argument
## and may accept other arguments for function solve that computes the inverse.
##First, the method checks if the inverse of the matrix is stored in object x and returns it if so.
## If not, it computes the inverse, caches the value, and returns it. 

cacheSolve <- function(x, ...) {
       
        x_inverse <- x$getinverse()
       
       #return the inverse if it has already been cached  
        if (!is.null(x_inverse)){
        	return(x_inverse)
        }
        
        #compute the inverse of the matrix if it is not in cache
        data <- x$get()
        x_inverse <- solve(data,...)
        x$setinverse(x_inverse)
        
         ## Return a matrix that is the inverse of 'x'
        x_inverse
}
