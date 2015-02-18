### Assigment 2 ############
## We will assume that the matrix supplied is always invertible

## makeCacheMatrix: Function which creates a special "matrix" object
## that can cache its inverse. 

makeCacheMatrix<-function(x=matrix()) {
	A<-NULL
	set<-function(y){
		x<<-y
		A<<-NULL
	}
	get<-function() x
	setsolve <- function(solve) A<<-solve
	getsolve <- function() A
	list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

## cacheSolve: Function that computes the inverse of the special
## "matrix" returned by the function makeCacheMatrix. It checks if the 
## inverse has already been calculated (and the matrix has not changed), then
## the function cacheSolve should retrieve the inverse from the cache. In other case, the 
## function compute the inverse.

cacheSolve <- function(A, ...) {
        I <- A$getsolve()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- A$get()
        I <- solve(data, ...)
        A$setsolve(I)
        I
}



