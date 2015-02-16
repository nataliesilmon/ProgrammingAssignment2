#This script will take a square or complex matrix, cache its inverse, and return the inverse of the input matrix.

#makeCacheMatrix creates a special matrix object that can cache its inverse. #Note: Non-square and non-complex matrices cannot have an inverse!

makeCacheMatrix<-function(x=matrix()){
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	 setmatrix <- function(solve) m <<- solve
            getmatrix <- function() m
            list(set = set, get = get,
                 setmatrix = setmatrix,
                 getmatrix = getmatrix)
	}

#cacheSolve function takes a cacheable matrix as input and returns the inverse of that matrix.

cacheSolve <- function(x, ...) {
		    m<-x$getmatrix()
	            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setmatrix(m)
            m

	
        ## Return a matrix that is the inverse of 'x'
}