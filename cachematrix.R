## makeCacheMatrix() creates a special "Matrix" which returns a
## list of functions. cacheSolve() determines the inverse of the
## given matrix by taking the special "Matrix" as argument

## makeCacheMatrix() takes the matrix whose inverse is to be
## determined and uses the functions which performms following operations:
##      1) set the value of vector
##      2) get the value of vector
##      3) set the value of mean
##      4) get the value of mean
## Then the function creates a special "Matrix" with list of above mentioned functions.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve() takes the special "Matrix" generated in makeCacheMatrix()
## Then, it checks if the inverse is already determined. If it is already
## determined, the function returns the inverse else it will calculate the 
## inverse (using solve()).

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
        m
        
}
