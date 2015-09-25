## Functions to cache a matrix and it's inverse to global
## variables. This allows the functions to work with local
## variables but pull from the cached global variables if
## they are available reducing computational overhead.

##Usage: cachedMatrix <- makeCacheMatrix(testMatrix)
##       cacheSolve(cachedMatrix)


## Cache a matrix to a global variable.

makeCacheMatrix <- function(x = matrix()) {

    ## Check to see if the matrix is square and warn
    ## if not

    if(nrow(x) != ncol(x)){

      message("The supplied matrix is not square and the
      results cannot be used in the cacheSolve function.")

    }

    ## Ensure the local matrix variable is empty
    m <- NULL

    ## Define internal functions

    set <- function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m

    ## 'Return' results

    list(set = set, get = get,
      setmatrix = setmatrix,
      getmatrix = getmatrix)

}


## Solves and caches a matrix using the output of the
## makeCacheMatrix function above. Although the
## makeCacheMatrix function will cache matrices of any
## size only square matrices can be solved. Attempting
## to solve a non-square matrix will result in an error.

cacheSolve <- function(x, ...) {

    ## Set local matrix to result of 'getmatrix'

    m <- x$getmatrix()

    ## If there is a local m value pull the cached data.

    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }

    ## Solve the matrix.

    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)

    ## 'Return' solved matrix
    m

}
