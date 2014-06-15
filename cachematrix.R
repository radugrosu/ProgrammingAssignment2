makeCacheMatrix <- function(mat = matrix()) {
    ## Takes the matrix provided as input and provides an object (R list)
    ## that stores the (computationally expensive) result of matrix inversion
    ## and makes it available as long as the initial matrix doesn't change.
    ## The values of the matrix and its inverse are read/written through
    ## getters and setters which act on the `mat' and `inv' values in the
    ## local environment.
    inv = NULL
    get = function() mat
    set = function(nmat) {mat <<- nmat; inv <<- NULL}
    getinv = function() inv
    setinv = function(ninv) inv <<- ninv
    list(get=get, set=set, getinv=getinv, setinv=setinv)
}

cacheSolve <- function(cmat, ...) {
    ## Takes in an object of type CacheMatrix (typically an R list created
    ## by the companion function makeCacheMatrix) and tries to retrieve the
    ## stored value of the inverse; if this value is not available it computes
    ## the inverse, stores the result in the corresponding slot of the input 
    ## object and returns the result.
    inv = cmat$getinv()
    if(!is.null(inv)) return(inv)
    ninv = solve(cmat$get(), ...)
    cmat$setinv(ninv)
    ninv
}