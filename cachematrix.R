makeCacheMatrix <- function(mat = matrix()) {
    ## Takes in a matrix and returns a cache object (using an R list)
    ## designed to facilitate the storage and retrieval of a
    ## computationally expensive result (e.g. matrix inversion).
    ## The local environment holds two local variables: the input value `mat' 
    ## and the value of interest `inv' and provides read/write access to them
    ## through corresponding getters and setters.
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
    ## stored value of interest (the matrix inverse); if this value is not
    # available it reads in the matrix, computes the inverse, stores the result 
    # in the corresponding slot of the cache object and returns the result.
    inv = cmat$getinv()
    if(!is.null(inv)) return(inv)
    ninv = solve(cmat$get(), ...)
    cmat$setinv(ninv)
    ninv
}