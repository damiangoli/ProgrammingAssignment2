## makeCacheMatrix and cacheSolve functions will be showed for the week 3 assignement of the coursera class.
## makeCacheMatrix will create a matrix that can cache its inverse
## cacheSolve will compute the inverse of the matrix provided by makeCacheMatrix function.
## If the matrix has not changed, cacheSolve will use the cache to retreive the inverse.


## First function,
## giving the matrix it will cache the inverse.  
makeCacheMatrix <- function(x = matrix()) {
  mCMinv <- NULL
  set <- function(y) {
    x <- y
    mCMinv <- NULL
  }
  get <- function() x
  setinv <- function(inv) mCMinv <- inv
  getinv <- function() mCMinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Next function is returning a matrix that is the inverse of 'x', or retreaving the inverse from the cache
## if the inverse has already been calculated (and the matrix has not changed)

cacheSolve <- function(x, ...) {
  mCMinv <- x$getinv()
  if(!is.null(mCMinv)) {
    message("getting cached data")
    mCMinv
  }
  mat <- x$get()
  mCMinv <- solve(mat)
  x$setinv(mCMinv)
  mCMinv
}
