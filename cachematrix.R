
## Generates a cached matrix based on the inputs
## The following function calculates the mean of the special "matrix" created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Invoking the solve() function to return the inverse
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## 

cacheSolve <- function(x, ...) {

## Computing the inverse of a square matrix can be done with the solve function in R.
      m <- x$solve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
    
        m <- mean(data, ...)
        x$setmean(m)
        m
}
