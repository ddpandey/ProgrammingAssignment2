
## Generates a cached matrix based on the inputs

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

cacheSolve <- function(x, ...) {

## Invoking the solve() function to return the inverse

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
