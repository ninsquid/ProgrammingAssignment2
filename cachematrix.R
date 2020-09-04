## cacheSolve(makeCacheMatrix()) will take in any nonsingular matrix 
## and produce value based on previous inputs


## Default value of m set as null so whenever x is reset m is also reset to null
## The new x is inversed and cached in setimat which is retrieved on next call
## via getimat
## Provide a list which can be accessed by cacheSolve using $ operator

makeCacheMatrix <- function(x = matrix()) {
                   m <- NULL
                   set <- function(y) {
                      x <<- y
                      m <<- NULL
  }
                   get <- function() x
                   setimat <- function(solve) m <<- solve
                   getimat <- function() m
                   list(set = set, get = get,
                        setimat = setimat,
                        getimat = getimat)
}

## Retrieves getimat from makeCacheMatrix & if m is !null then prints cache value
## If m is null then inverses input matrix and assign it to setimat for next call

cacheSolve <- function(x, ...) {
              m <- x$getimat()
              if(!is.null(m)) {
                message("getting cached data")
              return(m)
  }
              data <- x$get()
              m <- solve(data, ...)
              x$setimat(m)
              m
        ## Return a matrix that is the inverse of 'x'
}
