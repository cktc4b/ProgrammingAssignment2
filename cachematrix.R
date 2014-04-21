## These two functions create a place to store information about the inverse of a matrix
## and a way to use the stored inverse of a matrix to save computing power

## The first function creates variables to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        x <- x
        set <- function (y) {
                x <<- y
                m <<- Null
        }
        get <- function () x
        seti <- function (solve) m <<- solve
        geti <- function () m
        list(set = set, get = get, seti = seti, geti = geti)
}


## The second checks checks for a cached value for the inverse of a given matrix, and if
## available returns the inverse matrix, if none is available it then calculates the 
## inverse

cacheSolve <- function(x, ...) {
        m <- x$geti()
        if(!is.null(m)) {
                message("getting chached data")
                return(m)
        }
        else{
                data <- x$get()
                m <- solve(data, ...)
                x$seti(m)
        }
        m
}

