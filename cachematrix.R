## Coursera-R-Programming
## Week-3
## Assignment 2: Lexical Scoping
## Daniel de Wet
## 2016-05-22

# Write a pair of functions that cache the inverse of a matrix

# This function creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # set the inverse indicator to null
    set <- function(y) { # used to set a new matrix
        x <<- y # replace the old matrix
        i <<- NULL # set the inverse indicator to null
    }
    get <- function() x # get the matrix
    setInverse <- function(inverse) i <<- inverse 
    getInverse <- function() i # establish whether the matrix has been inverted
    list(set = set
         ,get = get
         ,setInverse = setInverse
         ,getInverse = getInverse)
}

# This function computes the inverse of the special 'matrix' returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), then the
# cacheSolve should retrieve the inverse of the cache
cacheSolve <- function(x,...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) { # establish whether the matrix has been inverted
        message('Getting cached data') # if so display the cached matrix
        return(i)
    }
    m <- x$get() # if not load the matrix
    i <- solve(m,...) # and perform the inversion
    x$setInverse(i)
    i
}

## Testing the functions
myMatrix <- makeCacheMatrix(matrix(1:4,2,2))
myMatrix$get()
myMatrix$getInverse()
cacheSolve(myMatrix)

cacheSolve(myMatrix)
myMatrix$getInverse()

myMatrix$set(matrix(c(9,9,1,16),2,2))
myMatrix$get()
myMatrix$getInverse()
cacheSolve(myMatrix)

cacheSolve(myMatrix)
myMatrix$getInverse()