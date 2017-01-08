# FAST MATRIX INVERSION ---------------------------------------------------

# Caching and using the inversed matrix with 2 functions instead of 
# using precious computational resources by enumerating it again and again.

# NOTE: The input matrix has to be invertible (ncol = nrows)
# Functions ---------------------------------------------------------------

## Creating the cached (inversed) matrix
makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL
        set <- function(y) {
                x <<- y
                mx <<- NULL
        }
        get <- function() x
        set.inv <- function(solve) mx <<- solve
        get.inv <- function() mx
        list(set = set, get = get, 
             set.inv = set.inv, get.inv = get.inv)
}

## Returning the cached inversed matrix(if computed with 'makeCacheMatrix')
## Otherwise computing the inversed matrix with the cached values
cacheSolve <- function(x, ...) {
        mx <- x$get.inv()
        if(!is.null(mx)) {
                message("cached data")
                return(mx)
        }
        data <- x$get()
        mx <- solve(data, ...)
        x$set.inv(mx)
        mx
}

# Test 01 -----------------------------------------------------------------

t1 <- matrix(data = 1:4,nrow = 2,ncol = 2)
m1 <- makeCacheMatrix(t1)
cacheSolve(m1)
#cached data
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

identical(cacheSolve(m1),solve(t1))
#[1] TRUE

# Test 02 -----------------------------------------------------------------
# NOTE: nrow != ncol
x <- c(1,2,3,4,5,
       1,2,3,4,5,
       1,2,3,4,5)

m <- matrix(x, ncol=5, byrow=TRUE)
m

solve(m) 
# Error in solve.default(m) : 'a' (3 x 5) must be square

m2 <- makeCacheMatrix(m)
cacheSolve(m2)
# Error in solve.default(m) : 'a' (3 x 5) must be square

# Test 03 -----------------------------------------------------------------

x <- c(23,17,14,
       15,99,17,
       39,83,0)

t3 <- matrix(x, ncol=3, byrow=TRUE)
class(t3)
m3 <- makeCacheMatrix(t3)
cacheSolve(m3)
#            [,1]         [,2]         [,3]
#[1,]  0.02440923 -0.020101720  0.018977269
#[2,] -0.01146940  0.009445386  0.003131163
#[3,]  0.04525482  0.021554856 -0.034979068

identical(cacheSolve(m3),solve(t3))
# [1] TRUE
