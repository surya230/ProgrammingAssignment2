
## 27 January 2018
##
## This file contains two functions: makeCacheMatrix and cacheSolve. These functions take
## advantage of the lexical scoping convention in the R language. The functions work together
## to return the inverse of an input matrix. However, the computationally costly inverse operation
## is only performed once for a given input; repeated calls will retrieve the inverse from the cache
## instead.

## makeCacheMatrix
## This function takes a matrix as input and returns a 4-element list, including the 
## inverse of the input matrix. The elements in the list form a matrix-like object that
## can cache the matrix inverse.

## Example for an invertible 3x3 matrix M0 and its inverses M1 and M2:

##>  M0 <- matrix( rbind( c(2,4,6),c(-1,3,6),c(9,1,2)), nrow=3, ncol=3 )
##>  M0
##         [,1] [,2] [,3]
##  [1,]    2    4    6
##  [2,]   -1    3    6
##  [3,]    9    1    2

##> M1 <- cacheSolve( M0 )
##> M1                                  Matrix inverse computed and cached
##        [,1]        [,2]       [,3]
##  [1,]  0.0 -0.03571429  0.1071429
##  [2,]  1.0 -0.89285714 -0.3214286
##  [3,] -0.5  0.60714286  0.1785714

##> cacheSolve( M1 )
##  getting cached data                 Matrix inverse retrieved from cache
##        [,1]        [,2]       [,3]
##  [1,]  0.0 -0.03571429  0.1071429
##  [2,]  1.0 -0.89285714 -0.3214286
##  [3,] -0.5  0.60714286  0.1785714

##> M2 <- cacheSolve( M1 )
##  getting cached data

##> round(M0 %*% M2)                    Confirming that original x inverse = identity
##         [,1] [,2] [,3]
##  [1,]    1    0    0
##  [2,]    0    1    0
##  [3,]    0    0    1

makeCacheMatrix <- function( x = matrix() ) {
  
  Minv <- NULL
  set <- function(y) {
    x <<- y
    Minv <<- NULL
  }
  get    <- function() x
  setinv <- function(solve) Minv <<- solve
  getinv <- function() Minv
  
  list( set = set, get = get, setinv = setinv, getinv = getinv )
}

## cacheSolve
## This function returns the matrix inverse of the input matrix, either from a fresh 
## calculation or from cache.

cacheSolve <- function(x, ...) {
  
  Minv <- x$getinv()
  
  if(!is.null( Minv )) {
    message("getting cached data")
    return( Minv )
  }
  data <- x$get()
  Minv <- solve( data, ... )
  x$setinv( Minv )
  Minv
}
