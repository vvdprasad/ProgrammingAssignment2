# Matrix inversion is usually a costly computation. For repeated computations, 
# the inverse can be cached and retrieved. The following functions cache
# the inverse of a matrix and retrieves it from cache on a repeat 
# operation.
#
# To test this, run the following:
# a<-makeCacheMatrix()
# a$set(matrix(runif(4000000),2000,2000))
# cacheSolve(a)  # This will perform the inverse and caches it
# cacheSolve(a)  # This will get the result from cache instead of recomputing
# 
# Observe that the first call to cacheSolve(a) will take a little while and
# the second call will retrieve the result very fast (as it is fetched from 
# cache)



# makeCacheMatrix function creates a special "matrix" that can cache its inverse
# It contains a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse'd matrix
# 4. get the value of the inverse'd matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

# cacheSolve function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    mymatrix<-x$get()
    m<-solve(mymatrix, ...)
    x$setmatrix(m)
    m
}
