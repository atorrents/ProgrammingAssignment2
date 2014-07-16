## Time-saving system, caching the inverse of a matrix
## when we need calculate it several times

## function: makeCacheMatrix
## (uses special operator <<- which stores values in "parent" environment)

## Usage:
## a<-makeCacheMatrix(matrix <default NULL>) >> creates cache matrix a
## This matrix will cache its inverse by means of four methods
##  managed by the function cacheSolve:
##      a$get() >> gets actual value of a
##      a$set(matrix) >> sets matrix as the value of a (clear cache)
##      a$setSolve() >> caches the inverse of a
##      a$getSolve() >> delivers (from cache) the inverse of a

makeCacheMatrix <- function(x = matrix()) {
    ## creates cache and methods list
    m<<-NULL        ## in "parent" env.
    set<-function(y){
        x<<-y       ## set value in "parent" env.
        m<<-NULL    ## in "parent" env.
    }
    get<-function() x
    setSolve<-function (inverse) m<<-inverse
    getSolve<-function() m
    list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
}


## function: cacheSolve

## Usage: cacheSolve(a) >> retrieves (from cache, if available;
##      otherwise computes and caches) the inverse of a

cacheSolve <- function(x, ...) {
        ## Return the inverse matrix of 'x'
    m<-x$getSolve()         ## Trying to avoid calculation 
    if(!is.null(m)){        ## Success, inverse cached!
        ##  message("getting cached data") - unnecessary here
        return(m)
    }
    data<-x$get()           ## Getting matrix to invert
    m<-solve(data, ...)     ## Inverse: time-consuming calculation
    x$setSolve(m)           ## Caching result, time-saving next time
    m
}
