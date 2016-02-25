## This pair of fucntions will calculate inverse for the matrix and save the result in cache.
## So in case you will need inverse matrix for this matrix again, you may skip computations.


## This function does nothing but save the matrix and creates a list of functions applicable
## to this particular matrix and also creates cache variable s to store the solve result. 
## In the beginning s set to NULL, and after matrix solved, the result will be stored here.

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        setMatrix<- function (y) {
                x<<-y
                s<<-NULL
        }
        getMatrix <-function() x
        setSolve <-function(solve) s<<-solve
        getSolve <- function () s
        list (set=setMatrix, get=getMatrix, setSolve=setSolve, getSolve=getSolve)
}


## This function uses the previous function as an argument. 
## First it checks, if this matrix was calculated before.
## If true it just returns the previous result and exit. If not, it
## solves the matrix, saves the result to the cache and returns the result.

cacheSolve <- function(x, ...) {
        s<-x$getSolve()
        if (!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data, ...)
        x$setSolve(s)
        s  ## Return a matrix that is the inverse of 'x'
         
}
