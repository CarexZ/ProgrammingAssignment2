## This pair of fucntions will calculate inverse for the matrix and save the result in cache.
## So in case you will need inverse matrix for this matrix again, you may skip computations.


## This function does nothing but save the matrix and creates a list of functions applicable
## to this particular matrix and also creates cache variable s to store the solve function result. 
## In the beginning s set to NULL, and after matrix solved, the result will be stored here.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL ##initially set 's' to NULL
        set <- function (y) { ##this part is actually useless and can be omitted, as it will be never called by cacheSolve
                x <<- y
                s <<- NULL
        }
        get <- function() x ##return the matrix
        setSolve <- function(solve) s <<- solve ##assign 'solve' to s 'variable'
        getSolve <- function() s ##return 's' (solved matrix or NULL)
        list (set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


## This function uses the previous function as an argument. 
## First it checks, if this matrix was calculated before.
## If true it just returns the previous result and exit. If not, it
## solves the matrix, saves the result to the cache and returns the result.

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if (!is.null(s)) { ##check if this matrix was solved before
                message("getting cached data")
                return(s) ##return result from cache and exit the function
        }
        data <- x$get()  ##if matrix was not solved, assign matrix 'x' to 'data' variable
        s <- solve(data, ...) ##solve this matrix
        x$setSolve(s)  ##save the result to s
        s  ## Return a matrix that is the inverse of 'x'
         
}
