## Creates and saves the inverse matrix of a user supplied square matrix. Caches
## the last inverse matrix value evaluated. 
## Assumes invertability of passed matrices.
 


## Creates matrix and special list of functions whose purpose is 
## to find and set inverse values for supplied matrices.
## Pass standard matrix arguments. matrix(c(),row#, column#, ...)

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL 
    
    set <- function(y){ ## Sets new matrices whose inverses we wish to find
        x <<- y
        inv <<- NULL
    }
    get <- function() ## Returns the current matrix 
        x
    setInv <- function(inverse) ## Sets the inverse matrix 
        inv <<- inverse
    getInv <- function() ## Returns the inverse matrix 
        inv
    
    ## Allows us to call and set makeCacheMatrix functions by using extractor '$' 
    list(set = set, get = get, 
          setInv = setInv,
          getInv = getInv) 
}


## Function finds inverse matrix if not cached for the passed matrix and 
## then moves to cache the result. Otherwrise returns cached inverse matrix.
## uses solve() and lets 'b' be taken as identity matrix
cacheSolve <- function(x, ...) {
       inv <- x$getInv()  ## retrieves inverse value
       
       if(!is.null(inv)){ ## checks to see if inverse matrix already exists and returns it
           message("retrieving cached inverse matrix")
           inv 
       }
       
       matrix <- x$get()        ## If inverse matrix doesn't exist: finds current
       inv <- solve(matrix,...) ## matrix, solves, caches and returns it.
       x$setInv(inv)    
       inv              
       
}
