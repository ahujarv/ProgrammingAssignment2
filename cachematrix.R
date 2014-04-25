## Set of these two functions that 
## 1. create a matrix that can cache inverse of the square matrix provided
## 2. check in cache if the inverse of provided square matrix already exists
## 3. return the inverse matrix from cache if found in cache
## 3. calculate the inverse matrix, if not found in cache then cache and return it.

## This function creates a special square matrix that can hold the inverse
## matrix of the square matrix provided as input.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMtrx <- function(mtrx) {
                X <<- mtrx
                inv <<- NULL
        }
        getMtrx <- function() {
                x
        } 
        setInvMtrx <- function(invmtrx) {
                inv <<- invmtrx
        }
        getInvMtrx <- function() {
                inv
        }
        list(setMtrx = setMtrx, getMtrx = getMtrx, getInvMtrx = getInvMtrx,
             setInvMtrx = setInvMtrx)
}


## This function calculates the inverse of the square matrix provided.  
## If the incoming squre matrix is the same as one provided before, 
##    then this function returns the inverse from cache instead of calculating it 
##    else it calculates the inverse,
##            stores the calcualted inverse in cache and 
##            returns the calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMtrx <- x$getInvMtrx()
        if(!is.null(invMtrx)) {
                message("getting cached Inverse Matrix")
                return(invMtrx)
        }
        mtrx <- x$getMtrx()
        invMtrx <- solve(mtrx, ...)
        x$setInvMtrx(invMtrx)
        invMtrx
}