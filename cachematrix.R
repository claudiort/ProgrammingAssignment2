## makeCacheMatrix -> creates a special matrix that caches it's inverse.
## cacheSolve -> calculates the inverse matrix of a matrix created by 
##               makeCacheMatrix function

## Creates a special matrix that caches it's inverse matrix and returns a list
## with access functions:
##      -> setInteralMatrix: stores the actual matrix.
##      -> getInternalMatrix: returns the stored matrix.
##      -> storeInvMatrix: stores the inverse matrix in the cache.
##      -> getInvMatrix: retuns the inverse matrix stored in the cache.
makeCacheMatrix <- function( internalMatrix = matrix()) {
        invCache = NULL
        setInternalMatrix <- function( newMatrix )
        {
                internalMatrix <<- newMatrix
                invCache <<- NULL
        }
        getInternalMatrix <- function() { internalMatrix }
        storeInvMatrix <- function( someMatrix ) { invCache <<- someMatrix }
        getInvMatrix <- function() { invCache }
        
        list( getInternalMatrix = getInternalMatrix,
              setInternalMatrix = setInternalMatrix,
              getInvMatrix = getInvMatrix,
              storeInvMatrix = storeInvMatrix)
}


## Similar to solve, but works on the list returned by makeCacheMatrix.
## Attempts to get the cached inverser matrix, if the cache is invalid (NULL)
## then it calculates the inverse and stores it in the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getInvMatrix()
        if( is.null(inv) )
        {
            inv <- solve( x$getInternalMatrix() )
            x$storeInvMatrix(inv)
        }
        else
        {
            print( "Using cached inverse matrix" )
        }
        return(inv)
}
