
## Function that extends a matrix object
## to cache its computed inverse
makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        changed <- FALSE
        set <- function(y){
                ## check if matrix has been changed
                ## through set property
                changed <<- !all(x==y)    
                x <<- y
        }
        get<- function() x
        setinverse <- function(i) {
                inversed <<- i
                changed <<- FALSE
        }
        getinverse <- function() inversed
        ischanged <- function() changed
        list(get = get, set = set, setinverse = setinverse, 
             getinverse = getinverse, ischanged = ischanged)
}


## Function that returns the inverse of a cached matrix
cacheSolve <- function(x, ...) {
        ## Get the inversed property of the matrix, if there is
        i <- x$getinverse()
        changed <- x$ischanged()
        ## if the inverse has already been calculated and the matrix has not changed,
        ## return that calculated value
        if (!is.null(i) && !changed){
                message("getting cached data")
                return(i)
        }
        ## else, get the current matrix value
        m <- x$get()
        ## get the inverse of the matrix
        inv <- solve(m)
        ## set it to the inversed property
        x$setinverse(inv)
        ## return the inversed property
        inv
}
