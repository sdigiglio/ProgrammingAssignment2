#Function to make the inverse of a matrix 
#create the MakeCacheMatrix which is a function

makeCacheMatrix <- function(x =matrix()) {
        inv <- NULL 
        set <- function(y) {
#double arrow needed to consider the environment/variables/parameters of the first function when calling the second function
                x <<-y 
                inv <<- NULL 
        }
        get <- function() {x}
        setInv <- function(inv) {inv <<- inv}
        getInv <- function() {inv}
        list(set = set, get = get, setInv = SetInv, getInv = getInv)
}

#second function created to computes the inverse of the special Martix returned by the 
# makeCacheMatrix above. If the inverse has alrady been calculated (and the matric has not changed) 
#then the cacheSolve should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setInv(inv_x)
                return(inv_x)
        }
}