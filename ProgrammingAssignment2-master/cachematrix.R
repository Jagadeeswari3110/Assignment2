## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix())
    {
        n<-NULL
        set<-function(y)
        {
            x<<-y
            n<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) n <<- solve
        getmatrix<-function() n
        list(set=set, get=get,
            setmatrix=setmatrix,
            getmatrix=getmatrix)
    
    }


## This funciton computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been computed then cacheSolve should retrieve the inverse

cacheSolve <- function(x, ...) 
    {
    ## Return a matrix that is the inverse of 'x'
        n<-x$getmatrix()
        if(!is.null(n))
        {
            message("getting cached data")
            return(n)
        }
        matrix<-x$get()
        n<-solve(matrix, ...)
        x$setmatrix(n)
        n
        
    }
