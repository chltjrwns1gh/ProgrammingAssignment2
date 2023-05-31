makeCacheMtrix <- function(x = matrix()){cache_memory <- NULL
    set <- function(cache_input){
        x <<- cache_input
        cache_memory <<- NULL # setting new value -> need to clear cache_memory(saving matrix_inverse)
    }
    get <- function() return(x)
    set_inverse <- function(inverse) cache_memory <<- inverse
    get_inverse <- function() return(cache_memory)
    return(list(set=set, get=get,
                set_inverse=set_inverse,
                get_inverse=get_inverse))
}
cacheSolve <- function(matrix_original){cache_memory <- matrix_original$get_inverse()
    if(!is.null(cache_memory)){
        print("getting cached data")
        return(cache_memory)
    }
    else{
        data <- matrix_original$get()
        cache_memory <- solve(data)
        matrix_original$set_inverse(cache_memory)
        return(cache_memory)
    }
}

matrix_example <- matrix(sample(1:100, size=9), nrow=3, ncol=3)

while(lapply(eigen(matrix_example), prod)[1] == 0){
    matrix_example <- matrix(sample(1:100, size=9), nrow=3, ncol=3)
}

cache_1 = makeCacheMtrix(matrix_example) # cache creating
cacheSolve(cache_1)
cacheSolve(cache_1)

