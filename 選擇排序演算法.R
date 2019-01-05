find.smallest <- function(arr){                 #找最小值的函數
    smallest <- arr[1]
    smallest_index <- 0
    for (i in c(1 : length(arr))){
        if (arr[i] <= smallest){
            smallest <- arr[i]
            smallest_index <- i
        }
    }
    return(smallest_index)
}

selection.sort <- function(arr){                #依序放進新向量裡
    new.arr <- c()
    arr <- arr[!is.na(arr)]
    for (i in c(1 : length(arr))){
        smallest <- find.smallest(arr)
        new.arr <- c(new.arr,arr[smallest])
        arr <- arr[-smallest]
    }
    return(new.arr)
}

selection.sort(c(NA,8,7,3,NA,10))
