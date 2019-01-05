quicksort <- function(array){           #################################
    if (length(array)<2){               #  剩0個或1個元素就return       #
        return(array)                   #  設定樞元                     #
    }else{                              #  分為大於樞元及小於樞元兩組   #
        pivot <- array[1]               #  用遞迴各個擊破               #
        less <- array[array < pivot]    #################################
        greater <- array[array > pivot]
        return(c(quicksort(less),pivot,quicksort(greater)))
    }
}

quicksort(c(10,5,2,3))