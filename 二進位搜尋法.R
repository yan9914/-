binary.search <- function(list,item){       #定義函數,list需要經過排序
    low <- 1                                #設定頭尾
    high <- length(list)
    while(low <= high){
        mid <- round((low+high)/2)          #找中點,小數四捨五入
        guess <- list[mid]                  #猜測值在中點的位置
        if(guess==item) {                   #猜對,回傳位置
            return(mid)
        }else if(guess>item){               #猜測值太大,上限下修
            high <- mid-1
        }else{low <- mid+1}                 #猜測值太小,下限上修
    }
    return("None")                          #若找不到回傳結果None
}

x <- c(1,3,5,7,9)
binary.search(x,1)
binary.search(x,5)
binary.search(x,10)

y <- c("Chen","Lee","Wang","Lin","Tsai")
binary.search(y,"Lee")
binary.search(y,"Bachi")
