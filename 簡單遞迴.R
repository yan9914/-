# ­pºâ¶¥­¼factorial
fact <- function(x){
    if (x == 1){
        return(1)
    }
    else{
        return(x*fact(x-1))         #°ïÅ|fact¨ç¼Æ
    }
}

fact(5)
