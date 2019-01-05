#############################################################
# 從朋友圈找到芒果商                                        #
#                                                           #
#                                                           #
#                      -------               -------        #
#               ____  | alice |  __________ | peggy |       #
#              /       -------               -------        #
#             /                           /                 #
#            /                           /                  #
#  -----    /           -----           /    ------         #
# | you |  _________   | bob |   ______/___ | anuj |        #
#  -----    \           -----                ------         #
#            \                                              #
#             \                              ------         #
#              \      --------         ____ | thom |        #
#               \___ | claire |  _____/      ------         #
#                     --------        \                     #
#                                      \     -------        #
#                                       \__ | jonny |       #
#                                            -------        #
#                                                           #
#############################################################


#用list建立雜湊表, 將各名稱對應到不同的值
graph <- list()
graph$you <- c("alice","bob","claire")
graph$bob <- c("anuj","peggy")
graph$alice <- c("peggy")
graph$claire <- c("thom","jonny")

 
#建立判斷是否為芒果商的函數, 這裡簡單假設名字字尾為m的人就是芒果商
who.is.seller <- function(a){
    return(substr(a,nchar(a),nchar(a))=="m")
}


#建立從朋友圈尋找芒果商的函數
search.seller <- function(name){
    search_queue <- c()                             #建立佇列
    search_queue <- c(search_queue, graph[[name]])  #將一等連接排入佇列
    searched <- c()                         #搜尋過的名單
    while (length(search_queue) != 0){      #只要佇列還有人便執行迴圈
        per <- search_queue[1]              #佇列為FIFO
        search_queue <- search_queue[-1]    #先進先出
        if(!(per %in% searched)){           #排除搜尋過的人, 避免無限迴圈
            if(who.is.seller(per)){
                print(paste(per," is a mango seller!"))
                return("TRUE")
            }else{          #若該人不是芒果商, 將該人的朋友(二等連接)排入佇列
                search_queue <- c(search_queue,graph[[per]])
                searched <- c(searched,per) #並將他加入搜尋過的名單
            }
        }
    }
    return("FALSE")     #找不到人回傳"FALSE"
}

search.seller("you")
