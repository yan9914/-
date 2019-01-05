#
#                  6
#               ------> A ------> 1
#              /                 \ 
#             /         ^         \
#    start ---        3 |          --- final
#             \         |         /
#              \   2             / 
#               ------> B ------> 5
#
#
#
graph <- list()                                     #圖形
graph[["start"]] <- list(a=6, b=2)
graph[["a"]] <- list(final=1)
graph[["b"]] <- list(a=3, final=5)
graph[["final"]] <- list()

costs <- list(a=6, b=2, final=Inf)                  #成本

parents <- list(a="start", b="start", final=NULL)   #父項

processed <- c()    #追蹤已處理的節點

find.lowest.cost.node <- function(costs){       #尋找最低成本的節點
    lowest.cost <- Inf
    lowest.cost.node <- NA
    for (node in names(costs)){
        cost <- costs[[node]]
        if (cost < lowest.cost & !(node %in% processed) ){
            lowest.cost <- cost
            lowest.cost.node <- node
        }
    }
    return(lowest.cost.node)
}

node <- find.lowest.cost.node(costs)
while (!is.na(node)){
    cost <- costs[[node]]
    neighbors <- graph[[node]]
    for (n in names(neighbors)){
        new.cost <- cost + neighbors[[n]]   #通往相鄰節點的總成本
        if (costs[[n]] > new.cost){         #如果通過該節點前往相鄰節點較便宜
            costs[[n]] <- new.cost
            parents[[n]] <- node
        }
    }
    processed <- c(processed, node)
    node <- find.lowest.cost.node(costs)    #尋找下一個須處理的節點並且重複
}

road <- "final"
i <- "final"
while (i != "start"){
    road <- paste(parents[[i]]," -> ",road)
    i <- parents[[i]]
}
print(road)