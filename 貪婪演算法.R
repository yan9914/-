#以最少的電台數,覆蓋全部的州
states.needed <- c("mt","wa","or","id","nv","ut","ca","az")
#電台清單及它們能覆蓋的州的雜湊表
stations <- list()
stations$kone <- c("id","nv","ut")
stations$ktwo <- c("wa","id","mt")
stations$kthree <- c("or","nv","ca")
stations$kfour <- c("nv","ut")
stations$kfive <- c("ca","az")
#選取的清單
final.stations <- c()

while (length(states.needed) != 0){     #做到全部的州被覆蓋為止
    best.station <- NULL                #初始化電台選擇
    states.covered <- c()               #初始化best.station覆蓋的州
    for (station in names(stations)){   #對於在清單內的電台
        states.for.station <- stations[[station]]   #此電台覆蓋的州
        #此電台有覆蓋 且 還沒被其他電台覆蓋過 的州 = 此電台多覆蓋的州
        covered <- intersect(states.needed, states.for.station)
        #若此電台多覆蓋的州數 多於 目前best.station覆蓋的州數
        if (length(covered) > length(states.covered)){
            best.station <- station     #將此電台設為best.station
            states.covered <- covered   #更新best.station覆蓋的州
        }
    }
    #覆蓋過的州從需求清單移除
    states.needed <- setdiff(states.needed, states.covered)
    #將發現的best.station加入選取清單
    final.stations <- c(final.stations, best.station)
}
#最後選取的電台
print(final.stations)

