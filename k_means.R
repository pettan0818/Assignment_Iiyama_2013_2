#データの読み込み

udata <- read.csv("udata+aorder_labeled_work.csv")
id <- c(9999, 0, 1, 90, 7, 2, 0, 25, 6, 1, 1)

#クラスタリングと結合
udata.r <- rbind(udata,id)
udata.k <- kmeans(udata.r,8)
udata.t <- transform(udata,cluster=udata.k$cluster[1:5000])

#同クラスタの人を抽出
udata.s <- subset(udata.t,cluster==udata.k$cluster[5001])

#同クラスタの人の各すし順位の平均
udata.s.o <- udata.s[,13:22]
o.mean <- apply(udata.s.o,2,mean)

#並び替え
sort(o.mean)