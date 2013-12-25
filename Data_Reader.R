# ここはご自分の環境に合わせて変更のこと

setwd("C:/Dropbox/Document/大学授業/C4-飯山ゼミ/+Assignment/@Data")

# Sushi3.idata
# おすしのアイテムデータ
idata=read.csv("sushi3.idata.csv",header=F)
names(idata)=c("ID","ネタの名前","スタイル","大分類","小分類","こってり度","食べる頻度","価格(正規化)","販売頻度")


# Sushi3.udata
# 質問を受けた人のデータ
udata=read.csv("sushi3.udata.csv",header=F)
names(udata)=c("ID","Sex","Age","回答時間","15歳までの県","15歳までの地域","15歳までの東西","今住んでいる県","今住んでいる地域","今住んでいる東西","今と昔が一緒=0")

# アイテム集合Aの各人の好きなもの順
item_a_ranking=read.csv("sushi3a.5000.10.order.csv")
item_a_ranking=item_a_ranking[,-c(1,2)]
names(item_a_ranking)=c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
head(item_a_ranking)

# アイテム集合Bの各人の好きなもの順
item_b_ranking=read.csv("sushi3b.5000.10.order.csv")
item_b_ranking=item_b_ranking[,-c(1,2)]
names(item_b_ranking)=c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
head(item_b_ranking)

