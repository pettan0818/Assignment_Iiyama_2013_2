# お呼び出し�?�方�?
# source("C:/Dropbox/Document/大学授業/C4-飯山ゼ�?/+Assignment/Data_Reader.R",encoding="utf-8",echo=T)

# ここはご�?��?の環�?に合わせて変更のこと

# Local用設定ファイルの読みこみ
source("local.r")

setwd("C:/users/kazu/documents/test/assignment_iiyama_2013_2/@data/original_files")
# Sushi3.idata
# おすし�?�アイ�?�?�?ータ
idata=read.csv("sushi3.idata.csv",header=F)
names(idata)=c("ID","ネタの名前","スタイル","大�?�?","小�??�?","こってり度","食べる�?�度","価格(正規化)","販売頻度")


# Sushi3.udata
# 質問を受けた人の�?ータ
udata=read.csv("sushi3.udata.csv",header=F)
names(udata)=c("ID","Sex","Age","回答時�?","15歳までの�?","15歳までの地�?","15歳までの東西","今住んで�?る県","今住んで�?る地�?","今住んで�?る東西","今と昔が一�?=0")

# アイ�?�?�?�?Aの�?人の好きなも�?��?
item_a_ranking=read.csv("sushi3a.5000.10.order.csv",header=F)
item_a_ranking=item_a_ranking[,-c(1,2)]
names(item_a_ranking)=c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
head(item_a_ranking)
summary(item_a_ranking)

# アイ�?�?�?�?Bの�?人の好きなも�?��?
item_b_ranking=read.csv("sushi3b.5000.10.order.csv",header=F)
item_b_ranking=item_b_ranking[,-c(1,2)]
names(item_b_ranking)=c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
head(item_b_ranking)
summary(item_b_ranking)