# Utility
###################################################################
# セッション判定(返り値:(BOOL)Interactive = F, Non-Interactive = T)
# 引数は、各入力データ(sex, age, pref_u15, pref_now)。これらがすべてNAの時スクリプトセッションだと判定する。
analyzer_session = function(a, b, c, d){	
	return(is.na(a) & is.na(b) & is.na(c) & is.na(d))
}

# K近傍法の実処理関数
# 引数は、元データ、新しく入力されたデータの二つ。
# 返り値は、同クラスタの人々のデータ
k_means = function(original, input_data, cluster = 8){
	# ユーザーの特徴量の切り出し
	# 5001行目のユーザーが今回入力されたデータ
	user_data = rbind(data[,c(1,2,3,4,5)],input_data)
	
	# クラスタリング(8個のクラスタによるK近傍法)執行
	k_class = kmeans(user_data, 8)
	cluster_data = k_class$cluster
	input_cluster = k_class$cluster[5001]
	cluster_data = cluster_data[-5001]
	
	# 同クラスタのデータ抽出
	same_cluster_users = data[cluster_data == input_cluster,]
	
	return(same_cluster_users)
}
##############################################################
source("../local.r",encoding='utf-8')

# 順位化データの読みこみ
data = read.csv("udata+aorder_labeled_work.csv", header = T, fileEncoding = 'shift-jis')

# 利用データの切り出し
use_column = c(3, 4, 6, 9, 12, 13:22)

data = data[,use_column]
colnames(data) = c("Sex","Age","pref_u15","pref_now","equivalent","shrimp","conger_eel","tuna","squid","sea_urchin","salmon_roe","egg","fatty flesh","tuna_roll","cucumber")
head(data)

# If this Session is non-interactive session, I'll use arguments.
sex = as.integer(readline(prompt = "Please, Enter your Sex...(M = 0, F = 1)> "))

age = as.integer(readline(prompt = "Please, Enter your Age...(-20 = 0, 20- = 1, 30- = 2, 40- = 3, 50- = 4, 60- = 5)> "))

pref_u15 = as.integer(readline(prompt = "Please Enter where had you lived in? (Answer in Pref Number)> "))

pref_now = as.integer(readline(prompt = "Please Enter where do you live in? (Answer in Pref Number)> "))

# [TODO]: 値域による異常値の削除

# Non-Interactive Session Analysis
if (analyzer_session(sex, age, pref_u15, pref_now)){
	sex = as.integer(commandArgs(trailingOnly = T)[1])
	age = as.integer(commandArgs(trailingOnly = T)[2])
	pref_u15 = as.integer(commandArgs(trailingOnly = T)[3])
	pref_now = as.integer(commandArgs(trailingOnly = T)[4])
	if (pref_u15 == pref_now) equivalent = 0 else equivalent = 1
}

# 今と昔が一緒.0の自動設定
if (pref_u15 == pref_now) equivalent = 0 else equivalent = 1

# New_Client
#"Sex","Age","pref_u15","pref_now","equivalent"
input_data = c(sex, age, pref_u15, pref_now, equivalent)

# (Should Remove) Default Nums(Should Remove)
if(0){
if (analyzer_session(sex, age, pref_u15, pref_now)){
	sex = 0
	age = 1
	pref_u15 = 13
	pref_now = 13
	equivalent = 0
	}
}

# subset関数、条件指定時に単純条件演算子を使うこと
# [TODO] Subsetでやりたい 
ans_data = data[(data[,1] == sex & data[,2] == age & data[,3] == pref_u15 & data[,4] == pref_now & data[,5] == equivalent),]
ans_data = subset(data, data[,1] == sex & data[,2] == age & data[,3] == pref_u15 & data[,4] == pref_now & data[,5] == equivalent)

# データベースに一致がないとき、K近傍法で処理する。
if (nrow(ans_data) == 0){
	print("Using K_means...")
	ans_data =  k_means(data, input_data, 4)
}

print(summary(ans_data))

rank = apply(ans_data[,6:15],2,mean)
# 最頻値を求めてみた。
rank = apply(ans_data[,6,15],2,table)

print(names(sort(rank)))