# Vaguly_Recommendation With Aggressive Function maker...

# This file includes test method...

# Utility
###################################################################
# Data_Reader
# 使う列数さえ指定してくれれば、自動で、名前もつけて返します。
data_reader = function(use_column){
	data = read.csv("udata+aorder_labeled_work.csv", header = T, fileEncoding = 'shift-jis')
	
	data = data[,use_column]
	colnames(data) = c("sex","age","pref_u15","pref_now","equivalent","shrimp","conger_eel","tuna","squid","sea_urchin","salmon_roe","egg","fatty flesh","tuna_roll","cucumber")
	
	return(data)
}

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
	# 与えられたデータをデータベースに追加した上で、分類実行。
	k_class = kmeans(user_data, cluster)
	cluster_data = k_class$cluster
	# For Debug
	print(head(cluster_data,100))
	print(table(cluster_data))
	# 与えられたデータのクラスタ番号のみ抽出
	input_cluster = k_class$cluster[5001]
	# 元々のデータのクラスタ番号を抽出
	cluster_data = cluster_data[-5001]
	
	# 同クラスタのデータ抽出
	same_cluster_users = data[cluster_data == input_cluster,]
	
	return(same_cluster_users)
}

# ホールドアウト検定用の関数
# tester(フラグ, オリジナルデータ, サンプリング個数)
# デフォルトでは、サンプリング個数は、1割。
# 返り値について、
tester = function(with_test_flag, data, sampler = nrow(data)/10){
	data_origin = data
	sampled_row = sample(nrow(data),500)
	data_sampled = data[sampled_row,]
	data = data[-sampled_row,]
}
##############################################################
source("../local.r",encoding='utf-8')

# 順位化データの読みこみ
use_column = c(3, 4, 6, 9, 12, 13:22)
data = data_reader(use_column)


# test Session
with_test_flag = as.integer(readline(prompt = "[Test Flag] Do you want to suggest with test?(Yes = 1, No = 0)> "))
if(is.na(with_test_flag)) with_test_flag = 0

# If this Session is non-interactive session, I'll use arguments.
input_data["sex"] = as.integer(readline(prompt = "Please, Enter your Sex...(M = 0, F = 1)> "))

input_data["age"] = as.integer(readline(prompt = "Please, Enter your Age...(-20 = 0, 20- = 1, 30- = 2, 40- = 3, 50- = 4, 60- = 5)> "))

input_data["pref_u15"] = as.integer(readline(prompt = "Please Enter where had you lived in? (Answer in Pref Number)> "))

input_data["pref_now"] = as.integer(readline(prompt = "Please Enter where do you live in? (Answer in Pref Number)> "))

if (input_data == input_data) equivalent = 0 else equivalent = 1

#"Sex","Age","pref_u15","pref_now","equivalent"
names(input_data) = c("sex", "age", "pref_u15", "pref_now", "equivalent")

