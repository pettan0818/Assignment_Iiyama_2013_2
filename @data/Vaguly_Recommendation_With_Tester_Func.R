# Vaguly_Recommendation With Aggressive Function maker...

# This file only includes test method...
# Object Algorithm
# 
testing_Algo = function(input_data, data){
	# 取り出しは、全部一致
	terms = data[,1] == input_data[1] & data[,2] == input_data[2] & data[,3] == input_data[3] & data[,4] == input_data[4] & data[,5] == input_data["equivalent"]
	ans_data = data[terms,]
	
	# データベースに一致がないとき、K近傍法で処理する。
	if (nrow(ans_data) == 0){
		warning("[Attention] Found 0 Consensus, Using K_means...")
		ans_data =  k_means(data, input_data, 8)
	}
	
	print(summary(ans_data))
	
	# アルゴリズム適用部
	rank = apply(ans_data[,6:15],2,mean)
	
	return(rank)
}

# Utility Functions
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
	# 元々のデータ+1行目のユーザーが今回入力されたデータ
	user_data = rbind(original[,c(1,2,3,4,5)],input_data)
	user_data_row = nrow(original)+1
	
	# クラスタリング(8個のクラスタによるK近傍法)執行
	# 与えられたデータをデータベースに追加した上で、分類実行。
	k_class = kmeans(user_data, cluster)
	cluster_data = k_class$cluster
	# For Debug
	print(head(cluster_data,100))
	print(table(cluster_data))
	# 与えられたデータのクラスタ番号のみ抽出
	input_cluster = k_class$cluster[user_data_row]
	# 元々のデータのクラスタ番号を抽出
	cluster_data = cluster_data[-user_data_row]
	
	# 同クラスタのデータ抽出
	same_cluster_users = data[cluster_data == input_cluster,]
	
	return(same_cluster_users)
}

# ホールドアウト検定用の関数
# tester(フラグ, オリジナルデータ, サンプリング個数)
# デフォルトでは、サンプリング個数は、1割。
# 返り値について、サンプルデータ除去済み生データ、サンプルデータ、生データの順番で作ったリストで返す。
test_data_maker = function(data, sampler = nrow(data)/10){
	warning("[Testing] Test Method is now activated...")
	# 元のデータの保管
	data_origin = data
	# ランダムサンプリング
	sampled_row = sample(nrow(data), sampler)
	data_sampled = data[sampled_row,]
	data = data[-sampled_row,]
	# 複数返り値の場合は、リストにしてかえす
	return(list(data, data_sampled, data_origin))
}
##############################################################
# 以下、ロジック
##############################################################
# それぞれの環境に合わせたデータフォルダの適用
source("../local.r",encoding='utf-8')

# 順位化データの読みこみ
use_column = c(3, 4, 6, 9, 12, 13:22)
data = data_reader(use_column)

# 入力データの管理
# 初期化には、あり得ないデータを突っ込む
input_data = rep(-1,5)
names(input_data) = c("sex", "age", "pref_u15", "pref_now", "equivalent")

# Test Session Confirm
with_test_flag = c(NA)
# with_test_flag = as.integer(readline(prompt = "[Test Flag] Do you want to suggest with test?(Yes = 1, No = 0)> "))
if(is.na(with_test_flag)) with_test_flag = 1

# テストする場合は、まず、それ用のデータを作る。
if(with_test_flag) {
	temp_reciver = test_data_maker(data)
	data = temp_reciver[[1]]
	data_sampled = temp_reciver[[2]]
	data_origin = temp_reciver[[3]]
	# よく分からないものはさっさと殺す。
	rm(temp_reciver)
}

# 実際にテストする。
for(i in 1:nrow(data_sampled)){
	input_data = data_sampled[i, 1:5]
	# アルゴリズムを分離して、テスト恒常性を高める。
	testing_Algo(input_data, data)
}