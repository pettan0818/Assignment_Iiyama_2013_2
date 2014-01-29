# Vaguly_Recommendation With Aggressive Function maker...

# This file only includes test method...
# Object Algorithm

###
# アルゴリズムのユーティリティ
###

# 距離測定関数(返り値: 重み付け用の数値を返す。)
# 引数: 推定する特徴量, データベースとの一致, 重み付けに変更する際の方法(逆数 == reciprocal, 自然対数 == ln), 距離の種類
dist_calc = function(input_data, ans_data, method_multipler = "ln", method_dist = "canberra"){
	# 特徴量の抽出と被験者の挿入。
	dist_source = rbind(input_data, ans_data[,1:5])
	# 実際に距離を求める。
	dist_ans_input = dist(dist_source, method = method_dist)
	# dist型は、一旦数列に置き換得ると幸せに。今回は、被験者を1番目に入れたので、1行目を取ってくる。
	# applyでつまるので、データフレームに。
	dist_ans_input = data.frame(as.matrix(dist_ans_input)[1,-1])
	
	# 重み付け変換
	# 逆数で変換
	if(method_multipler == "reciprocal"){
		return(1/(dist_ans_input+1)[[1]])
		# 自然対数で変換。
	}else if(method_multipler == "ln"){
		# dist_ans_input[[1]]は、expを掛ける際に、リスト化されているので、解除する。
		return(exp(dist_ans_input[[1]])*-1)
		# Finally
	}else{
		return(ans_data)
	}
}

# 順位を元にした寿司の人気度指数変換関数。(返り値: それぞれの寿司ネタの人気度指数)
# 引数: 元々のデータ、重み付け変数、点数化パターン
point_convert = function(ans_data, ans_weight = NA, point_pattern = "regular"){
	# 順位ごとの基本値を作成
	# 通常
	if (point_pattern == "regular"){
		point_pattern = seq(100, 10, length = 10)
	# 二次関数
	}else if(point_pattern == "quad"){
		point_pattern = seq(10, 1, length = 10)
		point_pattern = point_pattern^2
	}else{
		point_pattern = seq(100, 10, length = 10)
	}
	
	# Point作成
	point_ans = ans_data[,6:15]
	
	for(i in 1:ncol(point_ans)){
		for(t in 1:nrow(point_ans)){
			point_ans[t,i] = point_pattern[point_ans[t,i]]*ans_weight[t]
		}
	}
	ans_data[,6:15] = point_ans
	return(ans_data)
}

###
# アルゴリズム本体
###
testing_Algo = function(input_data, data, k_cluster = 8){
	# 取り出しは、全部一致
	# [FIXME] 論理演算が全行にいってない。。。
	# As.integerでかいけつしたっぽい
	
	terms = data[,1] == as.integer(input_data["sex"]) & data[,2] == as.integer(input_data["age"]) & data[,3] == as.integer(input_data["pref_u15"]) & data[,4] == as.integer(input_data["pref_now"]) & data[,5] == as.integer(input_data["equivalent"])
	# 近傍法のみでやる
	#terms=FALSE
	ans_data = data[terms,]
	
	# データベースに一致がないとき、K近傍法で処理する。
	if (nrow(ans_data) == 0){
		warning("[Attention] Found 0 Consensus, Using K_means...")
		ans_data =  k_means(data, input_data, k_cluster)
		# とりあえず、逆数で距離計算。
		ans_weight = dist_calc(input_data, ans_data)
		ans_data = point_convert(ans_data, ans_weight,"Regular")
	}
	
	tabler = function(data){
		test = table(data)
		return(as.integer((names(sort(test)[10]))))
	}
	# アルゴリズム適用部
	ranked = apply(ans_data[,6:15],2,mean)
	#ranked = apply(ans_data[,6:15],2,tabler)
	
	# それぞれのネタに順位番号を格納。
	# かなり悩んだのでWikiにあげた
	returner_ranked = c(1:10)
	returner_ranked[sort.int(ranked, index.return=T)$ix] = 1:10
	
	return(returner_ranked)
}

###
# Utility Functions
###

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
	#print(head(cluster_data,100))
	#print(table(cluster_data))
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

# 成績表示関数(返り値:なし)
# 解答データを引数にとる。
result_view = function(result){
	# コラムデータが化けるので、リセット
	colnames(result) = c("shrimp","conger_eel","tuna","squid","sea_urchin","salmon_roe","egg","fatty flesh","tuna_roll","cucumber")
	
	# 全体合致率
	print("AllOver Rate")
	# 合致するかどうかの論理演算
	rate_data = table(result == data_sampled[1:nrow(data_sampled),6:15])
	print(rate_data)
	
	print(as.integer(rate_data[2])/{as.integer(rate_data[1])+as.integer(rate_data[2])}*100)
	
	# 特定順位の正答率算定
	# 4以上をNAにする。
	print("Specific Rate")
	remover = function(data){
		# 複数条件は、ifの括弧の中に論理演算子で追加のこと
		if(data > 3) data = NA
		return(data)
	}
	# 二重ループの必要なし!
	removed_result = apply(result, c(1, 2), remover)
	
	rate_data = table(removed_result == data_sampled[1:nrow(data_sampled),6:15], useNA = "always")
	print(rate_data)
	
	print(as.integer(rate_data[2])/{as.integer(rate_data[1])+as.integer(rate_data[2])}*100)
}

###
# 以下、ロジック
###

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
result = data.frame()
for(i in 1:nrow(data_sampled)){
	input_data = data_sampled[i, 1:5]
	# アルゴリズムを分離して、テスト恒常性を高める。
	# result変数に、毎回の順位を格納。
	temp = testing_Algo(input_data, data, 8)
	#print(temp)
	result = rbind(result, temp)
}

# リザルト表示
result_view(result)