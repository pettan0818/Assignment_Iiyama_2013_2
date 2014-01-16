# C3課題 (#5:なんとなく協調マッチング)

# セッション判定(返り値:(BOOL)Interactive = F, Non-Interactive = T)
analyzer_session = function(a, b, c, d){	
	return(is.na(a) & is.na(b) & is.na(c) & is.na(d))
}

work_dir = paste(getwd(), "Assignment_Iiyama_2013_2/@data",sep = "")

setwd("C:/Dropbox/Document/大学授業/C4-飯山ゼミ/+Assignment/@data")

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

# 今と昔が一緒.0の自動設定
if (pref_u15 == pref_now) equivalent = 0 else equivalent = 1

# Non-Interactive Session Analysis
if (analyzer_session(sex, age, pref_u15, pref_now)){
	sex = as.integer(commandArgs(trailingOnly = T)[1])
	age = as.integer(commandArgs(trailingOnly = T)[2])
	pref_u15 = as.integer(commandArgs(trailingOnly = T)[3])
	pref_now = as.integer(commandArgs(trailingOnly = T)[4])
	if (pref_u15 == pref_now) equivalent = 0 else equivalent = 1
}

# (Should Remove) Default Nums(Should Remove)
if (analyzer_session(sex, age, pref_u15, pref_now)){
	sex = 0
	age = 1
	pref_u15 = 13
	pref_now = 13
	equivalent = 0
}

# subset関数、条件指定時に単純条件演算子を使うこと
ans_data = subset(data, data[,1] == age & data[,2] == sex & data[,3] == pref_u15 & data[,4] == pref_now & data[,5] == equivalent)

summary(ans_data)

rank = apply(ans_data[,6:15],2,mean)

names(sort(rank))