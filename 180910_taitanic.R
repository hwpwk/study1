# stringsAsFactors = Fは文字列をfactor型として読み込まないようにするため
# na.strings=(c("NA", ""))は欠損値をNAとするもの
# なぜならRは空白＝NAとしないため、Pythonのように空白をNAとしておく必要がある
df_train <- read.csv("train.csv", stringsAsFactors = F,na.strings=(c("NA", "")))
df_test <- read.csv("test.csv", stringsAsFactors = F,na.strings=(c("NA", "")))

# データ型を確認
str(df_train)

# Survivedと関係なさそうかつ数値型以外のカラムを削除（関係ありそうなカラムを残す）
library(dplyr)
df_train2 <- df_train %>% 
    select(PassengerId, Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)
head(df_train2)

# テストデータも同様に
df_test2 <- df_test %>% 
  select(PassengerId, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)
head(df_test2)

# 文字型変数をダミー変数に変換
df_train2$Sex <- ifelse(df_train2$Sex=="male", 0, 1)
df_train2$Embarked <- ifelse(df_train2$Embarked=="S",0,ifelse(df_train2$Embarked=="C",1,2))

# テストデータも同様に文字型変数をダミー変数に変換する
df_test2$Sex <- ifelse(df_test2$Sex=="male", 0, 1)
df_test2$Embarked <- ifelse(df_test2$Embarked=="S",0,ifelse(df_test2$Embarked=="C",1,2))

# 欠損値の数をカウント(全体)
sum(is.na(df_train2))
# 欠損値の数を列ごとにカウント
# apply(x,i,f):	行列・データフレーム（x）に対して、各行（i = 1）または各列（i = 2）ごとに演算（f）を行う
apply(is.na(df_train2), 2, sum)
# Age:177個、 Embarked:2個の欠損値
# テストデータも同様に欠損値の数を列ごとにカウント
apply(is.na(df_test2), 2, sum)
# Age:86個、Fare:1個の欠損値

# 欠損値の補完
median(df_train2$Age)
# この段階ではNAが含まれているからmedianが計算できない、、
# na.rm = TRUEを加えることでNAを除外して計算可能に
# http://d.hatena.ne.jp/benikujyaku/20120116/1326721283
median(df_train2$Age, na.rm = TRUE)
# 中央値:28
mean(df_train2$Age, na.rm = TRUE)
# 平均:29.69912
hist(df_train2$Age)
# 20~30歳の度数が大きい
# 中央値と平均値の違いはそれほどないので中央値で欠損値補完
df_train2$Age <- ifelse(is.na(df_train2$Age), 28, df_train2$Age)

# テストデータのAgeも同様に処理
median(df_test2$Age, na.rm = TRUE)
# 中央値:27
mean(df_test2$Age, na.rm = TRUE)
# 平均値:30.27259
df_test2$Age <- ifelse(is.na(df_test2$Age), 27, df_test2$Age)

# Embarkedの欠損値補完
# Embarkedの各ダミー変数をカウントし最も頻度が大きいダミー変数で欠損値補完する
df_train2 %>% 
  group_by(Embarked) %>% 
  count()
# 0が644個で1番カウント数が多いので0で補完する

df_train2$Embarked <- ifelse(is.na(df_train2$Embarked), 0, df_train2$Embarked)

# Fareの欠損値補完
median(df_test2$Fare, na.rm = TRUE)
# 中央値:14.4542
mean(df_test2$Fare, na.rm = TRUE)
# 平均値:35.62719

# 中央値:14.5で欠損値補完
df_test2$Fare <- ifelse(is.na(df_test2$Fare), 14.5, df_test2$Fare)

# すべての欠損値が補完されたか確認
sum(is.na(df_train2))
sum(is.na(df_test2))
# どちらも0なので補完完了
plot(df_train2)
# Survivedと他の変数との関係が0or1で直線的な相関が見えにくい、、シグモイド関数的な相関関係

cor(df_train2)
# Suviviedと相関が強いのは順にsex:0.54, Pclass:0.33, Fare:0.25