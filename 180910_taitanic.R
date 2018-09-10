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
# Survivedと他の変数との関係が0or1で直線的な相関が見えにくい、、シグモイド関数的な相関関係？

cor(df_train2)
# Suviviedと相関が強いのは順にsex:0.54, Pclass:0.33, Fare:0.25

# モデル構築
# 今回はランダムフォレストを選択
install.packages('randomForest')
library(randomForest)

# The response has five or fewer unique values.というエラー
# 「予測結果の値が5種類以下しか無いが、本当にregressionがしたいのか? ( classification がしたいのでは? )」という意味
# 目的変数がlogical型になっていると、regression モード になるっぽい。 
# 目的変数をfactor型に変換してあげればclassification モードになる。
# 参考:http://shinya131-note.hatenablog.jp/entry/2015/10/26/233823
df_train2$Survived <- as.factor(df_train2$Survived)

model <- randomForest(Survived~.,
                      data = df_train2)
model
# OOB estimate of  error rate: 17.96%



# 特徴量毎の重要度を表示
importance(model)
# 複数行を1度にコメントアウトするには コメント箇所をドラッグしCTRL + SHIFT + C
# MeanDecreaseGini
# PassengerId         48.90437
# Pclass              33.20005
# Sex                 98.02156
# Age                 49.70134
# SibSp               15.60726
# Parch               12.64400
# Fare                64.11726
# Embarked            10.86850

# 特徴量の重要度をプロット
varImpPlot(model)


# 重要度が高い特徴量のみでランダムフォレストをやってみる
model2 <- randomForest(Survived~Sex+Age+Fare,
                       data = df_train2)
model2
# OOB estimate of  error rate: 20.76%

importance(model2)
# MeanDecreaseGini
# Sex         106.82327
# Age          30.50964
# Fare         56.67634

# 精度が良いmodelの方をテストデータに当てはめる
pred <- predict(model, newdata = df_test2)

head(pred, 10)
# このときpredは0or1

# cbindにするとなぜか1,2データになる,,
# output <- cbind(df_test2$PassengerId, pred)
# colnames(output) <- c('PassengerId', 'Survived')
# data.frameにすると0,1データになった
# data.frameにする際にはカラム名を指定する
# http://zeema.hatenablog.com/entry/2017/09/04/003400#%E3%83%AD%E3%82%B8%E3%82%B9%E3%83%86%E3%82%A3%E3%83%83%E3%82%AF%E5%9B%9E%E5%B8%B0%E3%83%A2%E3%83%87%E3%83%AB%E3%81%AE%E6%8E%A8%E5%AE%9A1%E5%9B%9E%E7%9B%AE
output1 <- data.frame(PassengerId=df_test2$PassengerId, Survived=pred)

# csvファイルとして出力
# row.names=FALSEってしておかないと、行列の行番号がそのままCSVの1列目のデータとして出力されてしまう
write.csv(output1, 'submission_1.csv', row.names=FALSE)

# scoreは0.74641