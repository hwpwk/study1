df <- read.csv('data01.csv')
head(df)

install.packages('ggplot2')
library(ggplot2)

install.packages('dplyr')
library(dplyr)

# 時系列グラフを表示
# http://www.ner.takushoku-u.ac.jp/masano/class_material/waseda/keiryo/3.2_data_visualization.html
# ラベルサイズはelement_text(size =8,,,で
# https://heavywatal.github.io/rstats/ggplot2.html
ggplot(df, aes(date, sales, group=sales)) +
  geom_point(stat = "identity") +
  geom_step() +
  theme_classic() +
  theme(axis.text.x = element_text(size =8, angle = 90, hjust = 1))


# 異常検知の対象データの指定(外れ値の評価対象日を4/13にする)
target <- 33

# 学習データの行の指定(3/14~4/12:外れ値の評価対象日の30日前～1日前)
learn_data_start <- target-30
learn_data_end <- target-1

# 学習データの設定
# learn_data_startからlearn_data_end行まで抽出
# 今回はdateとSalesのうちSalesの値を抽出したいので「2」を指定
# それをts関数で時系列オブジェクトに変換
learn_data <- ts(df[learn_data_start:learn_data_end, 2])

# 異常検知データの対象データの設定
# dfファイルのtarget=33行目の2列目を数値に変換して抽出
y <- as.numeric(df[target, 2])

###ここまででデータの読み込み、設定は完了###

# 学習データの描画
plot(learn_data)

# 階差データの生成
# 1階差の生成
diff(learn_data)
# 2階差の生成
diff(diff(learn_data))
# 1階差データの描画
plot(diff(learn_data))
# 2階差データの描画
plot(diff(diff(learn_data)))

# 階差データにすることで上昇傾向や下流傾向などのトレンドを消すことができる
# ただし、元データに外れ値が残っていればその影響を受けてしまう
# 今回は元データに外れ値を残したままなので0階差のデータでモデル作成を進める

# ARモデルを使用するためにforcastパッケージをインストールする
install.packages('forecast')
library(forecast)

# forecastライブラリのauto.arima関数を使うことでARモデルの面倒なパラメータ設定を自動で行ってくれる
# 最適な自己回帰と階差を自動で選択してくれる
# というわけでauto.arimaを使えば目で階差データを確認する必要はない
# max.p:自己回帰の最大何日前まで使えるか　max.d:階差
ARmodel <- auto.arima(learn_data,
                      max.p = 5, 
                      max.q = 0, 
                      max.d = 2,
                      max.P = 0, 
                      max.Q = 0, 
                      max.D = 0)
# ARモデルの構築結果
ARmodel

# ARモデルの（3/14~4/12の30日間の学習からの）予測値
ARmodel_fitted <- fitted(ARmodel)

# ARモデルの実測値と予測値の描画
# まずは実現値
# type="l":線プロット(折れ線グラフ)
plot(learn_data,
     col='blue',
     type='l',
     ylim=c(0,500),
     xlab='時系列',
     ylab='売上')

# 実現値と予測値のグラフを重ねたい、その際にはlinesを使う
lines(ARmodel_fitted,
      col="red",
      type="l",
      lty=3)

# グラフを見ると実現値と予測値に乖離がある箇所がある
# そこで残差を確認する
# ARモデルの残差
ARmodel_res <- residuals(ARmodel)

# ARモデルの残差の描画
plot(ARmodel_res, ylim=c(-100,100))

# グラフを見ると外れ値があるので除去する
# 残差の絶対値が20より大きい場合はARmodel_fitted(予測値)の値を新たなデータとして使い、
# 20より小さい場合はlearn_data(実現値元データ)を使う、という条件分岐をしたデータセットを作成
learn_data2 <- ifelse(abs(ARmodel_res)>20,
                      ARmodel_fitted,
                      learn_data)
# 外れ値処理をしたデータで再度auto.arima関数でARモデルを構築
ARmodel <- auto.arima(learn_data2,
                      max.p = 5, 
                      max.q = 0, 
                      max.d = 2,
                      max.P = 0, 
                      max.Q = 0, 
                      max.D = 0)
ARmodel
# ARIMA(1,0,0):オーダー1の自己回帰モデル(今回は1日前)
# d階差分をとった系列が定常かつ反転可能なARMA(p,q)過程に従う過程は次数(p,d,q)の自己回帰和分移動平均過程
# もしくはARIMA(p,d,q)と呼ばれる。
# 基本的にはd = 1と相場が決まっている
# https://tjo.hatenablog.com/entry/2013/07/12/184704

# 外れ値処理したデータで学習したARモデルでの予測値
ARmodel_fitted <- fitted(ARmodel)

# ARモデルの実現値と予測値の描写
plot(learn_data2,
     col="blue",
     type="l",
     ylim=c(0,500),
     xlab="時系列", 
     ylab="売上")

# 予測値のグラフを重ねる
lines(ARmodel_fitted, col='red', type='l', lty=3)

# ARモデルの残差
ARmodel_res <- residuals(ARmodel)

# ARモデルの残差の標準偏差
ARmodel_res_sd <- sd(ARmodel_res)

# ARモデルの残差の平均
ARmodel_res_mean <- mean(ARmodel_res)

###ステップ1 時系列モデルの構築終了###

###ここからステップ2 外れ値スコアの算出###

# 今回作成したARモデルを使用して将来の予測
# 将来の予測にはforecast関数 h=1 は1期分という意味
# $meanは1点のみの予測
ARmodel_yosoku <- forecast(ARmodel, h=1)$mean

# ARモデルの評価対象(4/13)の残差
ARmodel_gap <- y - ARmodel_yosoku

# 外れ値スコア算出
# 今回は正規分布を仮定しているので正規分布の確率密度でPを求める
# 正規分布の確率密度を求める関数がdnorm関数
# dnorm関数は最初に評価対象の残差を指定、次に残差の平均値、その次に残差の標準偏差

LOF <- -log(dnorm(ARmodel_gap, ARmodel_res_mean, ARmodel_res_sd)) 
LOF
# 外れ値スコアの値が大きければ大きいほど過去のデータから見てあり得ない予測値になっているという意味
# 今回の77.04085は大きい、評価対象日が外れ値の日なので