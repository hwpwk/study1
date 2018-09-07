# データの読み込み
df3 <- read.csv('data0203.csv')

# 学習データ
learn_data3 <- df3[1:1260, 2:9]

# 評価データ
new_data3 <- df3[1261, 3:9]

# ロジスティック回帰モデルの構築
fit3 <- glm(y~.,
            family = binomial(link = 'logit'),
            data = learn_data3)

# ロジスティック回帰モデルの定数と係数
coef(fit3)
# log(p/(1-p))=-4.9667971+1.4422063x1-5.5300510x2-2.8529357x3+0.6537161x4-1.6559984x5-3.8919853x6+6.2416034x7
# pはy=1(受注)の確率 受注確度

# ロジスティック回帰モデルの評価対象データの予測値
pred_newy <- predict(fit3, newdata = new_data3)

pred_newy

# log(p/(1-p))をPにしたい
# 対数なのでexp()で元に戻すことが可能
# 下記式を展開するとPになる
pred_newy <- exp(pred_newy)/(1+exp(pred_newy))

# P(受注確度)を確認する
pred_newy
# 0.9667771

# 学習で使ったデータを作成したロジスティック回帰モデルがどれだけ当てているかを確認
# ロジスティック回帰モデルの予測値（学習データと対象データのすべて）
pred_y <- predict(fit3, newdata = learn_data3[, 2:8])

# log(p/(1-p))をPにする
# 学習データによる予測値
predict_fit3 <- exp(pred_y)/(1+exp(pred_y))

# 受注か失注かの判定(判定の予測)
hantei <- ifelse(predict_fit3 > 0.5, 1, 0)

hantei

# df3の1~1260行までのデータフレームにpredict_fit3, hanteiを列側に追加
new_data0203 <- cbind(df3[1:1260,], predict_fit3, hantei)

# 予測した判定と実際のyを比較、整合性を見る
# 実現値の予測値のクロス集計
table(new_data0203$y, new_data0203$hantei)

# 評価対象の顧客IDの実現値は「0」であったが予測値は「1」
# つまり、過去の傾向から考えると失注は考えられず、
# 定性的な要因を探る必要がある
