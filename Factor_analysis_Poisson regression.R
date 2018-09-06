# データの読み込み
df2 <- read.csv('data0202.csv')

###学習データと評価対象データの設定
# 学習データ
learn_data2 <- df2[1:35, 3:7]

# 評価対象データ
new_data2 <- df2[36, 4:7]

# ポアソン回帰モデルの構築
fit2 <- glm(y~x1+x2+x3+x4,
            family = poisson(link = 'log'),
            data = learn_data2)

# ポアソン回帰モデルの定数と係数
coef(fit2)
# Log(y) = 0.8264677943+0.0116671112x1+0.0119328212x2+0.0001646072x3+0.6353763995x4
# 上記は対数でありyに戻すためにはエクスポネンシャル(exp)を使う

# ポアソン回帰モデルの対象データの予測値
# exp関数でlog(y)をyに変換
exp(predict(fit2, newdata = new_data2))

# ポアソン回帰モデルの予測値（学習データと対象データのすべて）
predict_fit2 <- exp(predict(fit2, newdata = df2[,4:7]))

# cbind(x, y). x と y を横に並べて結合する cbindは横につなげる
new_data0202 <- cbind(df2,predict_fit2)


# 実現値と予測値の描画
# 実現値
plot(new_data0202$y,
     col='blue',
     type = 'l',
     ylim = c(0,40),
     xlab = '時系列（月単位）',
     ylab = '受注件数')

# 予測値
lines(new_data0202$predict_fit2,
      col='red',
      type = 'l',
      lty = 3)

# 最後の月までは実現値と予測値がほぼ同じで最後の月だけ予測以上であった
# 最後の月は値引きを実施したものの予測以上に受注件数が跳ね上がった
# 予測以上に跳ね上がった部分についてはさらに深堀する必要がある
# このように要因を探る際には定量的な要因に加え、定性的な要因も探る必要がある

