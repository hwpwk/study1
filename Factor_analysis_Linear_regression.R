# データの読み込み
df <- read.csv('data0201.csv')

# 学習データを設定
learn_data <- df[1:43, 3:5]

# 評価対象データを設定
new_data <- df[44:45, 4:5]

# 線形回帰モデルの構築
# 目的変数はy, 説明変数にはx1,x2を設定
# family = gaussian(link = 'identity'):線形回帰
fit1 <- glm(y~x1+x2,
            family = gaussian(link = 'identity'),
            data = learn_data)

# 線形回帰モデルの構築結果
fit1
# 線形回帰モデルの定数と係数
coef(fit1)
# y = 1047.58866654 -9.29474668x1+0.01617133x2

# 線形回帰モデルの学習データの予測値(fit1$とすると候補が表示される)
fit1$fitted.values

# 線形回帰モデルの残差(fit1$とすると候補が表示される)
fit1$residuals
# ここまでは学習期間という過去データの話

# 評価対象日のデータを使って将来を予測する場合はpredict
predict(fit1,newdata = new_data)

# 学習期間と評価対象日の予測データを結合する
# 学習期間、評価対象日関係なくすべてのデータを選択
# すべての期間の予測値を算出
predict_fit1 <- predict(fit1, newdata = df[, 4:5])

# 実現値と予測値を1つのデータとしてまとめる
# predict_fit1カラムが生成されdfに結合される
new_data0201 <- cbind(df, predict_fit1)

# 実現値と予測値の描画
# 実現値
plot(new_data0201$y,
     col='blue',
     type='l',
     ylim=c(0,2500),
     xlab='時系列（休日）',
     ylab='日販')

# 予測値
lines(new_data0201$predict_fit1,
      col='red',
      type = 'l',
      lty=3)

# グラフを見たところ、評価対象日の実現値と予測値がほぼ同じ
# つまり残差が小さいということになり予測がうまくいっている
# よって、使用した説明変数によって今回の売上減少の理由を説明できるということ
# この後は説明変数の実データを見て売上減少要因を探っていく