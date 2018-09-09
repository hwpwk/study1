# step2

# 主成分回帰モデル（パターン1）

# 学習データ
learn_data <- as.data.frame(cbind(
  df2$y,
  df2$d_mon,
  df2$d_wed,
  df2$d_thu,
  df2$d_fri,
  df2$d_sat,
  df2$d_sun,
  df2$x2,
  pca$x[, 1]
))

# 列名変更
colnames(learn_data) <- c('y',
                          'd_mon',
                          'd_wed',
                          'd_thu',
                          'd_fri',
                          'd_sat',
                          'd_sun',
                          'x2',
                          'pc1')
# 線形回帰モデルの構築
fit3 <- glm(y~.,
            family = gaussian(link = 'identity'),
            data = learn_data)

coef(fit3)

# 出力結果を見やすくする
transform(coef(fit3))

# Ridge回帰モデル（パターン2、マルチコ含む）

# 学習データ
# as.matrix:行列に変換
# Ridge回帰モデルを使うにはmatrix型でないといけない
learn_data <- as.matrix(cbind(
  df2$y,
  df2$d_mon,
  df2$d_wed,
  df2$d_thu,
  df2$d_fri,
  df2$d_sat,
  df2$d_sun,
  df2$x2,
  df2$x3,
  df2$x4_new
))

colnames(learn_data) <- c('y','d_mon','d_wed','d_thu','d_fri','d_sat','d_sun','x2','x3','x4_new')

# learn_dataを目的変数と説明変数に分割
learn_data_y <- learn_data[, 1]
# -1:1列目以外
learn_data_x <- learn_data[, -1]

# クロスバリデーションを利用したRidge回帰モデルのラムダの計算
install.packages('glmnet')
library(glmnet)
# cv.glmnet:Ridge回帰を使用するとき
# alphaによってRidgeかLassoかを指定可能
# inf:無限大　upper:上限 lower:下限
# upperの0:降水量が0、降水量が多ければ多いほど売上減少になることが事前にわかっているので
# 降水量だけupper=0に設定している
# lowerの0は値引き率とチラシ配布数、どちらも多ければ多いほど売上増加するのでlower=0にしている
ridgemodel <- cv.glmnet(
  y = learn_data_y,
  x = learn_data_x,
  family = 'gaussian',
  alpha = 0,
  upper.limits = c(Inf, Inf, Inf, Inf, Inf, Inf, 0, Inf, Inf),
  lower.limits=c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,0,0)
)
# これで最適なラムダを得ることができる

# Ridge回帰モデルの構築
# このときはcvはいらずglmnetだけでよい
# またlambda=ridgemodel$lambda.minが加わっている点も注意
ridgemodel2 <- glmnet(
  y = learn_data_y,
  x = learn_data_x,
  family = 'gaussian',
  alpha = 0,
  lambda = ridgemodel$lambda.min,
  upper.limits = c(Inf, Inf, Inf, Inf, Inf, Inf, 0, Inf, Inf),
  lower.limits=c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,0,0)
)

coef(ridgemodel2)
# 線形回帰だとチラシ配布量の係数がマイナスだったがRidge回帰だとプラスになるという常識的な結果に
# Ridge回帰だと主成分分析を使用しなくてもそれなりの結果になった
