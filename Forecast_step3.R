# step3
# 予測モデルの構築

# 学習データ
# 9/19~10/23のデータ
df2_learn <- df2[106:140, ]

learn_data <- as.data.frame(cbind(
  df2_learn$y,
  df2_learn$d_mon,
  df2_learn$d_wed,
  df2_learn$d_thu,
  df2_learn$d_fri,
  df2_learn$d_sat,
  df2_learn$d_sun,
  df2_learn$x2,
  pca$x[106:140,1]
))

colnames(learn_data) <- c('y','d_mon','d_wed','d_thu','d_fri','d_sat','d_sun','x2','pc1')

# 線形回帰モデルの構築
fit4 <- glm(y~.,
            family = gaussian(link = 'identity'),
            data = learn_data)
transform(coef(fit4))

# この学習モデルを使用して予測していく
# まずはCSVファイルの読み込み
df_p0301 <- read.csv('data03_plan01.csv')
df_p0302 <- read.csv('data03_plan02.csv')
df_p0303 <- read.csv('data03_plan03.csv')
df_p0304 <- read.csv('data03_plan04.csv')

# 日付以外の説明変数を選択
# predict()関数の第2引数の「newdata」ってのは説明変数を与える部分
# （説明変数がどういう値だったときの予測値を知りたいかを出力するわけだから）

plan01_predict <- predict(fit4, newdata = df_p0301[, -1])
plan02_predict <- predict(fit4, newdata = df_p0302[, -1])
plan03_predict <- predict(fit4, newdata = df_p0303[, -1])
plan04_predict <- predict(fit4, newdata = df_p0304[, -1])

plan_predict <- cbind(plan01_predict,
                      plan02_predict,
                      plan03_predict,
                      plan04_predict)
colnames(plan_predict) <- c('現状維持','対策案1','対策案2','対策案3')

rownames(plan_predict) <- c('2016/10/24','2016/10/25','2016/10/26','2016/10/27','2016/10/28','2016/10/29','2016/10/30')

plan_predict

# 日販を週販にするためすべてを足す
# apply(x,i,f):	行列・データフレーム（x）に対して、各行（i = 1）または各列（i = 2）ごとに演算（f）を行う
apply(plan_predict, 2, sum)
