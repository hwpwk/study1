###step.1-1周期性問題###
# データの読み込み
df <- read.csv('data0301.csv')

# 目的変数
y <- df$y

# 目的変数の自己相関（何日前かずらした過去との相関）
# 自己相関を求めるときはacf関数
y_acf <- acf(y)
# 実行するとグラフが表示される
# Lag=0はyとyとの相関、Lag=1はyと1日前のyの相関、という見方をする
# 1週間ごとに相関が高くなっていることがわかる
# つまり、1週間毎の周期性があると言える
# acf関数によって周期性を見ることが可能
# y_acf$lag:lag0,1,2,,,,,　y_acf$acf: lag0,1,2,,,,,に対応する自己相関
# 元が横なのでそれ縦にして確認したい
cbind(y_acf$lag, y_acf$acf)

# 目的変数の偏自己相関
# 偏自己相関とは他のlagの情報を取り除いた部分
# 例えば、2週間目の自己相関が大きいのは
# 1週間目の自己相関の影響の可能性ある、これを検証する
# pacf関数で偏自己相関が計算可能
y_pcf <- pacf(y)
# グラフを見ると7日目は相関が大きいが14日目、21日目は小さい
cbind(y_pcf$lag, y_pcf$acf)
# よって1週間周期はあるが、2週間、3週間周期はないと言える

###step.1-2ラグ問題###
# 目的変数と各説明変数の相関を散布図で可視化
plot(df[, 4:8])
# yとx2,yとx3に相関がありそう

# 相関係数を確認
round(cor(df[, 4:8]),3)
# ここで「'x' は数値でなければなりません」というエラー
## 以下x4,yをfactorからnumericにする
sapply(df[,4:8], class)
df$x4 <- as.numeric(df$x4)
sapply(df[,4:8], class)

df$y <- as.numeric(df$y)
sapply(df[,4:8], class)
##
# この状態で相関係数を確認するとエラーなし
round(cor(df[, 4:8]),3)
# x2:降水量,x3:値引きに相関が見られる
# x4:チラシ配布枚数がマイナスになるのはlagがあるから
# つまり、日曜日にチラシを配布したとしても日曜日には売れず、後の日に売れると考えられるため
# というわけでlagを見る必要がある

# x4:チラシ配布枚数をx4に格納
x4 <- df$x4

# 交差相関
# 交差相関はccf関数 yとx4の相関を見る
# 交差相関とはyとx4を1日ずつ徐々にずらしていく
y_x4 <- ccf(y,x4)
# グラフより、lag1,2のときのACFの値が大きい
# つまり、yには1日前、2日前のチラシ配布が効いており、
# 金曜日に配布したチラシが土曜日、日曜日に効いているということ
cbind(y_x4$lag, y_x4$acf)
# lag1,lag2の値が大きくなっていることがわかるのでこれらを説明変数に入れる必要がある

###step.1-3マルチコ問題###
# データの読み込み
df2 <- read.csv('data0302.csv')

# 散布図
plot(df2[, 4:9])

# 相関係数
round(cor(df2[, 4:9]), 3)
# またエラーなのでfactorをnumericにする

sapply(df2[,4:9], class)
df2$x4 <- as.numeric(df2$x4)
df2$y <- as.numeric(df2$y)
sapply(df2[,4:9], class)

# 再度相関係数
# 相関係数
round(cor(df2[, 4:9]), 3)
# 今回はyとの関係ではなく説明変数同士の関係を見る
# yとの相関の低いx1,x4を省き、それ以外の説明変数の相関を見る
round(cor(df2[, c(6,7,9)]), 3)
# x3,x4の相関が0.999と高い、つまり値引きとチラシ配布は一緒にやっていたということ
# x3,x4はマルチコの可能性がある

# 目的変数y, 説明変数x2,x3,x4_newで学習データを作成
learn_data <- as.data.frame(
  cbind(df2$y, df2$x2, df2$x3, df2$x4_new)
)
# 作成データフレームを見るとカラム名が仮の名前なので適切な名前に変更する
colnames(learn_data) <- c('y', 'x2', 'x3', 'x4_new')

# 線形回帰モデルの構築
fit1 <- glm(y~.,
            family = gaussian(link = 'identity'),
            data = learn_data)
#線形回帰モデルの定数と係数
coef(fit1)
# 係数をみるとx4_newの値がマイナス、つまり、チラシ配布すればするほど売れないという意味
# このように常識と照らし合わせておかしなことになるのがマルチコの影響
# マルチコを防ぐためには相関がある組み合わせを相関がない組み合わせに変換することが必要
# その際に使用するのが主成分分析

# 主成分分析の準備
# 主成分分析用のデータとして相関が高い変数同士を抽出
pca_data <- as.data.frame(
  cbind(df2$x3, df2$x4_new)
)

colnames(pca_data) <- c('x3', 'x4_new')

# 主成分分析
# scale=T は数値のスケールが合っていないときに用いるオプションであり
# スケールが統一されている場合は入力する必要はない
# スケールが合っていないとは，次元1の単位が cm であるときに，
# 次元2の単位が kg であるようなときのことをいう
# https://data-science.gr.jp/implementation/ida_r_pca.html
pca <- prcomp(pca_data, scale=T)

# 主成分得点
# 主成分得点は元の相関の高い変数を変換した後のデータ
pca$x

# 主成分得点の相関
cor(pca$x)
# 2つの変数の相関が低く変換されていることを確認

# 相関の高い変数を相関の低い変数に変えた変換式を求める
# 固有ベクトル（タテ）
pca$rotation

# 線形回帰モデルでは相関の低い、主成分得点の方を使う
summary(pca)
# Proportion of Varianceを見るとPC1が0.9995なので
# PC1だけ今回のモデルは99.95%説明できるとういうこと、PC2はなくても良い
# というわけで今回はPC1のみでモデルを作成する

# 主成分分析用のデータの平均（列ごと）
# apply関数によって一度に各列の平均値を求めることが可能
# apply(x,i,f):	行列・データフレーム（x）に対して、各行（i = 1）または各列（i = 2）ごとに演算（f）を行う
pca_data_mean <- apply(pca_data[, 1:2], 2, mean)

# 主成分分析用のデータの標準偏差（列ごと）
pca_data_sd <- apply(pca_data[, 1:2], 2, sd)

# 作成したデータを結合する
# 行ベクトル単位で結合する場合は関数 rbind()
pca_data_mean_sd <- rbind(pca_data_mean, pca_data_sd)

rownames(pca_data_mean_sd) <- c('mean', 'sd')

# 主成分得点を使用して学習データを再度作成(pcaの第一主成分)
# 多次元データを低次元に縮約したいだけなら，これらの項目の中では主成分スコア，$x だけを考えれば良い
learn_data <- as.data.frame(cbind(df2$y, df2$x2, pca$x[,1]))
colnames(learn_data) <- c('y', 'x2', 'pc1')

# この学習データで再度線形回帰モデルの構築
fit2 <- glm(y~.,
            family = gaussian(link = 'identity'),
            data = learn_data)
coef(fit2)
