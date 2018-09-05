install.packages('carData')
library(carData)
library(tidyverse)

carData::Prestige %>% 
  head()

df <- carData::Prestige

# データの確認
View(df)
summary(df)

df2 <- df %>% 
  select(-census)

# すべての変数のペアに対する散布図と
# ペアの一方がfactor型ならヒストグラム、回帰係数を表示する
# これで全体のデータの特徴を見る
install.packages('GGally')
library(GGally)
GGally::ggpairs(df2)

mod1 <- lm(formula = income ~ education + women + prestige + type, data = df)
summary(mod1)
# women,prestigeに有意差あり

# 多重共線性を確認
# 多重共線性の指標として,分散拡大係数 (VIF) があり 
# car::vif() で確認可能
# carパッケージに自動でVIFを算出してくれるものがある
install.packages('car')
library(car)
car::vif(mod1)

# VIF統計量は一般的にに10以下であれば多重共線性がないとされる。
# 理想値は2以下である。
# VIF統計量が10を超えた変数がある場合にはモデルからその変数を外してもう一度VIF統計量を計算する
# 今回は5をカットオフ値とするとeducation,typeのどちらかを省くべき
# educationを省いた場合
mod4 <- lm(formula = income ~ women + prestige + type, data = df)
summary(mod4)
car::vif(mod4)

# typeを省いた場合
mod5 <- lm(formula = income ~ education + women + prestige, data=df)
summary(mod5)
car::vif(mod5)

# 多重共線性を確認して多重共線性が発生していなければ
# 最後にモデルを可視化して本当に良いのかを評価する

# which =1:Residuals vs Fitted
# 予測したデータが実測値からどれだけ離れているか
plot(mod5, which = 1)

# which = 2:Nomal Q-Q
# 残差が正規分布に従っているかを見る
# 正規分布に完全に従っている場合は点線上にすべての点が並ぶはず
# 今回は右端、左端で外れている値あり
# 正規分布かどうかの明確な判断基準は設定されておらず、人によって解釈が異なる可能性あり
plot(mod5, which = 2)

# which = 3:Scale-Location
# 等分散性の確認
# グラフが横に一直線だとデータが一定の帯の間に収まっている
# 一直線になっていないと怪しい
# 等分散が仮定できない場合は最小二乗法による推定が信頼できなくなるので注意が必要
plot(mod5, which = 3)

plot(mod5, which = 5)
# which = 5:Residuals vs Leverage
# 予測との「ずれ」の大きさは、最初も後も同じ
# しかし、最小二乗法による予測では、xの値が全体の平均より大きい場合は同じずれでも、予測に与える影響が大きく変わる
# これを、leverageが大きい（ある説明変数が説明変数の平均からどれだけずれているか）と言う

# また、Cook’s Distanceは、その観測データiが持つ影響力というイメージをもってもらえれば、
# Cook's Distanceがある程度あって(Cook's Distanceの外側）、Leverageが高い点をモデルに含むかどうかを検討しなければならない
# Residuals vs Leverageを描いた際にCook's Distanceの外側の点がある場合、その点が結果に大きく影響してしまっている可能性
