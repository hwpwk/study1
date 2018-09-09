# AHP参考:https://www.slideshare.net/soultoru/rahp

# AHPのパッケージの読み込み
# http://aoki2.si.gunma-u.ac.jp/R/AHP.html:ここを参考に
source("http://aoki2.si.gunma-u.ac.jp/R/src/AHP.R", encoding="euc-jp")

# 一対比較の結果
# xは効果・費用対効果・実現可能性の一対比較評価結果
x<-c(1/3,5,5)
# yは効果・費用対効果・実現可能性それぞれの現状維持,対策案1,2,3の一対比較評価結果
y1<-c(7,1/3,5,1/7,1/3,5)
y2<-c(7,1/3,7,1/7,5,9)
y3<-c(1,1,1/7,1,1/5,1/5)

y<-cbind(y1,y2,y3)

x_n<-c("効果", "効率性", "実現性")
y_n<-c("現状維持", "対策案1", "対策案2", "対策案3")

# AHPの実行
# x:評価基準に対する一対評価
# y:各評価基準の代替案の一対評価
AHP(x,y,labels.x = x_n, labels.y = y_n)
