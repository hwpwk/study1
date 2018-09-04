########################
#関数
########################

# ステップ1の時系列モデルの構築から
# ステップ2の外れ値スコアの算出までをパッケージ化する
# target:評価対象の行のこと
# trim:外れ値処理の際にどれくらい大きければ外れ値とするのかの基準の値

LOF <- function(df,target,trim) {
  
  #データセット
  learn_data_start <- target-30
  learn_data_end <- target-1
  learn_data <- ts(df[learn_data_start:learn_data_end,2])
  y <- as.numeric(df[target,2])
  
  #ARモデル構築（外れ値処理前）
  ARmodel <- auto.arima(learn_data,
                        max.p = 5, max.q = 0, max.d = 2,
                        max.P = 0, max.Q = 0, max.D = 0)
  
  ARmodel_fitted<-fitted(ARmodel)
  ARmodel_res<-residuals(ARmodel)
  
  #ARモデル構築（外れ値処理）
  learn_data2 <- ifelse(abs(ARmodel_res)>trim,
                        ARmodel_fitted,
                        learn_data)
  learn_data <- learn_data2 
  ARmodel <- auto.arima(learn_data,
                        max.p = 5, max.q = 0, max.d = 2,
                        max.P = 0, max.Q = 0, max.D = 0)
  ARmodel_fitted<-fitted(ARmodel)
  ARmodel_res<-residuals(ARmodel)
  
  #残差の標準偏差と平均値
  ARmodel_res_sd<-sd(ARmodel_res)
  ARmodel_res_mean<-mean(ARmodel_res)
  
  #評価対象の予測と残差
  ARmodel_yosoku <- forecast(ARmodel,h=1)$mean
  ARmodel_gap <- y-ARmodel_yosoku
  
  #外れ値度
  LOF <- -log(dnorm(ARmodel_gap,ARmodel_res_mean,ARmodel_res_sd))
  
  #出力
  output_data <- c(LOF,ARmodel_res_mean,ARmodel_res_sd,y,ARmodel_yosoku,ARmodel_gap)
  output_name <- c("LOF", "Mean", "SD","Measured value","Predicted value","Gap")  
  names(output_data) <- output_name 
  return(output_data)
  
}

# 関数の呼び出し
# 4/13
LOF(df,33,20)
# 7/23
LOF(df,134,20)

# 引数にtrimなしバージョン
LOF2 <- function(df,target) {
  
  #データセット
  learn_data_start <- target-30
  learn_data_end <- target-1
  learn_data <- ts(df[learn_data_start:learn_data_end,2])
  y <- as.numeric(df[target,2])
  
  #ARモデル構築（外れ値処理前）
  ARmodel <- auto.arima(learn_data,
                        max.p = 5, max.q = 0, max.d = 2,
                        max.P = 0, max.Q = 0, max.D = 0)
  
  ARmodel_fitted<-fitted(ARmodel)
  ARmodel_res<-residuals(ARmodel)
  
  #残差の標準偏差と平均値
  ARmodel_res_sd<-sd(ARmodel_res)
  ARmodel_res_mean<-mean(ARmodel_res)
  
  #ARモデル構築（統計的な外れ値処理）　有意水準5%以上のようなもの だいたい2か3
  # 統計的に起こりそうにないことが起こったら、起こり得そうなARmodel_fittedを使用し、そうではければ元のlearn_dataを使用する
  learn_data2 <- ifelse(abs((ARmodel_res-ARmodel_res_mean)/ARmodel_res_sd)>2,
                        ARmodel_fitted,
                        learn_data)
  
  learn_data <- learn_data2 
  ARmodel <- auto.arima(learn_data,
                        max.p = 5, max.q = 0, max.d = 2,
                        max.P = 0, max.Q = 0, max.D = 0)
  ARmodel_fitted<-fitted(ARmodel)
  ARmodel_res<-residuals(ARmodel)
  
  #残差の標準偏差と平均値
  ARmodel_res_sd<-sd(ARmodel_res)
  ARmodel_res_mean<-mean(ARmodel_res)
  
  #評価対象の予測と残差
  ARmodel_yosoku <- forecast(ARmodel,h=1)$mean
  ARmodel_gap <- y-ARmodel_yosoku
  
  #外れ値度
  LOF <- -log(dnorm(ARmodel_gap,ARmodel_res_mean,ARmodel_res_sd))
  
  #出力
  output_data <- c(LOF,ARmodel_res_mean,ARmodel_res_sd,y,ARmodel_yosoku,ARmodel_gap)
  output_name <- c("LOF", "Mean", "SD","Measured value","Predicted value","Gap")  
  names(output_data) <- output_name 
  return(output_data)
  
}

LOF2(df,33)
LOF2(df,134)
# LOFが2.13...と数値が小さい→外れ値ではない
