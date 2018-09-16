install.packages('rtweet')
library(rtweet)


consumerKey <- '...'
consumerSecret <- '...'
appname <- '...'
accesstokenkey <- '...'
accesstokensecret <- '...'

twitter_token <- create_token(
  app <- appname,
  consumer_key = consumerKey,
  consumer_secret = consumerSecret,
  access_token = accesstokenkey,
  access_secret = accesstokensecret
)
# ここでエラー
# Error in twitter_init_oauth1.0(self$endpoint, self$app, permission = self$params$permission,  : Unauthorized (HTTP 401).
# Callback URLs　を入力したら再度エラー
# Error in twitter_init_oauth1.0(self$endpoint, self$app, permission = self$params$permission,  : Forbidden (HTTP 403).
# twitter_tokenに　access_tokenとaccess_secretを加えることでエラー解消
# 参考:https://github.com/mkearney/rtweet/issues/265

rt <- search_tweets(
  'データサイエンティスト', n =100, include_rts = FALSE
)

# これでrtに「データサイエンティスト」検索結果のデータが格納される
