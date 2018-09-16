# スクレイピングの流れ
# 1.対象URLを読み込み
# 2.HTML中の要素or属性にアクセス
# 3.値を取得

library(rvest)

# 対象URLの文字列が長いので変数に格納
url <- 'https://kabutan.jp/stock/kabuka?code=0000'
# 対象URLの読み込み(DOMとして保存)
url_res <- read_html(url)
# 読み込むことができたかどうかを確認
url_res

# 下記css = だとタイトルの取得ができず
# →できました。
title3 <- read_html(url) %>% 
  html_nodes(css = 'head > title') %>% 
  html_text()
title3


# urlからtitle要素を抽出
url_title <- html_nodes(url_res, xpath = '/html/head/title')
url_title
# 抽出要素を文字列に変換
title2 <- html_text(url_title)
title2

# 上記コードをパイプ演算子を使って書き換える 
title <- read_html(url) %>% 
  html_nodes(xpath = '/html/head/title') %>% 
  html_text()

title

# 表形式部分のデータの取得
# ディベロッパーツールで「table」箇所を選択XPathをコピーしてhtml_nodeの中で指定
kabuka <-read_html(url) %>% 
  html_node(xpath = '//*[@id="stock_kabuka_table"]/table[2]') %>% 
  html_table()

head(kabuka, 10)

# 行数を確認
dim(kabuka)
# 行数が29なので29日分のデータを取得できる

# 参考書籍:RユーザのためのRStudio［実践］入門 P.52-59
