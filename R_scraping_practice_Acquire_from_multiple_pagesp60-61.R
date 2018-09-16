library(rvest)

urls <- NULL
kabukas <- list()

# ページ番号抜きのURLを用意
base_url <- 'https://kabutan.jp/stock/kabuka?code=0000&ashi=day&page='

for (i in 1:5) {
  # as.character():文字列型に変換
  pgnum <- as.character(i)
  # paste0():文字列の結合、引数指定の文字列が結合される
  urls[i] <- paste0(base_url, pgnum)
  
  # 表データを取得する場合はhtml_table()
  kabukas[[i]]<- read_html(urls[i]) %>% 
    html_node(xpath = '//*[@id="stock_kabuka_table"]/table[2]') %>% 
    html_table() %>% 
    # 前日比の列はいったん文字列として読み込み
    # mutate_at():特定の列に特定の関数を適用する関数、指定した（複数の)列に同じ操作を行うP.115-P.117
    dplyr::mutate_at('前日比', as.character)
  # 1ページ取得したら1秒停止
  Sys.sleep(1)
}

# データフレームのリストを縦につなげて1つのデータフレームに
dat <- dplyr::bind_rows(kabukas)
# 行末の確認
tail(dat, 10)

str(dat)

# 参考書籍:RユーザのためのRStudio［実践］入門 P.60-61
