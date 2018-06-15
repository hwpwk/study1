reserve_tb <- read.csv('data/reserve.csv', fileEncoding = 'UTF-8', header = TRUE, stringsAsFactors = FALSE)

reserve_tb %>%
  group_by(hotel_id) %>%
  summarise(rsv_cnt=n(),
            cus_cnt=n_distinct(customer_id))

reserve_tb %>%
  group_by(hotel_id, people_num) %>%
  summarise(price_sum=n())