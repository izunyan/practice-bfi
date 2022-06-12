bfi %>%
  as_tibble() %>%                    # tibble形式に変換
  select(gender,education,A1) %>% # 変数を選択
  drop_na() %>% 
  group_by(gender, education) %>% 
  count(A1) %>% 
  mutate(perc = n/sum(n))

group_perc <- function(data, g1, g2, outcome){
  data %>% 
    as_tibble() %>% 
    select({{g1}},{{g2}},{{outcome}}) %>%
    drop_na() %>% 
    group_by({{g1}},{{g2}}) %>% 
    count({{outcome}}) %>% 
    mutate(perc = n/sum(n))
}

group_perc(bfi, gender, education, A1)
group_perc(bfi, gender, education, "A1")


map(c("A1","A2"), ~group_perc(bfi, gender, education, .x))

quos(A1, A2) %>% 
map( ~group_perc(bfi, gender, education, !!.x))
