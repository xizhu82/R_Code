


#### 크롤링 #####




##### 정규식 #####
Pitcher_data_fin$Date <- as.Date(str_extract(Pitcher_data_fin$Date, "^[^ ]+")) # Date 열을 날짜 형식으로 변환하면서 공백으로 분리된 첫 번째 부분만 추출


##### 데이터 전처리 #####

# 이동평균 #
Starter_data <- Starter_data %>%
  group_by(`Season`,Pitching) %>%
  mutate(across(.cols = c(IP,ER,luck,WHIP,FIP,BBK,IPHR),
                .fns = list(
                  cumsum = ~ slide_dbl(., sum, .before = 12, .complete = FALSE) ,
                  movavg = ~ slide_dbl(., mean, .before = 6, .complete = FALSE)
                )))

# 비율 계산을 위해 mutate와 across 사용 #
Starter_data <- Starter_data %>%
  group_by(Season,Pitching) %>%
  mutate(across(.cols = c("ER_cumsum","luck_cumsum","BBK_cumsum"),
                .fns = ~ ./IP_cumsum,
                .names = "{.col}_IP"))  # 생성될 새 열 이름 형식 지정

Starter_data <- Starter_data %>% group_by(Season,Pitching) %>%
  mutate(across(.cols = c(ends_with("_IP"),WHIP_cumsum,WHIP_movavg,FIP_cumsum,FIP_movavg,IPHR_cumsum,IPHR_movavg), 
                .fns = ~ lag(., 1)))  # 지연(lag) 처리가 필요한 모든 열에 대해 지연 처리 적용

# 텍스트 열 제거 #
MLB_Reliver <- MLB_Reliver[, !names(MLB_Reliver) %in% text_columns] 

# 선택한 범위에서 숫자형 변수만 선택 #
numeric_indices <- sapply(MLB, is.numeric) # 숫자형 변수의 인덱스 찾기
cols_to_select <- c(6:112) # 선택할 열의 범위를 정의
numeric_cols_to_process <- MLB[, numeric_indices & colnames(MLB) %in% colnames(MLB)[cols_to_select]] # 해당 열의 숫자형 변수만 선택

# 컬럼 순서 조정 #
desired_order <- c("Date", "Team","Pitching","SR") # 조정하고 싶은 컬럼들
remaining_cols <- setdiff(names(Pitcher_data_fin), desired_order) # 나머지 컬럼들 유지
Pitcher_data_fin <- Pitcher_data_fin[, c(desired_order, remaining_cols)] # 순서 조정



