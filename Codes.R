


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

# 타자 이름 포지션  "Shohei Ohtani DH-P" 공백 기준으로 나누기 #
# 공백 기준으로 나누고 마지막 텍스트 삭제 #
Batter_data_fin <- Batter_data_fin %>%
  rowwise() %>%
  mutate(Batting = paste(unlist(strsplit(Batting, " "))[1:2], collapse = " ")) %>%
  ungroup()
# 공백 기준으로 나누고, 마지막 컬럼 제거 separate 사용 #
Batter_data_fin <- Batter_data_fin %>%
  separate(Batting, into = c("first_name", "last_name", "position"), sep = " ", extra = "drop") %>%
  unite("player_name", first_name, last_name, sep = " ")

# 데이터프레임을 Season과 Pitching 기준으로 그룹화하고, 가장 최근 날짜의 팀 정보를 유지
Pitcher_data_Home <- Pitcher_data_Home %>%
  group_by(Season, Pitching) %>%
  filter(Date == max(Date)) %>%
  ungroup()



# 필요한 라이브러리 로드
library(dplyr)

# Sheet1 데이터 로드
sheet1 <- data.frame(
  name = c("song", "song"),
  location = c("seoul", "busan"),
  start = as.Date(c("2020-01-01", "2020-06-01")),
  end = as.Date(c("2020-12-31", "2020-08-31"))
)

# busan의 기간을 기준으로 seoul의 기간을 나누기 위한 코드
seoul <- sheet1 %>% filter(location == "seoul")
busan <- sheet1 %>% filter(location == "busan")

# 서울의 기간을 busan의 기간에 맞춰 나누기
seoul_split <- seoul %>%
  mutate(
    start1 = start,
    end1 = busan$start - 1,  # busan이 시작하기 전 날짜까지
    start2 = busan$end + 1,  # busan이 끝난 후 날짜부터
    end2 = end
  )

# 데이터 정리 및 필요 없는 열 제거
seoul_split <- seoul_split %>%
  select(name, location, start1, end1, start2, end2) %>%
  gather(key, value, start1:end2) %>%
  separate(key, into = c("period", "type"), sep = -1) %>%
  spread(type, value) %>%
  filter(!is.na(start) & !is.na(end))

# busan 데이터와 병합
sheet2 <- bind_rows(seoul_split, busan)

# 결과 출력
print(sheet2)


