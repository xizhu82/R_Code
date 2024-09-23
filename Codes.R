


#### 크롤링 #####




##### 정규식 #####
Pitcher_data_fin$Date <- as.Date(str_extract(Pitcher_data_fin$Date, "^[^ ]+")) # Date 열을 날짜 형식으로 변환하면서 공백으로 분리된 첫 번째 부분만 추출


##### 데이터 전처리 #####

# 이동평균 #
Starter_data <- Starter_data %>% group_by(`Season`,Pitching) %>%
  mutate(across(.cols = c(IP,ER,luck,WHIP,FIP,BBK,IPHR),
                .fns = list(
                  cumsum = ~ slide_dbl(., sum, .before = 12, .complete = FALSE) ,
                  movavg = ~ slide_dbl(., mean, .before = 6, .complete = FALSE)
                )))

# 비율 계산을 위해 mutate와 across 사용 #
Starter_data <- Starter_data %>% group_by(Season,Pitching) %>%
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

# 중복 코드 확인 및 삭제
duplicated_rows <- MLB_Schedule[duplicated(MLB_Schedule[,c("Date","Team","Opp","TS","OS")]),]
MLB_Schedule <- MLB_Schedule[!duplicated(MLB_Schedule[,c("Date","Team","Opp","TS","OS")]),]



# 샘플 데이터 생성
df <- data.frame(
  사람 = c('홍길동', '홍길동', '이순신', '이순신'),
  장소 = c('서울', '부산', '인천', '서울'),
  시작날짜 = as.Date(c('2024-01-01', '2024-01-03', '2024-01-05', '2024-01-07')),
  끝날짜 = as.Date(c('2024-01-05', '2024-01-06', '2024-01-10', '2024-01-15'))
)

# 같은 사람이 다른 장소로 이사 가는 기간이 겹치는지 확인하고, 겹치는 구간을 추가하는 함수
add_overlapping_rows_person_location <- function(df) {
  new_rows <- data.frame()  # 추가할 새로운 행들을 저장할 데이터 프레임
  
  for (i in 1:(nrow(df)-1)) {
    for (j in (i+1):nrow(df)) {
      # 같은 사람인지 확인
      if (df$사람[i] == df$사람[j] && df$장소[i] != df$장소[j]) {
        시작1 <- df$시작날짜[i]
        끝1 <- df$끝날짜[i]
        시작2 <- df$시작날짜[j]
        끝2 <- df$끝날짜[j]
        
        # 기간이 겹치는지 확인
        if (시작1 <= 끝2 && 시작2 <= 끝1) {
          # 겹치는 구간의 시작과 끝 날짜 계산
          겹침_시작 <- max(시작1, 시작2)
          겹침_끝 <- min(끝1, 끝2)
          
          # 앞, 중복, 뒤로 새로운 행 추가
          앞_시작 <- min(시작1, 시작2) - 1
          뒷_끝 <- max(끝1, 끝2) + 1
          
          # 새로운 행 추가 (사람과 장소 정보 유지)
          new_rows <- rbind(new_rows, data.frame(사람 = df$사람[i], 장소 = df$장소[i], 시작날짜 = 앞_시작, 끝날짜 = 겹침_시작))
          new_rows <- rbind(new_rows, data.frame(사람 = df$사람[i], 장소 = df$장소[i], 시작날짜 = 겹침_시작, 끝날짜 = 겹침_끝))
          new_rows <- rbind(new_rows, data.frame(사람 = df$사람[i], 장소 = df$장소[j], 시작날짜 = 겹침_끝, 끝날짜 = 뒷_끝))
        }
      }
    }
  }
  
  return(rbind(df, new_rows))  # 원래 데이터와 새로 추가된 행 합치기
}

# 함수 실행
new_df <- add_overlapping_rows_person_location(df)
print(new_df)