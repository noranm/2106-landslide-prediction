# [1] 날씨 데이터 연결
weather <- read.csv("./WeatherInfo2020.csv", fileEncoding="euc-kr")
weather$date2 <- as.character(as.Date(as.character(weather$날짜), "%Y%m%d"))
head(weather)ㅣ
weather[weather$UMD=="장목면",]

sub <- read.csv("./참가번호.csv", fileEncoding="euc-kr")
head(sub)
str(sub)
#sub$date2 <- as.Date(as.character(sub$date), "%Y%m%d")
#sub$X1day <- as.Date(as.character(sub$X1day), "%Y%m%d")
#sub$X2day <- as.Date(as.character(sub$X2day), "%Y%m%d")

head(weather)
head(sub)
weather$날짜 <- NULL
head(sub)
head(weather)
sub$umd[sub$umd=="삼거동"] <- "상문동"
sub$umd[sub$umd=="어곡동"] <- "강서동"
sub$umd[sub$umd=="주진동"] <- "소주동"

sub$umd[sub$umd=="초전면"] <- "성주읍"
sub$umd[sub$umd=="시동"] <- "불국동" 
sub$umd[sub$umd=="진보면"] <- "청송읍" 



day1Sub <- merge(sub[,c("sd", "sgg", "umd", "date", "X1day")], weather,
                  by.x=c("sd", "sgg", "umd", "X1day"),
                  by.y=c("SIDO", "SGG_NM", "UMD", "date2"), all.x=TRUE)

colnames(day1Sub)[colnames(day1Sub) %in% c("최대시우량", "하루총강수량", "최대풍속", "강수비율")] <- paste0(c("최대시우량", "하루총강수량", "최대풍속", "강수비율"), "_1일전")
head(day1Sub)
colSums(is.na(day1Sub))
day1Sub[is.na(day1Sub$하루총강수량_1일전),]

# [2] 토양/임상 데이터 연결
KN_TY2_SGG <- read.csv("./data/SGG_MAP/KN_TOYANG2_SGG.csv", fileEncoding="euc-kr")
KB_TY2_SGG <- read.csv("./data/SGG_MAP/KB_TOYANG2_SGG.csv", fileEncoding="euc-kr")

# IS
KN_IS2_SGG <- read.csv("./data/SGG_MAP/KN_IMSANG2_SGG.csv", fileEncoding="euc-kr")
KB_IS2_SGG <- read.csv("./data/SGG_MAP/KB_IMSANG2_SGG.csv", fileEncoding="euc-kr")

TY_SGG <- rbind(KN_TY2_SGG, KB_TY2_SGG)
IS_SGG <- rbind(KN_IS2_SGG, KB_IS2_SGG)

# TY
KN_TY2 <- read.csv("./data/UMD_MAP/KN_TOYANG2.csv", fileEncoding="euc-kr")
KB_TY2 <- read.csv("./data/UMD_MAP/KB_TOYANG2.csv", fileEncoding="euc-kr")

# IS
KN_IS2 <- read.csv("./data/UMD_MAP/KN_IMSANG2.csv", fileEncoding="euc-kr")
KB_IS2 <- read.csv("./data/UMD_MAP/KB_IMSANG2.csv", fileEncoding="euc-kr")

TY <- rbind(KN_TY2, KB_TY2)
IS <- rbind(KN_IS2, KB_IS2)
TY$SGG_NM[TY$SGG_NM == "창원시의창구"] <- "창원시 의창구"
TY_SGG$SGG_NM[TY_SGG$SGG_NM == "창원시의창구"] <- "창원시 의창구"

day1Sub <- merge(day1Sub, TY[2:18], by.x=c("sgg", "umd"), by.y=c("SGG_NM", "UMD"), all.x=TRUE)
colSums(is.na(day1Sub))

unique(day1Sub[is.na(day1Sub$PRRCK_LARG_화성암), c("sgg")])
TY_var <- colnames(TY)[4:18]

# 결측치 대체
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "사천시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="사천시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "거제시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="거제시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "경주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="경주시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "구미시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="구미시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "김천시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="김천시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "김해시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="김해시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "상주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="상주시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "양산시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="양산시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "영주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="영주시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "영천시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="영천시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "포항시 남구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "포항시 북구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "진주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="진주시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "창원시 성산구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 성산구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "창원시 진해구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 진해구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "창원시 마산합포구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 마산합포구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "창원시 마산회원구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 마산회원구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "창원시 의창구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 의창구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "통영시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="통영시", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "하동군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="하동군", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "함안군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="함안군", TY_var]

# 없음
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "영덕군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "영양군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="봉화군", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "울릉군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "울진군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day1Sub[is.na(day1Sub$PRRCK_LARG_화성암) & (day1Sub$sgg == "청송군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="안동시", TY_var]

## 임상도
day1Sub <- merge(day1Sub, IS[2:18], by.x=c("sgg", "umd"), by.y=c("SGG_NM", "UMD"), all.x=TRUE)
colSums(is.na(day1Sub))
head(day1Sub)

unique(day1Sub[is.na(day1Sub$STORUNST_CD_입목지), c("sgg")])
IS_var <- colnames(IS)[4:18]

# 결측치 대체
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "거제시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="거제시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "사천시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="사천시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "경주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="경주시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "구미시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="구미시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "김천시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="김천시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "김해시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="김해시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "상주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="상주시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "양산시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="양산시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "영주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="영주시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "영천시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="영천시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "포항시 남구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "포항시 북구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "진주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="진주시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "창원시 성산구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 성산구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "창원시 진해구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 진해구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "창원시 마산합포구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 마산합포구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "창원시 마산회원구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 마산회원구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "창원시 의창구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 의창구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "통영시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="통영시", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "하동군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="하동군", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "함안군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="함안군", IS_var]

# 없음
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "영덕군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "영양군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="봉화군", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "울릉군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "울진군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day1Sub[is.na(day1Sub$STORUNST_CD_입목지) & (day1Sub$sgg == "청송군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="안동시", IS_var]

colSums(is.na(day1Sub))
file_format <- file('./day1Sub.csv',encoding="euc-kr")
write.csv(day1Sub, file_format, row.names=FALSE)



day1Sub <- merge(sub[,c("sd", "sgg", "umd", "date", "X1day")], weather,
                 by.x=c("sd", "sgg", "umd", "X1day"),
                 by.y=c("SIDO", "SGG_NM", "UMD", "date2"), all.x=TRUE)

colnames(day1Sub)[colnames(day1Sub) %in% c("최대시우량", "하루총강수량", "최대풍속", "강수비율")] <- paste0(c("최대시우량", "하루총강수량", "최대풍속", "강수비율"), "_1일전")
head(day1Sub)
colSums(is.na(day1Sub))
day1Sub[is.na(day1Sub$하루총강수량_1일전),]

# [2] 토양/임상 데이터 연결
KN_TY2_SGG <- read.csv("./data/SGG_MAP/KN_TOYANG2_SGG.csv", fileEncoding="euc-kr")
KB_TY2_SGG <- read.csv("./data/SGG_MAP/KB_TOYANG2_SGG.csv", fileEncoding="euc-kr")

# IS
KN_IS2_SGG <- read.csv("./data/SGG_MAP/KN_IMSANG2_SGG.csv", fileEncoding="euc-kr")
KB_IS2_SGG <- read.csv("./data/SGG_MAP/KB_IMSANG2_SGG.csv", fileEncoding="euc-kr")

TY_SGG <- rbind(KN_TY2_SGG, KB_TY2_SGG)
IS_SGG <- rbind(KN_IS2_SGG, KB_IS2_SGG)

# TY
KN_TY2 <- read.csv("./data/UMD_MAP/KN_TOYANG2.csv", fileEncoding="euc-kr")
KB_TY2 <- read.csv("./data/UMD_MAP/KB_TOYANG2.csv", fileEncoding="euc-kr")

# IS
KN_IS2 <- read.csv("./data/UMD_MAP/KN_IMSANG2.csv", fileEncoding="euc-kr")
KB_IS2 <- read.csv("./data/UMD_MAP/KB_IMSANG2.csv", fileEncoding="euc-kr")

TY <- rbind(KN_TY2, KB_TY2)
IS <- rbind(KN_IS2, KB_IS2)
TY$SGG_NM[TY$SGG_NM == "창원시의창구"] <- "창원시 의창구"
TY_SGG$SGG_NM[TY_SGG$SGG_NM == "창원시의창구"] <- "창원시 의창구"




day2Sub <- merge(sub[,c("sd", "sgg", "umd", "date", "X2day")], weather,
                 by.x=c("sd", "sgg", "umd", "X2day"),
                 by.y=c("SIDO", "SGG_NM", "UMD", "date2"), all.x=TRUE)

colnames(day2Sub)[colnames(day2Sub) %in% c("최대시우량", "하루총강수량", "최대풍속", "강수비율")] <- paste0(c("최대시우량", "하루총강수량", "최대풍속", "강수비율"), "_2일전")
head(day2Sub)
colSums(is.na(day2Sub))

day2Sub <- merge(day2Sub, TY[2:18], by.x=c("sgg", "umd"), by.y=c("SGG_NM", "UMD"), all.x=TRUE)
colSums(is.na(day2Sub))

unique(day2Sub[is.na(day2Sub$PRRCK_LARG_화성암), c("sgg")])
TY_var <- colnames(TY)[4:18]

# 결측치 대체
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "사천시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="사천시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "거제시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="거제시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "경주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="경주시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "구미시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="구미시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "김천시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="김천시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "김해시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="김해시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "상주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="상주시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "양산시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="양산시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "영주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="영주시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "영천시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="영천시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "포항시 남구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "포항시 북구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "진주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="진주시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "창원시 성산구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 성산구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "창원시 진해구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 진해구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "창원시 마산합포구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 마산합포구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "창원시 마산회원구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 마산회원구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "창원시 의창구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 의창구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "통영시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="통영시", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "하동군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="하동군", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "함안군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="함안군", TY_var]

# 없음
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "영덕군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "영양군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="봉화군", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "울릉군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "울진군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day2Sub[is.na(day2Sub$PRRCK_LARG_화성암) & (day2Sub$sgg == "청송군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="안동시", TY_var]

## 임상도
day2Sub <- merge(day2Sub, IS[2:18], by.x=c("sgg", "umd"), by.y=c("SGG_NM", "UMD"), all.x=TRUE)
colSums(is.na(day2Sub))
head(day2Sub)

unique(day2Sub[is.na(day2Sub$STORUNST_CD_입목지), c("sgg")])
IS_var <- colnames(IS)[4:18]

# 결측치 대체
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "거제시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="거제시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "사천시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="사천시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "경주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="경주시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "구미시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="구미시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "김천시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="김천시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "김해시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="김해시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "상주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="상주시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "양산시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="양산시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "영주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="영주시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "영천시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="영천시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "포항시 남구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "포항시 북구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "진주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="진주시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "창원시 성산구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 성산구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "창원시 진해구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 진해구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "창원시 마산합포구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 마산합포구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "창원시 마산회원구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 마산회원구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "창원시 의창구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 의창구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "통영시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="통영시", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "하동군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="하동군", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "함안군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="함안군", IS_var]

# 없음
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "영덕군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "영양군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="봉화군", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "울릉군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "울진군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day2Sub[is.na(day2Sub$STORUNST_CD_입목지) & (day2Sub$sgg == "청송군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="안동시", IS_var]

colSums(is.na(day2Sub))
file_format <- file('./day2Sub.csv',encoding="euc-kr")
write.csv(day2Sub, file_format, row.names=FALSE)
