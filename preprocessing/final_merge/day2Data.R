# [1] 날씨 데이터 연결
weather <- read.csv("./WeatherInfo.csv", fileEncoding="euc-kr")
weather$date2 <- as.Date(as.character(weather$날짜), "%Y%m%d")
head(weather)

LS <- read.csv("./LANDSLIDE.csv", fileEncoding="euc-kr")
sub <- read.csv("./210072.csv", fileEncoding="euc-kr")

LS$date <- as.character(LS$date)
LS$date2 <- as.Date(LS$date, "%Y%m%d")
LS$day1Before <- LS$date2 - 1
LS$day2Before <- LS$date2 - 2

head(weather)
head(LS)
weather$날짜 <- NULL
day2Data <- merge(LS[,c("sd", "sgg", "umd", "sum_cnt", "date2", "day2Before")], weather,
                  by.x=c("sd", "sgg", "umd", "day2Before"),
                  by.y=c("SIDO", "SGG_NM", "UMD", "date2"), all=TRUE)

colnames(day2Data)[colnames(day2Data) %in% c("최대시우량", "하루총강수량", "최대풍속", "강수비율")] <- paste0(c("최대시우량", "하루총강수량", "최대풍속", "강수비율"), "_1일전")
head(day2Data)
colSums(is.na(day2Data))
day2Data[is.na(day2Data$하루총강수량_1일전),]

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

day2Data <- merge(day2Data, TY[2:18], by.x=c("sgg", "umd"), by.y=c("SGG_NM", "UMD"), all.x=TRUE)
colSums(is.na(day2Data))

unique(day2Data[is.na(day2Data$PRRCK_LARG_화성암), c("sgg")])
TY_var <- colnames(TY)[4:18]

# 결측치 대체
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "경주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="경주시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "구미시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="구미시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "김천시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="김천시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "김해시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="김해시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "상주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="상주시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "양산시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="양산시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "영주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="영주시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "영천시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="영천시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "포항시 남구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "포항시 북구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="포항시 남구", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "진주시") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="진주시", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "창원시 성산구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 성산구", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "창원시 진해구") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="창원시 진해구", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "하동군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="하동군", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "함안군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="함안군", TY_var]

# 없음
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "영덕군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="영덕군", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "영양군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="영양군", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "울릉군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="울릉군", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "울진군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="울진군", TY_var]
day2Data[is.na(day2Data$PRRCK_LARG_화성암) & (day2Data$sgg == "청송군") , TY_var] <- TY_SGG[TY_SGG$SGG_NM=="청송군", TY_var]

## 임상도
day2Data <- merge(day2Data, IS[2:18], by.x=c("sgg", "umd"), by.y=c("SGG_NM", "UMD"), all.x=TRUE)
colSums(is.na(day2Data))
head(day2Data)

unique(day2Data[is.na(day2Data$STORUNST_CD_입목지), c("sgg")])
IS_var <- colnames(IS)[4:18]

# 결측치 대체
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "경주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="경주시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "구미시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="구미시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "김천시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="김천시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "김해시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="김해시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "상주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="상주시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "양산시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="양산시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "영주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="영주시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "영천시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="영천시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "포항시 남구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "포항시 북구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="포항시 남구", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "진주시") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="진주시", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "창원시 성산구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 성산구", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "창원시 진해구") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="창원시 진해구", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "하동군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="하동군", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "함안군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="함안군", IS_var]

# 없음
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "영덕군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="영덕군", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "영양군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="영양군", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "울릉군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="울릉군", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "울진군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="울진군", IS_var]
day2Data[is.na(day2Data$STORUNST_CD_입목지) & (day2Data$sgg == "청송군") , IS_var] <- IS_SGG[IS_SGG$SGG_NM=="청송군", IS_var]

day2Data$sum_cnt[is.na(day2Data$sum_cnt)] <- 0
file_format <- file('./day2Data.csv',encoding="euc-kr")
write.csv(day2Data, file_format, row.names=FALSE)
