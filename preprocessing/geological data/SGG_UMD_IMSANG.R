library(rgdal)
library(ggplot2)
library(dplyr)
library(raster)

# 좌표계 수정
ls_crs = list(wgs84 = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

UMD <- readOGR("./WeatherBigdata/LSMD_ADM_SECT_UMD_KN/LSMD_ADM_SECT_UMD_48.shp", encoding="euc-kr")
UMD = spTransform(UMD, CRSobj = CRS(ls_crs$wgs84))

UMDdata <- data.frame(UMD@data)
UMDdata$id <- row.names(UMDdata)

head(UMDdata)
UMDpoly <- fortify(UMD)
UMD_name <- data.frame(cbind("id" = row.names(UMDdata), 
                             "NO" = UMDdata$COL_ADM_SE,
                             "UMD" = UMDdata$EMD_NM ))

UMDpoly <- merge(UMDpoly, UMD_name, by="id")

unique(UMDdata$COL_ADM_SE)
# Setting round 6
SGG_NUM <- data.frame("num" = c(48310, 48880, 48820, 48250, 48840, 48270, 48240, 48860,
                                48330, 48720, 48170, 48740, 48125, 48127, 48123, 48121,
                                48129, 48220, 48730, 48850, 48870, 48890),
                      "SGG" = c("거제시", "거창군", "고성군", "김해시", "남해군", "밀양시", "사천시",  "산청군",
                                "양산시", "의령군", "진주시", "창녕군", "창원시 마산합포구", "창원시 마산회원구", "창원시 성산구",  "창원시 의창구", 
                                "창원시 진해구", "통영시", "하동군", "함안군", "함양군", "합천군"))

path = "./WeatherBigdata/FRT001003/FRT001003_"
i=48310
df_shp_final <- NULL
for (i in SGG_NUM$num){
  SGG_NM <- SGG_NUM$SGG[SGG_NUM$num==i]
  file_name <- paste0(path, i, "/TB_FGDI_FS_IM5000_", i, ".shp")
  print(file_name)
  # shp file load
  shp_file <- rgdal::readOGR(file_name, encoding="euc-kr")
  shp_file = spTransform(shp_file, CRSobj = CRS(ls_crs$wgs84))
  shp_file@proj4string
  
  # data 불러오기
  df_shp = shp_file@data
  df_shp$id <- row.names(df_shp)
  # 지역이름 연결
  df_shp$SGG_NM <- SGG_NUM$SGG[SGG_NUM$num==i]
  head(df_shp)
  
  polygon <- fortify(shp_file)
  
  centroid <- polygon %>% group_by(id) %>%
    summarise(centroid_lat = mean(lat), centroid_lon = mean(long) ) %>% data.frame()
  
  tryCatch( {
    if (i %in% c(48125, 48127, 48123, 48121,48129)) {
      j=48120
    } else{
      j=i
    }
    UMDtmp <- UMDpoly[UMDpoly$NO == j, ]
    UMD_i_cent <- UMDtmp %>% group_by(id, UMD) %>%
      summarise(centroid_lat = mean(lat), centroid_lon = mean(long)) %>% data.frame()
    
    diff_lat <- outer(centroid$centroid_lat, UMD_i_cent$centroid_lat, "-")
    diff_lon <- outer(centroid$centroid_lon, UMD_i_cent$centroid_lon, "-")
    diff <- (diff_lat^2 + diff_lon^2)
    centroid$UMD <- UMD_i_cent$UMD[apply(diff, 1, FUN=which.min)]
    df_shp <- merge(df_shp, centroid[,c("id","UMD")], by='id') },
  error = function(e) {df_shp$UMD <- NA},
  finally = function(f) print(i)
  )
  # 최종 파일 모든 지역 데이터 : df_shp_final
  df_shp_final <- rbind(df_shp_final, df_shp)
}

file_format <- file('./KN_IMSANG.csv',encoding="euc-kr")
write.csv(df_shp_final, file_format)

df_shp_final <- read.csv("./data/UMD_MAP/KN_IMSANG.csv", fileEncoding = 'euc-kr')
df_shp_final$STORUNST <- as.numeric(df_shp_final$STORUNST)
df_shp_final$FROR_CD <- as.numeric(df_shp_final$FROR_CD)
df_shp_final$FRTP_CD <- as.numeric(df_shp_final$FRTP_CD)

KN_IMSANG <- df_shp_final %>% group_by(SGG_NM, UMD) %>% 
  summarise("STORUNST_CD_입목지" = mean(as.numeric(STORUNST==1), na.rm=TRUE),
            "STORUNST_CD_무립목지" = mean(as.numeric(STORUNST==2), na.rm=TRUE),
            "STORUNST_CD_비산림" = mean(as.numeric(STORUNST==0), na.rm=TRUE),
            
            "FROR_CD_인공림" = mean(as.numeric(FROR_CD==1), na.rm=TRUE),
            "FROR_CD_천연림" = mean(as.numeric(FROR_CD==2), na.rm=TRUE),
            "FROR_CD_비산림" = mean(as.numeric(FROR_CD==0), na.rm=TRUE),
            
            "FRTP_CD_침엽수림" = mean(as.numeric(FRTP_CD==1), na.rm=TRUE),
            "FRTP_CD_활엽수림" = mean(as.numeric(FRTP_CD==2), na.rm=TRUE),
            "FRTP_CD_혼효림" = mean(as.numeric(FRTP_CD==3), na.rm=TRUE),
            "FRTP_CD_죽림" = mean(as.numeric(FRTP_CD==4), na.rm=TRUE),
            "FRTP_CD_비산림" = mean(as.numeric(FRTP_CD==0), na.rm=TRUE),
            
            "KOFTR_GROU_CD_소나무" = mean(as.numeric(KOFTR_GROU==11), na.rm=TRUE),
            "KOFTR_GROU_CD_낙엽송" = mean(as.numeric(KOFTR_GROU==13), na.rm=TRUE),
            
            "MEAN_DMCLS_CD" = mean(as.numeric(DMCLS_CD), na.rm=TRUE),
            "MEAN_AGCLS_CD" = mean(as.numeric(AGCLS_CD), na.rm=TRUE) ) %>% data.frame()

table(KN_IMSANG$KOFTR_GROU_CD_소나무)
head(KN_IMSANG)
file_format <- file('./KN_IMSANG2.csv',encoding="euc-kr")
write.csv(KN_IMSANG, file_format)

# KB
UMD <- readOGR("./WeatherBigdata/LSMD_ADM_SECT_UMD_KB/LSMD_ADM_SECT_UMD_47.shp", encoding="euc-kr")
UMD = spTransform(UMD, CRSobj = CRS(ls_crs$wgs84))

UMDdata <- data.frame(UMD@data)
UMDdata$id <- row.names(UMDdata)

UMDpoly <- fortify(UMD)
UMD_name <- data.frame(cbind("id" = row.names(UMDdata), "NO" = UMDdata$COL_ADM_SE,
                             "UMD" = UMDdata$EMD_NM))
UMDpoly <- merge(UMDpoly, UMD_name, by="id")


SGG_NUM <- data.frame("num" = c(47290, 47130, 47830, 47190, 47720, 
                                47150, 47280, 47920, 47250, 47840, 
                                47170, 47210, 47230, 47900, 47730,
                                47820, 47850, 47111, 47113),
                      "SGG" = c("경산시", "경주시", "고령군", "구미시", "군위군",
                                "김천시", "문경시", "봉화군", "상주시", "성주군",
                                "안동시", "영주시", "영천시", "예천군", "의성군",
                                "청도군", "칠곡군", "포항시 남구", "포항시 북구"))
i=47190
path = "./WeatherBigdata/FRT001003/FRT001003_"
df_shp_final <- NULL

for (i in SGG_NUM$num){
  print(i)
  file_name <- paste0(path, i, "/TB_FGDI_FS_IM5000_", i, ".shp")
  print(file_name)
  # shp file load
  shp_file <- rgdal::readOGR(file_name, encoding="euc-kr")
  shp_file = spTransform(shp_file, CRSobj = CRS(ls_crs$wgs84))
  shp_file@proj4string
  
  # data 불러오기
  df_shp = shp_file@data
  df_shp$id <- row.names(df_shp)  
  # 지역이름 연결
  df_shp$SGG_NM <- SGG_NUM$SGG[SGG_NUM$num==i]
  
  polygon <- fortify(shp_file)
  
  centroid <- polygon %>% group_by(id) %>%
    summarise(centroid_lat = mean(lat), centroid_lon = mean(long) ) %>% data.frame()
  
  tryCatch( {
    if (i %in% c(47111, 47113)) {
      j=47110
    } else{
      j=i
    }
    UMDtmp <- UMDpoly[UMDpoly$NO == j, ]
    UMD_i_cent <- UMDtmp %>% group_by(id, UMD) %>%
      summarise(centroid_lat = mean(lat), centroid_lon = mean(long)) %>% data.frame()
    
    diff_lat <- outer(centroid$centroid_lat, UMD_i_cent$centroid_lat, "-")
    diff_lon <- outer(centroid$centroid_lon, UMD_i_cent$centroid_lon, "-")
    diff <- (diff_lat^2 + diff_lon^2)
    centroid$UMD <- UMD_i_cent$UMD[apply(diff, 1, FUN=which.min)]
    df_shp <- merge(df_shp, centroid[,c("id","UMD")], by='id')
    head(df_shp)
  },
  error = function(e) {
    df_shp$UMD <- NA
  },
  finally = function(f) {
  } )
  # 최종 파일 모든 지역 데이터 : df_shp_final
  df_shp_final <- rbind(df_shp_final, df_shp)
}

file_format <- file('./KB_IMSANG.csv', encoding="euc-kr")
write.csv(df_shp_final, file_format)

df_shp_final <- read.csv("./data/UMD_MAP/KB_IMSANG.csv", fileEncoding = 'euc-kr')
df_shp_final$STORUNST <- as.numeric(df_shp_final$STORUNST)
df_shp_final$FROR_CD <- as.numeric(df_shp_final$FROR_CD)
df_shp_final$FRTP_CD <- as.numeric(df_shp_final$FRTP_CD)

KB_IMSANG <- df_shp_final %>% group_by(SGG_NM, UMD) %>% 
  summarise("STORUNST_CD_입목지" = mean(as.numeric(STORUNST==1), na.rm=TRUE),
            "STORUNST_CD_무립목지" = mean(as.numeric(STORUNST==2), na.rm=TRUE),
            "STORUNST_CD_비산림" = mean(as.numeric(STORUNST==0), na.rm=TRUE),
            
            "FROR_CD_인공림" = mean(as.numeric(FROR_CD==1), na.rm=TRUE),
            "FROR_CD_천연림" = mean(as.numeric(FROR_CD==2), na.rm=TRUE),
            "FROR_CD_비산림" = mean(as.numeric(FROR_CD==0), na.rm=TRUE),
            
            "FRTP_CD_침엽수림" = mean(as.numeric(FRTP_CD==1), na.rm=TRUE),
            "FRTP_CD_활엽수림" = mean(as.numeric(FRTP_CD==2), na.rm=TRUE),
            "FRTP_CD_혼효림" = mean(as.numeric(FRTP_CD==3), na.rm=TRUE),
            "FRTP_CD_죽림" = mean(as.numeric(FRTP_CD==4), na.rm=TRUE),
            "FRTP_CD_비산림" = mean(as.numeric(FRTP_CD==0), na.rm=TRUE),
            
            "KOFTR_GROU_CD_소나무" = mean(as.numeric(KOFTR_GROU==11), na.rm=TRUE),
            "KOFTR_GROU_CD_낙엽송" = mean(as.numeric(KOFTR_GROU==13), na.rm=TRUE),
            
            "MEAN_DMCLS_CD" = mean(as.numeric(DMCLS_CD), na.rm=TRUE),
            "MEAN_AGCLS_CD" = mean(as.numeric(AGCLS_CD), na.rm=TRUE) ) %>% data.frame()

file_format <- file('./KB_IMSANG2.csv',encoding="euc-kr")
write.csv(KB_IMSANG, file_format)
