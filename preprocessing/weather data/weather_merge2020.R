########## KN
library(dplyr)
path = "./data2/경상남도/"
KN_SGGs <- list.files(path)
final1 <-  final2 <-  final3 <- NULL
for (sgg in KN_SGGs) {
  print(sgg)
  sgg_dir = paste0(path, sgg, "/")
  DATEs = list.files(sgg_dir)
  
  for (dt in DATEs){
    dt_dir = paste0(sgg_dir, dt, "/")
    FILEs = list.files(dt_dir)
    
    for (f in FILEs) {
      set <- strsplit(f, "_")
      umd = set[[1]][1]; value = set[[1]][2]; first_date = set[[1]][3]; end_date = substr(set[[1]][4],1,6)
      CSVfile = read.csv(paste0(dt_dir, f))
      CSVfile <- na.omit(CSVfile)
      colnames(CSVfile) <- c("day", "hour", "val")
      CSVfile$day <- as.numeric(CSVfile$day)
      CSVfile$hour <- as.numeric(CSVfile$hour)
      CSVfile$val <- as.numeric(CSVfile$val)
      CSVfile$month <- c(rep(6,30*24), rep(7,31*24), rep(8,31*24), rep(9,30*24))
      CSVfile$val[CSVfile$val == -1] <- 0
      
      if (value == "강수") {
        tmp1 <- CSVfile %>% group_by(month, day) %>% summarise(var1 = max(val, na.rm=TRUE),
                                                        var2 = sum(val, na.rm=TRUE)) %>% data.frame()
        colnames(tmp1) <- c("month", "day", "최대시우량", "하루총강수량")
        tmp1$SIDO <- "경상남도"; tmp1$SGG_NM <- sgg; tmp1$UMD <- umd; 
        tmp1$날짜 <- as.character(as.numeric(202000 + tmp1$month)*100 + tmp1$day); 
        final1 <- rbind(final1, tmp1)
      } else if (value == "풍속") {
        tmp2 <- CSVfile %>% group_by(month, day) %>% summarise(var = max(val, na.rm=TRUE)) %>% data.frame()
        colnames(tmp2) <- c("month", "day", "최대풍속")
        tmp2$SIDO <- "경상남도"; tmp2$SGG_NM <- sgg; tmp2$UMD <- umd; 
        tmp2$날짜 <- as.character(as.numeric(202000 + tmp2$month)*100 + tmp2$day);
        final2 <- rbind(final2, tmp2)
      } else {
        # 없음(0), 비(1), 비/눈(2), 눈(3)
        tmp3 <- CSVfile %>% group_by(month, day) %>% summarise("var1" = sum(val != 0, na.rm=TRUE)/24) %>% data.frame()
        
        colnames(tmp3) <- c("month", "day", "강수비율")
        tmp3$SIDO <- "경상남도"; tmp3$SGG_NM <- sgg; tmp3$UMD <- umd; 
        tmp3$날짜 <- as.character(as.numeric(202000 + tmp3$month)*100 + tmp3$day);
        final3 <- rbind(final3, tmp3)
        
      }
      
    }
    
  }
}

KN_final <- merge( final1[,c("SIDO", "SGG_NM", "UMD", "날짜", "최대시우량", "하루총강수량")],
                   final2[,c("SIDO", "SGG_NM", "UMD", "날짜", "최대풍속")],
                   by= c("SIDO", "SGG_NM", "UMD", "날짜"),
                   all=TRUE)
KN_final <- merge( KN_final, final3[,c("SIDO", "SGG_NM", "UMD", "날짜", "강수비율")],
                   by= c("SIDO", "SGG_NM", "UMD", "날짜"),
                   all=TRUE)
colSums(is.na(KN_final))
KN_final[KN_final$UMD == "장목면",]

########## KB
path = "./data2/경상북도/"
KB_SGGs <- list.files(path)
final1 <-  final2 <-  final3 <- NULL
for (sgg in KB_SGGs) {
  print(sgg)
  sgg_dir = paste0(path, sgg, "/")
  DATEs = list.files(sgg_dir)
  
  for (dt in DATEs){
    dt_dir = paste0(sgg_dir, dt, "/")
    FILEs = list.files(dt_dir)
    
    for (f in FILEs) {
      set <- strsplit(f, "_")
      umd = set[[1]][1]; value = set[[1]][2]; first_date = set[[1]][3]; end_date = substr(set[[1]][4],1,6)
      CSVfile = read.csv(paste0(dt_dir, f))
      CSVfile <- na.omit(CSVfile)
      colnames(CSVfile) <- c("day", "hour", "val")
      CSVfile$day <- as.numeric(CSVfile$day)
      CSVfile$hour <- as.numeric(CSVfile$hour)
      CSVfile$month <- c(rep(6,30*24), rep(7,31*24), rep(8,31*24), rep(9,30*24), rep(10,31*24))
      CSVfile$val[CSVfile$val == -1] <- 0
      
      if (value == "강수") {
        tmp1 <- CSVfile %>% group_by(month, day) %>% summarise(var1 = max(val, na.rm=TRUE),
                                                               var2 = sum(val, na.rm=TRUE)) %>% data.frame()
        colnames(tmp1) <- c("month", "day", "최대시우량", "하루총강수량")
        tmp1$SIDO <- "경상북도"; tmp1$SGG_NM <- sgg; tmp1$UMD <- umd; 
        tmp1$날짜 <- as.character(as.numeric(202000 + tmp1$month)*100 + tmp1$day); 
        final1 <- rbind(final1, tmp1)
      } else if (value == "풍속") {
        tmp2 <- CSVfile %>% group_by(month, day) %>% summarise(var = max(val, na.rm=TRUE)) %>% data.frame()
        colnames(tmp2) <- c("month", "day", "최대풍속")
        tmp2$SIDO <- "경상북도"; tmp2$SGG_NM <- sgg; tmp2$UMD <- umd; 
        tmp2$날짜 <- as.character(as.numeric(202000 + tmp2$month)*100 + tmp2$day);
        final2 <- rbind(final2, tmp2)
      } else {
        # 없음(0), 비(1), 비/눈(2), 눈(3)
        tmp3 <- CSVfile %>% group_by(month, day) %>% summarise("var1" = sum(val != 0, na.rm=TRUE)/24) %>% data.frame()
        
        colnames(tmp3) <- c("month", "day", "강수비율")
        tmp3$SIDO <- "경상북도"; tmp3$SGG_NM <- sgg; tmp3$UMD <- umd; 
        tmp3$날짜 <- as.character(as.numeric(202000 + tmp3$month)*100 + tmp3$day);
        final3 <- rbind(final3, tmp3)
        
      }
    }
    
  }
}

KB_final <- merge( final1[,c("SIDO", "SGG_NM", "UMD", "날짜", "최대시우량", "하루총강수량")],
                   final2[,c("SIDO", "SGG_NM", "UMD", "날짜", "최대풍속")],
                   by= c("SIDO", "SGG_NM", "UMD", "날짜"),
                   all=TRUE)
KB_final <- merge( KB_final, final3[,c("SIDO", "SGG_NM", "UMD", "날짜", "강수비율")],
                   by= c("SIDO", "SGG_NM", "UMD", "날짜"),
                   all=TRUE)
colSums(is.na(KB_final))

final <- rbind(KN_final, KB_final)
colSums(is.na(final))
final <- final[!is.na(final$최대시우량),]

file_format <- file("./WeatherInfo2020.csv", encoding='euc-kr')
write.csv(final, file_format, row.names=FALSE)