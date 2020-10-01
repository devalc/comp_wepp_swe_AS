df1 <- readRDS("data/final.rds")
df2 <- read.csv("data/SNOTEL_Temperature_sensor_change_dates__dictionary1.csv")
colnames(df2)[1] <- "sntl_id"

df3 <- dplyr::left_join(df1,df2,by= "sntl_id")

write_rds(df3,"data/final_with_temp.RDS")

# df4 <- readRDS("data/final_with_temp.RDS")
