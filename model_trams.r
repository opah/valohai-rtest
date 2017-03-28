library(lubridate)
library(stringr)
#library(ggplot2)
library(dplyr)
library(mgcv)

installed.packages()

leg_times_trams <- read.csv("/valohai/inputs/leg_times/leg_times_trams_sample.csv", stringsAsFactors = FALSE)
str(leg_times_trams)

lines <- sort(unique(leg_times_trams$desi))

model_list <- list()
res_model_list <- list()
model_info <- data.frame(id = numeric(), line = character(), dir = numeric())
for (v_line in lines){
  for (v_dir in 1:2){
    print(str_c("line: ", v_line, " dir: ", v_dir))
    md_leg = leg_times_trams %>% filter(desi == v_line, dir == v_dir)
    print("Fitting leg time model...")
    gam_model <- gam(legTime ~ factor(nextStop) + factor(weekday) + s(model_hour,by=factor(weekday)), 
                     family = gaussian(link = "identity"), 
                     data = md_leg)
    print("Fitting variance model...")
    gam_res_model <- gam(res ~ factor(nextStop) + factor(weekday) + s(model_hour,by=factor(weekday)), family = Gamma(link = "log"),
                         data = md_leg %>% 
                           mutate(res = abs(legTime - predict(gam_model))))
    model_id <- str_c("L",v_line,v_dir)
    model_list[[model_id]] <- gam_model
    res_model_list[[model_id]] <- gam_res_model
    png("/valohai/outputs/test.png")
    plot(gam_model, pages=1)
    dev.off()
  }
}
#save(model_list, res_modell_list, fil)

wd <- "~/feikkiopas-data-analysis"
setwd(wd)
