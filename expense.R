library(dplyr)
library(ggplot2)
library(googlesheets4)


expense_gsheet_url <- "https://docs.google.com/spreadsheets/d/1VE3YzMftYbyScsspqp8ZuHIEzRp01CeOqN6TYUJgylk/edit#gid=0"
gexp <- read_sheet(expense_gsheet_url)

head(gexp)
gexp <- as.data.frame(gexp)
gexp$mth_yr <- format(as.Date(gexp$Date,"%Y-%m-%d"),"%b%Y")
list_mthyr <- unique(gexp$mth_yr)

exp.summ <- gexp %>%
  filter(mth_yr %in% list_mthyr[1]) %>%
  group_by(mth_yr,Category) %>%
  summarise(tot_amt = sum(Amount)) %>%
  arrange(mth_yr,desc(tot_amt))
exp.summ <- exp.summ[,c(2,3)]
names(exp.summ)[2] <- list_mthyr[1]

for(i in 2:length(list_mthyr)){
  
  temp <- gexp %>% filter(mth_yr %in% list_mthyr[i]) %>%
    group_by(Category) %>%
    summarise(tot_amt = sum(Amount))
  names(temp)[2] <- list_mthyr[i]
  exp.summ <- exp.summ %>% full_join(temp,by=c("Category"))
}
exp.summ[is.na(exp.summ)] <- 0
exp.summ$total <- rowSums(exp.summ[,c(list_mthyr)])
colSums(exp.summ[,c(list_mthyr)])
sum(exp.summ$tot_amt)

start <- 0
pmt <- 4000
int <- .01
mths <- c(1:60)

sch <- data.frame(months = mths,
                  princ = as.numeric(NA),
                  pmt = as.numeric(NA),
                  interest = as.numeric(NA),
                  total = as.numeric(NA),
                  stringsAsFactors = F)

for(i in 1:length(mths)){
  if(i==1){
    sch$princ[i] <- start
    sch$pmt[i] <- pmt
    sch$interest[i] <- (sch$princ[i] + sch$pmt[i]) * (int)
    sch$total[i] <- (sch$princ[i] + sch$pmt[i]) * (1+int)
  }else{
    sch$princ[i] <- sch$total[i-1]
    sch$pmt[i] <- pmt
    sch$interest[i] <- (sch$princ[i] + sch$pmt[i]) * (int)
    sch$total[i] <- (sch$princ[i] + sch$pmt[i]) * (1+int)
  }
}

sch
