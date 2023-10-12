###################################################################
#) Econvote_MediaSubs/SentimentData_PW/code_nn/stimson_NYT_USA_plts.R 
###################################################################
# Date Run:	February, 13 2023
#           September 7 2023
# Purpose: Produced plots of NYT and USAToday series with their individual PA, LN and smooth/unsmooth series in each plot. 
# 

#Data in:	Stim_NYT, NYT_pa_agg, NYT_ln_agg
#         stim_USA, USA_pa_agg, USA_ln_agg

#Data Out:	NYT.PA&LN_latent_series.pdf, long_series.pdf, USA.PA&LN_latent_series.pdf
#.          NYT.pa.ln.smth.pdf, USA.pa.ln.smth.pdf


###################################################################

library(dplyr)
library(ggplot2)
library(data.table)
library(readr)
library(dplyr)
library(tidyr)
library(DyadRatios)
library(lubridate)
library(tseries)
library(astsa)
library(gridExtra)

###################################################################

###################################################################
# Plot All 4 NYT series: Original PA and LN series & Stimson series (with and without smoothing)
###################################################################
#for original series read in data as LN.agg and PA.agg
stim_NYT <- read_csv("~/stim.NYT.csv")
NYT_pa_agg <- read_csv("~/NYT_pa_agg.csv")
NYT_ln_agg <- read_csv("~/NYT_ln_agg.csv")

## Plot Series 
# NYT PA
nyt.pa.ts <- ts(NYT_pa_agg$weighted.avg.pa, start=c(1947,1),end=c(2014,12), freq=12)
# NYT Lexis Nexis
nyt.lexi.ts <- ts(NYT_ln_agg$weighted.avg.ln, start=c(1980,6),end=c(2022,11), freq=12)
# Stim Smooth
stim.sm <- ts(stim_NYT$smooth, start = c(1947,1), end = c(2022,11), freq=12)
# Stim NON Smooth
#stim.nonsm <- ts(stim_NYT$nonsmooth, start = c(1947,1), end = c(2022,11), freq=12)

#pdf("NYT.PA&LN_latent_series.pdf",
#    width = 10, height = 7)

pdf("NYT.pa.ln.smth.pdf",
    width = 10, height = 7)

ts.plot(nyt.pa.ts, nyt.lexi.ts,stim.sm,
        gpars=list(main="Economic Sentiment In Newspapers, NYT Political Analysis & Lexis Nexis Series and Stimson Series",
                   xlab="Year", 
                   ylab="Average Positive Probability", 
                   lty=c(1:2),
                   lwd= 1,
                   col = c("red", "blue", "black"))
)

legend("bottomright", legend = c("NYT Political Analysis: Jan, 1947 - Dec, 2014", 
                                 "NYT LexisNexis: Jun, 1980 - Nov, 2022",
                                 "Stimson Values w/ Smooth: Jan, 1947 - Nov, 2022"
                                 ),
       col = c("red", "blue", "black"), 
       lty = 1:2, cex = 0.5)

dev.off()


###################################################################
# correlation NYT Series
###################################################################

NYT_pa_agg$Date <- as.Date(paste(NYT_pa_agg$year, NYT_pa_agg$month, 1, sep = "/"))
NYT_ln_agg$Date <- as.Date(paste(NYT_ln_agg$year, NYT_ln_agg$month, 1, sep = "/"))

NYT_agg <- left_join(NYT_pa_agg, NYT_ln_agg, by = c("Date" = "Date"))
NYT_agg <- left_join(NYT_agg, stim_NYT, by = c("Date" = "Date"))

NYT_agg <- NYT_agg %>% rename("article_freq.pa" = "article_freq.x")
NYT_agg <- NYT_agg %>% rename("article_freq.ln" = "article_freq.y")
write.csv(NYT_agg, "NYT_ttl_series.csv")

NYT_corr <- NYT_agg %>%
  select("weighted.avg.pa", "article_freq.pa", "weighted.avg.ln", "article_freq.ln", "smooth", "nonsmooth")
round(cor(NYT_corr, use = "pairwise.complete.obs"), 2) 
#                 weighted.avg.pa article_freq.pa weighted.avg.ln article_freq.ln smooth nonsmooth
#weighted.avg.pa            1.00           -0.29            0.72           -0.48   0.89      0.97
#article_freq.pa           -0.29            1.00            0.13            0.34  -0.26     -0.25
#weighted.avg.ln            0.72            0.13            1.00           -0.42   0.90      0.90
#article_freq.ln           -0.48            0.34           -0.42            1.00  -0.57     -0.50
#smooth                     0.89           -0.26            0.90           -0.57   1.00      0.93
#nonsmooth                  0.97           -0.25            0.90           -0.50   0.93      1.00

############ attempt 2 at a plot
NYT_agg2 <- NYT_agg %>%
  select("weighted.avg.pa", "weighted.avg.ln", "smooth", "nonsmooth", "Date")
data_long <- pivot_longer(NYT_agg2, cols = -Date, names_to = "series", values_to = "values")

ggplot(data_long, aes(x= Date, y =values)) +
  geom_line(na.rm = TRUE, aes(col = series, linetype = series), size =0.05) +
  theme_bw()
ggsave("long_series.pdf")


###################################################################
# Plot All 4 USAToday series: Original PA and LN series & Stimson series (with and without smoothing)
###################################################################
#for original series read in data as LN.agg and PA.agg
stim_USA <- read_csv("~/stim.USA.csv")
USA_ln_agg <- read_csv("~/USA_ln_agg.csv")
USA_pa_agg <- read_csv("~/USA_pa_agg.csv")

## Plot Series 
# USA PA
usa.pa.ts <- ts(USA_pa_agg$weighted.avg.pa, start=c(1987,4),end=c(2014,12), freq=12)
# USA Lexis Nexis
usa.lexi.ts <- ts(USA_ln_agg$weighted.avg.ln, start=c(1989,1),end=c(2022,11), freq=12)
# Stim Smooth
stim.sm <- ts(stim_USA$smooth, start = c(1987,4), end = c(2022,11), freq=12)
# Stim NON Smooth
#stim.nonsm <- ts(stim_USA$nonsmooth, start = c(1987,4), end = c(2022,11), freq=12)

#pdf("USA.PA&LN_latent_series.pdf",
#    width = 10, height = 7)

pdf("USA.pa.ln.smth.pdf",
    width = 10, height = 7)

ts.plot(usa.pa.ts, usa.lexi.ts,stim.sm,
        gpars=list(main="Economic Sentiment In Newspapers, USA Political Analysis & Lexis Nexis Series and Stimson Series",
                   xlab="Year", 
                   ylab="Average Positive Probability", 
                   lty=c(1:2),
                   lwd= 1,
                   col = c("red", "blue", "black"))
)

legend("bottomright", legend = c("USA Political Analysis: April, 1987 - Dec, 2014", 
                                 "USA LexisNexis: Jun, 1989 - Nov, 2022",
                                 "Stimson Values w/ Smooth: April, 1987 - Nov, 2022"),
       col = c("red", "blue", "black"), 
       lty = 1:2, cex = 0.5)

dev.off()

###################################################################
# correlation USA Series
###################################################################
USA_pa_agg$Date <- as.Date(paste(USA_pa_agg$year, USA_pa_agg$month, 1, sep = "/"))
USA_ln_agg$Date <- as.Date(paste(USA_ln_agg$year, USA_ln_agg$month, 1, sep = "/"))

USA_agg <- left_join(USA_pa_agg, USA_ln_agg, by = c("Date" = "Date"))
USA_agg <- left_join(USA_agg, stim_USA, by = c("Date" = "Date"))

USA_agg <- USA_agg %>% rename("article_freq.pa" = "article_freq.x")
USA_agg <- USA_agg %>% rename("article_freq.ln" = "article_freq.y")
write.csv(USA_agg, "USA_ttl_series.csv")

USA_corr <- USA_agg %>%
  select("weighted.avg.pa", "article_freq.pa", "weighted.avg.ln", "article_freq.ln", "smooth", "nonsmooth")
round(cor(USA_corr, use = "pairwise.complete.obs"), 2) 
#                   weighted.avg.pa article_freq.pa weighted.avg.ln article_freq.ln smooth nonsmooth
#weighted.avg.pa            1.00            0.08            0.78           -0.17   0.85      0.93
#article_freq.pa            0.08            1.00            0.27            0.77   0.20      0.17
#weighted.avg.ln            0.78            0.27            1.00           -0.06   0.90      0.95
#article_freq.ln           -0.17            0.77           -0.06            1.00  -0.16     -0.12
#smooth                     0.85            0.20            0.90           -0.16   1.00      0.92
#nonsmooth                  0.93            0.17            0.95           -0.12   0.92      1.00
