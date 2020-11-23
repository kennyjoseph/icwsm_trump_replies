library(tidyverse)
library(bigrquery)
library(tidytext)
library(quanteda)
library(MASS)
theme_set(theme_minimal(20))
source("util.r")
################### Load in demographics and reply data ##################
demographics <- fread("panel_demographics.csv") 
demographics[, twProfileID := as.character(twProfileID)]
panel_twitter_info <- read_simple_user_info("../panel_twitter_info_9_4_19.tsv")
panel_twitter_info[, uid := as.character(uid)]
panel_twitter_info <- panel_twitter_info[!duplicated(uid)]
demographics <- demographics[twProfileID %in% panel_twitter_info[statuses_count > 10]$uid]

raw_trump_data <- read_raw_data("../zijian/zijian_realDonaldTrump_reply.csv.gz")
raw_biden_data <- read_raw_data("../zijian/zijian_JoeBiden_reply.csv.gz")
raw_trump_data$sentby <- "Trump"
raw_biden_data$sentby <- "Biden"
raw_data <- rbind(raw_trump_data, raw_biden_data)
raw_data[, length(unique(inreplyto_tid)), by= sentby]

raw_data[,min(date), by = sentby]
raw_data[,max(date), by = sentby]

raw_data[, nrow(.SD),by=sentby]




trump_reply <- get_reply_data_agg(raw_trump_data)
biden_reply <- get_reply_data_agg(raw_biden_data)
trump_reply$sentby <- "Trump"
biden_reply$sentby <- "Biden"
reply_data <- rbind(trump_reply, biden_reply)




############ RQ 1 ###################

############## A 
rg <- reply_data[party %in% c("Democrat","Republican") &
                !is.na(age) & !is.na(race_ethnicity) & !is.na(sex) & sex !="Unknown" &
                race_ethnicity %in% c("African-American","Asian","Caucasian","Hispanic") &
                statuses_count > 10 & age >= 18 & age < 80]

rg$race_ethnicity <- relevel(factor(rg$race_ethnicity), ref="Caucasian")

trump_mod <- gen_reg_results(rg[sentby=="Trump"])
biden_mod <- gen_reg_results(rg[sentby=="Biden"])
trump_mod$Politician <- "Trump"
biden_mod$Politician <- "Biden"
mod_res <- rbind(trump_mod[3:nrow(trump_mod)],
                 biden_mod[3:nrow(biden_mod)])

# Plot other coefficients
fig2 <- ggplot(mod_res, aes(name,Estimate, 
                           ymin=`2.5%`,ymax=`97.5%`,
                           color=Politician,
                           shape=Politician))+ 
                geom_point(position=position_dodge(.3),size=3.2) +
                geom_linerange(position=position_dodge(.3)) +
                coord_flip() + 
                geom_hline(yintercept=1,color='black') + 
                theme(legend.position = c(.85,.8),
                      legend.box.background = element_rect(colour = "black",fill="white")) + 
                scale_color_manual(values=c("blue","red"))  +xlab("") +
                ylab("Incident Rate Ratio")
fig2      
ggsave("figure2.pdf",fig2,h=4,w=9)

############## B
fig3 <- ggplot(reply_data, aes(x_reply,c_reply,color=sentby,linetype=sentby))+
  geom_line(size=1.1) + scale_x_log10("% of Users in Panel",
                              breaks=c(0.001,0.01,0.1,1),
                              labels=c("0.01%","0.1%","10%","100%")) + 
  scale_y_log10("% of Replies", labels=percent) + 
  geom_vline(xintercept = reply_data[c_reply > .5][1]$x_reply, 
             color='orange',linetype='dashed',size=1.2) + 
  theme(legend.position = c(.85,.2), 
        legend.box.background = element_rect(colour = "black",fill="white")) + 
  scale_color_manual("Politician",values=c("blue","red"))  +
  scale_linetype_manual("Politician", values=c("solid","dotted"))
ggsave("ccdf.pdf",fig3,h=4,w=6)

reply_data[, sum(N_reply > 0)/nrow(.SD), by=sentby]
reply_data[, sum(is_active_reply == "active"), by=sentby]

reply_data[, sum(is_active_reply == "active")/nrow(.SD[N_reply > 0]),by=sentby]

reply_data[, sum(is_active_reply == "active")/nrow(.SD),by=sentby]
reply_data[, sum(is_active_reply != "active"),by=sentby]
reply_data[, sum(is_active_reply != "active" & N_reply > 0), by=sentby]

write.csv(reply_data[N_reply > 0, .(uid,is_active_reply, sentby,party)], "./active_nonactive_user_categorization2.csv", row.names=F)
########## C ###############
sp_reply_biden <- get_sp(reply_data[sentby=="Biden"], 
                         "../zijian/zijian_JoeBiden_reply.csv.gz")
sp_reply_trump <- get_sp(reply_data[sentby=="Trump"], 
                         "../zijian/zijian_realDonaldTrump_reply.csv.gz")


y <- boot(sp_reply_trump$active, gini, 1000)
gini(sp_reply_trump$active)
quantile(y$t, probs=c(0.025, 0.975))

y <-boot(sp_reply_trump$non_active, gini, 1000)
gini(sp_reply_trump$non_active)
quantile(y$t, probs=c(0.025, 0.975))

y <- boot(sp_reply_biden$active, gini, 1000)
gini(sp_reply_biden$active)
quantile(y$t, probs=c(0.025, 0.975))

y <-boot(sp_reply_biden$non_active, gini, 1000)
gini(sp_reply_biden$non_active)
quantile(y$t, probs=c(0.025, 0.975))

mean(sp_reply_trump$active / sp_reply_trump$total)
mean(sp_reply_biden$active / sp_reply_biden$total)
nrow(sp_reply_trump[active > non_active])/nrow(sp_reply_trump)
nrow(sp_reply_biden[active > non_active])/nrow(sp_reply_biden)
