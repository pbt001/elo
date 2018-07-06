# Initialize packages
library(reshape2)
library(scales)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(PlayerRatings)

# Initialize common theme
ztheme<-function(){
  theme_bw()+
    theme(plot.background=element_rect(fill="#F0F0F0", color="#F0F0F0"))+
    theme(plot.title=element_text(color="#252525"))+
    theme(plot.subtitle=element_text(color="#737373"))+
    theme(plot.caption=element_text(color="#D9D9D9"))+
    theme(strip.background=element_rect(fill="#D9D9D9", color="#D9D9D9"))+
    theme(strip.text=element_text(color="#525252", face="bold"))+
    theme(panel.background=element_rect(fill="#F0F0F0", color="#F0F0F0"))+
    theme(panel.border=element_rect(fill=NA, color="#D9D9D9"))+
    theme(panel.grid=element_line(color="#D9D9D9"))+
    theme(axis.ticks=element_line(color="#BDBDBD"))+
    theme(axis.text=element_text(color="#737373"))}

# Initialize data frame
df<-data.frame(date=character(),
              start=character(),
              name.v=character(),
              pts.v=numeric(),
              name.h=character(),
              pts.h=numeric(),
              season=numeric())

# Load all files
list<-list.files(pattern=".csv")
for(n in 1:length(list)){
  df<-rbind(df, data.frame(read_csv(list[n]),
                 season=as.numeric(substr(list[n],6,9)))) }
rm(list=c("list", "n"))

# Convert date format to numeric
df$date<-as.numeric(as.POSIXct(df$date, format="%a, %b %d, %Y"))+as.numeric(as.POSIXct(df$start, format="%H:%M:%S"))
df$date<-floor((df$date)/100000)

# Mark the result
df$result<-ifelse(df$pts.v>df$pts.h, 1, 0)

# Calculate our final standings
# df$date<-as.numeric(df$date)
result<-glicko(subset(df, season==2017)[,c(1,3,5,8)],
               init=c(1500, 350), cval=18.13, history=T)

# Order factors
result$ratings$Player<-factor(result$ratings$Player,
                        levels=result$ratings$Player[order(result$ratings$Rating)])

# Grab History
history<-melt(result$history)
names(history)<-c("name","round","variable","value")
history<-spread(history, variable, value)

# Plot animated historical result
# for(n in 1:max(history$round)){
#   subhistory<-subset(history, round==n)
#   subhistory$name<-factor(subhistory$name,
#                       levels=subset(history, round==max(history$round))$name[order(subset(history, round==max(history$round))$Rating)])
#   print(
#     ggplot(subhistory, aes(name, Rating))+
#       geom_pointrange(aes(ymin=Rating-1.96*Deviation, ymax=Rating+1.96*Deviation, fill=Rating), shape=21, size=1, color="gray50")+
#       scale_fill_gradientn(limits=c(1200, 1800), colors=brewer.pal(11, "Spectral"), oob=squish)+
#       scale_y_continuous(limits=c(600, 2400), breaks=seq(0, 3000, 100), minor_breaks=NULL)+
#       labs(x="", y="Glicko Rating",
#            title="NBA Team Ratings",
#            subtitle="for the 2017-2018 season",
#            caption="created by /u/zonination")+
#       guides(fill=F)+
#       coord_flip()+
#       ztheme())
#   ggsave(paste("games_", formatC(n ,width=5, flag="0"), ".png", sep=""), height=7, width=10, dpi=120, type="cairo-png")}
# rm(list=c("subhistory", "n"))

# History for all teams
ggplot(history, aes(group=name, x=round, y=Rating))+
  geom_hline(yintercept=1500, alpha=.1)+
  geom_ribbon(aes(ymin=Rating-1.96*Deviation, ymax=Rating+1.96*Deviation), alpha=.1)+
  geom_hline(data=summarise(group_by(subset(history, Lag<=5), name), rating=mean(Rating)), aes(yintercept=rating), color="red")+
  geom_path()+
  scale_x_continuous(breaks=seq(0 , 300, 20))+
  scale_y_continuous(breaks=seq(100,3100,200))+
  labs(x="", y="",
       title="Glicko Rating of NBA Teams",
       subtitle="for the season ending in 2013",
       caption="created by /u/zonination")+
  facet_wrap(~name, ncol=6)+
  ztheme()
ggsave("history2017.png", height=9, width=16, dpi=120, type="cairo-png")

# Print result to terminal
print(result)