library(ggplot2)
data_tim[data_tim$time_out2==1,][1:10,]
data_tim = data_tim[order(data_tim$GameID,data_tim$time),]
names(data_new)
names(data_tim)

test=data.frame(GameID =  data_new$GameID,time = data_new$time,s1 = data_new$s1,s2 = data_new$s2,time_out1 = 0, time_out2 = 0)
test2 = data.frame(GameID = data_tim$GameID,time = data_tim$time,s1 = NA, s2 = NA, time_out1 = data_tim$time_out1,time_out2 = data_tim$time_out2)
whole_test = rbind(test,test2)
whole_test = whole_test[order(whole_test$GameID,whole_test$time),]
for (i in 1:(dim(whole_test)[1])){
  if((whole_test$time_out1[i]==1)){
    whole_test$s1[i]=whole_test$s1[i-1]
    whole_test$s2[i] = whole_test$s2[i-1]
  }
  if(whole_test$time_out2[i]==1){
    whole_test$s1[i]=whole_test$s1[i-1]
    whole_test$s2[i] = whole_test$s2[i-1]
  }
}
time_out_whole_score_dif = matrix(NA, ncol = 3,nrow = dim(sub_whole_test)[1])
sub_whole_test = whole_test[c(which(whole_test$time_out1==1),which(whole_test$time_out2==1)),]
sub_whole_test = sub_whole_test[order(sub_whole_test$GameID,sub_whole_test$time),]
for(i in 1:(dim(sub_whole_test)[1])){
  if(sub_whole_test$time_out1[i]==1){
    time_out_whole_score_dif[i,] = c(sub_whole_test$time_out1[i],sub_whole_test$time_out2[i],as.numeric(as.character(sub_whole_test$s1[i]))-as.numeric(as.character(sub_whole_test$s2[i])))}
  
  if(sub_whole_test$time_out2[i]==1){
    time_out_whole_score_dif[i,] = c(sub_whole_test$time_out1[i],sub_whole_test$time_out2[i],
                                     as.numeric(as.character(sub_whole_test$s2[i]))-as.numeric(as.character(sub_whole_test$s1[i])))}
}
hist(time_out_whole_score_dif[,3])
sub_whole_test = data.frame(sub_whole_test,dif = time_out_whole_score_dif[,3])


par(mfrow = c(3,2))
plot(formal_test$dif,formal_test$score_before,ylim = c(-15,15))
title("3 minuites before the time out")
plot(formal_test$dif,formal_test$score_after,ylim = c(-15,15))
title("3 minuites after the time out")
plot(formal_2min_test$dif,formal_2min_test$score_before,ylim = c(-15,15))
title("2 minuites before the time out")
plot(formal_2min_test$dif,formal_2min_test$score_after,ylim = c(-15,15))
title("2 minuites after the time out")
plot(formal_1min_test$dif,formal_1min_test$score_before,ylim = c(-15,15))
title("1 minuites before the time out")
plot(formal_1min_test$dif,formal_1min_test$score_after,ylim = c(-15,15))
title("1 minuites after the time out")


change_1_before = c()
change_1_after = c()
change_2_before = c()
change_2_after = c()
for (i in 1:(dim(whole_test)[1])){
  if((whole_test$time_out1[i]==1)){
    tic = whole_test$time[i]# 
    time_3min_after = tic+180
    time_3min_before = tic-180
    new_data_frame = whole_test[whole_test$GameID==as.character(whole_test[i,]$GameID),]
    quzuihou = new_data_frame[new_data_frame$time<time_3min_after,]
    qudiyi = new_data_frame[new_data_frame$time>time_3min_before,]
    zuihou = max(as.numeric(as.character(quzuihou$s1)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s1)),na.rm = TRUE)
    change_1_before[i]=as.numeric(as.character(whole_test$s1[i]))-diyi
    change_1_after[i] = zuihou-as.numeric(as.character(whole_test$s1[i]))
    zuihou = max(as.numeric(as.character(quzuihou$s2)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s2)),na.rm = TRUE)
    change_2_before[i]=as.numeric(as.character(whole_test$s2[i]))-diyi
    change_2_after[i] = zuihou-as.numeric(as.character(whole_test$s2[i]))
  }
  if(whole_test$time_out2[i]==1){
    tic = whole_test$time[i]# 
    time_3min_after = tic+180
    time_3min_before = tic-180
    new_data_frame = whole_test[whole_test$GameID==as.character(whole_test[i,]$GameID),]
    quzuihou = new_data_frame[new_data_frame$time<time_3min_after,]
    qudiyi = new_data_frame[new_data_frame$time>time_3min_before,]
    zuihou = max(as.numeric(as.character(quzuihou$s1)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s1)),na.rm = TRUE)
    change_1_before[i]=as.numeric(as.character(whole_test$s1[i]))-diyi
    change_1_after[i] = zuihou-as.numeric(as.character(whole_test$s1[i]))
    zuihou = max(as.numeric(as.character(quzuihou$s2)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s2)),na.rm = TRUE)
    change_2_before[i]=as.numeric(as.character(whole_test$s2[i]))-diyi
    change_2_after[i] = zuihou-as.numeric(as.character(whole_test$s2[i]))
  }
}

hehe_test = data.frame(whole_test,change_1_before=c(change_1_before,NA),
                        change_1_after=c(change_1_after,NA),
                       change_2_before=c(change_2_before,NA),change_2_after=c(change_2_after,NA))
formal_test = data.frame(whole_test,score_before = c(change_1_before,NA)-c(change_2_before,NA),score_after = c(change_1_after,NA)-c(change_2_after,NA))

formal_test1 = formal_test[which(formal_test$time_out1==1),]
formal_test2 = formal_test[which(formal_test$time_out2==1),]
formal_test2$score_after = -formal_test2$score_after
formal_test = rbind(formal_test1,formal_test2)
formal_test = formal_test[order(formal_test$GameID,formal_test$time),]
formal_test = data.frame(formal_test,dif = sub_whole_test$dif)
plot(formal_test$dif,formal_test$score_before,ylim = c(-15,15))
title("3 minuites before the time out")
plot(formal_test$dif,formal_test$score_after,ylim = c(-15,15))
title("3 minuites after the time out")
plot(formal_test$dif,formal_test$score_before,ylim = c(-15,15),xlim = c(-10,10),col="blue")
par(new = TRUE)
plot(formal_test$dif,formal_test$score_after,ylim = c(-15,15),xlim = c(-10,10),col="red")
plot(formal_test$dif,formal_test$score_after-formal_test$score_before,ylim = c(-20,20))


df <- data.frame(x = formal_test$dif,y=formal_test$score_before)
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point() + labs(x="absolute difference",y="relative difference",title = "3 minuites before the time out")

df <- data.frame(x = formal_test$dif,y=formal_test$score_after)
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()+ labs(x="absolute difference",y="relative difference",title = "3 minuites after the time out")

df <- data.frame(x = formal_test$dif,y=(formal_test$score_after-formal_test$score_before))
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()+ labs(x="absolute difference",y="improvement",title = "3 minuites after the time out")
plot(formal_test$dif,formal_test$score_before,ylim = c(-15,15),xlim = c(25,50))
title("3 minuites before the time out")
plot(formal_test$dif,formal_test$score_after,ylim = c(-15,15),xlim = c(25,50))
title("3 minuites after the time out")
plot(formal_test$dif,formal_test$score_after-formal_test$score_before,ylim = c(-15,15),xlim = c(25,50))
title("3 minuites improvement")

for (i in 1:(dim(whole_test)[1])){
  if((whole_test$time_out1[i]==1)){
    tic = whole_test$time[i]#
    time_3min_after = tic+120
    time_3min_before = tic-120
    new_data_frame = whole_test[whole_test$GameID==as.character(whole_test[i,]$GameID),]
    quzuihou = new_data_frame[new_data_frame$time<time_3min_after,]
    qudiyi = new_data_frame[new_data_frame$time>time_3min_before,]
    zuihou = max(as.numeric(as.character(quzuihou$s1)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s1)),na.rm = TRUE)
    change_1_before[i]=as.numeric(as.character(whole_test$s1[i]))-diyi
    change_1_after[i] = zuihou-as.numeric(as.character(whole_test$s1[i]))
    zuihou = max(as.numeric(as.character(quzuihou$s2)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s2)),na.rm = TRUE)
    change_2_before[i]=as.numeric(as.character(whole_test$s2[i]))-diyi
    change_2_after[i] = zuihou-as.numeric(as.character(whole_test$s2[i]))
  }
  if(whole_test$time_out2[i]==1){
    tic = whole_test$time[i]# 
    time_3min_after = tic+120
    time_3min_before = tic-120
    new_data_frame = whole_test[whole_test$GameID==as.character(whole_test[i,]$GameID),]
    quzuihou = new_data_frame[new_data_frame$time<time_3min_after,]
    qudiyi = new_data_frame[new_data_frame$time>time_3min_before,]
    zuihou = max(as.numeric(as.character(quzuihou$s1)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s1)),na.rm = TRUE)
    change_1_before[i]=as.numeric(as.character(whole_test$s1[i]))-diyi
    change_1_after[i] = zuihou-as.numeric(as.character(whole_test$s1[i]))
    zuihou = max(as.numeric(as.character(quzuihou$s2)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s2)),na.rm = TRUE)
    change_2_before[i]=as.numeric(as.character(whole_test$s2[i]))-diyi
    change_2_after[i] = zuihou-as.numeric(as.character(whole_test$s2[i]))
  }
}
hehe_2min_test = data.frame(whole_test,change_1_before=c(change_1_before,NA),
                       change_1_after=c(change_1_after,NA),
                       change_2_before=c(change_2_before,NA),change_2_after=c(change_2_after,NA))
formal_2min_test = data.frame(whole_test,score_before = c(change_1_before,NA)-c(change_2_before,NA),score_after = c(change_1_after,NA)-c(change_2_after,NA))
formal_2min_test1 = formal_2min_test[which(formal_2min_test$time_out1==1),]
formal_2min_test2 = formal_2min_test[which(formal_2min_test$time_out2==1),]
formal_2min_test2$score_after = -formal_2min_test2$score_after
formal_2min_test = rbind(formal_2min_test1,formal_2min_test2)
formal_2min_test = formal_2min_test[order(formal_2min_test$GameID,formal_2min_test$time),]
formal_2min_test = data.frame(formal_2min_test,dif = sub_whole_test$dif)
plot(formal_2min_test$dif,formal_2min_test$score_before,ylim = c(-15,15))
title("2 minuites before the time out")
plot(formal_2min_test$dif,formal_2min_test$score_after,ylim = c(-15,15))
title("2 minuites after the time out")
df <- data.frame(x = formal_2min_test$dif,y=formal_2min_test$score_before)
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point() + labs(x="absolute difference",y="relative difference",title = "2 minuites before the time out")

df <- data.frame(x = formal_2min_test$dif,y=formal_2min_test$score_after)
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()+ labs(x="absolute difference",y="relative difference",title = "2 minuites after the time out")
df <- data.frame(x = formal_2min_test$dif,y=(formal_2min_test$score_after-formal_2min_test$score_before))
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()+ labs(x="absolute difference",y="improvement",title = "2 minuites after the time out")


for (i in 1:(dim(whole_test)[1])){
  if((whole_test$time_out1[i]==1)){
    tic = whole_test$time[i]
    time_3min_after = tic+60
    time_3min_before = tic-60
    new_data_frame = whole_test[whole_test$GameID==as.character(whole_test[i,]$GameID),]
    quzuihou = new_data_frame[new_data_frame$time<time_3min_after,]
    qudiyi = new_data_frame[new_data_frame$time>time_3min_before,]
    zuihou = max(as.numeric(as.character(quzuihou$s1)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s1)),na.rm = TRUE)
    change_1_before[i]=as.numeric(as.character(whole_test$s1[i]))-diyi
    change_1_after[i] = zuihou-as.numeric(as.character(whole_test$s1[i]))
    zuihou = max(as.numeric(as.character(quzuihou$s2)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s2)),na.rm = TRUE)
    change_2_before[i]=as.numeric(as.character(whole_test$s2[i]))-diyi
    change_2_after[i] = zuihou-as.numeric(as.character(whole_test$s2[i]))
  }
  if(whole_test$time_out2[i]==1){
    tic = whole_test$time[i]
    time_3min_after = tic+60
    time_3min_before = tic-60
    new_data_frame = whole_test[whole_test$GameID==as.character(whole_test[i,]$GameID),]
    quzuihou = new_data_frame[new_data_frame$time<time_3min_after,]
    qudiyi = new_data_frame[new_data_frame$time>time_3min_before,]
    zuihou = max(as.numeric(as.character(quzuihou$s1)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s1)),na.rm = TRUE)
    change_1_before[i]=as.numeric(as.character(whole_test$s1[i]))-diyi
    change_1_after[i] = zuihou-as.numeric(as.character(whole_test$s1[i]))
    zuihou = max(as.numeric(as.character(quzuihou$s2)),na.rm = TRUE)
    diyi = min(as.numeric(as.character(qudiyi$s2)),na.rm = TRUE)
    change_2_before[i]=as.numeric(as.character(whole_test$s2[i]))-diyi
    change_2_after[i] = zuihou-as.numeric(as.character(whole_test$s2[i]))
  }
}
hehe_1min_test = data.frame(whole_test,change_1_before=c(change_1_before,NA),
                            change_1_after=c(change_1_after,NA),
                            change_2_before=c(change_2_before,NA),change_2_after=c(change_2_after,NA))
formal_1min_test = data.frame(whole_test,score_before = c(change_1_before,NA)-c(change_2_before,NA),score_after = c(change_1_after,NA)-c(change_2_after,NA))
formal_1min_test1 = formal_1min_test[which(formal_1min_test$time_out1==1),]
formal_1min_test2 = formal_1min_test[which(formal_1min_test$time_out2==1),]
formal_1min_test2$score_after = -formal_1min_test2$score_after
formal_1min_test = rbind(formal_1min_test1,formal_1min_test2)
formal_1min_test = formal_1min_test[order(formal_1min_test$GameID,formal_1min_test$time),]
formal_1min_test = data.frame(formal_1min_test,dif = sub_whole_test$dif)
plot(formal_1min_test$dif,formal_1min_test$score_before,ylim = c(-15,15))
title("1 minuites before the time out")
plot(formal_1min_test$dif,formal_1min_test$score_after,ylim = c(-15,15))
title("1 minuites after the time out")
df <- data.frame(x = formal_1min_test$dif,y=formal_1min_test$score_before)
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point() + labs(x="absolute difference",y="relative difference",title = "1 minuites before the time out")

df <- data.frame(x = formal_1min_test$dif,y=formal_1min_test$score_after)
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()+ labs(x="absolute difference",y="relative difference",title = "1 minuites after the time out")

df <- data.frame(x = formal_1min_test$dif,y=(formal_1min_test$score_after-formal_1min_test$score_before))
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()+ labs(x="absolute difference",y="improvement",title = "1 minuites after the time out")



# from one game
BOSNYK_formal_test = formal_test[formal_test$GameID=="20111225BOSNYK",]
plot(BOSNYK_formal_test$dif,BOSNYK_formal_test$score_before,ylim = c(-15,15),xlab = "BOSNYK absolute difference",ylab = "relative difference")
text(BOSNYK_formal_test$dif,BOSNYK_formal_test$score_before,c(1:16))
par(new = TRUE)
plot(BOSNYK_formal_test$dif,BOSNYK_formal_test$score_after,ylim = c(-15,15),col="red",xlab = "BOSNYK absolute difference",ylab = "relative difference")
text(BOSNYK_formal_test$dif,BOSNYK_formal_test$score_after,c(1:16),col = "red")
title("BOSNYK")



# score_before vs score_after
df <- data.frame(x = formal_test$score_before,y=formal_test$score_after)
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()+ labs(x="absolute difference",y="improvement",title = "3 minuites after the time out")

df <- data.frame(x = formal_1min_test$score_before,y=formal_1min_test$score_after)
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point()+ labs(x="absolute difference",y="improvement",title = "1 minuites after the time out")

# 






# 