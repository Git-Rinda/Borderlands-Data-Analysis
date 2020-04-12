



## THIS FILE CONTINUES FROM WHERE THE IPYTHON 'CLEANING AND ORGANIZING IPYNB' LEFT OFF. IF YOU HAVEN'T RUN THE CODE
## THERE, PLEASE RETURN TO THE IPYTHON FILE AND FOLLOW THE SEQUENCE.

setwd('C:/Users/sidew/Desktop/gearbox')
notjson = read.csv('wona.csv', header = T)
isjson = read.csv('wojson.csv', header = T)

library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

str(isjson)
str(notjson)


notjson$PLATFORM = as.factor(as.character(notjson$PLATFORM))

crits = ddply(isjson, c("DATETIME","session_guid","map","class"), summarise, tot_cr = sum(criticals))
fires = ddply(isjson, c("DATETIME","session_guid","map","class"), summarise, tot_fired = sum(fired))
relos = ddply(isjson, c("DATETIME","session_guid","map","class"), summarise, tot_relos = sum(reloads))
trigs = ddply(isjson, c("DATETIME","session_guid","map","class"), summarise, tot_tri = sum(trigger_pulls))

figs = as.data.frame(cbind(crits,'tot_fired' = fires[,5], 'tot_relos' = relos[,5], 'tot_tri' = trigs[,5]))

op = figs[which(figs$class == 'operative'),]
bm = figs[which(figs$class == 'beastmaster'),]
gn = figs[which(figs$class == 'gunner'),]
si = figs[which(figs$class == 'siren'),]

### CRITS, RELOADS, TRIGGER PULLS, AND FIRED - CHECKING FOR DEPENDENCIES - NOTHING SUBSTANTIAL FOUND

ggplot(data = op, mapping = aes(x = tot_cr, y = tot_tri)) +
  geom_point()

ggplot(data = op, mapping = aes(x = tot_cr, y = tot_relos)) +
  geom_point()

ggplot(data = op, mapping = aes(x = tot_cr, y = tot_fired)) +
  geom_point()

ggplot(data = op, mapping = aes(x = tot_tri, y = tot_relos)) +
  geom_point()

rm(op,bm,gn,si,crits,fires,relos,trigs,figs)



#### DAMAGE, MAP, CHARACTER, HARDWARE DEPENDENCIES


exp = ddply(isjson, c("DATETIME","session_guid","map","class"), summarise, tot_dam = sum(damage))
exp_aoe = ddply(isjson, c("DATETIME","session_guid","map","class"), summarise, tot_aoe = sum(aoe_damage))
exp_crit = ddply(isjson, c("DATETIME","session_guid","map","class"), summarise, tot_crit = sum(crit_damage))

finexp = cbind(exp,'tot_aoe' = exp_aoe$tot_aoe, 'tot_crit' = exp_crit$tot_crit)

boxplot(tot_aoe ~ class, finexp) #for all 3 types of damage to identify outliers

which(finexp$tot_dam > 1.2e+11)
which(finexp$tot_aoe > 1.2e+11) 
which(finexp$tot_crit > 1.2e+11) 

outliers1 = finexp[c(9649,9015,9494,17515,18021,19785,33322,64995,74575,78442,86657,96946,102803,103440,115570,121569,127609,131978,133498),]
finexp1 = finexp[-c(9649,9015,9494,17515,18021,19785,33322,64995,74575,78442,86657,96946,102803,103440,115570,121569,127609,131978,133498),]


which(finexp$tot_dam > 2.2e+13) #96946
which(finexp$tot_aoe > 2.2e+13) #9015  19785  64995 102803 131978
which(finexp$tot_crit > 4e+11) #9649 18021 86657

outliers2 = finexp[c(9015,9649,18021,19785,64995,86657,96946,102803,131978),]
finexp = finexp[-c(9015,9649,18021,19785,64995,86657,96946,102803,131978),]


finexp1$fin_dmg = rowSums(finexp1[,c(5,6,7)])
finexp$fin_dmg = rowSums(finexp[,c(5,6,7)])

classes = finexp[which(finexp$class %in% c('beastmaster','gunner','operative','siren')),]
classes1 = finexp1[which(finexp1$class %in% c('beastmaster','gunner','operative','siren')),]

ft = as.data.frame(table(classes1$class))
ft = ft[which(ft$Var1 %in% c('beastmaster','gunner','operative','siren')),]
ft = ft %>% mutate(share=Freq/sum(Freq)*100.0) %>% arrange(Freq)
columns = c('class','frequency','share')
colnames(ft) = columns


### VISUALIZATIONS 

ggplot(ft, aes("", share, fill = class)) +
  geom_bar(width = 1, size = 1, color = "black", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.5),hjust = 1) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Class Spread") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#330000"))




ggplot(classes1, aes(x = factor(class), y = tot_dam/10**9, fill = class)) + 
  geom_bar(stat = "identity") + 
  labs(y = "Dmg in Billions", x = "Hunter Class") +
  theme(panel.background = element_rect(fill = 'grey30', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Damage by Classes Across Maps')


#TOTAL SUMMED DAMAGE PLOTS - GUNNER LACKS FAR BEHIND

ggplot(classes1, aes(x = factor(class), y = fin_dmg/10**9, fill = class)) + 
  geom_bar(stat = "identity") + 
  labs(y = "Total Dmg in Billions", x = "Hunter Class") +
  theme(panel.background = element_rect(fill = 'grey30', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Total Damage by Classes Across Maps')


ggplot(classes1, aes(x = factor(class), y = tot_aoe/10**9, fill = class)) + 
  geom_bar(stat = "identity") + 
  labs(y = "Dmg in Billions", x = "Hunter Class") +
  theme(panel.background = element_rect(fill = 'grey30', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('AOE Damage by Classes Across Maps')

rm(finexp, outliers2, classes)



gun_class = classes1[which(classes1$class == 'gunner'),]
bm_class = classes1[which(classes1$class == 'beastmaster'),]
op_class = classes1[which(classes1$class == 'operative'),]
si_class = classes1[which(classes1$class == 'siren'),]



#AVERAGE DAMAGE PLOTS - CONFIRM GUNNER'S LACK OF IMPACT

y = as.data.frame(rbind(colMeans(gun_class[,c(5,6,7,8)]),colMeans(bm_class[,c(5,6,7,8)]),colMeans(op_class[,c(5,6,7,8)]),
          colMeans(si_class[,c(5,6,7,8)])))
y$class = c('gunner','beastmaster','operative','siren')

ggplot(y, aes(x = class, y = fin_dmg/10**6, fill = class)) + 
  geom_bar(stat = "identity",colour = 'black') + 
  labs(y = "Dmg in millions", x = "Hunter Class") +
  theme(panel.background = element_rect(fill = 'grey30', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Avg Damage by Classes Across Maps')




#PLATFORM COMPARISON ON DAMAGE AND ENGAGEMENT

exp2 = ddply(isjson, c("DATETIME","session_guid","map","class","hardware"), summarise, tot_dam = sum(damage))
exp_aoe2 = ddply(isjson, c("DATETIME","session_guid","map","class","hardware"), summarise, tot_aoe = sum(aoe_damage))
exp_crit2 = ddply(isjson, c("DATETIME","session_guid","map","class","hardware"), summarise, tot_crit = sum(crit_damage))

finexp2 = cbind(exp2,'tot_aoe' = exp_aoe2$tot_aoe, 'tot_crit' = exp_crit2$tot_crit)
finexp2 = finexp2[-c(9649,9015,9494,17515,18021,19785,33322,64995,74575,78442,86657,96946,102803,103440,115570,121569,127609,131978,133498),]

finexp2$fin_dmg = rowSums(finexp2[,c(6,7,8)])

z = count(finexp2, "hardware")
xc = z %>% mutate(share=freq/sum(freq)*100.0)


#PIE CHART OF HARDWARE SHARES

ggplot(xc, aes("", share, fill = hardware)) +
  geom_bar(width = 1, size = 1, color = "black", stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = paste0(round(share), "%")), 
            position = position_stack(vjust = 0.5),hjust = 1) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Platform Share") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#330000"))



hware = finexp2[which(finexp2$hardware %in% c('pc','ps4','xboxone')),]


#HARDWARE VS DAMAGE

ggplot(hware, aes(x = hardware, y = fin_dmg/10**9, fill = hardware)) + 
  geom_bar(stat = "identity") + 
  labs(y = "Dmg in billions", x = "Platform") +
  theme(panel.background = element_rect(fill = 'grey30', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  scale_fill_manual(values = c("#999933","#66cc66","#ff66cc"))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Avg Damage Across Platforms')


w = count(hware, c("hardware","class"))
w = w[which(w$class %in% c('beastmaster','gunner','operative','siren')),]

w_pc = w[which(w$hardware == 'pc'),]
w_ps4 = w[which(w$hardware == 'ps4'),]
w_xbox = w[which(w$hardware == 'xboxone'),]

w_pc = w_pc %>% mutate(share=freq/sum(freq)*100.0) %>% arrange(freq)
w_ps4 = w_ps4 %>% mutate(share=freq/sum(freq)*100.0) %>% arrange(freq)
w_xbox = w_xbox %>% mutate(share=freq/sum(freq)*100.0) %>% arrange(freq)
w = as.data.frame(rbind(w_pc,w_ps4,w_xbox))

ggplot(w, aes(factor(hardware), share, fill = class)) +
  geom_text(aes(label = paste0(round(share), "%")), position = position_dodge(width = 1), vjust = -0.75, hjust = 0.5)+
  geom_bar(stat="identity", position = "dodge", colour = 'black')+
  labs(y = "Percent picked", x = "Hardware") +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Class Popularity across platforms')


rm(w,w_pc,w_ps4,w_xbox)





notjson$TIMESTAMP = as.POSIXct(notjson$TIMESTAMP,format="%Y-%m-%d %H:%M:%S")

tail(names(sort(table(notjson$PLAYERID))), 5)


#5 MOST FREQUENT PLAYERS: "00003665a4bca4f87bb61cf79baf2434" "00004a51ae31a846df8da156500cb521" "000014a03d92bd691a1ccbfb701365d9" "000047d1606e5afeda493ae303a38907" "000028b34698cc248d76557bdeacf475"

head(names(sort(table(notjson$PLAYERID))), 5)

#'''5 LEAST FREQUENT PLAYERS: "0000091ae5267f5780d6945c82800c1f" "00001bf609974c245aad2f2af70466a7" "000039aa67fd7763a096c8f8afbc2032" "0000407bfefde54d98b44b3732f27395" "00000f813fe135f57db3ee1823fbed35"'''


play1 = notjson[which(notjson$PLAYERID == "00003665a4bca4f87bb61cf79baf2434"),]
play2 = notjson[which(notjson$PLAYERID == "00004a51ae31a846df8da156500cb521"),]
play3 = notjson[which(notjson$PLAYERID == "000014a03d92bd691a1ccbfb701365d9"),]
play4 = notjson[which(notjson$PLAYERID == "000047d1606e5afeda493ae303a38907"),]
play5 = notjson[which(notjson$PLAYERID == "000028b34698cc248d76557bdeacf475"),]

ggplot(play1, aes(x = factor(MAP), y = PLAYEDTIME/(3600*(10**3)), fill = MAP)) + 
  geom_bar(stat = "identity") + 
  labs(y = "Time in 1000 hrs", x = "Map") +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10),angle = 90))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Most popular maps for frequent players')

ggplot(play2, aes(x = factor(MAP), y = PLAYEDTIME/(3600*(10**3)), fill = MAP)) + 
  geom_bar(stat = "identity") + 
  labs(y = "Time in 1000 hrs", x = "Map") +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10),angle = 90))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Most popular maps for frequent players')



least1 = notjson[which(notjson$PLAYERID == "0000091ae5267f5780d6945c82800c1f"),]
least2 = notjson[which(notjson$PLAYERID == "00001bf609974c245aad2f2af70466a7"),]
least3 = notjson[which(notjson$PLAYERID == "000039aa67fd7763a096c8f8afbc2032"),]
least4 = notjson[which(notjson$PLAYERID == "0000407bfefde54d98b44b3732f27395"),]
least5 = notjson[which(notjson$PLAYERID == "00000f813fe135f57db3ee1823fbed35"),]


#PLAYER ENGAGEMENT

newd = notjson[,c(4,5,7)]
mapd = ddply(newd, "MAP", summarise, tim = sum(PLAYEDTIME))
mapd = mapd[order(-mapd$tim),]
lapd = mapd

ggplot(lapd[c(1:5),], aes(x = factor(MAP), y = tim/(3600*(10**6)), fill = MAP)) + 
  geom_bar(stat = "identity") + 
  labs(y = "Time in hrs", x = "Map") +
  theme(panel.background = element_rect(fill = 'grey30', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Most engaging maps')

#top 5 engaging maps are 
#circleofslaughter, Loader, wetlands, provinggrounds, Trashtown_P

mapd = newd[which(newd$MAP %in% c('circleofslaughter', 'Loader', 'wetlands', 'provinggrounds', 'Trashtown_P')),]
mapd = aggregate( PLAYEDTIME ~ MAP, mapd, mean )

ggplot(mapd, aes(x = factor(MAP), y = PLAYEDTIME/3600, fill = MAP)) + 
  geom_bar(stat = "identity") + 
  labs(y = "Avg Time in hrs", x = "Map") +
  theme(panel.background = element_rect(fill = 'grey30', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Avg Time on most engaging maps')


mapd = newd[which(newd$MAP %in% c('circleofslaughter', 'Loader', 'wetlands', 'provinggrounds', 'Trashtown_P')),]
mapd = mapd[which(mapd$CHARACTERID %in% c('beastmaster','siren','operative','gunner')),]

boxplot(PLAYEDTIME~CHARACTERID, mapd)
which(mapd$PLAYEDTIME > 2.5e+6)

outliers_map = mapd[which(mapd$PLAYEDTIME > 2.5e+6),]
mapd = mapd[-which(mapd$PLAYEDTIME > 2.5e+6),]


cos_map = mapd[which(mapd$MAP == 'circleofslaughter'),c(1,3)]
lod_map = mapd[which(mapd$MAP == 'Loader'),c(1,3)]
pg_map = mapd[which(mapd$MAP == 'provinggrounds'),c(1,3)]
wet_map = mapd[which(mapd$MAP == 'wetlands'),c(1,3)]
trash_map = mapd[which(mapd$MAP == 'Trashtown_P'),c(1,3)]

cos_map = aggregate( PLAYEDTIME ~ CHARACTERID, cos_map, mean)
lod_map = aggregate( PLAYEDTIME ~ CHARACTERID, lod_map, mean)
pg_map = aggregate( PLAYEDTIME ~ CHARACTERID, pg_map, mean)
wet_map = aggregate( PLAYEDTIME ~ CHARACTERID, wet_map, mean)
trash_map = aggregate( PLAYEDTIME ~ CHARACTERID, trash_map, mean)

cos_map$MAP = 'circleofslaughter'
lod_map$MAP = 'Loader'
pg_map$MAP = 'provinggrounds'
wet_map$MAP = 'wetlands'
trash_map$MAP = 'Trashtown_P'

fin_map = as.data.frame(rbind(cos_map,lod_map,pg_map,wet_map,trash_map))

ggplot(fin_map, aes(factor(CHARACTERID), PLAYEDTIME/3600, fill = MAP)) +
  geom_bar(stat="identity", position = "dodge", colour = 'black')+
  labs(y = "Avg Time", x = "Class") +
  theme(panel.background = element_rect(fill = 'grey', colour = 'black'),
        axis.text = element_text(face = "bold", colour = "darkslateblue", size = (10)))+
  theme(plot.title = element_text(face = "bold", size = (15)),
        axis.title = element_text(face="italic", size = (10)))+
  ggtitle('Avg Time on most engaging maps by each class')


rm(cos_map,lod_map,pg_map,wet_map,trash_map,fin_map,newd,mapd,lapd,play1,play2,play3,play4,play5)
