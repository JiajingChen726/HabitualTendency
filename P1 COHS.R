library(ggplot2)
library(dplyr)
library(readxl)
library(data.table)
library(tidyverse)
library(outliers)
library(bruceR)
library(ppcor) 

# clear workspace
rm(list=ls(all=TRUE))


# Load data ---------------------------------------------------------------
MA_CRF<-read_excel('C:/Users/gaaizing/Desktop/整理CRF/T0MACRF20240130_filNan_counted.xlsx')
MA_CRF$group='MA'
MA_CRF <- MA_CRF[!MA_CRF$minii1==1,]
MA_demo <- list(ID = MA_CRF$ID,
                group = MA_CRF$group,
                age = MA_CRF$age,
                eduyear = MA_CRF$eduyear,
                BAI = MA_CRF$BAI,
                BDI = MA_CRF$BDI,
                AgeofFirstUse = MA_CRF$firstdruga,
                Abstinence = MA_CRF$treatkeep,
                Course = MA_CRF$course,
                Dosage_recent1year = MA_CRF$'1yeardose',
                Dosage_Total = MA_CRF$alldose,
                last1yearfre = MA_CRF$last1yearfre,
                OCDUS_FR = MA_CRF$OCDUS_FR,
                OCDUS_MH = MA_CRF$OCDUS_MH,
                OCDUS_CM = MA_CRF$OCDUS_CM,
                OCDUS = MA_CRF$OCDUS,
                COHS_Automaticity = MA_CRF$'cohs-Automaticity',
                COHS_Routine = MA_CRF$'cohs-Routine',
                BIS_MOV=MA_CRF$'BIS_MOV',
                BIS_COG=MA_CRF$'BIS_COG',
                BIS_NOP=MA_CRF$'BIS_NOP',
                BIS=MA_CRF$'BIS',
                CTQ=MA_CRF$'CTQ',
                CTQ_Abuse=MA_CRF$'Abuse',
                CTQ_Neglect=MA_CRF$'Neglect',
                OCI=MA_CRF$'OCI_R')
MA_demo <- as.data.frame(MA_demo)
# #CTQ outliers
MA_demo <- MA_demo[!MA_demo$ID==157,]
MA_demo <- MA_demo[!MA_demo$ID==291,]
MA_demo <- MA_demo[!MA_demo$ID==48,]
MA_demo <- MA_demo[!MA_demo$ID==306,]
MA_demo <- MA_demo[!MA_demo$ID==30,]
MA_demo <- MA_demo[!MA_demo$ID==17,]
#AGE/EDU match
MA_demo <- MA_demo[!MA_demo$ID==612,]
MA_demo <- MA_demo[!MA_demo$ID==161,]
MA_demo <- MA_demo[!MA_demo$ID==610,]
MA_demo <- MA_demo[!MA_demo$ID==43,]
MA_demo <- MA_demo[!MA_demo$ID==256,]
MA_demo <- MA_demo[!MA_demo$ID==128,]
MA_demo <- MA_demo[!MA_demo$ID==169,]
#EDU match
MA_demo <- MA_demo[!MA_demo$ID==829,]
MA_demo <- MA_demo[!MA_demo$ID==777,]



MA_demo_nao <- MA_demo[!is.na(MA_demo$'COHS_Automaticity'), ]

#HC CRF data
HC_CRF<-read_excel('C:/Users/gaaizing/Desktop/整理CRF/HC2023_filNan_counted.xlsx')
HC_demo <- list(ID = HC_CRF$id,
                group = HC_CRF$group,
                age = HC_CRF$age,
                eduyear = HC_CRF$eduyear,
                BAI = HC_CRF$BAI,
                BDI = HC_CRF$BDI,
                AgeofFirstUse = HC_CRF$firstdruga,
                Abstinence = HC_CRF$treatkeep,
                Course = HC_CRF$course,
                Dosage_recent1year = HC_CRF$'1yeardose',
                Dosage_Total = HC_CRF$alldose,
                last1yearfre = HC_CRF$last1yearfre,
                OCDUS_FR = HC_CRF$OCDUS_FR,
                OCDUS_MH = HC_CRF$OCDUS_MH,
                OCDUS_CM = HC_CRF$OCDUS_CM,
                OCDUS = HC_CRF$OCDUS,
                COHS_Automaticity = HC_CRF$'cohs-Automaticity',
                COHS_Routine = HC_CRF$'cohs-Routine',
                BIS_MOV=HC_CRF$'BIS_MOV',
                BIS_COG=HC_CRF$'BIS_COG',
                BIS_NOP=HC_CRF$'BIS_NOP',
                BIS=HC_CRF$'BIS',
                CTQ=HC_CRF$'CTQ',
                CTQ_Abuse=HC_CRF$'Abuse',
                CTQ_Neglect=HC_CRF$'Neglect',
                OCI=HC_CRF$'OCI_R')
HC_demo <- as.data.frame(HC_demo)


#EDU match
HC_demo <- HC_demo[!HC_demo$ID==537,]
HC_demo <- HC_demo[!HC_demo$ID==621,]
HC_demo <- HC_demo[!HC_demo$ID==515,]
HC_demo <- HC_demo[!HC_demo$ID==532,]
# # CTQ outlier
HC_demo <- HC_demo[!HC_demo$ID==524,]
HC_demo <- HC_demo[!HC_demo$ID==807,]
HC_demo <- HC_demo[!HC_demo$ID==818,]
HC_demo <- HC_demo[!HC_demo$ID==636,]
HC_demo <- HC_demo[!HC_demo$ID==671,]

HC_demo_nao <- HC_demo[!is.na(HC_demo$'COHS_Automaticity'), ]

#combine
demo = rbind(MA_demo_nao, HC_demo_nao)

#outlier
grubbs.test(demo$COHS_Automaticity)
grubbs.test(demo$COHS_Routine)
grubbs.test(demo$BIS)
grubbs.test(demo$OCI)
grubbs.test(demo$CTQ)
grubbs.test(demo$CTQ_Abuse)
grubbs.test(demo$CTQ_Neglect)
plot(demo$CTQ, demo$ID, main="Scatterplot Example", xlab="X轴标签", ylab="Y轴标签", pch=19, col="blue")
grubbs.test(demo$age)
grubbs.test(demo$eduyear)

#clobach's alpha
library(psych)
library(dplyr)
# CFA
MA_id<-unique(MA_demo_nao$ID)
MA_CFA<-MA_CRF[MA_CRF$ID %in% MA_id,]
MA_COHS<-MA_CFA[,c(455:481)]
Automaticity <- dplyr::select(MA_COHS, 3,5,8,9,11,16,19,21,23,25,26) #each number refers to the column
Routine <- dplyr::select(MA_COHS, 1,2,4,6,7,10,12,13,14,15,17,20,22,24,27)
MA_alphaA <- psych::alpha(Automaticity)
MA_alphaR <- psych::alpha(Routine)

HC_id<-unique(HC_demo_nao$ID)
HC_CFA<-HC_CRF[HC_CRF$id %in% HC_id,]
HC_COHS<-HC_CFA[,c(318:344)]
Automaticity <- dplyr::select(HC_COHS, 3,5,8,9,11,16,19,21,23,25,26) #each number refers to the column
Routine <- dplyr::select(HC_COHS, 1,2,4,6,7,10,12,13,14,15,17,20,22,24,27)
HC_alphaA <- psych::alpha(Automaticity)
HC_alphaR <- psych::alpha(Routine)


# comparison ----------------------------------------------------------------

#demo_comparison
model <- t.test(age ~ group, data = demo)
model
Describe(MA_demo_nao$age)
Describe(HC_demo_nao$age)

model <- t.test(eduyear ~ group, data = demo)
model
Describe(MA_demo_nao$eduyear)
Describe(HC_demo_nao$eduyear)

model <- t.test(BDI ~ group, data = demo)
model
Describe(MA_demo_nao$BDI)
Describe(HC_demo_nao$BDI)

model <- t.test(BAI ~ group, data = demo)
model
Describe(MA_demo_nao$BAI)
Describe(HC_demo_nao$BAI)



#OCI_R
model <- aov(OCI ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$OCI)
Describe(HC_demo_nao$OCI)

#BIS
model <- aov(BIS ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$BIS)
Describe(HC_demo_nao$BIS)

#BIS_COG
model <- aov(BIS_COG ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$BIS_COG)
Describe(HC_demo_nao$BIS_COG)

#BIS_MOV
model <- aov(BIS_MOV ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$BIS_MOV)
Describe(HC_demo_nao$BIS_MOV)

#BIS_NOP
model <- aov(BIS_NOP ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$BIS_NOP)
Describe(HC_demo_nao$BIS_NOP)

#CTQ_ABUSE
model <- aov(CTQ ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$CTQ)
Describe(HC_demo_nao$CTQ)

#CTQ_ABUSE
model <- aov(CTQ_Abuse ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$CTQ_Abuse)
Describe(HC_demo_nao$CTQ_Abuse)

#CTQ_Neglect
model <- aov(CTQ_Neglect ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$CTQ_Neglect)
Describe(HC_demo_nao$CTQ_Neglect)

p_values <- c(0.05, 0.00001, 0.000001,0.002,0.00001,0.003,0.997,0.474,0.355,0.000424)
p.adjust(p_values, method = "hommel")


# COHS comparisons --------------------------------------------------------

#COHS_Automaticity
model <- aov(COHS_Automaticity ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$COHS_Automaticity)
Describe(HC_demo_nao$COHS_Automaticity)

library(ggrain)
library('ggsignif ')
COHSA<-ggplot(demo, aes(group, COHS_Automaticity, fill = group, color = group)) +
  geom_rain(alpha = .9,rain.side = 'f1x1',
            boxplot.args = list(color =c("#220879", "#B0783D"),
                                outlier.shape = NA)) +
  scale_y_continuous(limits = c(0, 63))+
  theme_classic() +
  scale_fill_manual(values=c("#5861AC", "#d0a583")) +
  scale_color_manual(values=c("#5861AC", "#d0a583")) +
  guides(color = 'none')+
  labs(fill = "Group",x='Group') +
  ggsignif::geom_signif(
    comparisons = list(c("HC", "MA")),
    map_signif_level = TRUE,
    textsize = 8,
    color='black')+
  theme(axis.text.x = element_text(size = 14),  # 更改x轴标签字体大小
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),  # 更改x轴标签字体大小
        axis.title.y = element_text(size = 16))
COHSA

#COHS_Routine
model <- aov(COHS_Routine ~ group+age+BDI+BAI, data = demo)
summary(model)
Describe(MA_demo_nao$COHS_Routine)
Describe(HC_demo_nao$COHS_Routine)

COHSR<-ggplot(demo, aes(group, COHS_Routine, fill = group, color = group)) +
  geom_rain(alpha = .9,rain.side = 'f1x1',
            boxplot.args = list(color =c("#220879", "#B0783D"),
                                outlier.shape = NA)) +
  scale_y_continuous(limits = c(0, 88))+
  theme_classic() +
  scale_fill_manual(values=c("#5861AC", "#d0a583")) +
  scale_color_manual(values=c("#5861AC", "#d0a583")) +
  guides(color = 'none')+
  labs(fill = "Group",x='Group') +
  ggsignif::geom_signif(
    comparisons = list(c("HC", "MA")),
    map_signif_level = TRUE,
    textsize = 5,
    color='black')+
  theme(axis.text.x = element_text(size = 14),  # 更改x轴标签字体大小
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),  # 更改x轴标签字体大小
        axis.title.y = element_text(size = 16))
COHSR

#combined pic
library('gridExtra')
COHS_combined<-grid.arrange(COHSA, COHSR, nrow = 1)
ggsave("C:/Users/gaaizing/Desktop/P1/COHSR.png", COHSR, width = 5, height = 5, units = "in")



# MA CHARACTERISTICS ------------------------------------------------------
Describe(MA_demo$OCDUS)
Describe(MA_demo$OCDUS_FR)
Describe(MA_demo$OCDUS_MH)
Describe(MA_demo$OCDUS_CM)
Describe(MA_demo$AgeofFirstUse)
Describe(MA_demo$Course)
Describe(MA_demo$Abstinence)
Describe(MA_demo$Dosage_recent1year)
Describe(MA_demo$Dosage_Total)




# 相关  -----------------------------------------------
library('Hmisc')
#correlation
#drug-related
cohsmat<-MA_demo_nao[,c('COHS_Automaticity','COHS_Routine',
                        'AgeofFirstUse','Abstinence','Dosage_recent1year','Dosage_Total','Course',
                        'OCDUS','OCDUS_FR','OCDUS_MH','OCDUS_MH')] %>% drop_na()
cohscormat<-rcorr(as.matrix(cohsmat), type = c('pearson'))
cor_matrix <- cor(cohsmat)
p_matrix <- rcorr(as.matrix(cohsmat))$P

library('corrplot')
library('ggcorrplot')
cormat.r<-cohscormat$r[c(3:11),c(2,1)]
cormat.p<-cohscormat$P[c(3:11),c(2,1)]
corp_drug_related<-ggcorrplot(cormat.r, 
                              ggtheme = ggplot2::theme_void(base_size = 15), #主题修改
                              colors = c("#1d3d39", "white", "#76553c"), #自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
                              lab = T,lab_size = 4.5,    #相关系数文本字体大小
                              tl.cex = 12,             #坐标轴字体大小
                              p.mat = cormat.p,         #添加显著性信息
                              sig.level = 0.05) + theme(
                                legend.position = "bottom", # Or "left", "top", "right", or a vector of two numbers for custom positioning
                                legend.box = "horizontal" ,# Or "vertical"
                                # 要调整图例文字和标题的大小，可以添加以下代码：
                                # legend.text = element_text(size = 10), # 调整图例文本大小
                                # legend.title = element_text(size = 12) # 调整图例标题大小
                                legend.key.width = unit(2, "cm"))
corp_drug_related
ggsave("C:/Users/gaaizing/Desktop/P1/corp_drug_related.png", corp_drug_related, width = 8, height = 5, units = "in")

#MA correlation
cohsmat<-MA_demo_nao[,c('COHS_Automaticity','COHS_Routine',
                 'BIS','BIS_MOV','BIS_COG','BIS_NOP',
                 'OCI','CTQ','CTQ_Neglect','CTQ_Abuse','age')] %>% drop_na()
cohscormat<-rcorr(as.matrix(cohsmat), type = c('pearson'))

library('corrplot')
library('ggcorrplot')
cormat.r<-cohscormat$r[c(1:2),c(3:10)]
cormat.p<-cohscormat$P[c(1:2),c(3:10)]
corp_MA<-ggcorrplot(cormat.r, 
                     ggtheme = ggplot2::theme_void(base_size = 15), #主题修改
                     colors = c("#334d7d", "white", "#960e39"), #自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
                     lab = T,lab_size = 4.5,    #相关系数文本字体大小
                     tl.cex = 14,             #坐标轴字体大小
                     p.mat = cormat.p,         #添加显著性信息
                     sig.level = 0.05,        #显著性水平
                     insig = 'blank') + theme(
                      legend.position = "bottom", # Or "left", "top", "right", or a vector of two numbers for custom positioning
                      legend.box = "horizontal" ,# Or "vertical"
                      legend.key.width = unit(2, "cm"))
corp_MA
ggsave("C:/Users/gaaizing/Desktop/P1/corp_MA.png", corp_MA, width = 8, height = 5, units = "in")

#partial correlation
library(psych)
partial_cor_matrix <- psych::partial.r(cohscormat$r, y="age")
cp_p<-corr.p(partial_cor_matrix,129,adjust="hommel")$p

library('corrplot')
library('ggcorrplot')
cormat.r<-partial_cor_matrix[c(1:2),c(10,9,8,7,6,5,4,3)]
cormat.p<-cp_p[c(1:2),c(10,9,8,7,6,5,4,3)]
corp_MA_partial<-ggcorrplot(cormat.r, 
                            ggtheme = ggplot2::theme_void(base_size = 15), #主题修改
                            colors = c("#6965a1", "white", "#c35602"), #自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
                            lab = T,lab_size = 4.5,    #相关系数文本字体大小
                            tl.cex = 12,             #坐标轴字体大小
                            p.mat = cormat.p,         #添加显著性信息
                            sig.level = 0.05,        #显著性水平
                            insig = 'blank') + theme(
                              legend.position = "bottom", # Or "left", "top", "right", or a vector of two numbers for custom positioning
                              legend.box = "horizontal" ,# Or "vertical"
                              # 要调整图例文字和标题的大小，可以添加以下代码：
                              # legend.text = element_text(size = 10), # 调整图例文本大小
                              # legend.title = element_text(size = 12) # 调整图例标题大小
                              legend.key.width = unit(2, "cm"))
corp_MA_partial
ggsave("C:/Users/gaaizing/Desktop/P1/corp_MA_partial.png", corp_MA_partial, width = 8, height = 5, units = "in")


library('ggpubr')
cormapMAd_BISM_COHSA<-ggplot(cohsmat, aes(x = BIS_MOV, y = COHS_Automaticity)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "BIS_MOV", y = "COHS_Automaticity")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 14),  # 更改x轴标签字体大小
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),  # 更改x轴标签字体大小
        axis.title.y = element_text(size = 16))
cormapMAd_BISM_COHSA
ggsave("C:/Users/gaaizing/Desktop/P1/cormapMAd_BISM_COHSA.png", cormapMAd_BISM_COHSA, width = 6, height = 5, units = "in")


cormapMAd_Neg_COHSR<-ggplot(cohsmat, aes(x = CTQ_Neglect, y =COHS_Routine )) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "CTQ_Neglect", y = "COHS_Routine")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 14),  # 更改x轴标签字体大小
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),  # 更改x轴标签字体大小
        axis.title.y = element_text(size = 16))
cormapMAd_Neg_COHSR
ggsave("C:/Users/gaaizing/Desktop/P1/cormapMAd_Neg_COHSR.png", cormapMAd_Neg_COHSR, width = 6, height = 5, units = "in")


#HC correlation
cohsmat<-HC_demo_nao[,c('COHS_Automaticity','COHS_Routine',
                        'BIS','BIS_MOV','BIS_COG','BIS_NOP',
                        'OCI','CTQ','CTQ_Neglect','CTQ_Abuse','age')] %>% drop_na()
cohscormat<-rcorr(as.matrix(cohsmat), type = c('pearson'))

library('corrplot')
library('ggcorrplot')
cormat.r<-cohscormat$r[c(1:2),c(10,9,8,7,6,5,4,3)]
cormat.p<-cohscormat$P[c(1:2),c(10,9,8,7,6,5,4,3)]
corp_HC<-ggcorrplot(cormat.r, 
                    ggtheme = ggplot2::theme_void(base_size = 15), #主题修改
                    colors = c("#3D6C8F", "white", "#C65218"), #自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
                    lab = T,lab_size = 4.5,    #相关系数文本字体大小
                    tl.cex = 14,             #坐标轴字体大小
                    tl.srt= 45,
                    p.mat = cormat.p,         #添加显著性信息
                    sig.level = 0.05,        #显著性水平
                    insig = 'blank')  + theme(
                      legend.position = "bottom", # Or "left", "top", "right", or a vector of two numbers for custom positioning
                      legend.box = "horizontal" ,
                      legend.key.width = unit(2, "cm"))
corp_HC
ggsave("C:/Users/gaaizing/Desktop/P1/corp_HC.png", corp_HC, width = 8, height = 5, units = "in")

# partial correlation
library(psych)
partial_cor_matrix <- psych::partial.r(cohscormat$r, y="age")
cp_p<-corr.p(partial_cor_matrix,126,adjust="hommel")$p

library('corrplot')
library('ggcorrplot')
cormat.r<-partial_cor_matrix[c(1:2),c(10,9,8,7,6,5,4,3)]
cormat.p<-cp_p[c(1:2),c(10,9,8,7,6,5,4,3)]
corp_HC_partial<-ggcorrplot(cormat.r, 
                    ggtheme = ggplot2::theme_void(base_size = 15), #主题修改
                    colors = c("#6965a1", "white", "#c35602"), #自定义颜色，看自己喜欢，或是参考好看的文献Figure用法。
                    lab = T,lab_size = 4.5,    #相关系数文本字体大小
                    tl.cex = 14,             #坐标轴字体大小
                    tl.srt=45,
                    p.mat = cormat.p,         #添加显著性信息
                    sig.level = 0.05,        #显著性水平
                    insig = 'blank') + theme(
                      legend.position = "bottom", # Or "left", "top", "right", or a vector of two numbers for custom positioning
                      legend.box = "horizontal" ,# Or "vertical"
                      # 要调整图例文字和标题的大小，可以添加以下代码：
                      # legend.text = element_text(size = 10), # 调整图例文本大小
                      # legend.title = element_text(size = 12) # 调整图例标题大小
                      legend.key.width = unit(2, "cm"))
corp_HC_partial
ggsave("C:/Users/gaaizing/Desktop/P1/corp_HC_partial.png", corp_HC_partial, width = 8, height = 5, units = "in")

cormapHCd_BIS_COHSA<-ggplot(cohsmat, aes(x = BIS_MOV, y = COHS_Automaticity)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "BIS_MOV", y = "COHS_Automaticity")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 14),  # 更改x轴标签字体大小
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),  # 更改x轴标签字体大小
        axis.title.y = element_text(size = 16))
cormapHCd_BIS_COHSA
ggsave("C:/Users/gaaizing/Desktop/P1/cormapHCd_BIS_COHSA.png", cormapHCd_BIS_COHSA, width = 6, height = 5, units = "in")

cormapHCd_BISM_COHSR<-ggplot(cohsmat, aes(x = BIS_MOV, y = COHS_Routine)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "BIS_MOV", y = "COHS_Routine")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 14),  # 更改x轴标签字体大小
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),  # 更改x轴标签字体大小
        axis.title.y = element_text(size = 16))
cormapHCd_BISM_COHSR
ggsave("C:/Users/gaaizing/Desktop/P1/cormapHCd_BISM_COHSR.png", cormapHCd_BISM_COHSR, width = 6, height = 5, units = "in")

# step regression ---------------------------------------------------------

library('MASS')
#COHS_AU:MA
MA_lmdata<-MA_demo_nao[,c('age','eduyear','BIS_COG','BIS_MOV','BIS_NOP',
                      'OCI','CTQ_Neglect','CTQ_Abuse','BAI','BDI',
                      'Course','Abstinence','Dosage_recent1year',
                      'OCDUS_FR','OCDUS_MH','OCDUS_MH',
                      'COHS_Automaticity'
                      )]%>%drop_na()
# 创建线性回归模型
lm_model <- lm(COHS_Automaticity ~ .,
               data = MA_lmdata)
# 进行逐步回归分析
step_model <- stepAIC(lm_model, direction = "both")
# 查看逐步回归得到的最终模型
summary(step_model)
library(car)
vif(step_model)

#COHS_AU:HC
HC_lmdata<-HC_demo_nao[,c('age','eduyear','BIS_COG','BIS_MOV','BIS_NOP',
                      'OCI','CTQ_Neglect','CTQ_Abuse','BAI','BDI',
                      'COHS_Automaticity'
)]%>%drop_na()
# 创建线性回归模型
lm_model <- lm(COHS_Automaticity ~ .,
               data = HC_lmdata)
# 进行逐步回归分析
step_model <- stepAIC(lm_model, direction = "both")
# 查看逐步回归得到的最终模型
summary(step_model)
library(car)
vif(step_model)

#COHS_AU:MA
MA_lmdata<-MA_demo_nao[,c('age','eduyear','BIS_COG','BIS_MOV','BIS_NOP',
                      'OCI','CTQ_Neglect','CTQ_Abuse','BAI','BDI',
                      'Course','Abstinence','Dosage_recent1year',
                      'OCDUS_FR','OCDUS_MH','OCDUS_MH',
                      'COHS_Routine'
)]%>%drop_na()
# 创建线性回归模型
lm_model <- lm(COHS_Routine ~ .,
               data = MA_lmdata)
# 进行逐步回归分析
step_model <- stepAIC(lm_model, direction = "both")
# 查看逐步回归得到的最终模型
summary(step_model)
library(car)
vif(step_model)

#COHS_AU:HC
HC_lmdata<-HC_demo_nao[,c('age','eduyear','BIS_COG','BIS_MOV','BIS_NOP',
                      'OCI','CTQ_Neglect','CTQ_Abuse','BAI','BDI',
                      'COHS_Routine'
)]%>%drop_na()
# 创建线性回归模型
lm_model <- lm(COHS_Routine ~ .,
               data = HC_lmdata)
library(car)
# 进行逐步回归分析
step_model <- stepAIC(lm_model, direction = "both")
# 查看逐步回归得到的最终模型
vif(step_model)
summary(step_model)


# network analysis --------------------------------------------------------

library("tidyverse")
library('networktools')
library("bootnet")
library("qgraph")


### Get the partial correlation matrix
MAnetworkdata<-MA_demo_nao[,c('BIS_COG','BIS_MOV','BIS_NOP',
                            'OCI','CTQ_Neglect','CTQ_Abuse',
                            'COHS_Automaticity','COHS_Routine'
)]%>%drop_na()
MAnetworkdata_zeroorder <- cor(MAnetworkdata)
### Get the partial correlation network (figure 1)
pdf(file= 'C:/Users/gaaizing/Desktop/P1/MAfigure1-1.pdf', width = 10, height = 7)
network <- qgraph::qgraph(MAnetworkdata_zeroorder, layout="spring",
                          color = c("lightblue", "lightsalmon","antiquewhite"), graph = "pcor",vsize =4, label.cex =2)
dev.off()

### Implement the glasso (figure 2)
pdf(file= 'C:/Users/gaaizing/Desktop/P1/MAfigure1-2.pdf', width = 10, height = 7)
aen_glasso <- qgraph::EBICglasso(MAnetworkdata_zeroorder, n=nrow(MAnetworkdata), gamma = 0.4)  
lasso <- qgraph::qgraph(aen_glasso, layout = "spring",
                        color = c("lightblue", "lightsalmon","antiquewhite"), vsize=4, label.cex =2)
dev.off()

sum(aen_glasso!=0) #非0路径数量，总数量=节点*(第一个节点可连接数量+最后一个节点可连接数量)/2

### centrality (Table s3)
centrality <- centrality_auto(aen_glasso, weighted = TRUE, signed = TRUE)
nc <- centrality$node.centrality #node centrality

### plot Centrality measures (figure S1)
pdf(file= 'C:/Users/gaaizing/Desktop/P1/MAfigure2.pdf', width = 10, height = 7)
qgraph::centralityPlot(lasso, include = c("Closeness", "Betweenness", "Strength"), scale = "raw") # plot all centrality indices
dev.off()

# stabilility
MAnetwork <- estimateNetwork(MAnetworkdata, tuning=0.1, default="EBICglasso", threshold = FALSE, corMethod = "cor", refit=TRUE)
boot <- bootnet(network, ncores=8, nboots=10, type="case",statistics = c("strength","closeness","betweenness"))
corStability(boot)
pdf(file= 'C:/Users/gaaizing/Desktop/P1/MAfigure3.pdf', width = 10, height = 7)
plot(boot,c("strength","closeness","betweenness"))
dev.off()

### Get the partial correlation matrix
HCnetworkdata<-HC_demo_nao[,c('BIS_COG','BIS_MOV','BIS_NOP',
                              'OCI','CTQ_Neglect','CTQ_Abuse',
                              'COHS_Automaticity','COHS_Routine'
)]%>%drop_na()
HCnetworkdata_zeroorder <- cor(HCnetworkdata)
### Get the partial correlation network (figure 1)
pdf(file= 'C:/Users/gaaizing/Desktop/P1/HCfigure1-1.pdf', width = 10, height = 7)
network <- qgraph::qgraph(HCnetworkdata_zeroorder, layout="spring",
                          color = c("lightblue", "lightsalmon","antiquewhite"), graph = "pcor",vsize =4, label.cex =2)
dev.off()

### Implement the glasso (figure 2)
pdf(file= 'C:/Users/gaaizing/Desktop/P1/HCfigure1-2.pdf', width = 10, height = 7)
aen_glasso <- qgraph::EBICglasso(HCnetworkdata_zeroorder, n=nrow(HCnetworkdata), gamma = 0.4)  
lasso <- qgraph::qgraph(aen_glasso, layout = "spring",
                        color = c("lightblue", "lightsalmon","antiquewhite"), vsize=4, label.cex =2)
dev.off()

sum(aen_glasso!=0) #非0路径数量，总数量=节点*(第一个节点可连接数量+最后一个节点可连接数量)/2

### centrality (Table s3)
centrality <- centrality_auto(aen_glasso, weighted = TRUE, signed = TRUE)
nc <- centrality$node.centrality #node centrality

### plot Centrality measures (figure S1)
pdf(file= 'C:/Users/gaaizing/Desktop/P1/HCfigure2.pdf', width = 10, height = 7)
qgraph::centralityPlot(lasso, include = c("Closeness", "Betweenness", "Strength"), scale = "raw") # plot all centrality indices
dev.off()

# stabilility
HCnetwork <- estimateNetwork(HCnetworkdata, tuning=0.1, default="EBICglasso", threshold = FALSE, corMethod = "cor", refit=TRUE)
boot <- bootnet(network, ncores=8, nboots=10, type="case",statistics = c("strength","closeness","betweenness"))
corStability(boot)
pdf(file= 'C:/Users/gaaizing/Desktop/P1/HCfigure3.pdf', width = 10, height = 7)
plot(boot,c("strength","closeness","betweenness"))
dev.off()
]

【结果不好】
# Network Comparison Test -------------------------------------------------
library('NetworkComparisonTest')
set.seed(123) # random permutation seed
nct_results <- NCT(HCnetwork, MAnetwork,
                   it = 5000,
                   progressbar = T,
                   test.centrality = T,
                   test.edges = T)

nct_results