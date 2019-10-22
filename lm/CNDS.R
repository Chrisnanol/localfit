# 首先加载数据包并将scenario变量变为因子变量
politeness = readr::read_csv('https://raw.githubusercontent.com/usplos/self-programming/master/politeness_data.csv')
politeness$scenario = factor(politeness$scenario)
library(lmerTest)

#建立模型，用summary()函数查看结果， 这里需要注意：
#如果设置随机效应，模型可能无法收敛或者自由度溢出，
#这个时候需要调整或者取消随机效应：

#固定效应就是固定因子的效应，是考察的自变量（scenario和attitude）
#而随机效应就是分组的因素（subject和gender）
fit1 = lmer(frequency ~ scenario * attitude + (1+attitude|subject) + (1+attitude|gender), data = politeness)
#(1+attitude|subject)中1是随机截距
#attitude|subject表示自变量attitude会受随机变量subject的影响
summary(fit1)
#这里是把scenario的第一个水平作为基线，其他水平和他比较的结果，
#看出第3、4类场景显著高于第一类场景

#这里的固定效应不是主效应和交互作用，
#要查看主效应和交互作用需要用anova()函数:
anova(fit1)

#考虑全模型
fitAll = lmer(frequency ~ scenario * attitude + (1+attitude * scenario|subject) + 
                (1+attitude * scenario|gender), data = politeness)
#错误：number of random effects (=84) for term (1 + attitude * scenario | subject); 
#the random-effects parameters and the residual variance 
#(or scale parameter) are probably unidentifiable

#移除交互作用后，模型无法收敛
fitAll2 = lmer(frequency ~ scenario * attitude + (1+attitude + scenario|subject) + 
                 (1+attitude + scenario|gender), data = politeness)
#下一步，要选择移除哪一个随机因子，以及移除subject还是gender上的随机因子，于是有四种模型：
fitAll3_1 = lmer(frequency ~ scenario * attitude + (1+attitude|subject) + (1+attitude + scenario|gender), data = politeness);
fitAll3_2 = lmer(frequency ~ scenario * attitude + (1+scenario|subject) + (1+attitude + scenario|gender), data = politeness);
fitAll3_3 = lmer(frequency ~ scenario * attitude + (1+attitude+ scenario|subject) + (1+attitude|gender), data = politeness);
fitAll3_4 = lmer(frequency ~ scenario * attitude + (1+attitude + scenario|subject) + (1+scenario|gender), data = politeness)

#同时要建立一个零模型（只有随机截距
fitZero = lmer(frequency ~ scenario * attitude + (1|subject) + (1|gender), data = politeness)
#依次比较零模型和以上的四个模型
#结果四个模型和零模型都不显著，这种情况下，选取p值最小的模型，即fitAll3_3，
#虽然包含随机斜率的模型和不包含随机斜率的模型无明显差异，但是仍然要考虑随机斜率，
#因为实验中的被试只是个样本，不能保证样本之外的个体随机斜率也没有影响。
anova(fitZero, fitAll3_1)
anova(fitZero, fitAll3_2)
anova(fitZero, fitAll3_3)
anova(fitZero, fitAll3_4)

#优先考虑随机斜率
#优先考虑全模型
#舍弃或削减模型的标准是该模型不能收敛，或者自由度溢出
#优先削减交互作用的随机效应
#当遇到需削减同水平位置的随机因子时（比如两个随机因子需要舍弃一个时），应考虑所有情况，并将其和零模型作比较；优先保留与零模型有显著差异的模型；当比较都不显著时，优先考虑保留p较小的模型。

