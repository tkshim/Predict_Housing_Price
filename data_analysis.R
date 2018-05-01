raw_data <- read.csv('house_data.csv')

#欠損値削除
hdata = na.omit(raw_data)

#データ概観
summary(hdata)

#価格と各説明変数をプロット
plot(hdata$size,  hdata$price)
plot(hdata$year,  hdata$price)
plot(hdata$rooms, hdata$price)
plot(hdata$district, hdata$price)

#外れ値の確認
library(outliers)
grubbs.test(hdata$size)

#corにて相関関係確認
cor(hdata)

#サイズを使って単回帰分析
hdata.lm <- lm(price~size, data=hdata)

#P値が0.01以下でMultiple R-squaredが１に近いため
#一定の相関を確認
summary(hdata.lm)

# 単回帰の方程式を描写
plot(price~size, data=hdata)
abline(hdata.lm, lwd=1, col='red')

#
#以降は参考
#

#予測値から残差を計算
hdata.predict <- predict(hdata.lm) #予測値
hdata.residuals <- residuals(hdata.lm) #残差
data.frame(hdata, hdata.predict, hdata.residuals)

#単回帰診断図を表示
par(mfrow=c(2,2))
plot(hdata.lm)

#データ標準化
hdata.n <- scale(hdata)
hdata.n.df <- data.frame(hdata.n)
head(hdata.n.df)

#重回帰分析
hdataw.lm <- lm(price~size+district+year+rooms, data=hdata.n.df)
summary(hdataw.lm)

#独立変数同士の無相関を確認
#2.0より小さいと問題ないが、sizeとroomsに相関あることを確認
#sizeとroomsの併用に問題のある可能性を示唆
library(car)
vif(hdataw.lm)