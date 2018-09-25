###練習問題
```{r}
x <- 1:10
x^2
sqrt(x)
sum(x)
mean(x)
```
###データの読み込み

mac の場合
```{r}
setwd("~/Desktop/gliomaSet")
```
windowsの場合
```{r}
setwd("C:/Users/koabe/Desktop/gliomaSet")
```
```{r}
gliomaSet_express <-read.csv("gliomaSet_express.csv")
gliomaSet_surv <-read.csv("gliomaSet_surv.csv")
library(dplyr)
gliomaSet <- left_join(gliomaSet_surv,gliomaSet_express,by="X") #データを結合
#gliomaSet <- merge(gliomaSet_surv,gliomaSet_express,by="X") #でもよい
gliomaSet[1:7,1:7] #1から7列目の1から7行目を表示
```
#いろいろなプロット
```{r}
library(ggplot2)
#いろいろなプロット
ggplot(gliomaSet,aes(x=g1,y=g2))+
  geom_point() #散布図
#plot(g2~g1,data=gliomaSet) #と同じ
ggplot(gliomaSet,aes(x=Age,fill=Gender))+
  geom_histogram() #ヒストグラム
ggplot(gliomaSet,aes(x=Age,colour=Gender))+
  geom_freqpoly() #ヒストグラムを折れ線で表示したもの
ggplot(gliomaSet,aes(x=Age,colour=Gender))+
  geom_freqpoly()+
  geom_rug() #データのある位置をバーコード状に表示したもの
ggplot(gliomaSet,aes(x=Gender,y=g3))+
  geom_boxplot() #箱ひげ図
#boxplot(g3~Gender,data = gliomaSet) #と同じ
ggplot(gliomaSet,aes(x=Gender,y=g3))+
  geom_violin() #バイオリンプロット
ggplot(gliomaSet,aes(x=g1,y=g2,colour=Gender))+
  geom_point() #散布図（色付き）

#help(package="ggplot2") #他に様々なグラフがある。ヘルプから探してみよう

ggplot(gliomaSet,aes(x=g1,y=g2,colour=Gender))+
  geom_point()+
  theme_bw() #白地のテーマ

ggplot(gliomaSet,aes(x=g1,y=g2,colour=Gender))+
  geom_point()+
  theme_bw(base_size = 20) #フォントサイズの変更

ggplot(gliomaSet,aes(x=g1,y=g2,colour=Gender))+
  geom_point()+
  scale_color_manual(values = c("royalblue","orange2")) #色を変更
```
###ここから主成分分析
```{r}
#df_expression <- gliomaSet[,6:105]
df_expression <-select(gliomaSet,g1:g100) #g1~g100の列だけ取り出す
pc_gli <-prcomp(df_expression,center = FALSE) #主成分分析
z1 <-pc_gli$x #主成分得点
w <- pc_gli$rotation #重み
x <- as.matrix(df_expression) #行列として認識させる
z2 <- x %*% w #行列の掛け算
z1[1:7,1:5] #主成分得点
z2[1:7,1:5]　#主成分得点
#結果が一致するはず

pc_gli <- prcomp(df_expression) #中心が0になるようにして主成分分析
pc_gli_df <-data.frame(pc_gli$x) #主成分得点を取り出す

ggplot(pc_gli_df,aes(x=PC1,y=PC2))+ #第一、第二主成分の散布図
  geom_point()
```
###ここからクラスター分析
```{r}
d_expression <- dist(df_expression) #距離行列の作成
dmat <- as.matrix(d_expression) #行列として認識させる
dmat[1:5,1:5]
#距離の計算の仕方を確認
sqrt(sum((df_expression[1,]-df_expression[2,])^2)) #サンプル1と2の距離
sqrt(sum((df_expression[1,]-df_expression[3,])^2)) #サンプル1と3の距離

hc_expression <- hclust(d_expression, method = "ward.D")
plot(hc_expression) #樹形図の描画

cluster <- cutree(hc_expression,k=2) #クラスターを2つに分ける
head(cluster)
cluster <- factor(cluster) #ファクター（因子）として認識
head(cluster)

ggplot(pc_gli_df,aes(x=PC1,y=PC2,colour=cluster))+
  geom_point() #散布図

ggplot(pc_gli_df,aes(x=PC1,y=PC2,colour=cluster))+
  geom_point(size=3) + #散布図2
  theme_bw()

gliomaSet <- mutate(gliomaSet,cluster=cluster)
gliomaSet <- mutate(gliomaSet,Status2=factor(Status))

ggplot(gliomaSet,aes(x=cluster,fill=Status2))+
  geom_bar() #棒グラフ

ggplot(gliomaSet,aes(x=cluster,fill=Status2))+
  geom_bar(position = "fill") #帯グラフ（積み上げ棒グラフを割合表示に）
```
###ここから生存時間分析
```{r}
library(survival) 
#生存関数の推定はsurvfit関数
sf_gli <-survfit(Surv(Time,Status)~cluster,data = gliomaSet)
Surv(gliomaSet$Time,gliomaSet$Status)

library(broom)
sf_gli_df <- tidy(sf_gli) #データフレームとして使用
head(sf_gli_df)
ggplot(sf_gli_df,aes(x=time,y=estimate,colour=strata))+
  geom_step() #階段状の折れ線プロット

#ログランク検定
#帰無仮説：「生存時間に差はない」で検定
sd_gli <-survdiff(Surv(Time,Status)~cluster,data = gliomaSet_surv)
print(sd_gli)
```

###t検定
```{r}
res1 <- t.test(g1~Status,data=gliomaSet)
print(res1)
res1$p.value #p値だけを取り出す

#t検定を繰り返す
m <- ncol(df_expression) #列の数を数える
res_pv <- numeric(m) #長さmの空の箱を用意 
for(i in 1:m){ #iが1からmまで動く
  res <- t.test(df_expression[,i]~gliomaSet$Status) #t検定
  res_pv[i] <- res$p.value #p値を取り出してi番目に代入
}

#which:条件を満たす値の場所を取り出す関数
which(res_pv<0.05) #p値が0.05を下回るのはどの遺伝子か
which(res_pv>=0.05)#p値が0.05以上なのはどの遺伝子か

res_pv_adj <- p.adjust(res_pv) #FDRの補正を行う
which(res_pv_adj<0.05)#p値が0.05を下回るのはどの遺伝子か

print(res1)
```
###以下複雑な集計
```{r}
group_by(gliomaSet,cluster) %>% 
  summarise(mean(g1))

group_by(gliomaSet,cluster) %>% 
  summarise_if(is.numeric,mean)
```
