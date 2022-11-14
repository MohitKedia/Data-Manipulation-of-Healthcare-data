library(readxl)

Drugs_data <- read_excel("Pre Launch Case Study Data.xlsx")
View(Drugs_data)

library(sqldf)
sqldf("SELECT SUM(Class) FROM Drugs_data")

Employees <- data.frame(Name=c("A","B","C","D","E","F"),Salary=c("1000","3948","2356","1000","2567","3948"))
View(Employees)

#Using SELF JOIN

sqldf("SELECT e1.Name as Name1,e1.Salary,e2.Name as Name2 FROM Employees e1 JOIN Employees e2 ON e1.Salary=e2.Salary
      AND e1.Name<>e2.Name")

library(dplyr)

Drugs_data <- rename(Drugs_data,Date=`Month Ending Date`)
Drugs_data <- mutate(Drugs_data,Year=substr(Date,1,4))
Drugs_data <- rename(Drugs_data,TRx=`TRx Count`)
Drugs_data <- rename(Drugs_data,NRx=`NRx Count`)
Drugs_data <- rename(Drugs_data,MBS=`TRx MBS Dollars`)
Drugs_data <- rename(Drugs_data,BG=`Branded/Generic`)

library(sqldf)
# Solution 1

sqldf("WITH CTE AS (SELECT Year,BG,SUM(TRx) as TRx FROM Drugs_data
      GROUP BY Year,BG)
      SELECT *,SUM(TRx) OVER (PARTITION BY Year) as Total_TRx,TRx*100/SUM(TRx) OVER (PARTITION BY Year) as sale
      FROM CTE")
      
#Solution 2

sqldf("WITH CTE AS (SELECT Class,Year,SUM(MBS) as MBS$ FROM Drugs_data
      GROUP BY Class,Year)
      SELECT *,SUM(MBS$) OVER (PARTITION BY Year) as Total_MBS$ , MBS$*100/SUM(MBS$) OVER (PARTITION BY Year) AS MarketShare
      FROM CTE")

#Solution 3

sqldf("SELECT Product,Year,SUM(MBS) as MBS$,LEAD(SUM(MBS)) OVER(PARTITION BY Product)-SUM(MBS) as Diff
      FROM Drugs_data
      GROUP BY Product,Year
      ORDER BY Product,Year")

#Solution 4

sqldf("WITH CTE AS (SELECT Class,Product,Year,SUM(TRx) as TRx FROM Drugs_data WHERE Class='GLP1'
      GROUP BY Class,Product,Year)
      SELECT *,SUM(TRx) OVER (PARTITION BY Year) as Total_TRx , TRx*100/SUM(TRx) OVER (PARTITION BY Year) AS MarketShare
      FROM CTE")

library(dplyr)

#Solution 1
BG <- Drugs_data %>% group_by(Year,BG) %>% summarise(TRx1=sum(TRx)) %>% mutate(TotalTrx=sum(TRx1),MarketShare=TRx1*100/TotalTrx)
#Solution 4
GLP1 <- Drugs_data %>% filter(Class=="GLP1") %>% group_by(Year,Class,Product) %>% summarise(TRx1=sum(TRx)) %>% mutate(TotalTrx=sum(TRx1),MarketShare=TRx1*100/TotalTrx)
View(GLP1)
ggplot(data = iris,aes(x = Species))+ geom_bar(col="blue",fill="orange")+
  ggtitle("Count of Species")+xlab("Species")+ylab("Count")

library(ggplot2)

ggplot(data = GLP1, aes(x=Product,y=MarketShare))+geom_bar(stat="identity",col="blue")
