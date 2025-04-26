setwd("C:/Users/FPT/Documents/Thong ke/PTDL/Thuchanh")
library(readstata13)
gender <- read.dta13("C:/Users/FPT/Documents/Thong ke/PTDL/Familygender roles_2012.dta")

library(dplyr)
names(gender) <- c("var.name", "var.label")
View(gender)
variable.names(gender, var.name = NULL, lang=NA)
varlabel(gender, var.name = NULL, lang=NA)
View(gender)
library(tidyverse)
gender5<- gender %>% 
  filter(COUNTRY=='Taiwan (TW)'|COUNTRY=='Philippines (PH)'|COUNTRY=='Japan (JP)'|COUNTRY=='United States (US)'|COUNTRY=='Sweden (SE)')

# Kiểm tra tên cột
colnames(gender)

# Giả sử cột bạn cần là "COUNTRY"
# Đổi tên nếu cần
names(gender)[names(gender) == "tên_cột_không_hợp_lệ"] <- "COUNTRY"

# Sau đó, thực hiện filter
gender5 <- gender %>% 
  filter(COUNTRY %in% c('Taiwan (TW)', 'Philippines (PH)', 'Japan (JP)', 'United States (US)', 'Sweden (SE)'))

View(gender5)
#Tạo bảng tần số phân phối cho các biến phân tích: questionnaire: Q2a-d ~v10-v13. gmodel package
library(gmodels)  
ca_count<-CrossTable(gender5$v11, gender5$COUNTRY) # var truoc laf row; var sau la column
library(writexl)
write_xlsx(ca_count, "bangtanso.xlsx")
#Cell Contents
#|-------------------------|
#N 
#Chi-square contribution | 
#N / Row Total | so quan sat chia cho tong qs theo dong
#N / Col Total | so quan sat chia cho tong qs theo coi
# N / Table Total | 
library(openxlsx)
openxlsx::write.xlsx(ca_count, 'ca_count.xlsx')

ca <- read.xlsx('ca_count.xlsx',1, colNames = TRUE, rowNames = TRUE)

#Kiiem dinh khi binh phuong
chisq <- chisq.test(ca)
chisq

#
library(FactoMineR)
library(factoextra)
#library(xtable)
res.ca <- CA(ca, graph = FALSE)

#print(res.ca)
summary(res.ca)
# tong phương sai chisq (2781.204)/N


fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 80))

#giua lai dim1 va dim2 
#mưc dong gop cos2 , mua dong gop ctr:
#lua chọn tp chinh qua bieu do
fviz_screeplot(res.ca) +
  geom_hline(yintercept=25, linetype=2, color="red")

fviz_contrib(res.ca, choice = "row", axes = 1)
fviz_contrib(res.ca, choice = "row", axes = 2)
#do thi doi xung 
fviz_ca_biplot(res.ca, repel = T)
#do thi bat doi xung: so sanh
fviz_ca_biplot(res.ca, 
               map ="rowprincipal", arrow = c(TRUE, FALSE ),
               repel = TRUE)
fviz_ca_biplot(res.ca, 
               map ="colprincipal", arrow = c(FALSE,TRUE ),
               repel = TRUE)
#Biến bổ sung
library(gmodels) 

ca_sex<-CrossTable(gender5$v11, gender5$v200)

library(openxlsx)

openxlsx::write.xlsx(ca_sex, 'ca_sex.xlsx')

## Read data from Excel

ca_add <- read.xlsx("ca_sex.xlsx", 1, colNames=TRUE, rowNames=TRUE)

View(ca_add)



ca_sup <- merge(ca, ca_add, by="row.names") 
View(ca_sup)
ca_sup1 <- ca_sup[,-1]
View(ca_sup1)
rownames(ca_sup1) <- ca_sup[,1]
View(ca_sup1)
res.ca1 <- CA (ca_sup1, col.sup = 6:7,
               graph = FALSE)
summary(res.ca1)
plot(res.ca1)
fviz_ca_biplot(res.ca1, repel = TRUE,arrows = c(TRUE,TRUE))
fviz_ca_biplot(res.ca, repel = TRUE)
