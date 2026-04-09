# ODEV3 : KESIFSEL VERI ANALIZ SURECI
# Suheda KARA
# Ogrenci No:20252052028

# 1) VERI URETEREK KESIFSEL VERI ANALIZ SURECI

# Rastgele veri olusturmak icin kutuphaneyi yukleyip aktif ediyoruz.
install.packages("MASS")
install.packages("zoo")
install.packages("caTools")
install.packages("corrplot")
install.packages("ggplot2")

library(MASS)
library(zoo)
library(caTools)
library(corrplot)
library(ggplot2)

#veri seti olusturma, tekrarlanabilirlik icin seed kullaniyoruz.
set.seed(123)

#n gözlem sayısı, x ler bagimsiz degiskenker, y bagimli degisken
n <- 120
x1 <- rnorm(n, mean = 10, sd = 2) 
x2 <- rnorm(n, mean = 5, sd = 1) 
x3 <- rnorm(n, mean = 3, sd = 0.5) 
x4 <- rnorm(n, mean = 7, sd = 1.5) 
x5 <- rnorm(n, mean = 15, sd = 3) 
y <- 5 + 2*x1 - 1.5*x2 + 0.5*x3 + 1.8*x4 - 1.2*x5 + rnorm(n, mean = 0, sd = 2) #rnorm; rassal hata

#Olusuturulan veri setini veri cercevesine donusturme
data <- data.frame(y, x1, x2, x3, x4, x5)

#Olusuturulan veri setini gosterme, head verinin ilk 6 satirini gosterir
head(data)

# x1 degiskenini faktorel degiskene donusturme
x1_factor <- as.factor(x1)

# x2 degiskenini sayisal degiskene donusturme
x2_numeric <- as.numeric(x2)

#Degisken turlerini kontrol etme
str(x1_factor)
str(x2_numeric)

#Ornek veri seti olusturma
set.seed(123)
n <- 120
x1_ev <- rnorm(n, mean = 10, sd = 2)
x2_ev <- rnorm(n, mean = 5, sd = 1)
y_ev <- rnorm(n, mean = 5 + 2*x1 - 1.5*x2, sd = 2)


#eksik veri tespiti
is_na_x1 <- is.na(x1_ev)
is_na_x2 <- is.na(x2_ev)

# Eksik verileri ortalama ile doldurma
mean_x1_ev <- mean(x1, na.rm = TRUE)
x1_filled <- ifelse(is.na(x1_ev), mean_x1, x1)

#LOCF Yontemı : na.locf fonksiyonu; eksik verileri bir oncekiyle doldurur
install.packages("zoo")
library(zoo)

# Vektor  olusturma ve doldurma
vec <- c(1, NA, NA, 4, NA, 6, NA, 8, NA)
vec_filled <- na.locf(vec)         
print(vec_filled)

# Eksik verileri medyan ile doldurma
median_y_ev <- median(y_ev, na.rm = TRUE)
y_filled <- ifelse(is.na(y_ev), median_y_ev, y_ev)


# Aykiri deger tespiti; z puan(ortalamaya gore) ya da IQR(medyana gore) hbulabiliriz
set.seed(123)
data <- rnorm(120)

# Kutu grafigi olusturark bakabiliriz.
boxplot(data)
sort(data)

# Z-puan hesaplayarak bulabiliriz
z_scores <- scale(data)

#Aykiri degerleri; mutlak z puanı 3 ten büyükse
outliers <- abs(z_scores) > 3

#Alt ve ust ceyrek, IQR hesaplama
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75) 
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

#Aykırı deger (outlier) belirleme
outliers <- data < lower_bound | data > upper_bound

# histogram olusturma
hist(data)

#Kutu grafigi olusturma
boxplot(data)

#Q-Q plot olusturma
qqnorm(data)
qqline(data)

#Kantillerden yarralanma
summary(data)

#multicollinearity  ( en başta olusturdugum veri setini kullanıyorum)
# Coklu dogrusal regresyon modeli olusturma
model <- lm(y ~ x1 + x2 + x3 + x4 + x5)

#Model ozetini alma
summary(model)

#Bagimsiz degiskenker arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(x1, x2, x3, x4, x5))
print(correlation_matrix)

#Ilıskiyi gorsellestirmek icin corrpilot paketi yükleyip, matrisi görsellestiriyoruz
library(corrplot)
corrplot(correlation_matrix, method = "color")

#Bagimsiz degiskenlerin grafiklerini ciziyoruz
par(mfrow=c(1,5)) # Grafiklerin yan yana yerlestirilmesi icin 
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "green", pch = 16)
plot(x4, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "purple", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "orange", pch = 16)

#Veriyi standartlastirma
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
x4_standardized <- scale(x4)
x5_standardized <- scale(x5)
y_standardized <- scale(y)

#Standartlasmis veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(x3_standardized)
summary(x4_standardized)
summary(x5_standardized)
summary(y_standardized)

## caTools paketini yukleme ve aktiflestirme
install.packages("caTools")
library(caTools)

#Veriyi test ve egitim alt kumelerine bolme
set.seed(123)
split <- sample.split(y, SplitRatio = 0.7) # 70% egitim, 30% test
train_data <- subset(data.frame(x1, x2, x3, x4, x5, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, x3, x4, x5, y), split == FALSE)

#Verinin boyutlarinin kontrol etme
dim(train_data)
dim(test_data)


# 2) HAZIR VERI SETI KULLANARAK KESIFSEL VERI ANALIZ SURECI

#kutuphanelerin yuklenmesi ve aktifleştirilmesi 
install.packages("readr")
library(readr)
library(corrplot)
library(psych)
library(ggplot2)

getwd()
setwd("C:/Users/Huawei/Documents")

#veri setinin r ye aktarılması

veri <- read.csv("student_learning_habits_dataset.csv")
veri <- read.csv("C:/Users/Huawei/Desktop/student_learning_habits_dataset.csv") 

#veriye genel bakış
head(veri)
str(veri)

#eksik veri kontrolu
print(colSums(is.na(veri)))

#Aykiri deger tespiti
boxplot(veri$exam_score)

Q1 <- quantile(veri$exam_score, 0.25)
Q3 <- quantile(veri$exam_score, 0.75) 
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- veri$exam_score < lower_bound | veri$exam_score > upper_bound

#z skoru 
z_scores <- scale(veri$exam_score)
outliers <- abs(z_scores) > 3

# histogram olusturma
hist(veri$exam_score)

# Mantiksal hatalari ayikliyoruz
veri_clean <- subset(veri, 
                     exam_score >= 0 & exam_score <= 100 & 
                       daily_study_hours >= 0 & daily_study_hours <= 24)

summary(veri_clean)

#veri donusturme,standartlastirme (Z-skor) 
veri_clean$study_hours_std <- scale(veri_clean$daily_study_hours)

summary(veri_clean$study_hours_std)

#veri normallestirme (sinav notlarini 0-1 arsina olcekliyoruz)
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }

veri_clean$exam_score_norm <- normalize(veri_clean$exam_score)

summary(veri_clean$exam_score_norm)


#Bagimsiz degiskenker arasındaki korelasyonu hesaplama
analiz_icin_veri <- data.frame(
  Sinav_Puani = veri_clean$exam_score_norm,
  Calisma_Saati = veri_clean$study_hours_std,
  Online_Kurs = veri_clean$online_courses_taken,
  Uyku_Saati = veri_clean$avg_sleep_hours
)

korelasyon_matrisi <- cor(analiz_icin_veri)

print(korelasyon_matrisi)

#korelasyon gorsellestirme
corrplot(korelasyon_matrisi, method = "color")

# multicollinearity,Coklu dogrusal regresyon modeli olusturma
# Sinav puani (bagimsiz degisken), digerleri bagimli degisken

model_1 <- lm(Sinav_Puani ~ Calisma_Saati + Online_Kurs + Uyku_Saati, 
              data = analiz_icin_veri)

summary(model_1)

install.packages("caTools")
library(caTools)

set.seed(123) # Sonuçların her seferinde aynı çıkması için (Tekrarlanabilirlik)

split <- sample.split(veri_clean$exam_score, SplitRatio = 0.7)

# Eğitim ve Test setlerinin oluşturulması
train_veri_clean <- subset(veri_clean, split == TRUE)
test_veri_clean  <- subset(veri_clean, split == FALSE)

#Verinin boyutlarinin kontrol etme
dim(train_veri_clean)
dim(test_veri_clean)

head(train_veri_clean)
head(test_veri_clean)




