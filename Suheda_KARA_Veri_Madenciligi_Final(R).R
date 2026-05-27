# BİL524 VERİ MADENCİLİĞİ FİNAL ÖDEVİ
# ÖĞRENCİNİN Adı-Soyadı: Şüheda KARA
# Öğrenci No: 20252052028

# Veri Seti: insurance.csv

# 1. HAZIRLIK VE VERİYİ İÇE AKTARMA
# R nin aktif olarak çalıştığı klasörün tespit edilmesi
getwd()

#Dosya masaüstünde olduğu için çalışma dizininin masaüstü klasörüne yönlendirilmesi
#setwd("C:/Users/Huawei/Desktop")

# Kutuphanelerin yuklenmesi
#install.packages("readr")
#install.packages("corrplot")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("neuralnet")

# Kütüphaneleri aktifleştirme
library(readr)
library(corrplot)
library(arules)
library(arulesViz)
library(neuralnet)

# Verinin okunması
df <- read_csv("insurance.csv")

# Yuklenen veri kumesinin satir ve sutun boyutlarinin kontrol edilmesi
print(dim(df))

# Verinin ilk 6 satırının ve yapısal tiplerinin doğrulanması
print(head(df))

# 2. VERİ ÖN İŞLEME VE TEMİZLİK

# Eksik (NA) değerlerini temizleme
df_clean <- na.omit(df)

# Mantıksız değerleri filtreleme (yaş ve masraflar 0'dan büyük olmalı)
df_clean <- subset(df_clean, age > 0 & bmi > 0 & charges > 0 & children >= 0)

# Temizlik işlemi sınrası satır ve sütunları kontrol etme
print(dim(df_clean))

# Vücut Kitle İndeksi (BMI) için aykırı değerleri IQR yöntemiyle belirleme
Q1_bmi <- quantile(df_clean$bmi, 0.25)
Q3_bmi <- quantile(df_clean$bmi, 0.75)
IQR_bmi <- Q3_bmi - Q1_bmi

#Alt ve üst sınırları belirleme
lower_bound_bmi <- Q1_bmi - 1.5 * IQR_bmi
upper_bound_bmi <- Q3_bmi + 1.5 * IQR_bmi

#Aykırı değerleri belirleme
outliers_bmi <- df_clean$bmi < lower_bound_bmi | df_clean$bmi > upper_bound_bmi

# Aykiri degerlerin filtreleme
df_no_outliers <- subset(df_clean, !outliers_bmi)

# Sağlık harcamaları (Charges) için aykırı değerleri hesaplama
Q1_charges <- quantile(df_no_outliers$charges, 0.25)
Q3_charges <- quantile(df_no_outliers$charges, 0.75)
IQR_charges <- Q3_charges - Q1_charges


#Ucret için alt ve üst sınırları belirleme
lower_bound_charges <- Q1_charges - 1.5 * IQR_charges
upper_bound_charges <- Q3_charges + 1.5 * IQR_charges

#Aykiri degerleri belirleme
outliers_charges <- df_no_outliers$charges < lower_bound_charges | df_no_outliers$charges > upper_bound_charges

# Aykiri degerleri temizleme
df_final <- subset(df_no_outliers, !outliers_charges)

print(dim(df_final))

# Kategorik Değişkenlerin Sayısallaştırılması (Encoding)
df_final$sex_encoded <- as.numeric(factor(df_final$sex))
df_final$smoker_encoded <- as.numeric(factor(df_final$smoker))
df_final$region_encoded <- as.numeric(factor(df_final$region))

print(head(df_final[, c("sex", "sex_encoded", "smoker", "smoker_encoded")]))

# Z-Skoru ile Standartlastirma
df_final$scaled_age <- scale(df_final$age)
df_final$scaled_bmi <- scale(df_final$bmi)
df_final$scaled_children <- scale(df_final$children)
df_final$scaled_charges <- scale(df_final$charges)

print(summary(df_final[, c("scaled_age", "scaled_bmi", "scaled_charges")]))

# 3. KEŞİFSEL VERİ ANALİZİ (EDA)
#Veri setindeki degisken tiplerinin kontrol edilmesi
str(df_final)

#Sayısal özniteliklerin merkezi eğilim ölçülerinin özetlenmesi
summary(df_final[, c("age", "bmi", "children", "charges")])

# Korelasyon Analizi
# Bağımsız değişkenler ve hedef değişken arasındaki korelasyonun hesaplanması
correlation_matrix <- cor(df_final[, c("age", "bmi", "children", "charges")])

print(correlation_matrix)

# korelasyon matrisinin görselleştirilmesi
corrplot(correlation_matrix, method = "color") 

# Görselleştirmeler

# Histogram Grafiği
par(mfrow = c(1, 2))
hist(df_final$bmi, main = "Müşteri BMI Dağılımı", xlab = "Vücut Kitle İndeksi", 
     col = "lightblue", border = "black")

hist(df_final$charges, main = "Yıllık Sigorta Harcamaları", xlab = "Maliyet Tutarı ($)", 
     col = "lightgreen", border = "black")

# Boxplot grafiği 
par(mfrow = c(1, 1))
boxplot(charges ~ smoker, 
        data = df_final, 
        main = "Sigara Kullanımı - Maliyet İlişkisi", 
        xlab = "Sigara Tüketim Durumu", 
        ylab = "Yıllık Sağlık Harcaması ($)", 
        col = c("skyblue", "tomato"))

# Saçılım (Scatter) Grafikleri
par(mfrow = c(1, 3))

plot(df_final$age, df_final$charges, 
     main = "Age vs. Charges", xlab = "Age", ylab = "Charges", 
     col = "blue", pch = 16)

plot(df_final$bmi, df_final$charges, 
     main = "BMI vs. Charges", xlab = "BMI", ylab = "Charges", 
     col = "red", pch = 16)

plot(df_final$children, df_final$charges, 
     main = "Children vs. Charges", xlab = "Children", ylab = "Charges", 
     col = "orange", pch = 16)

# 4. BİRLİKTELİK KURALLARI (APRIORI)

# Veriyi sepet formatına (kategorik) çevirme
df_basket <- data.frame(
  Yas_Grup = ifelse(df_final$age >= 45, "Yasli", "Genc"),
  Kilo_Grup = ifelse(df_final$bmi >= 30, "Obez", "NormalKilo"),
  Sigara_Durum = ifelse(df_final$smoker == "yes", "SigaraIcen", "SigaraIcmeyen"),
  Maliyet_Grup = ifelse(df_final$charges >= 12000, "YuksekMaliyet", "DusukMaliyet")
)

print(head(df_basket))

# Verinin kuralların üretilebilmesi için transactions yapısına çevrilmesi
trans <- as(df_basket, "transactions")

# Apriori Modelinin Kurulması
# Destek(0.10) ve Güven(0.60) eşik değerleriyle kuralları oluşturulması
rules <- apriori(trans, parameter = list(supp = 0.10, conf = 0.60))

# Üretilen toplam kural sayısının konsolda özetlenmesi
cat("\n===== ÜRETİLEN TOPLAM KURALLAR =====\n")

print(rules)

# En yüksek Lift değerine sahip ilk 5 kural
strong_rules <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(strong_rules, 5))

# Kuralların Görselleştirilmesi
plot(rules, measure = c("support", "confidence"), shading = "lift")
plot(head(strong_rules, 5), method = "graph", engine = "htmlwidget")

# 5. SINIFLANDIRMA: LOJİSTİK REGRESYON
# Sigara içme durumunu (0 ve 1) tahmin etme
df_final$BinarySmoker <- as.factor(ifelse(df_final$smoker == "yes", 1, 0))

# Dönüşüm sonrası dağılım frekanslarının kontrol edilmesi
print(table(df_final$BinarySmoker))

# Train ve Test olarak veriyi Ayırma (%70 Train, %30 Test
set.seed(123)
index <- sample(1:nrow(df_final), 0.7 * nrow(df_final))

# eğitim ve test alt veri çerçevlerini atama
train_data <- df_final[index, ]
test_data  <- df_final[-index, ]

print(dim(train_data))
print(dim(test_data))

# Modeli kurma
model_logit <- glm(BinarySmoker ~ age + bmi + charges, data = train_data, family = binomial)

summary(model_logit)

# Test verisiyle olasılık hesaplama ve  0.5 eşik değerine göre sınıflama
probabilities <- predict(model_logit, newdata = test_data, type = "response")
pred_logit <- as.factor(ifelse(probabilities > 0.50, 1, 0))

# Karmaşıklık Matrisi (Confusion Matrix) ve Performans
conf_logit <- table(Predicted = pred_logit, Actual = test_data$BinarySmoker)
print(conf_logit)

# Matris hücrelerinin değişkenkere atanması
TP <- conf_logit["1", "1"] 
TN <- conf_logit["0", "0"] 
FP <- conf_logit["1", "0"] 
FN <- conf_logit["0", "1"]

# Akademik başarı ölçütlerinin hesaplanması
accuracy_val  <- (TP + TN) / (TP + TN + FP + FN)
precision_val <- TP / (TP + FP)
recall_val    <- TP / (TP + FN)
f1_score_val  <- 2 * (precision_val * recall_val) / (precision_val + recall_val)

cat("\n===== LOGISTIK REGRESYON MODEL PERFORMANSI =====\n")
cat("Doğruluk (Accuracy)  :", accuracy_val, "\n")
cat("Kesinlik (Precision) :", precision_val, "\n")
cat("Duyarlılık (Recall)  :", recall_val, "\n")
cat("F1-Skor (F1-Score)   :", f1_score_val, "\n")

# 6. ÇOKLU DOĞRUSAL REGRESYON
# Sigorta masraflarını (charges) tahmin eden model
model_reg <- lm(charges ~ age + bmi + children, data = train_data)
summary(model_reg)

# Tahminler
pred_train <- predict(model_reg, newdata = train_data)
pred_test  <- predict(model_reg, newdata = test_data)

# Model başarısının ölçmek için R2, MAE ve RMSE değerlerini hesaplama
R2_train   <- summary(model_reg)$r.squared
MAE_train  <- mean(abs(train_data$charges - pred_train))
RMSE_train <- sqrt(mean((train_data$charges - pred_train)^2))

# Test verisinin R-kare değerinin formülü
ss_res <- sum((test_data$charges - pred_test)^2)
ss_tot <- sum((test_data$charges - mean(test_data$charges))^2)

R2_test <- 1 - (ss_res / ss_tot)

MAE_test  <- mean(abs(test_data$charges - pred_test))
RMSE_test <- sqrt(mean((test_data$charges - pred_test)^2))


# Sonuçların yazdırılması
cat("\n===== REGRESYON MODELİ EĞİTİM KÜMESİ PERFORMANSI =====\n")
cat("R-Kare (R2)  :", R2_train, "\n")
cat("MAE          :", MAE_train, "\n")
cat("RMSE         :", RMSE_train, "\n")

cat("\n===== REGRESYON MODELİ TEST KÜMESİ PERFORMANSI =====\n")
cat("R-Kare (R2)  :", R2_test, "\n")
cat("MAE          :", MAE_test, "\n")
cat("RMSE         :", RMSE_test, "\n")


# 7. KÜMELEME: K-MEANS

# Sadece sayısal ve standartlaştırılmış verilerin kullamılması
cluster_data <- df_final[, c("scaled_age", "scaled_bmi", "scaled_charges")]
print(head(cluster_data))

set.seed(123)

# Kümeler içi hata kareler toplamı için boş vektörün oluşturulması
wss <- numeric(10)

# 1 ile 10 arasındaki küme için döngünün çalıştırılması
for (i in 1:10) {
  km_model <- kmeans(cluster_data, centers = i, nstart = 10)
  wss[i] <- km_model$tot.withinss
}

par(mfrow = c(1, 1))
plot(1:10, wss, type = "b", 
     main = "Uygun Küme Sayısı İçin Elbow Yöntemi", 
     xlab = "Küme Sayısı (k)", 
     ylab = "Kümeler İçi Toplam Varyans (WSS)",
     col = "blue", pch = 19)

# Grafiğe göre centers = 3 parametresi kullanılarak K-Means modelinin kurulması
final_km <- kmeans(cluster_data, centers = 3, nstart = 10)

# Küme etiketlerinin veri setine yeni bir sütun olarak atanması
df_final$cluster_id <- as.factor(final_km$cluster)

print(table(df_final$cluster_id))
print(final_km$centers)


# Yaş ve Ücret ekseninde küme dağılım saçılım grafiğinin çizilmesi
par(mfrow = c(1, 1))
plot(df_final$age, df_final$charges, 
     col = final_km$cluster, 
     main = "K-Means Kümeleme Sonuçları", 
     xlab = "Yaş (Age)", 
     ylab = "Yıllık Sağlık Harcaması ($)", 
     pch = 16)

# Küme merkezlerinin grafik üzerine büyük belirgin noktalar olarak eklenmesi
points(final_km$centers[, c(1, 3)], 
       col = "black", 
       pch = 8, 
       cex = 2, 
       lwd = 3)

# 8. YAPAY SİNİR AĞLARI (YSA)

# Giriş katmanında kullanılacak ölçekli özniteliklerin kontrol edilmesi
print(head(train_data[, c("scaled_age", "scaled_bmi", "scaled_children", "scaled_charges")]))

set.seed(123)

# YSA modelinin kurulması

model_nn <- neuralnet(scaled_charges ~ scaled_age + scaled_bmi + scaled_children, 
                      data = train_data, 
                      hidden = 5, 
                      linear.output = TRUE)

print(model_nn)

# Ağın ağırlıklarını ve yapısını çizdirme
par(mfrow = c(1, 1))
plot(model_nn)

# Test seti üzerinde MSE performansı hesaplama

# Test kümesindeki bağımsız girdilerin (giriş katmanının) ayrıştırılması
test_inputs <- test_data[, c("scaled_age", "scaled_bmi", "scaled_children")]

# Modelden test girdilerine karşılık gelen tahmin çıktılarının alınması

nn_predictions <- compute(model_nn, test_inputs)

pred_nn_scaled <- nn_predictions$net.result

actual_nn_scaled <- test_data$scaled_charges

mse_nn <- mean((actual_nn_scaled - pred_nn_scaled)^2)

cat("\n===== YSA TEST PERFORMANSI =====\n")
cat("MSE Değeri:", mse_nn, "\n")





