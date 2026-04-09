# VERI MADENCILIGI ODEV 2
# AD-SOYAD: SUHEDA KARA
# OGRENCI NO: 20252052028

# Gerekli paketlerin kontrolC< ve yC<klenmesi
install.packages("tidyverse")
install.packages("caret")
install.packages("corrplot")

library(tidyverse)
library(caret)  
library(corrplot)

# VERI SETI OLUSTURMA
# GerC'ek veriyi simule eden 100 satD1rlD1k yapay bir veri seti oluEturuyoruz.
set.seed(123) # Sonuclarin her seferinde ayni cikmasi icin

veri <- data.frame(
  Ders_Katilim = sample(50:100, 100, replace = TRUE),
  IQ_Skoru = rnorm(100, mean = 110, sd = 10),
  Motivasyon = runif(100, 1, 5),
  Ara_Sinav = sample(20:100, 100, replace = TRUE),
  Calisma_Saati = rnorm(100, mean = 15, sd = 5),
  Gecme_Durumu = sample(c(0, 1), 100, replace = TRUE)
)

# Degiskenleri isimlendirme (SC<tunlarD1 indeksleme)
new_names <- paste("x_", 1:6, sep="")
veri_rn <- veri
names(veri_rn) <- new_names

# Tibble formatD1na dC6nC<EtC<rme ve kontrol
veri_df_tib <- as_tibble(veri_rn)
head(veri_df_tib)

# Degisken Atamalari
y <- veri_df_tib[6] # Gecmek (x_6)
y2 <- select(veri_df_tib, x_4, x_6) # Sinav notu ve Gecme durumu
x <- subset(veri_df_tib, select = -c(x_4, x_6)) # Bagimsiz degiskenler

# Numerik Filtreleme ve Fonksiyon TanD1mlama
is_numeric_nu_na <- function(col){
  is.numeric(col) & !anyNA(col)
}

num_colu_no_na_all <- Filter(is_numeric_nu_na, veri_df_tib)

# Tanimlayici istatistikler: summary fonksiyonu; mean, max.min,Q1,Q3 gibi degerleri verir
print(summary(num_colu_no_na_all))

# Korelasyon Analizi
corr_mat_all <- cor(num_colu_no_na_all)

# 0.70 uzerindeki yC<ksek korelasyonlu degiskenleri tespit etme
highly_cor_indices <- findCorrelation(corr_mat_all, cutoff = 0.7)

print(names(num_colu_no_na_all)[highly_cor_indices])


# Korelasyon matrisi gC6rsellestirmesi
corrplot(corr_mat_all, method = "color")

# # x: x_5 (Calisma saati), y: x_4(sinav notu)
ggplot(veri_df_tib,aes(x=x_5,y=x_4))+
  geom_point(color="blue",size=2)+
  facet_wrap(~ x_6) +
  theme_minimal() +
  labs(title = "Calisma Saati ve Sinav Notu iliskisi",
       x = "Haftalik Calisma Saati",
       y = "Ara Sinav Notu")

