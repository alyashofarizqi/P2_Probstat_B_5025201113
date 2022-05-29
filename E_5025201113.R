#Nama : Alya Shofarizqi Inayah
#NRP : 5025201113
#PROBSTAT E

# 1.a. Carilah Standar Deviasi dari data selisih pasangan pengamatan 
#      tabel diatas

R <- c(1,2,3,4,5,6,7,8,9)
X <- c(78,75,67,77,70,72,78,74,77)
Y <- c(100,95,70,90,90,90,89,90,100)

selisih <- sd(X-Y)
print(selisih)

# 1.b. carilah nilai t (p-value)

t.test (Y, X, paired = TRUE, var.equal = FALSE)

# 1.c. tentukanlah apakah terdapat pengaruh yang signifikan secara 
#      statistika dalam hal kadar saturasi oksigen

mu0 = mean(X)
xbar = mean(Y)          
s = sd(Y)
n = length(Y)
t = (xbar-mu0)/(s/sqrt(n)) 

alpha = 0.05 
t.half.alpha = qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha)

pval <- 2*pt(t, df=n-1)
pval

# 2.a. Apakah Anda setuju dengan klaim tersebut?

# lebih dari 20000km
# ukuran sampel (n) = 1000
# mean (x) = 23500
# sd = 3900
# H0 = miu <= 20000
# H1 = miu > 20000
zsum.test(mean.x = 23500, sigma.x = 3900, n.x = 100,
          alternative = "greater", mu = 20000,
          conf.level = 0.95)

# Setuju, karena kesimpulan dari uji z menolak H0,
# sehingga mobil dikemudikan rata-rata lebih dari
# 20000 kilometer per tahun

# 2.b. Jelaskan maksud dari output yang dihasilkan!
# Output dari z test adalah, hipotesis alternatif
# alternative hypothesis: true mean is greater than 20000
# atau H1 diterima sehingga klaim benar. 

# 2.c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
# P-value dari uji tes z adalah 2.2e-16 atau mendekati 0,
# dari hasil p-value tersebut hipotesis awal dapat ditolak
# dan H1 diterima.

# 3.a. H0 dan H1
# H0 = (miu1 = miu2)
# H1 = (miu1 != miu2)

# 3.b. Hitung Sampel Statistik

tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, 
          mean.y=2.79, s.y = 1.32, n.y = 27, 
          alternative = "two.sided", mu = 0, var.equal = TRUE,
          conf.level = 0.95)

# 3.c. Lakukan Uji Statistik (df =2)

plotDist(dist ='t', df = 2, col="pink")

# 3.d. Nilai Kritikal

qt(p = 0.05, df = 2, lower.tail = FALSE)

# 3.e. Keputusan

cat("Karena p-value < a , Hipotesis awal ditolak")

# 3.f. Kesimpulan

cat("Dengan tingkat keyakinan 95%, diyakini bahwa tidak terdapat 
    perbedaan rata-rata saham pada perusahaan di Bandung dan Bali.")

# 4.a. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup"
#      Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
#      lihat apakah ada outlier utama dalam homogenitas varians.

my_data <- read.delim(file.choose())

my_data$Group <- as.factor(my_data$Group)
my_data$Group = factor(my_data$Group, labels = c("grup1", "grup1", "grup3"))

grup1 <- subset(my_data, Group == "grup1")
grup2 <- subset(my_data, Group == "grup1")
grup3 <- subset(my_data, Group == "grup3")

qqnorm(grup1$Length)
qqnorm(grup2$Length)
qqnorm(grup3$Length)

# 4.b. Berapa nilai p yang didapatkan? 
#      Apa hipotesis dan kesimpulan yang dapat diambil ?

bartlett.test(Length ~ Group, data = my_data)

# 4.c buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1

model1 <- aov(Length ~ Group, data = my_data)
summary(model1)

# 4.d. Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
# nilai p adalah 0.0013, maka H0 ditolak

# 4.e. apakah satu jenis kucing lebih panjang dari yang lain?

TukeyHSD(model1)

# 4.f. Visualisasikan data dengan ggplot2

library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")

# 5.a. Buatlah plot sederhana untuk visualisasi data

library(dplyr)
library(multcompView)

gtl <- read.csv(file.choose())

qplot(x = Temp, y = Light, geom = "point", data = gtl) +
  facet_grid(.~Glass, labeller = label_both)

# 5.b. Lakukan uji ANOVA dua arah

gtl$Glass <- as.factor(gtl$Glass)
gtl$Temp_Factor <- as.factor(gtl$Temp)
str(gtl)

gtlaov <- aov(Light ~ Glass*Temp_Factor, data = gtl)
summary(gtlaov)

# 5.c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk
#      setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)

data_summary <- group_by(gtl, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))

print(data_summary)

# 5.d. Lakukan uji Tukey

tukey <- TukeyHSD(gtlaov)
print(tukey)

# 5.e. Gunakan compact letter display untuk menunjukkan perbedaan signifikan
#      antara uji Anova dan uji Tukey

tukey.cld <- multcompLetters4(gtlaov, tukey)
print(tukey.cld)