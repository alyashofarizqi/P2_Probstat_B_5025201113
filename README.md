# P2_Probstat_E_5025201113
- Nama : Alya Shofarizqi Inayah
- NRP : 5025201113
- Kelas : PROBSTAT-E

# Soal 1
### Penjelasan Soal
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap
kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel
sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat
kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut
diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali
kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden
mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴

![messageImage_1653840767796](https://user-images.githubusercontent.com/80805094/170879818-dbae5481-a31d-45b8-8022-96093c152aa6.jpg)

Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari
responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah
melakukan aktivitas 𝐴 sebanyak 70.

## 1.a. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas
```R
R <- c(1,2,3,4,5,6,7,8,9)
X <- c(78,75,67,77,70,72,78,74,77)
Y <- c(100,95,70,90,90,90,89,90,100)

selisih <- sd(X-Y)
print(selisih)
```
hasil :

![messageImage_1653841134290](https://user-images.githubusercontent.com/80805094/170880140-77c6637a-8ffb-4226-8589-6bbc1928e9b3.jpg)

## 1.b. Carilah nilai t (p-value)
```R
t.test (Y, X, paired = TRUE, var.equal = FALSE)
```

hasil:

![messageImage_1653841207844](https://user-images.githubusercontent.com/80805094/170880208-156d8c5b-445d-455d-94ea-5995fc0bba8e.jpg)

# 1.c. Tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen
```R
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
```

hasil :

![messageImage_1653841262089](https://user-images.githubusercontent.com/80805094/170880256-07804b63-1bf1-439f-8177-b386ec29e518.jpg)

# Soal 2
### Penjelasan Soal
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun.
Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk
mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata
23.500 kilometer dan standar deviasi 3900 kilometer. (Kerjakan menggunakan
2library seperti referensi pada modul).

# 2.a. Apakah Anda setuju dengan klaim tersebut?

> lebih dari 20000km
> ukuran sampel (n) = 1000
> mean (x) = 23500
> sd = 3900
> H0 = miu <= 20000
> H1 = miu > 20000

```R
zsum.test(mean.x = 23500, sigma.x = 3900, n.x = 100,
          alternative = "greater", mu = 20000,
          conf.level = 0.95)
```

Setuju, karena kesimpulan dari uji z menolak H0, sehingga mobil dikemudikan rata-rata lebih dari 20000 kilometer per tahun

# 2.b. Jelaskan maksud dari output yang dihasilkan!
Output dari z test adalah, hipotesis alternatif
alternative hypothesis: true mean is greater than 20000 atau H1 diterima sehingga klaim benar. 

# 2.c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
P-value dari uji tes z adalah 2.2e-16 atau mendekati 0, dari hasil p-value tersebut hipotesis awal dapat ditolak dan H1 diterima.

# Soal 3
### Penjelasan Soal
Diketahui perusahaan memiliki seorang data analyst ingin memecahkan
permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya
didapatkanlah data berikut dari perusahaan saham tersebut.

![messageImage_1653841388431](https://user-images.githubusercontent.com/80805094/170880382-4b07c959-6abb-4ef4-9437-bc66bf175709.jpg)

Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil
diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada
rata-ratanya (α= 0.05)?

# 3.a. H0 dan H1
> H0 = (miu1 = miu2)
> H1 = (miu1 != miu2)

# 3.b. Hitung Sampel Statistik
```R
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, 
          mean.y=2.79, s.y = 1.32, n.y = 27, 
          alternative = "two.sided", mu = 0, var.equal = TRUE,
          conf.level = 0.95)
```

# 3.c. Lakukan Uji Statistik (df =2)
```R
plotDist(dist ='t', df = 2, col="pink")
```

# 3.d. Nilai Kritikal
```R
qt(p = 0.05, df = 2, lower.tail = FALSE)
```

# 3.e. Keputusan
Karena p-value < a , Hipotesis awal ditolak

# 3.f. Kesimpulan
Dengan tingkat keyakinan 95%, diyakini bahwa tidak terdapat perbedaan rata-rata saham pada perusahaan di Bandung dan Bali.


# Soal 4
### Penjelasan Soal
Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya
ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
kucing putih dengan panjangnya masing-masing.
Jika : diketahui dataset https://intip.in/datasetprobstat1
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya
sama

# 4.a. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.

```R
my_data <- read.delim(file.choose())

my_data$Group <- as.factor(my_data$Group)
my_data$Group = factor(my_data$Group, labels = c("grup1", "grup1", "grup3"))

grup1 <- subset(my_data, Group == "grup1")
grup2 <- subset(my_data, Group == "grup1")
grup3 <- subset(my_data, Group == "grup3")

qqnorm(grup1$Length)
qqnorm(grup2$Length)
qqnorm(grup3$Length)
```

# 4.b. Berapa nilai p yang didapatkan? Apa hipotesis dan kesimpulan yang dapat diambil ?
```R
bartlett.test(Length ~ Group, data = my_data)
```

# 4.c buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1
```R
model1 <- aov(Length ~ Group, data = my_data)
summary(model1)
```

# 4.d. Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
nilai p adalah 0.0013, maka H0 ditolak

# 4.e. apakah satu jenis kucing lebih panjang dari yang lain?
```R
TukeyHSD(model1)
```

# 4.f. Visualisasikan data dengan ggplot2
```R
library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")
```

# Soal 5
### Penjelasan Soal
Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil Eksperimen.

# 5.a. Buatlah plot sederhana untuk visualisasi data
```R
library(dplyr)
library(multcompView)

gtl <- read.csv(file.choose())

qplot(x = Temp, y = Light, geom = "point", data = gtl) +
  facet_grid(.~Glass, labeller = label_both)
```

# 5.b. Lakukan uji ANOVA dua arah
```R
gtl$Glass <- as.factor(gtl$Glass)
gtl$Temp_Factor <- as.factor(gtl$Temp)
str(gtl)

gtlaov <- aov(Light ~ Glass*Temp_Factor, data = gtl)
summary(gtlaov)
```

# 5.c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
```R
data_summary <- group_by(gtl, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))

print(data_summary)
```

# 5.d. Lakukan uji Tukey
```R
tukey <- TukeyHSD(gtlaov)
print(tukey)
```

# 5.e. Gunakan compact letter display untuk menunjukkan perbedaan signifikan
#      antara uji Anova dan uji Tukey
```R
tukey.cld <- multcompLetters4(gtlaov, tukey)
print(tukey.cld)
```
