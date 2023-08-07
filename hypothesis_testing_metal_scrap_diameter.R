#hypothesis testing

#SOAL 1
#Input
library(readxl)
x <- read_excel("C:/Users/krist/OneDrive/Desktop/Files/DATA UJI HIPOTESIS.xlsx",
                sheet = "contoh diameter logam")
x = as.numeric(x$`diameter potongan logam`)
xbar = mean(x)  #mean sampel
mu0 = 1.09      #nilai hipotesis
s = sd(x)       #standar deviasi sampel
n = length(x)   #banyak observasi
alpha = 0.05    #taraf signifikansi

#Cara Manual (bandingkan t hitung dengan t tabel)
(t = (xbar-mu0)/(s/sqrt(n)))    #t hitung
(t.lower = qt(alpha,df = n-1))  #t tabel eka arah

#p-value (bandingkan p-value dengan alpha)
(pval.lower = pt(t, df=n-1))       #eka arah

#Cara Otomatis
t.test(x, mu=mu0, alternative = "less", conf.level = 0.95)


#SOAL 2
#Input 
x <- read_excel("C:/Users/krist/OneDrive/Desktop/Files/Assignments/Praktikum Statdas/Praktikum ke-5/DATA UJI HIPOTESIS.xlsx",
               sheet = "latihan no 6")
x1 = as.numeric(x$Penipuan)
x2 = as.numeric(x$`Senjata Api`)
x2 = na.omit(x2) #menghapus missing value atau NA
xbar1 = mean(x1) #mean sampel x1 
xbar2 = mean(x2) #mean sampel x2
mu0 = -10        #nilai hipotesis 
S1 = sd(x1)      #standar deviasi sampel x1
S2 = sd(x2)      #standar deviasi sampel x2 
n1 = length(x1)  #banyak observasi 
n2 = length(x2) 
alpha = 0.05     #taraf signifikansi

#Cara Manual (Bandingkan Statistik Uji T hitung dan T tabel)
(df = ((S1^2/n1) + (S2^2/n2))^2/
    (((1/(n1-1))*(S1^2/n1)^2)+((1/(n2-1))*(S2^2/n2)^2)))
(xbar = xbar1 - xbar2)
(t = (xbar-mu0)/(sqrt((S1^2/n1)+(S2^2/n2)))) #t hitung 
(t.lower = qt(alpha, df)) #t tabel eka arah

#p-value (bandingkan p-value dengan alpha)
(pval.lower = pt(t, df))      #eka arah


#SOAL 3
library(readxl) 
x <-read_excel("C:/Users/krist/OneDrive/Desktop/Files/Assignments/Praktikum Statdas/Praktikum ke-5/DATA UJI HIPOTESIS.xlsx", 
               sheet = "contoh diameter logam") 
x <- as.numeric(x$`diameter potongan logam`) 
S = sd(x)       #stadar deviasi sampel 
sigma0 = 0.01^2 #nilai hipotesis variansi 
n = length(x)   #banyak data alpha=0.05

#Cara Manual (Bandingkan chi hitung dan chi tabel) 
(chi = (n-1)*S^2/sigma0) #chi hitung 
(chi.upper = qchisq(1-alpha, df=n-1)) #chi tabel eka arah

#P-value (bandingkan p-value dengan alpha) 
(pval.upper = pchisq(chi, df=n-1, lower.tail = FALSE)) #eka arah

#Cara Otomatis 
library(TeachingDemos) 
sigma.test(x, sigma=sqrt(sigma0), alternative = "greater", 
           conf.level = 0.95)
