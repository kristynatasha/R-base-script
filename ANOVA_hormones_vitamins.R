#Kristy Natasha Yohanes
#12819027
#Praktikum 6:ANOVA

#CONTOH SOAL 1
#Input Data
library(readxl)
Hormone <- read_excel("C:/Users/krist/OneDrive/Desktop/Files/
                      DATA ANOVA.xlsx", sheet="contoh hormone")
Hormone

#Bentuk data menjadi sebuah vektor 
r = c(t(as.matrix(Hormone)))

#Variabel perlakuan
f = c("Au","Si","Gi") #faktor variabel perlakuan 
k=3  #banyak perlakuan 
n=5 #banyak pengamatan per perlakuan 
N=15  #banyak seluruh pengamatan

#Vektor faktor perlakuan sesuai vektor r 
tm = gl(k,1,n*k,factor(f))

#Function 
av = aov(r ~ tm) 
summary(av) #tabel ANOVA

#F critical atau tabel 
df1 = k-1 
df2 = N-k 
alpha = 0.05 
Fcrit = qf(1-alpha,df1,df2) 
Fcrit

#CONTOH SOAL 2
#Input Data 
library(readxl)
vitaminC <- read_excel("C:/Users/krist/OneDrive/Desktop/Files/
                      DATA ANOVA.xlsx", sheet="kadar asam")

#Bentuk data menjadi sebuah vektor 
r1 = c(t(as.matrix(vitaminC$`0_hari`))) 
r2 = c(t(as.matrix(vitaminC$`3_hari`))) 
r3 = c(t(as.matrix(vitaminC$`7_hari`))) 
r2 = na.omit(r2) #menghapus missing value 
r3 = na.omit(r3) 
r=c(r1,r2,r3)

#Variabel perlakuan 
f=c("0 hari","3 hari","7 hari") #faktor variabel perlakuan
k=3             #banyak perlakuan 
n1=length(r1)   #banyak pengamatan pada perlakuan ke-1 
n2=length(r2)   #banyak pengamatan pada perlakuan ke-2 
n3=length(r3)   #banyak pengamatan pada perlakuan ke-3
N=n1+n2+n3      #banyak seluruh pengamatan

#Vektor faktor perlakuan sesuai vektor r 
tm = factor(rep(f,times=c(n1,n2,n3)))

#Function 
av = aov(r ~ tm) 
summary(av) #tabel ANOVA

#F critical atau tabel
df1 = k-1 
df2 = N-k 
alpha = 0.05 
Fcrit = qf(1-alpha,df1,df2) 

#CONTOH SOAL 3
#Input Data 
library(readxl)
df <- read_excel("C:/Users/krist/OneDrive/Desktop/FilesDATA ANOVA.xlsx", 
                         sheet = "contoh soal sistem rudal")
df=df[,2:5] 
r=c(t(as.matrix(df))) 
f1 = c("a1","a2","a3") 
f2 = c("b1","b2","b3","b4") 
k1 = length(f1) 
k2 = length(f2) 
n = 2

#vektor perlakuan 
rudal = gl(k1,n*k2,n*k1*k2,factor(f1)) 
bahanbakar = gl(k2,1,n*k1*k2,factor(f2))

#tabel ANOVA two way 
av <- aov(r ~ bahanbakar*rudal) 
summary(av)




















