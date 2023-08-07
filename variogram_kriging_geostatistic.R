#variogram

#Mengaktifkan library
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(maptools)
library(readxl)

#Input Data
library(readxl)
data <- read_excel("C:/Users/krist/OneDrive/Desktop/Files/Assignments/Analisis Variogram.xlsx")
datasumurjtb <- data.frame(data)

# Merubah class "data frame" menjadi "Spatial Point Data Frame (SPDF)" 
coordinates(datasumurjtb) = ~x+y

# melihat cuplikan informasi dari data
glimpse(datasumurjtb)

# Membuat diagram pencar dengan ukuran titik menunjukkan
# besar kecilnya nilai di lokasi tersebut
ggplot(data = as.data.frame(datasumurjtb), aes(x,y)) + 
  geom_point(aes(size = Hg), col = "blue", alpha = 0.6) + 
  ggtitle("Permeabilitas (mD)") + coord_equal() + theme_bw()

# Mengitung Variogram eksperimental kemudian plot secara otomatis
(vgm1 <- variogram(Hg ~ 1, datasumurjtb))
plot(vgm1, cex = 1)

# Memodifikasi interval jarak dengan menambahkan width pada fungsi variogram
(vgm2 <- variogram(Hg ~ 1, datasumurjtb, width = 0.05))
plot(vgm2, cex = 1)

# Memodifikasi dengan transformasi (misalkan dengan logaritma natural) dan memperlebar interval kelas agar pasangan data di setiap kelas jarak ??? lebih banyak.
(vgm.ln.1 <- variogram(log(Hg) ~ 1, datasumurjtb))
plot(vgm.ln.1, main = "var.ln", cex = 1)

(vgm.ln.2 <- variogram(log(Hg)~1, datasumurjtb, width = 0.05))
plot(vgm.ln.2, main ="var.ln  wdth = 0.05" ,cex=1)

# Melihat contoh-contoh model variogram yang dapat dimodelkan R melalui fungsi vgm
show.vgms()
vgm()

# Menampilkan model variogram eksperimental terbaik diantara model (Exp", "Sph", "Gau")
# Fitting variogram
(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2,fit)

# Membuat grid untuk melihat peta kontur sebaran data untuk lokasi 
# tidak terobservasi dengan metode ordinary kriging # n: ukuran grid, 
# sesuaikan dengan koordinat data
n <- 0.02
kolom <- seq(datasumurjtb@bbox[1,1]-n,datasumurjtb@bbox[1,2]+n,by = n)
baris <- seq(datasumurjtb@bbox[2,1]-n,datasumurjtb@bbox[2,2]+n,by = n)
the.grid <- expand.grid(x = kolom, y = baris)
coordinates(the.grid) <- ~x+y
gridded(the.grid) <- T

# Plot grid dan titik observasi
par(mar=c(1,1,1,1))
plot(the.grid, cex=0.5, col="grey")
points(datasumurjtb, pch=1, col='red', cex=1)

# Melakukan penaksiran untuk membentuk peta kontur dengan ordinary kriging
kriging <- krige(Hg~1,datasumurjtb, the.grid, model=fit)

# Plot kontur sebagai hasil taksiran oleh kriging
titik <- SpatialPoints(datasumurjtb@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex =  0.8, col = 'red')

# Sesuaikan kolom data yang digunakan pada DataSumurJTB[[n]]
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(datasumurjtb[[4]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout=list(LayoutPoints,LayoutLabels))

# Menentukan koordinat; 
# contoh 5 lokasi (0.2,-1),(0.4,-0.8),(0.8,-1.0),(1,-1.4),dan (1.2,-0.5) 
titik2 <- SpatialPoints(cbind(c(0.2,0.4,0.8,1,1.2),c(-1,-0.8,-1,-1.4,-0.5)))
taksiran <- krige(k.fracture~1, datasumurjtb, titik2, model=fit)[[1]]

LayoutPoints.T <- list('sp.points', titik2, pch=19, cex=0.8, col='green')
LayoutLabels.T <- list('sp.pointLabel', titik2, label = as.character(taksiran), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Set Ordinary Kriging Prediction", sp.layout=list(LayoutPoints.T,LayoutLabels.T))


