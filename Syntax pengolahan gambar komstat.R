im=load.image("d:/latihan/wallpaper.jpg") 
plot.new(im) 
dim((im)) 
width(im) 
height(im) 
depth(im) 
spectrum(im)

noise <- as.cimg(array(runif(1920*1080*1*3),c(1920,1080,1,3))) 
plot(noise) #noise
plot(im+noise) #noise pada gambar
plot(im-noise) #noise lebih halus
plot(noise-im) #noise + efek negatif


im.blur=isoblur(im,5) #blur tipis
plot(im.blur) 
im.blur=isoblur(im,15) # lebih blur
plot(im.blur) 

#efek greyscale/abu-abu/hitam putih
im.gray= grayscale(im) 
dim(im.gray) 
plot(im.gray) 

#menyimpan gambar hasil
imager::save.image(im.gray, 
                   "d:/latihan/wallpaper1.jpg")

#nilai mean relative difference
noise2 <- as.cimg(array(runif(1920*1080*1*1),c(1920,1080,1,1))) 
im.gray2<-im.gray+noise2 
all.equal(im.gray, im.gray2) 

#split gambar
imsplit(im,"x",4)%>%plot #plot(imsplit(im, "x",4)) 
imsplit(im,"y",4)%>%plot 
im.split=imsplit(im,"y",-512) 
plot(im.split) 
im.split1=im.split[[1]] 
plot(im.split1) 
im.split2=im.split[[2]] 
plot(im.split2) 

# RGB belum tau maksudnya buat apaan, tapi hasilnya jadi kek pattern gitu
R <- as.cimg(function(x,y) sin(cos(3*x*y)),100,100) 
dim(R) 
G <- as.cimg(function(x,y) sin(cos(3*x*y + pi/2)),100,100) 
B <- as.cimg(function(x,y) exp(-.03*x),100,100) 
RGB <- imappend(list(R,G,B),"c") 
dim(RGB) 
plot(RGB) 

# crop, balik
plot(as.cimg(im[1:960,1:540,,])) #rasio lebar, tinggi
as.cimg(im[960:0,540:0,1,])%>%plot #dibalik vertikal
im.setengah<-imresize(im,1/2)%>% plot 
resize(im, 960, 540) %>% plot 

plot(as.cimg(im[-c(960:1),-c(540:1),,]))
plot(as.cimg(im[960:1,1:540,,]))

#belom paham yang ini buat apaan
im.g <- grayscale(im) 
df <- as.data.frame(im.g) 
m <- lm(value ~ x + y,data=df) 
df[,3]<-resid(m) 
im.g.balik<-as.cimg(df) 
par(mfrow=c(1,2)) 
plot(im.g) 
plot(im.g.balik)