
sp_code <- "BSH"
mod_code <- "me"

input_data <- paste("data/out/habitat_model/predict", sp_code, mod_code, sep="/")


library(animation)
oopts = if (.Platform$OS.type == "windows") {
  ani.options(ffmpeg = "C:/ffmpeg/bin/ffmpeg.exe", interval = 0.05, nmax=50, ani.width = 640,
              ani.height = 480)
}


imgs <- list.files(input_data, pattern="*.png", recursive = TRUE, full.names = TRUE)[1:10]
saveVideo({
  ani.options(interval = 1, nmax = 10)
  for(img in imgs){
    im <- magick::image_read(img)
    plot(as.raster(im))
  }
}, video.name = paste0(input_data, "animation.mp4"), other.opts = "-pix_fmt yuv420p -vcodec h264 -b 1000k -c:v libx264 -profile high -s 2500x1700")



imgs <- list.files(input_data, pattern="*.png", recursive = TRUE, full.names = TRUE)[1:10]
saveVideo({
  ani.options(interval = 0.5, nmax = 10, ani.dev="png", ani.height=1700)
  for(img in imgs){
    im <- magick::image_read(img)
    plot(as.raster(im))
  }
}, video.name = paste0(input_data, "animation.mp4"), other.opts = "-pix_fmt yuv420p -b 2000k -s 2500x1700")




"-pix_fmt yuv420p -b 300k"


system("C:/ffmpeg/bin/ffmpeg.exe -report -start_number 20120101 -i D:/temp/animation/%d_BSH_me_pred.png -b:v 2048k -vcodec h264 D:/temp/animation/new_video.mp4")


