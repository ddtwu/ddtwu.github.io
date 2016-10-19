KnitPost <- function(baseURL, filePath, input, output) {

  # baseURL: blog 的連結
  # filePath: blog 資料檔路徑
  # input: .rmd 所在的路徑
  # output: .md 輸出的路徑

  .libPaths(c("d:/0.R_packages", .libPaths()))
  require(knitr)
  
  inputPath <- paste0(filePath, input)
  outputPath <- paste0(filePath, output)
  
  dName <- sub(".Rmd$", "", basename(input))
  figPath <- paste0("figures/", dName, "/")
  
  opts_knit$set(base.url = baseURL)
  opts_chunk$set(fig.path = figPath, fig.cap = "center")
  render_jekyll()
  
  knit(input = inputPath, output = outputPath, envir = parent.frame(), encoding = 'UTF-8')
}

KnitPost(baseURL = 'https://ddtwu.github.io/',
         filePath = 'D:/MarvinWuPersonal/MarvinBlog/myJekyllBlog/',
         input = '_source/2016-10-03-tp-weather-fetch.Rmd',
         output = '_posts/2016-10-03-tp-weather-fetch.md')




