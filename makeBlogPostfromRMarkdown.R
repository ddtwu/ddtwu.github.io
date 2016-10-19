.libPaths(c("d:/0.R_packages", .libPaths()))

myjekyllsite = c("D:/MarvinWuPersonal/MarvinBlog/myJekyllBlog/")

KnitPost <- function(input, base.url = myjekyllsite) {
  require(knitr)

  opts_knit$set(base.url = base.url)
  fig.path <- paste0("figures/", sub(".Rmd$", "", basename(input)), "/")
  opts_chunk$set(fig.path = fig.path)
  opts_chunk$set(fig.cap = "center")
  render_jekyll()
  knit(input, envir = parent.frame())
  
  knitr::knit(encoding = 'UTF-8')
}

KnitPost(input = "D:/MarvinWuPersonal/MarvinBlog/myJekyllBlog/_source/2016-10-03-rmd-test.Rmd")





#----------------------------------------#
library(servr)

servr::jekyll(dir = "D:/MarvinWuPersonal/MarvinBlog/myJekyllBlog/",
              input = '_source',
              output = '_posts',
              serve = FALSE)

jekyll <- 
function (dir = ".", 
          input = c(".", "_source", "_posts"), 
          output = c(".",  "_posts", "_posts"), 
          script = c("Makefile", "build.R"), 
          serve = TRUE, 
          command = "jekyll build", ...) 
{
  baseurl = jekyll_config(dir, "baseurl", "")
  destination = jekyll_config(dir, "destination", "_site")
  jekyll_build = function() {
    if (system(command) != 0) 
      stop("Failed to run: ", command)
  }
  build_all = function() knit_maybe(input, output, script, 
                                    method = "jekyll")
  if (!serve) {
    in_dir(dir, {
      build_all()
      jekyll_build()
    })
    return()
  }
  dynamic_site(dir, ..., build = function(...) {
    update = build_all()
    if (update || !file_test("-d", destination)) 
      jekyll_build()
    update
  }, site.dir = destination, baseurl = baseurl)
}


