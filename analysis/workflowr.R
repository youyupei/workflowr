library(workflowr)
wflow_build()
wflow_publish("*.Rmd")
wflow_git_push()



## Build all files
for(i in list.files())
{
  if(grepl(".Rmd", i)){
    try(wflow_build(i))
  }
}
