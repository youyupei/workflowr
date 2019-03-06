library(workflowr)
wflow_build()
wflow_publish("*.Rmd")
wflow_git_push()