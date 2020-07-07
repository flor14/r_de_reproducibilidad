remotes::install_github("karthik/holepunch")
# Please report any installation problems in the issues
library(holepunch)

write_compendium_description(package = "r_de_reproducibilidad",
                             description = "Filminas para meetup")
# to write a description, with dependencies. Be sure to fill in placeholder text

usethis::use_git(message = "Initial commit")
usethis::use_github()


write_dockerfile(maintainer = "Flor")
# To write a Dockerfile. It will automatically pick the date of the last
# modified file, match it to that version of R and add it here. You can
# override this by passing r_date to some arbitrary date
# (but one for which a R version exists).

generate_badge() # This generates a badge for your readme.

# ----------------------------------------------
# At this time 🙌 push the code to GitHub 🙌
# ----------------------------------------------

# And click on the badge or use the function below to get the build
# ready ahead of time.
build_binder()
# 🤞🚀
