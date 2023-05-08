# commit.R

# This script commits to github.
require(gert)

gert::git_add(dir(all.files = TRUE))
gert::git_commit_all(message = "--fix commit issue--")
gert::git_push()

#gert::git_reset_soft(ref = "42fd2171")


