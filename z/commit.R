# commit.R

# This script commits to github.
require(gert)

gert::git_add(dir(all.files = TRUE))
gert::git_commit_all(message = "added folders")
gert::git_push()
