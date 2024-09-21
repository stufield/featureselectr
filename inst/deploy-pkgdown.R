# install dev version of pkgdown into a temp library
# install current version of pkg and update .libPaths()
# then deploy the pkgdown website to that remote branch
# clean up and delete ephemeral directories/branches
# -----------------------------
# Author: Stu Field
# -----------------------------
dev_lib <- tempfile("lib-")
dir.create(dev_lib)
remotes::install_dev("pkgdown", lib = dev_lib)
invisible(
  processx::run("R", 
                c("CMD", "INSTALL", "--use-vanilla", paste0("--library=", dev_lib), "."),
                echo_cmd = TRUE, echo = FALSE, error_on_status = TRUE
  )
)
.libPaths(c(dev_lib, .libPaths()))

library(pkgdown)
library(withr)

git <- function(...) {
  processx::run("git", c(...), echo_cmd = TRUE, echo = FALSE, error_on_status = TRUE)
}

deploy2branch <- function(dir, branch, remote = "origin") {
  # enable if on remote machine with CI
  #git("remote", "set-branches", remote, branch)
  git("fetch", remote, branch)
  git("worktree", "add", "--track", "-B", branch, dir, paste0(remote, "/", branch))
  defer(git("branch", "-D", branch))      # rm local branch (last)
  defer(git("worktree", "remove", dir))   # rm worktree (first)
  pkg <- as_pkgdown(".", override = list(destination = dir))
  commit_sha <- git("rev-parse", "HEAD")$stdout
  commit_message <- sprintf("Built site for %s: %s@%s", pkg$package, pkg$version,
                            substr(commit_sha, 1, 7))
  clean_site(pkg)
  build_site(pkg, devel = FALSE, preview = FALSE, install = FALSE, new_process = FALSE)
  with_dir(dir, {
    git("add", "-A", ".")
    git("commit", "--allow-empty", "-m", commit_message)
    writeLines(paste0("* Deploying to Bitbucket '", remote, ":", branch, "'"))
    git("remote", "-v")
    git("push", "--force", remote, paste0("HEAD:", branch))
  })
  invisible(NULL)
}

deploy2branch(dir = tempfile("pkgdown-"), branch = "bb-pkgdown")
unlink(dev_lib, recursive = TRUE, force = TRUE)
