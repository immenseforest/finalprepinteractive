test_store <- file.path(
  tempdir(),
  paste0("finalprep-community-test-", format(Sys.time(), "%Y%m%d%H%M%S"))
)
dir.create(test_store, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(FINALPREP_COMMENT_DIR = test_store)

source("app.R")
community_comments(empty_community_comments())
community_active_users(0L)

shiny::testServer(server, {
  stopifnot(isolate(community_active_users()) == 1L)

  session$setInputs(
    comment_handle = "SafeLearner",
    comment_vote = "down",
    comment_body = "<script>alert('unsafe')</script> More worked algebra would help."
  )
  session$setInputs(submit_comment = 1)
  session$flushReact()

  saved <- isolate(community_comments())
  stopifnot(nrow(saved) == 1)
  stopifnot(saved$handle[[1]] == "SafeLearner")
  stopifnot(saved$vote[[1]] == "down")
  stopifnot(file.exists(comment_store_path))

  history_html <- paste(as.character(output$community_comment_history), collapse = "")
  stopifnot(!grepl("<script>alert", history_html, fixed = TRUE))
  stopifnot(grepl("&lt;script&gt;", history_html, fixed = TRUE))
  stopifnot(grepl("Thumbs down", history_html, fixed = TRUE))
})

unlink(test_store, recursive = TRUE, force = TRUE)
cat("Community submission, persistence, live count, and HTML escaping passed.\n")
