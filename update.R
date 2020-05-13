# INCREMENTAL UPDATES OF ISSUES [[ONLINE]]
URL = 'https://bugs.r-project.org'

source('utils.R')
library(data.table)
library(gh)

check_credentials()
session = bugzilla_session(URL)

bug_file   = file.path('data', 'known_bugs.csv')
label_file = file.path('data', 'labels.csv')
last_exec_date_file = file.path('data', 'last_exec.timestamp')

bugDF   = fread(bug_file, colClasses = c(github_id = 'integer'))
labelDF =  fread(label_file)

# find when this script was last run -- initially, check the most recent issue
#   creation timestamp [could also use update times, which would e.g. capture
#   when comments were added, but could also be mixed with some manual work
#   being done on the repo, e.g. with adding labels ex-post; creation times are
#   "purer" in that I am much more loath to do manual creation of issues]
if (file.exists(last_exec_date)) {
  recent_date = as.IDate(readLines(last_exec_date_file))
} else {
  recent_issues = gh(
    "GET /repos/:owner/:repo/issues",
    owner = OWNER, repo = REPO, state = 'all',
    sort = 'created', direction = 'desc'
  )
  recent_date = as.IDate(as.POSIXct(
    recent_issues[[1L]]$created_at,
    format = '%FT%TZ', tz = 'UTC'
  ))
}

# built into slightly-more-readable format from the advanced search page results...
updated_bug_url = paste0(
  file.path(URL, 'bugzilla', 'buglist.cgi?'),
  paste(
    collapse = '&',
    c(
      paste(
        sep = '=',
        'chfield',
        sapply(FUN = URLencode, reserved = TRUE, USE.NAMES = FALSE, c(
          "[Bug creation]", "assigned_to", "bug_file_loc", "bug_severity",
          "bug_status", "cclist_accessible", "component", "deadline",
          "everconfirmed", "op_sys", "priority", "product", "qa_contact",
          "rep_platform", "reporter_accessible", "resolution", "short_desc",
          "status_whiteboard", "target_milestone", "version", "votes"
        ))
      ),
      format(recent_date, 'chfieldfrom=%F'),
      'chfieldto=Now', 'query_format=advanced'
    )
  )
)

# multiple links to each bug in each search result row -- we _could_
#   test that the text() is a number: string(number(text())) != "NaN" --
#   but this is overkill for a small set of results. Just use unique & move on
updated_bug_paths = jump_to(session, updated_bug_url) %>%
  html_nodes(xpath = '//a[contains(@href, "show_bug.cgi")]') %>%
  html_attr('href') %>% unique

for (updated_bug_path in updated_bug_paths) {
  bz_id = as.integer(gsub('.*=', '', updated_bug_path))
  bug = get_bug_data(jump_to(session, file.path(URL, 'bugzilla', updated_bug_path)))

  # patch an existing issue
  # NB: skipping binary search approach here because this table grows within
  #   this script, so we'd have to re-key the table each time it grows. more
  #   straightforward to just use a vector search each time.
  if (length(gh_id <- bugDF[bugzilla_id == bz_id, github_id])) {
    # TODO: FINISH THIS -- write patch_issue, it should return the issue status
    #   and we cross with bug$status to determine if this needs to be closed on GH
    patch_issue(bug)

    n_old_comments = length(gh(
      "GET /repos/:owner/:repo/issues/:issue_number/comments",
      owner = OWNER, repo = REPO, issue_number = gh_id
    ))

    for (comment in tail(bug$comment_info, -n_old_comments)) {
      create_comment(comment, bug$attachment_info, gh_id)
    }

    #if (need to close and not yet closed) close_issue(gh_id)

  } else { # this bug is not yet tracked on GitHub -- handled same as in backfill.R
    bugDF = rbind(bugDF, data.table(bugzilla_id = bz_id), fill = TRUE)
    # ---- 2. UPDATE LABEL DATA ----
    for (tag in TAG_FIELDS) {
      validate_label_and_update(paste(tag, '-', bug[[tolower(tag)]]), bz_id)
    }
    # ---- 3. POST TO GITHUB AND RECORD GITHUB ID ----
    receipt = create_issue(bug)
    gh_id = as.integer(gsub('.*/([0-9]+)$', '\\1', receipt$url))
    bugDF[.(bugzilla_id = bz_id), on = 'bugzilla_id', github_id := gh_id]
    # ---- 4. POST COMMENTS ----
    for (comment in bug$comment_info) {
      create_comment(comment, bug$attachment_info, gh_id)
    }
    # ---- 5. CLOSE CLOSED ISSUES ----
    if (grepl('CLOSED', bug$status)) close_issue(gh_id)
  }
}

# update the last_exec_date for the next run
writeLines(as.integer(as.IDate(format(Sys.time(), '%F', tz = 'UTC'))), last_exec_date_file)
fwrite(bugDF, bug_file)
fwrite(labelDF, label_file)
