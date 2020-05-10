# BACK-FILLING BUG REPORTS INCREMENTALLY [[OFFLINE]]
URL = 'https://bugs.r-project.org'
BUG_URL_FMT = file.path(URL, 'bugzilla', 'show_bug.cgi?id=%d')
MAX_BUGS_TO_READ = 10L

source('utils.R')
library(gh)
library(data.table)

check_credentials()
session = bugzilla_session(URL)

bug_file = file.path('data', 'known_bugs.csv')
label_file = file.path('data', 'labels.csv')

# force colClasses for the initialization -- all-NA column is read as logical
bugDF = fread(bug_file, colClasses = c(github_id = 'integer'))[is.na(github_id)]

labelDF = if (file.exists(label_file)) {
  fread(label_file)
} else data.table(
  name = character(), color = character(),
  n_observed = integer(), seed_issues = character()
)

# account for the trailing case when fewer than MAX_BUGS_TO_READ are left
for (bug_i in seq_len(nrow(head(bugDF, MAX_BUGS_TO_READ)))) {
  # ---- 1. EXTRACT BUG DATA FROM PAGE ----
  bz_id = bugDF$bugzilla_id[bug_i]
  bug = get_bug_data(jump_to(session, sprintf(BUG_URL_FMT, bz_id)))

  # ---- 2. UPDATE LABEL DATA ----
  for (tag in TAG_FIELDS) {
    validate_label_and_update(paste(tag, '-', bug[[tolower(tag)]]), bz_id)
  }

  # ---- 3. POST TO GITHUB AND RECORD GITHUB ID ----
  receipt = create_issue(bug)
  this_issue = as.integer(gsub('.*/([0-9]+)$', '\\1', receipt$url))
  bugDF[.(bugzilla_id = bz_id), on = 'bugzilla_id', github_id := this_issue]

  # ---- 4. POST COMMENTS ----
  for (comment in bug$comment_info) {
    create_comment(comment, bug$attachment_info, this_issue)
  }

  # ---- 5. CLOSE CLOSED ISSUES ----
  if (grepl('CLOSED', bug$status)) close_issue(this_issue)
}

fwrite(bugDF, bug_file)
fwrite(labelDF, label_file)
