# BACK-FILLING BUG REPORTS INCREMENTALLY [[OFFLINE]]
URL = 'https://bugs.r-project.org/bugzilla'
BUG_URL_STEM = file.path(URL, 'show_bug.cgi?id=')
MAX_BUGS_TO_READ = 500L

source('utils.R')
library(gh)
library(data.table)

options(warn = 2L)
check_credentials()
session = bugzilla_session(URL)

bug_file = file.path('data', 'known_bugs.csv')
label_file = file.path('data', 'labels.csv')

# force colClasses for the initialization -- all-NA column is read as logical
# NB: at the end, we fwrite directly, so we have to read the whole file, then
#     overwrite again with the full file
bugDF = fread(bug_file, colClasses = c(github_id = 'integer'))

labelDF = if (file.exists(label_file)) {
  fread(label_file)
} else data.table(
  name = character(), color = character(),
  n_observed = integer(), seed_issues = character()
)

# head: account for the trailing case when fewer than MAX_BUGS_TO_READ are left
to_process = head(bugDF[is.na(github_id), which = TRUE], MAX_BUGS_TO_READ)
for (ii in seq_along(to_process)) {
  bug_i = to_process[ii]
  # ---- 1. EXTRACT BUG DATA FROM PAGE ----
  bz_id = bugDF$bugzilla_id[bug_i]
  cat('\rProcessing Bugzilla #', bz_id, ', ', length(to_process)-ii, ' to go    ', sep = '')
  bug = get_bug_data(jump_to(session, paste0(BUG_URL_STEM, bz_id)))

  # ---- 2. UPDATE LABEL DATA ----
  labels = get_labels(bug, TAG_FIELDS)
  for (label in labels) validate_label_and_update(label, bz_id)

  # ---- 3. POST TO GITHUB AND RECORD GITHUB ID ----
  receipt = create_issue(bug)
  gh_id = as.integer(receipt$number)
  bugDF[.(bugzilla_id = bz_id), on = 'bugzilla_id', github_id := gh_id]

  # ---- 4. POST COMMENTS ----
  for (comment in bug$comment_info) {
    create_comment(comment, bug$attachment_info, gh_id)
  }

  # ---- 5. CLOSE CLOSED ISSUES ----
  if (grepl('CLOSED', bug$status)) close_issue(gh_id)
}

fwrite(bugDF, bug_file)
fwrite(labelDF, label_file)
