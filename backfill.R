# BACK-FILLING BUG REPORTS INCREMENTALLY [[OFFLINE]]
URL = 'https://bugs.r-project.org'
BUG_URL_FMT = file.path(URL, 'bugzilla', 'show_bug.cgi?id=%d')
MAX_BUGS_TO_READ = 500L

source('utils.R')

session = bugzilla_session(URL)

bugDF = read.csv(file.path('data', 'known_bugs.csv'))
bugDF = subset(bugDF, !mirrored)

# account for the trailing case when fewer than MAX_BUGS_TO_READ are left
for (bug_i in seq_len(nrow(head(bugDF, MAX_BUGS_TO_READ)))) {
  BUG_URL = sprintf(BUG_URL_FMT, bugDF$bug_id[bug_i])

  bug_page = jump_to(session, BUG_URL)

  bug = list(
    summary = bug_page %>% html_node('span#short_desc_nonedit_display') %>% html_text_clean,
    status = bug_page %>% html_node('span#static_bug_status') %>% html_text_clean,
    alias = bug_page %>% html_node(xpath = '//tr[@id="field_tablerow_alias"]/td') %>% html_text_clean
  )

}