# INITIALIZATION SCRIPT FOR THE REPO [[OFFLINE]]
URL = 'https://bugs.r-project.org/bugzilla'
FULL_BUGLIST_PATH = file.path(URL, 'buglist.cgi?limit=0')

source('utils.R')
library(data.table)

check_credentials()
session = bugzilla_session(URL)
# returns all 7000+ bug reports! heavy on the r-project.org servers.
#   !!  avoid doing this step as much as possible, or adjust   !!
#   !!  the limit= query parameter in this URL during testing  !!
bugs = jump_to(session, FULL_BUGLIST_PATH)
# record when this was done -- we know the universe of reported bugs
#   up through this date (a timestamp would be more exact...)
writeLines(
  format(Sys.time(), '%F', tz = 'UTC'),
  file.path('data', 'last_exec.date')
)

# two links to each bug -- one in ID column, the other in Summary;
#   filtering on td selects the ID column. unique should then be
#   unnecessary, but it can't hurt
bug_ids = bugs %>%
  html_nodes(xpath = '//td[contains(@class, "first-child")]/a/@href') %>%
  html_text %>% gsub('show_bug.cgi?id=', '', . , fixed = TRUE) %>%
  as.integer %>% unique %>% sort

bugDF = data.table(
  bugzilla_id = bug_ids,
  github_id = NA_integer_
)

# now checkout to data/known_bugs.csv & pass to backfill.R
dir.create('data', showWarnings = FALSE)
fwrite(bugDF, file.path('data', 'known_bugs.csv'))
