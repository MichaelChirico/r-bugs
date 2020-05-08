# INITIALIZATION SCRIPT FOR THE REPO [[OFFLINE]]
URL = 'https://bugs.r-project.org'
FULL_BUGLIST_PATH = file.path(
  URL, 'bugzilla', 'buglist.cgi?limit=0'
)

source('utils.R')

session = bugzilla_session(URL)
# returns all 7000+ bug reports! heavy on the r-project.org servers.
#   !!  avoid doing this step as much as possible, or adjust   !!
#   !!  the limit= query parameter in this URL during testing  !!
bugs = jump_to(session, FULL_BUGLIST_PATH)

# two links to each bug -- one in ID column, the other in Summary;
#   filtering on td selects the ID column. unique should then be
#   unnecessary, but it can't hurt
bug_ids = bugs %>%
  html_nodes(xpath = '//td[contains(@class, "first-child")]/a/@href') %>%
  html_text %>% gsub('show_bug.cgi?id=', '', . , fixed = TRUE) %>%
  as.integer %>% unique %>% sort

bugDF = data.frame(
  bug_id = bug_ids,
  mirrored = FALSE
)

# now checkout to data/known_bugs.csv & pass to backfill.R
dir.create('data', showWarnings = FALSE)
write.csv(
  bugDF, file.path('data', 'known_bugs.csv'),
  row.names = FALSE, quote = FALSE
)
