# INCREMENTAL UPDATES OF ISSUES [[ONLINE]]
URL = 'https://bugs.r-project.org/bugzilla'
BUG_URL_STEM = file.path(URL, 'show_bug.cgi?id=')

source('utils.R')
library(data.table)
library(gh)

options(warn = 2L)
check_credentials()
session = bugzilla_session(URL)

bug_file   = file.path('data', 'known_bugs.csv')
label_file = file.path('data', 'labels.csv')
last_update_time_file = file.path('data', 'last_update.time')

bugDF   = fread(bug_file, colClasses = c(github_id = 'integer'))
labelDF =  fread(label_file)

# this should be set during initialize.R, and is mostly correct assuming the
#   order of events (1) initialize (2) backfill (3) update.
# there will be some double counting of updated bugs which were updated before
#   backfill reached them, but after initialize is run, but the issue tracker
#   isn't massively active, so these edge cases are minimal.
recent_timestamp = .POSIXct(as.integer(readLines(last_update_time_file)), tz = 'UTC')

# built into slightly-more-readable format from the advanced search page results...
#   NB: earlier, tried building a query URL based on one produced using
#   the "Search By Change History" feature of Bugzilla Search page, but this
#   was missing some results. I think the following query includes new comments
#   in the change timestamp, whereas the previous one does not (and I don't see a way)
statuses = c(
  'UNCONFIRMED', 'NEW', 'WISH', 'ASSIGNED',
  'REOPENED', 'RESOLVED', 'VERIFIED', 'CLOSED'
)
updated_bug_url = paste0(
  file.path(URL, 'buglist.cgi?'),
  paste(collapse = '&', c(
    paste0('bug_status=', statuses),
    'order=bugs.delta_ts%20desc',
    'query_format=advanced'
  ))
)

# multiple links to each bug in each search result row -- we _could_
#   test that the text() is a number: string(number(text())) != "NaN" --
#   but this is overkill for a small set of results. Just use unique & move on
#   tr[td] -> exclude header row, which doesn't have td children
results = jump_to(session, updated_bug_url) %>%
  html_nodes(xpath = '//table[@class="bz_buglist"]/tr[td]')
update_times = results %>%
  html_node(xpath = 'td[@class="bz_changeddate_column nowrap"]') %>%
  html_text_clean

update_timestamps = .POSIXct(rep(NA_real_, length(update_times)), tz = 'UTC')
for (fmt in c('%T', '%a %H:%M', '%F')) {
  unmatched_idx = is.na(update_timestamps)
  # as.POSIXct doesn't infer the correct date here (it does for the other fmts)
  if (fmt == '%a %H:%M') {
    # keep this so as not to update the original
    tmp = update_times[unmatched_idx]
    # find those times that match the %a %H:%M format
    match_dow = !is.na(as.POSIXct(tmp, format = fmt, tz = 'UTC'))
    if (!any(match_dow)) next
    prev_week = as.Date(format(Sys.time(), tz = 'UTC')) - 0:6
    dowDT = data.table(dow = format(prev_week, '%a'), YMD = format(prev_week))
    for (ii in seq_len(nrow(dowDT))) {
      tmp[match_dow] = with(dowDT[ii], gsub(dow, YMD, tmp[match_dow], fixed = TRUE))
    }
    timestamp = as.POSIXct(tmp, '%F %H:%M', tz = 'UTC')
  } else {
    timestamp = as.POSIXct(update_times[unmatched_idx], format = fmt, tz = 'UTC')
  }
  update_timestamps[unmatched_idx] = timestamp
}

updated_bugs = data.table(
  path = results %>%
    html_nodes(xpath = './td[contains(@class, "bz_id_column")]/a') %>%
    html_attr('href'),
  time = update_timestamps
)

# process in chronological order
updated_bugs = updated_bugs[time > recent_timestamp][order(time)]

for (ii in seq_len(nrow(updated_bugs))) {
  updated_bug_path = updated_bugs[ii, path]
  bz_id = as.integer(gsub('.*=', '', updated_bug_path))
  cat('\rProcessing Bugzilla #', bz_id, ', ', nrow(updated_bugs)-ii, ' to go    ', sep = '')
  bug = get_bug_data(jump_to(session, file.path(URL, updated_bug_path)))

  # ---- 2. UPDATE LABEL DATA ----
  # don't check if labels have changed -- just PATCH all labels at once.
  #   this greedy option will lead to the n_observed field being different
  #   from "# of issues with this label" [e.g. as bugs migrate from
  #   Status - NEW to Status - CLOSED FIXED, etc.], which is not ideal, but
  #   it skirts around the can of worms of a "diff"-based approach,
  #   as the logic for adding labels was built for the backfill phase --
  #   if the issue log clears, Status - NEW labels will reach n_observed<10
  #   eventually -- this will cause an error when that label attempts to
  #   create itself again if the number bubbles up again.
  # be sure to get old_labels _before_ patching
  labels = get_labels(bug, TAG_FIELDS)
  for (label in labels) validate_label_and_update(label, bz_id)

  comments = bug$comment_info

  # patch an existing issue
  # NB: skipping binary search approach here because this table grows within
  #   this script, so we'd have to re-key the table each time it grows. more
  #   straightforward to just use a vector search each time.
  if (length(gh_id <- bugDF[bugzilla_id == bz_id, github_id])) {
    # ---- 3aI. PATCH ISSUE STATUS, ISSUE LABELS ----
    # only bother with this API hit if we might need to update the bug status
    if (grepl('CLOSED', bug$status)) {
      old_issue = gh(
        "GET /repos/:owner/:repo/issues/:issue_number",
        owner = OWNER, repo = REPO, issue_number = gh_id
      )
      old_labels = sapply(old_issue$labels, `[[`, 'name')
      old_status = grep('^Status', old_labels, value = TRUE)

      if (old_status != labels['Status']) close_issue(gh_id)
    }

    patch_labels(gh_id, labels)

    # ---- 3aII. REMOVE OLD COMMENTS ----
    n_old_comments = length(gh(
      "GET /repos/:owner/:repo/issues/:issue_number/comments",
      owner = OWNER, repo = REPO, issue_number = gh_id
    ))

    if (n_old_comments > 0L) comments = tail(comments, -n_old_comments)
  } else { # this bug is not yet tracked on GitHub -- handled same as in backfill.R
    bugDF = rbind(bugDF, data.table(bugzilla_id = bz_id), fill = TRUE)

    # ---- 3bI. POST TO GITHUB AND RECORD GITHUB ID ----
    receipt = create_issue(bug)
    gh_id = as.integer(receipt$number)
    bugDF[.(bugzilla_id = bz_id), on = 'bugzilla_id', github_id := gh_id]

    # ---- 3bII. CLOSE CLOSED ISSUES ----
    if (grepl('CLOSED', bug$status)) close_issue(gh_id)
  }

  # ---- 4. POST COMMENTS ----
  for (comment in comments) {
    create_comment(comment, bug$attachment_info, gh_id)
  }
}

# update the last_exec_date for the next run
writeLines(as.character(as.integer(max(update_timestamps))), last_update_time_file)
fwrite(bugDF, bug_file)
fwrite(labelDF, label_file)

# re-trigger interaction limits for the repo (easiest way to keep
#   the repo read-only that I know of...)
gh("PUT /repos/:owner/:repo/interaction-limits",
   owner = OWNER, repo = REPO, limit = 'collaborators_only',
   .accept = 'application/vnd.github.sombra-preview'
)
