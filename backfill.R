# BACK-FILLING BUG REPORTS INCREMENTALLY [[OFFLINE]]
URL = 'https://bugs.r-project.org'
BUG_URL_FMT = file.path(URL, 'bugzilla', 'show_bug.cgi?id=%d')
MAX_BUGS_TO_READ = 5L
OWNER = 'MichaelChirico'
REPO = 'r-bugs'

source('utils.R')
library(gh)
library(data.table)

check_credentials()
session = bugzilla_session(URL)

bug_file = file.path('data', 'known_bugs.csv')
label_file = file.path('data', 'labels.csv')

# force colClasses for the initialization -- all-NA column is read as logical
bugDF = fread(bug_file, colClasses = c(github_id = 'integer'))[is.na(github_id)]

# ---- FUNCTIONS FOR WORKING WITH LABELS ----
labelDF = if (file.exists(label_file)) {
  fread(label_file)
} else data.table(
  name = character(), color = character(),
  n_observed = integer(), seed_issues = character()
)
# check if we've seen this label. if not, create the label with a random color.
#   If so, update its observation count. Once a label reaches 10 times observed,
#   it gets POSTed to GitHub.
update_label = function(label, bz_id) {
  # this label is known; update its frequency
  if (label %chin% labelDF$name) {
    labelDF[.(name = label), on = 'name', c('n_observed', 'seed_issues') := {
      if (n_observed < 10L) {
        if (n_observed < 9L) {
          seed_issues = paste0(seed_issues, ',', bz_id)
        } else {
          # PROBLEM -- the first 9 instances of this label won't have it
          #  assigned to the
          gh('POST /repos/:owner/:repo/labels',
             owner = OWNER, repo = REPO,
             name = name, color = color
          )
          for (seed_issue in strsplit(seed_issues, ',')[[1L]]) {
            gh_id = bugDF[.(bugzilla_id = as.integer(seed_issue)), on = 'bugzilla_id', github_id]
            if (is.na(gh_id)) next
            # PATCH will overwrite, so we need to read current issues first & append
            issue_data = gh(
              "GET /repos/:owner/:repo/issues/:issue_number",
              owner = OWNER, repo = REPO,
              issue_number = gh_id
            )

            gh(
              "PATCH /repos/:owner/:repo/issues/:issue_number",
              owner = OWNER, repo = REPO,
              issue_number = gh_id,
              labels = c(sapply(issue_data$labels, `[[`, 'name'), label)
            )
          }
        }
      }
      .(n_observed + 1L, seed_issues)
    }]
  } else {
    labelDF <<- rbind(labelDF,
      data.table(name = label, color = rand_color(),
                 n_observed = 1L, seed_issues = as.character(bz_id))
    )
  }
}
validate_label_and_update = function(label, bz_id, flag) {
  force_scalar_character(label)
  return(if (nzchar(label)) update_label(label, bz_id) else invisible())
}

# ---- FUNCTIONS FOR DEALING WITH ISSUES ----
# map Bugzilla IDs to GitHub IDs when possible
add_github_links = function(bz_ids) {
  gh_ids = bugDF[.(bugzilla_id = bz_ids), on = 'bugzilla_id', github_id]

  fifelse(
    is.na(gh_ids),
    paste0('Bugzilla #', bz_ids),
    sprintf('[Bugzilla #%d](#%d)', bz_ids, gh_ids)
  )
}
BODY_TEMPLATE = "%s

------------------

#### METADATA

 - Bug author - %s
 - Creation time - %s
 - [Bugzilla link](%s)
 - Status - %s
 - Alias - %s
 - Component - %s
 - Version - %s
 - Hardware - %s
 - Importance - %s
 - Assignee - %s
 - URL - %s%s%s
"
RELATED_ISSUES_TEMPLATE = "

#### RELATED ISSUES

%s%s"
ATTACHMENT_TEMPLATE = "

#### INCLUDED PATCH

 - ID - %d
 - Author - %s
 - Link to download patch - %s
 - Timestamp - %s
 - Extra info - %s"
COMMENT_TEMPLATE = "%s

-----------

#### METADATA
 - Comment author - %s
 - Timestamp - %s%s
"

build_attachment_txt = function(title, info) {
  if (is.null(title) || is.na(title)) return('')
  with(
    info[[which(sapply(info, `[[`, 'comment_anchor') == title)]],
    sprintf(ATTACHMENT_TEMPLATE, id, author, url, timestamp, extra_info)
  )
}
build_body = function(params) {
  force_scalar_character(params$description_info$text, allow_empty = TRUE)
  force_scalar_character(params$description_info$author)
  force_scalar_character(params$description_info$timestamp)
  force_scalar_character(params$modified_info)

  if (!with(params, grepl(modified_info, reported_info, fixed = TRUE))) {
    modified_txt = paste("\n - Modification time -", params$modified_info)
  } else modified_txt = ""

  # NB: it is not always possible to link the corresponding GitHub issue --
  #   e.g. Bug 1 on Bugzilla references 15763,15764,15862 -- presumably
  #   this was added at the Modified date. I suppose it's possible to iterate
  #   through the bugs in ascending Modified time order as a way to prevent
  #   "future" bugs from being referenced in "earlier" bug reports, but I'm
  #   skeptical of investing effort here. I guess this is sufficiently rare
  #   that it can simply be added manually for less effort, and the
  #   monitor-and-update script can handle this after "backfilling" is done
  if (length(params$depends_on) || length(params$blocks)) {
    if (length(params$depends_on)) {
      depends_txt = paste(
        '## Depends on', toString(add_github_links(params$depends_on)), '\n'
      )
    } else depends_txt = ''

    if (length(params$blocks)) {
      blocks_txt = paste(
        '## Blocks', toString(add_github_links(params$blocks)), '\n'
      )
    } else blocks_txt = ''
    related_txt = sprintf(RELATED_ISSUES_TEMPLATE, depends_txt, blocks_txt)
  } else related_txt = ''

  attachment_txt = build_attachment_txt(
    params$description_info$attachment_title,
    params$attachment_info
  )

  sprintf(BODY_TEMPLATE,
    params$description_info$text,
    params$description_info$author,
    params$description_info$timestamp,
    sprintf(BUG_URL_FMT, params$id),
    params$status,
    params$alias,
    params$component,
    params$version,
    params$hardware,
    params$importance,
    params$assignee,
    params$url,
    modified_txt,
    related_txt,
    attachment_txt
  )
}

# account for the trailing case when fewer than MAX_BUGS_TO_READ are left
for (bug_i in seq_len(nrow(head(bugDF, MAX_BUGS_TO_READ)))) {
  # ---- 1. EXTRACT BUG DATA FROM PAGE ----
  bz_id = bugDF$bugzilla_id[bug_i]
  BUG_URL = sprintf(BUG_URL_FMT, bz_id)

  bug_page = jump_to(session, BUG_URL)

  # column of metadata on the LHS; filtering down first to this sub-node
  #   speeds up the repeated queries of its children substantially [~4x]
  meta_table = bug_page %>% html_node(xpath = '//td[@id="bz_show_bug_column_1"]')
  time_table = bug_page %>% html_node(xpath = '//td[@id="bz_show_bug_column_2"]')
  attachments = bug_page %>% html_node(xpath = '//table[@id="attachment_table"]') %>%
    html_nodes(xpath = './/tr[number(substring-after(@id, "a")) > 0]')
  comments = bug_page %>% html_nodes(xpath = '//div[@id and contains(@class, "bz_comment")]')
  get_comment_info = function(c) {
    list(
      id = html_attr(c, "id"),
      author = html_node_clean(c, './/span[@class="bz_comment_user"]'),
      timestamp = html_node_clean(c, './/span[@class="bz_comment_time"]'),
      text = html_node(c, xpath = './/pre[@class="bz_comment_text"]') %>%
        html_text %>% censor_email,
      # there are two <a>, one to "edit"; the one with name="..." is what we want
      attachment_title = html_node(c, xpath = './/pre[@class="bz_comment_text"]//a[@name]') %>%
        html_attr('name')
    )
  }

  bug = list(
    id = bz_id,
    summary = get_field(bug_page, 'span', 'short_desc_nonedit_display') %>% censor_email,

    status = get_field(meta_table, 'span', 'static_bug_status'),
    alias = get_field(meta_table, 'tr/td', 'field_tablerow_alias'),
    component = get_field(meta_table, 'td', 'field_container_component', clean = FALSE) %>%
      gsub('(show other bugs)', '', . , fixed = TRUE) %>% clean,
    version = get_field(meta_table, 'tr/td', 'field_tablerow_version'),
    hardware = get_field(meta_table, 'tr/td', 'field_tablerow_rep_platform'),
    importance = get_field(meta_table, 'tr/td', 'field_tablerow_importance'),
    assignee = get_field(meta_table, 'tr/td', 'field_tablerow_assigned_to'),
    url = get_field(meta_table, 'span', 'bz_url_input_area'),
    depends_on = get_field(meta_table, 'tr/td', 'field_tablerow_dependson', node_only = TRUE) %>%
      html_nodes_clean(xpath = './a') %>% as.integer,
    blocks = html_node(meta_table, xpath = './/tr/th[@id="field_label_blocked"]') %>%
      html_nodes_clean(xpath = './parent::node()/td/a') %>% as.integer,

    reported_info = get_field(time_table, 'tr/td', 'field_tablerow_reported'),
    modified_info = get_field(time_table, 'tr/td', 'field_tablerow_modified', clean = FALSE) %>%
      gsub('(History)', '', ., fixed = TRUE) %>% clean,

    attachment_info = lapply(attachments, function(a) {
      link = html_node(
        a, xpath = './/a[contains(@href, "cgi") and not(contains(@href, "action=edit"))]'
      )
      url = file.path(URL, 'bugzilla', html_attr(link, 'href'))

      meta = html_node(a, xpath = './/span[@class="bz_attach_extra_info"]')
      list(
        title = html_text_clean(link),
        url = url, id = as.integer(gsub('.*([0-9]+)$', '\\1', url)),
        author = html_node_clean(meta, './span[@class="vcard"]'),
        timestamp = html_node_clean(meta, './a[contains(@title, "Go to")]'),
        comment_anchor = html_node(meta, xpath = './a[contains(@title, "Go to")]') %>%
          html_attr('href') %>% gsub('^#', '', . ),
        # text is between parent node & first tag <br>
        extra_info = html_node_clean(meta, './br/preceding-sibling::text()')
      )
    }),

    description_info = get_comment_info(comments[[1L]]),
    comment_info = lapply(comments[-1L], get_comment_info)
  )

  # ---- 2. UPDATE LABEL DATA ----
  validate_label_and_update(bug$component, bz_id)
  validate_label_and_update(bug$version, bz_id)
  validate_label_and_update(bug$hardware, bz_id)
  validate_label_and_update(bug$importance, bz_id)

  # ---- 3. POST TO GITHUB AND RECORD GITHUB ID ----
  gh_call = list(
    endpoint = "POST /repos/:owner/:repo/issues",
    owner = OWNER, repo = REPO,
    title = sprintf("[BUGZILLA #%d] %s", bug$id, bug$summary),
    body = build_body(bug)
  )
  labels = labelDF[
    i = .(name = with(bug, c(component, version, hardware, importance))),
    on = 'name', name[n_observed >= 10L]
  ]
  if (length(labels)) gh_call$labels
  receipt = do.call(gh, gh_call)
  this_issue = as.integer(gsub('.*([0-9]+)$', '\\1', receipt$url))
  bugDF[.(bugzilla_id = bz_id), on = 'bugzilla_id', github_id := this_issue]

  # ---- 4. POST COMMENTS ----
  for (comment in bug$comment_info) {
    gh(
      "POST /repos/:owner/:repo/issues/:issue_number/comments",
      owner = OWNER, repo = REPO,
      issue_number = this_issue,
      body = sprintf(COMMENT_TEMPLATE,
        comment$text, comment$author, comment$timestamp,
        build_attachment_txt(comment$attachment_title, bug$attachment_info)
      )
    )
  }

  # ---- 5. CLOSE CLOSED ISSUES ----
  if (grepl('CLOSED', bug$status)) {
    gh(
      "PATCH /repos/:owner/:repo/issues/:issue_number",
      owner = OWNER, repo = REPO,
      issue_number = this_issue, state = 'closed'
    )
  }
}

fwrite(bugDF, bug_file)
fwrite(labelDF, label_file)
