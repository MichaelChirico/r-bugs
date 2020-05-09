# BACK-FILLING BUG REPORTS INCREMENTALLY [[OFFLINE]]
URL = 'https://bugs.r-project.org'
BUG_URL_FMT = file.path(URL, 'bugzilla', 'show_bug.cgi?id=%d')
MAX_BUGS_TO_READ = 500L
OWNER = 'MichaelChirico'
REPO = 'r-bugs'

source('utils.R')
library(gh)
library(data.table)

session = bugzilla_session(URL)

bugDF = fread(file.path('data', 'known_bugs.csv'))[!is.na(github_id)]

# ---- FUNCTIONS FOR WORKING WITH LABELS ----
labelDF = if (file.exists(label_file <- file.path('data', 'labels.csv'))) {
  fread(label_file)
} else data.table(
  name = character(), color = character(), n_observed = integer()
)
# check if we've seen this label. if not, create the label with a random color.
#   If so, update its observation count. Once a label reaches 10 times observed,
#   it gets POSTed to GitHub.
update_label = function(label) {
  # this label is known; update its frequency
  if (label %chin% labelDF$name) {
    labelDF[.(name = label), on = 'name', n_observed := {
      if (n_observed == 9L) {
        gh('POST /repos/:owner/:repo/labels',
          owner = OWNER, repo = REPO,
          name = name, color = color
        )
      }
      n_observed + 1L
    }]
  } else {
    labelDF <<- rbind(labelDF,
      data.table(name = label, color = rand_color(), n_observed = 1L)
    )
  }
}
validate_label_and_update = function(label, flag) {
  force_scalar_charater(label)
  return(if (nzchar(label)) update_label(label) else invisible())
}

# ---- FUNCTIONS FOR DEALING WITH ISSUES ----
add_github_links = function(bz_ids, ...) {

}
BODY_TEMPLATE = "# DESCRIPTION

%s

# METADATA

 - Bug Author - %s
 - Creation Time - %s%s%s
"
RELATED_ISSUES_TEMPLATE = "

# RELATED ISSUES

%s%s"
ATTACHMENT_TEMPLATE = "

# INCLUDED PATCH

 - ID - %d
 - Author - %s
 - Link to download patch - %s
 - Timestamp - %s
 - Extra info - %s"
BODY_PARAMS = c(
  'description_info', 'attachment_info', 'reported_info', 'modified_info'
)

build_body = function(params) {
  if (!all(BODY_PARAMS %chin% names(params)))
    stop("Missing components required to build issue: ",
         toString(setdiff(BODY_PARAMS, names(params))))
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
      gh_ids = bugDF[.(bugzilla_id = params$depends_on), on = 'bugzilla_id', github_id]

      depends_txt = paste(
        '## Depends on',
        toString(fifelse(
          is.na(gh_ids),
          paste0('Bugzilla #', params$depends_on),
          sprintf('[Bugzilla #%d](#%d)', params$depends_on, gh_ids)
        ))
      )
    }
  }

  if (!is.null(description_info$attachment_title) &&
      !is.na(description_info$attachment_title)) {
    info = params$attachment_info[[
      which(sapply(params$attachment_info, `[[`, 'comment_anchor') ==
              params$description_info$attachment_title)
    ]]
    attachment_txt = with(info,
      sprintf(ATTACHMENT_TEMPLATE, id, author, url, timestamp, extra_info)
    )
  } else attachment_txt = ''

  sprintf(BODY_TEMPLATE,
    params$description_info$text,
    params$description_info$author,
    params$description_info$timestamp,
    modified_txt,
    attachment_txt
  )
}

# account for the trailing case when fewer than MAX_BUGS_TO_READ are left
for (bug_i in seq_len(nrow(head(bugDF, MAX_BUGS_TO_READ)))) {
  # ---- 1. EXTRACT BUG DATA FROM PAGE ----
  bugzilla_id = bugDF$bugzilla_id[bug_i]
  BUG_URL = sprintf(BUG_URL_FMT, bugzilla_id)

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
      text = html_node(c, xpath = './/pre[@class="bz_comment_text"]') %>% html_text,
      # there are two <a>, one to "edit"; the one with name="..." is what we want
      attachment_title = html_node(c, xpath = './/pre[@class="bz_comment_text"]//a[@name]') %>%
        html_attr('name')
    )
  }

  bug = list(
    id = bugzilla_id,
    summary = get_field(bug_page, 'span', 'short_desc_nonedit_display'),

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
  validate_label_and_update(bug$component)
  validate_label_and_update(bug$version)
  validate_label_and_update(bug$hardware)
  validate_label_and_update(bug$importance)

  # ---- 3. POST TO GITHUB ----
  gh(
    "POST /repos/:owner/:repo/issues",
    owner = OWNER, repo = REPO,
    title = sprintf("[BUGZILLA #%d] %s", bug$id, bug$summary),
    body = bug$comment_info[[1L]]$text
  )

}
