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

bugDF = fread(file.path('data', 'known_bugs.csv'))[(!mirrored)]

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
  if (length(label) == 1L && is.character(label))
    return(if (nzchar(label)) update_label(label) else invisible())
  stop("Unexpected input at ",flag," : ", toString(x))
}

# account for the trailing case when fewer than MAX_BUGS_TO_READ are left
for (bug_i in seq_len(nrow(head(bugDF, MAX_BUGS_TO_READ)))) {
  # ---- 1. EXTRACT BUG DATA FROM PAGE ----
  bug_id = bugDF$bug_id[bug_i]
  BUG_URL = sprintf(BUG_URL_FMT, bug_id)

  bug_page = jump_to(session, BUG_URL)

  # column of metadata on the LHS; filtering down first to this sub-node
  #   speeds up the repeated queries of its children substantially [~4x]
  meta_table = bug_page %>% html_node(xpath = '//td[@id="bz_show_bug_column_1"]')
  time_table = bug_page %>% html_node(xpath = '//td[@id="bz_show_bug_column_2"]')
  attachments = bug_page %>% html_node(xpath = '//table[@id="attachment_table"]') %>%
    html_nodes(xpath = './/tr[number(substring-after(@id, "a")) > 0]')
  comments = bug_page %>% html_nodes(xpath = '//div[@id and contains(@class, "bz_comment")]')

  bug = list(
    id = bug_id,
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
      url = html_attr(link, 'href')

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

    comment_info = lapply(comments, function(c) {
      list(
        id = html_attr(c, "id"),
        author = html_node_clean(c, './/span[@class="bz_comment_user"]'),
        timestamp = html_node_clean(c, './/span[@class="bz_comment_time"]'),
        text = html_node(c, xpath = './/pre[@class="bz_comment_text"]') %>% html_text,
        # there are two <a>, one to "edit"; the one with name="..." is what we want
        attachment_title = html_node(c, xpath = './/pre[@class="bz_comment_text"]//a[@name]') %>%
          html_attr('name')
      )
    })
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
