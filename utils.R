library(rvest)
library(data.table)
library(gh)

# --- MISCELLANEOUS UTILITIES ----
# random hex color (16777216 is 256^3=16^6)
rand_color = function() sprintf('%06X', sample(16777216L, 1L))

# quick-and-dirty proper capitalization
to_proper = function(x) gsub('\\b(.)', '\\U\\1', x, perl = TRUE)

force_scalar_character = function(x, allow_empty = TRUE) {
  if (!is.character(x)) stop("Input must be character")
  if (length(x) != 1L) stop("Input must be scalar [length-1]")
  if (!allow_empty && (is.na(x) || !nzchar(x))) stop("Input must be non-missing and non-empty")
  return(invisible())
}

# ---- SCRAPING UTILITIES ----
check_credentials = function() {
  creds = Sys.getenv(
    c('BUGZILLA_USER', 'BUGZILLA_PASSWORD', 'GITHUB_PAT', 'GITHUB_TOKEN'),
    unset = NA
  )
  if (anyNA(creds[1:2])) stop("Must set BUGZILLA_USER nd BUGZILLA_PASSWORD to continue")
  if (all(is.na(creds[3:4]))) stop("Must set GITHUB_PAT or GITHUB_TOKEN")
  if (is.na(creds[3L])) Sys.setenv(GITHUB_PAT = creds[4L])
  return(invisible())
}

# begin a session in bugzilla & log in
bugzilla_session = function(URL = 'https://bugs.r-project.org') {
  session = html_session(URL)

  # the login form at the top of the page behind "login" button
  login = Filter(
    function(x) x$name == 'mini_login_top',
    html_form(session)
  )

  u_p = Sys.getenv(c('BUGZILLA_USER', 'BUGZILLA_PASSWORD'))
  # form IDs by manual inspection
  submit_form(
    session,
    set_values(login[[1L]], Bugzilla_login = u_p[1L], Bugzilla_password = u_p[2L])
  )

  return(session)
}

# strip several kinds of whitespace
clean = function(x, replace_newline = TRUE, replace_nbsp = TRUE, collapse_ws = TRUE) {
  text = trimws(x)
  if (replace_newline) text = gsub('\n', ' ', text, fixed = TRUE)
  if (replace_nbsp) text = gsub(intToUtf8(160L), ' ', text, fixed = TRUE)
  if (collapse_ws) text = gsub('\\s{2,}', ' ', text)
  return(text)
}

# wrapper for html_text %>% clean
html_text_clean = function(x, replace_newline = TRUE, replace_nbsp = TRUE, collapse_ws = TRUE) {
  return(clean(html_text(x), replace_newline, replace_nbsp, collapse_ws))
}

# wrapper for html_node %>% html_text_clean
html_node_clean = function(x, xpath, ...)
  html_text_clean(html_node(x, xpath = xpath), ...)

# ditto, for html_nodes
html_nodes_clean = function(x, xpath, ...)
  html_text_clean(html_nodes(x, xpath = xpath), ...)

# common extractions:
#   <tr id='...'><td>desired text</td></tr>
#   <span|td id='...'>desired test</span|td>
# .// (not //) to be sure we are only search "below" page on the tree
get_field = function(page, type, id, clean = TRUE, ...) {
  out = switch(type,
    span = , td =
      html_node(page, xpath = sprintf('.//%s[@id="%s"]', type, id)),
    'tr/td' =
      html_node(page, xpath = sprintf('.//tr[@id="%s"]/td', id)),
    stop("Unrecognized 'type'")
  )
  if (clean) html_text_clean(out, ...) else html_text(out)
}

get_attachment_info = function(a) {
  link = html_node(a,
    xpath = './/a[contains(@href, "cgi") and not(contains(@href, "action=edit"))]'
  )
  url = file.path(URL, 'bugzilla', html_attr(link, 'href'))

  meta = html_node(a, xpath = './/span[@class="bz_attach_extra_info"]')
  list(
    title = html_text_clean(link),
    url = url, id = as.integer(gsub('.*([0-9]+)$', '\\1', url)),
    author = html_node_clean(meta, './span[@class="vcard"]'),
    timestamp = html_node_clean(meta, './a[contains(@title, "Go to")]'),
    comment_anchor = gsub(
      pattern = '^#', replacement = '',
      html_text(html_node(meta, xpath = './a[contains(@title, "Go to")]/@href'))
    ),
    # text is between parent node & first tag <br>
    extra_info = html_node_clean(meta, './br/preceding-sibling::text()')
  )
}

# helper for getting comment metadata from a page
get_comment_info = function(c) {
  list(
    id = html_attr(c, "id"),
    author = html_node_clean(c, './/span[@class="bz_comment_user"]'),
    timestamp = html_node_clean(c, './/span[@class="bz_comment_time"]'),
    text = censor_email(html_text(html_node(c, xpath = './/pre[@class="bz_comment_text"]'))),
    # there are two <a>, one to "edit"; the one with name="..." is what we want
    attachment_title = html_text(
      html_node(c, xpath = './/pre[@class="bz_comment_text"]//a/@name')
    )
  )
}

# workhorse scraping function
get_bug_data = function(bug_page) {
  # column of metadata on the LHS; filtering down first to this sub-node
  #   speeds up the repeated queries of its children substantially [~4x]
  meta_table = html_node(bug_page, xpath = '//td[@id="bz_show_bug_column_1"]')
  time_table = html_node(bug_page, xpath = '//td[@id="bz_show_bug_column_2"]')
  attachments = html_nodes(bug_page,
    xpath = '//table[@id="attachment_table"]//tr[number(substring-after(@id, "a")) > 0]'
  )
  comments = html_nodes(bug_page, xpath = '//div[@id and contains(@class, "bz_comment")]')

  bug = list(
    id = bz_id,
    summary = censor_email(get_field(bug_page, 'span', 'short_desc_nonedit_display')),

    status = get_field(meta_table, 'span', 'static_bug_status'),
    alias = get_field(meta_table, 'tr/td', 'field_tablerow_alias'),
    component = clean(gsub(
      '(show other bugs)', '', fixed = TRUE,
      get_field(meta_table, 'td', 'field_container_component', clean = FALSE)
    )),
    version = get_field(meta_table, 'tr/td', 'field_tablerow_version'),
    hardware = get_field(meta_table, 'tr/td', 'field_tablerow_rep_platform'),
    importance = get_field(meta_table, 'tr/td', 'field_tablerow_importance'),
    assignee = get_field(meta_table, 'tr/td', 'field_tablerow_assigned_to'),
    url = get_field(meta_table, 'span', 'bz_url_input_area'),
    depends_on = as.integer(html_nodes_clean(
      html_node(meta_table, xpath = './/tr[@id="field_tablerow_dependson"]/td'),
      xpath = './a'
    )),
    blocks = as.integer(html_nodes_clean(
      html_node(meta_table, xpath = './/tr/th[@id="field_label_blocked"]'),
      xpath = './parent::node()/td/a'
    )),

    reported_info = get_field(time_table, 'tr/td', 'field_tablerow_reported'),
    modified_info = clean(gsub(
      '(History)', '', fixed = TRUE,
      get_field(time_table, 'tr/td', 'field_tablerow_modified', clean = FALSE)
    )),

    attachment_info = lapply(attachments, get_attachment_info),
    description_info = get_comment_info(comments[[1L]]),
    comment_info = lapply(comments[-1L], get_comment_info)
  )
}

# this might be over-aggressive, e.g. there are probably some
#   public-facing emails like the R mailing lists
censor_email = function(s) {
  gsub('([a-zA-Z0-9._]+)@([a-zA-Z0-9._]+)', '\\1@<::CENSORED -- SEE ORIGINAL ON BUGZILLA::>', s)
}

# ---- MIRRORING UTILITIES ----
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

# ---- GITHUB UTILITIES
create_issue = function(params) {
  gh_call = list(
    endpoint = "POST /repos/:owner/:repo/issues",
    owner = OWNER, repo = REPO,
    title = sprintf("[BUGZILLA #%d] %s", params$id, params$summary),
    body = build_body(params)
  )
  labels = labelDF[
    i = .(name = with(params, c(component, version, hardware, importance))),
    on = 'name', name[n_observed >= 10L]
  ]
  if (length(labels)) gh_call$labels = labels
  do.call(gh, gh_call)
}

close_issue = function(issue_id) {
  gh(
    "PATCH /repos/:owner/:repo/issues/:issue_number",
    owner = OWNER, repo = REPO,
    issue_number = issue_id, state = 'closed'
  )
}

create_comment = function(comment, attachment_info, issue_id) {
  gh(
    "POST /repos/:owner/:repo/issues/:issue_number/comments",
    owner = OWNER, repo = REPO,
    issue_number = issue_id,
    body = sprintf(COMMENT_TEMPLATE,
      comment$text, comment$author, comment$timestamp,
      build_attachment_txt(comment$attachment_title, attachment_info)
    )
  )
}

