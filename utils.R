library(rvest)
library(data.table)
library(gh)

# ---- MISCELLANEOUS UTILITIES ----
# random hex color (16777216 is 256^3=16^6)
rand_color = function() sprintf('%06X', sample(16777216L, 1L))

force_scalar_character = function(x, allow_empty = TRUE) {
  if (!is.character(x)) stop("Input must be character")
  if (length(x) != 1L) stop("Input must be scalar [length-1]")
  if (!allow_empty && (is.na(x) || !nzchar(x))) stop("Input must be non-missing and non-empty")
  return(invisible())
}

# ---- TEXT FORMATTING UTILITIES ----
# strip several kinds of whitespace
clean_ws = function(x, replace_newline = TRUE, replace_nbsp = TRUE, collapse_ws = TRUE) {
  text = trimws(x)
  if (replace_newline) text = gsub('\n', ' ', text, fixed = TRUE)
  if (replace_nbsp) text = gsub(intToUtf8(160L), ' ', text, fixed = TRUE)
  if (collapse_ws) text = gsub('\\s{2,}', ' ', text)
  return(text)
}

# this might be over-aggressive, e.g. there are probably some
#   public-facing emails like the R mailing lists
censor_email = function(s) {
  gsub('([a-zA-Z0-9._]+)@([a-zA-Z0-9._]+)', '\\1@<::CENSORED -- SEE ORIGINAL ON BUGZILLA::>', s)
}

# comments copied from e-mails might include the e-mail signature [and hence phone #]
#   attempts:
#     - https://stackoverflow.com/a/20971688/3576984 <matches way too much>
#     - [-0-9() ]{7,} <matches 7+ spaces in a row>
#     - [+0-9()][-0-9() ]{5,}[0-9()] <matches timestamps like 1998-08-08 03>
#     - [+][0-9]{1,3} [0-9]{3,8}(?: [0-9]{3,8})? <misses (+XX) XXXXXXXX in BZ#65>
#     - \\(?[+][0-9]{1,3}\\)? [0-9]{3,8}(?: [0-9]{3,8})? <misses (+XX X)   XXX XX XX in BZ#52>
#  generally this appears to be a tough problem. approach now is to look for the
#    call-out phrases (phone/tel/fax) and then censor subsequent text -- works
#    under the assumption that these phone #s are only showing up in e=mail signatures,
#    so that hopefully just axing everything below doesn't block content.
#  a close alternative -- simply censoring the lines with these anchors -- fails to catch
#    constructions like that in BZ#3, where two numbers are coupled over two lines
# TODO: fix case like BZ#135, where "ph.ven" is part of the description & caught as phone #
censor_phone = function(x) {
  lines = strsplit(x, '\n', fixed = TRUE)[[1L]]
  for (ii in seq_along(lines)) {
    if (grepl('\\b(?:tel|fax|phone|ph)\\b', lines[ii], ignore.case = TRUE)) {
      lines[ii] = '<CENSORING FROM DETECTED PHONE NUMBER ONWARDS; SEE BUGZILLA>'
      lines = head(lines, ii)
    }
  }
  paste(lines, collapse = '\n')
}

censor = function(x) censor_phone(censor_email(x))

# attempt to maintain fidelity between Bugzilla text and GitHub Markdown text
#   - Bugzilla quote blocks (lines starting with >) --> Markdown code fences
#       this also subsumes true quotes, but it should be fine to en-code those
#   - some code elements outside of code blocks that end up interpreted as HTML
#       + ~ in formulas [interpreted as strikethrough]
#       + # as comment [interpreted as section header]
#       + lines with only hyphens [interpret previous line as section header]
#       + plus sign (+) [interpreted as list item initially]
#       + TODO: LaTeX-style quotes (`...') becomes inline code if there's > 1 [BZ#196]
#       + TODO: print(matrix) outside quote block --> 4 spaces before colnames,
#       +       hence interpreted as code block for that line only [BZ#193]
#   + TODO: cross-references like Bug 7988 --> #GH_ID [BZ#7987]
markdown_convert = function(text) {
  lines = strsplit(text, '\n', fixed = TRUE)[[1L]]

  # BZ#193 -- '> data' and '>data' both are interpreted as quotes
  # BZ#178 -- '   > data' interpreted as quote
  code_block = grepl('^\\s*>', lines)
  if (any(code_block)) {
    lines[code_block] = gsub('^\\s*>\\s*', '', lines[code_block])

    with(rle(code_block), {

      cum_lengths = cumsum(lengths)
      for (ii in which(values)) {
        if (ii == 1L) {
          lines[1L] = paste0('```\n', lines[1L])
        } else {
          start_idx = cum_lengths[ii - 1L] + 1L
          lines[start_idx] = paste0('```\n', lines[start_idx])
        }
        end_idx = cum_lengths[ii]
        lines[end_idx] = paste0(lines[end_idx], '\n```')
      }
      lines <<- lines
    })
  }
  if (!all(code_block)) {
    ncode_block = !code_block
    # use of ~ is used for strikethrough, even across multiple lines,
    #   but also, it's commmon in URLs line .../~ripley/
    lines[ncode_block] = gsub('[^/]~', '&sim;', lines[ncode_block])
    # initial '# ' is treated as a section header. NB: this
    #   also handles sub-sections (##...) since breaking the first #
    #   also breaks the interpretation of the latter #
    lines[ncode_block] = gsub('^(\\s*)#(#*\\s)', '\\1&num;\\2', lines[ncode_block])
    # initial '+' is treated as a list item
    lines[ncode_block] = gsub('^(\\s*)[+]', '\\1&#43;', lines[ncode_block])
    # if the line is all hyphens, it's either interpreted as a section header
    #   (for the line before) or as a horizontal separator (if it's alone).
    #   conservatively look only for all-hyphen lines, to mitigate potential
    #   spillover effects of replacing with the HTML entity.
    # NB: &minus; is too wide, &hyphen; is too narrow vs. original - on GitHub.
    #   &#45; seems just right. Also, - is read as [hex code] 2d from
    #   the sample BZ#134, so - in the character class suffices for now
    all_dash = grepl('^\\s*-[ -]*$', lines[ncode_block])
    lines[ncode_block][all_dash] =
      gsub('-', '&#45;', lines[ncode_block][all_dash], fixed = TRUE)
    # ditto for lines with only =
    all_equal = grepl('^\\s*=[ =]*$', lines[ncode_block])
    lines[ncode_block][all_equal] =
      gsub('=', '&#61;', lines[ncode_block][all_equal], fixed = TRUE)
  }

  paste(lines, collapse = '\n')
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
bugzilla_session = function(URL = 'https://bugs.r-project.org/bugzilla') {
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

# wrapper for html_text %>% clean
html_text_clean = function(x, replace_newline = TRUE, replace_nbsp = TRUE, collapse_ws = TRUE) {
  return(clean_ws(html_text(x), replace_newline, replace_nbsp, collapse_ws))
}

# wrapper for html_node %>% html_text_clean
html_node_clean = function(x, xpath, ...)
  html_text_clean(html_node(x, xpath = xpath), ...)

# ditto, for html_nodes
html_nodes_clean = function(x, xpath, ...)
  html_text_clean(html_nodes(x, xpath = xpath), ...)

# BZ#14005 has a massive description. GitHub API limits to 2^16 characters
MAX_BODY_SIZE = 65536L
txt_truncate = function(x, BUFFER) {
  if (nchar(x) + BUFFER > MAX_BODY_SIZE) { # BUFFER is the rest of the issue text
    trunc_msg = '\n-------\n## MESSAGE TRUNCATED. SEE BUGZILLA'
    return(paste0(
      substr(x, 1L, MAX_BODY_SIZE - BUFFER - nchar(trunc_msg)),
      trunc_msg
    ))
  }
  x
}

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
  # attachment was deleted & marked as "obsolete" on BZ#14414
  if (is.na(link) && length(html_node(a, xpath = '//span[@class="bz_obsolete"]'))) {
    return(list(
      title = '<OBSOLETE ATTACHMENT; SEE BUGZILLA>',
      url = '', id = '', author = '', timestamp = '',
      comment_anchor = '', extra_info = ''
    ))
  }
  url = file.path(URL, html_attr(link, 'href'))

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
    text = censor(markdown_convert(html_text(
      html_node(c, xpath = './/pre[@class="bz_comment_text"]')
    ))),
    # there are two <a>, one to "edit"; the one with name="..." is what we want
    attachment_title = html_text(
      html_node(c, xpath = './/pre[@class="bz_comment_text"]//a/@name')
    ),
    # Attachment may be a reference to a different page, hence the name
    #   anchor won't be matched in get_attachment_info(), e.g. BZ#7987
    attachment_href = html_text(
      html_node(c, xpath = './/pre[@class="bz_comment_text"]//a[@name]/@href')
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
    summary = censor(get_field(bug_page, 'span', 'short_desc_nonedit_display')),

    status = get_field(meta_table, 'span', 'static_bug_status'),
    alias = get_field(meta_table, 'tr/td', 'field_tablerow_alias'),
    component = clean_ws(gsub(
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
    modified_info = clean_ws(gsub(
      '(History)', '', fixed = TRUE,
      get_field(time_table, 'tr/td', 'field_tablerow_modified', clean = FALSE)
    )),

    attachment_info = lapply(attachments, get_attachment_info),
    description_info = get_comment_info(comments[[1L]]),
    comment_info = lapply(comments[-1L], get_comment_info)
  )
}

# ---- MIRRORING UTILITIES ----
# some rules for mapping label names as read into a more useful label name
MAX_LABEL_SIZE = 50L
relabel = function(labels) {
  # format on Bugzilla is CLOSED DUPLICATE of bug XXXXX
  if (any(idx <- grepl('CLOSED DUPLICATE', labels, fixed = TRUE))) {
    labels[idx] = 'Status - CLOSED DUPLICATE'
  }
  # 'Mac OS X' uses 4 redundant characters; with the 50-character constraint
  #   coming up next, this was leading to 10.5, 10.6, etc being combined
  if (any(idx <- grepl('Mac OS X', labels, fixed = TRUE))) {
    labels[idx] = gsub('Mac OS X', 'OS X', labels[idx], fixed = TRUE)
  }
  # maximum label size on GitHub is 50 characters;
  #  bites for [Hardware - x86_64/x64/amd64 (64-bit) Windows 64-bit], e.g. from BZ#14502
  if (any(idx <- nchar(labels) > MAX_LABEL_SIZE)) {
    labels[idx] = substr(labels[idx], 1L, MAX_LABEL_SIZE)
  }
  labels
}
# check if we've seen this label. if not, create the label with a random color.
#   If so, update its observation count. Once a label reaches 10 times observed,
#   it gets POSTed to GitHub.
update_label = function(label, bz_id, dryrun = FALSE) {
  label = relabel(label)
  # this label is known; update its frequency
  if (label %chin% labelDF$name) {
    labelDF[.(name = label), on = 'name', c('n_observed', 'seed_issues') := {
      if (n_observed < 10L) {
        if (n_observed < 9L) {
          seed_issues = paste0(seed_issues, ',', bz_id)
        } else if (!dryrun) {
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
 - [Bugzilla link](%s%s)
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

# In some cases, e.g. BZ#16356, a referred attachment could be deleted
#   or have its info on another bug's page. _maybe_ could have kept a separate
#   DB mapping attachment IDs to Bugzilla issue numbers, but it's pretty late
#   in the game for that... maybe something to PATCH later?
build_attachment_txt = function(title, info, backup_href) {
  if (is.null(title) || is.na(title)) return('')
  idx = which(sapply(info, `[[`, 'comment_anchor') == title)
  if (!length(idx)) {
    params = list(
      id = as.integer(gsub('.*[?]id=', '', backup_href)),
      author = '&lt;SEE BUGZILLA>', url = file.path(URL, backup_href),
      timestamp = '?', extra_info = '?'
    )
  } else {
    params = info[[idx]]
  }
  with(params, sprintf(ATTACHMENT_TEMPLATE, id, author, url, timestamp, extra_info))
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
    params$attachment_info,
    params$description_info$attachment_href
  )

  param_fields = c(
    'id', 'status', 'alias', 'component', 'version',
    'hardware', 'importance', 'assignee', 'url'
  )
  fmt_args = c(
    list(BODY_TEMPLATE),
    params$description_info[c('text', 'author', 'timestamp')],
    list(BUG_URL_STEM),
    params[param_fields],
    list(modified_txt, related_txt, attachment_txt)
  )
  out = do.call(sprintf, unname(fmt_args))
  if (nchar(out) > MAX_BODY_SIZE) {
    fmt_args$text = txt_truncate(fmt_args$text, nchar(out) - nchar(fmt_args$text))
    out = do.call(sprintf, unname(fmt_args))
  }
  out
}

# ---- GITHUB UTILITIES
OWNER = 'MichaelChirico'
REPO = 'r-bugs'
TAG_FIELDS = c('Status', 'Component', 'Version', 'Hardware', 'Importance')

create_issue = function(params, dryrun=FALSE) {
  gh_call = list(
    endpoint = "POST /repos/:owner/:repo/issues",
    owner = OWNER, repo = REPO,
    title = sprintf("[BUGZILLA #%d] %s", params$id, params$summary),
    body = build_body(params)
  )
  labels = paste(TAG_FIELDS, '-', unlist(params[tolower(TAG_FIELDS)]))
  labels = labelDF[
    i = .(name = relabel(labels)),
    on = 'name', name[n_observed >= 10L]
  ]
  # as.list needed -- length-1 input fails
  if (length(labels)) gh_call$labels = as.list(labels)
  if (dryrun) gh_call else do.call(gh, gh_call)
}

close_issue = function(issue_id) {
  gh(
    "PATCH /repos/:owner/:repo/issues/:issue_number",
    owner = OWNER, repo = REPO,
    issue_number = issue_id, state = 'closed'
  )
}

create_comment = function(comment, attachment_info, issue_id, dryrun = FALSE) {
  attachment_txt = build_attachment_txt(
    comment$attachment_title,
    attachment_info,
    comment$attachment_href
  )

  fmt_args = c(
    list(COMMENT_TEMPLATE),
    comment[c('text', 'author', 'timestamp')],
    list(attachment_txt)
  )
  body = do.call(sprintf, unname(fmt_args))
  if (nchar(body) > MAX_BODY_SIZE) {
    fmt_args$text = txt_truncate(fmt_args$text, nchar(body) - nchar(fmt_args$text))
    body = do.call(sprintf, unname(fmt_args))
  }
  if (dryrun) return(body)
  gh(
    "POST /repos/:owner/:repo/issues/:issue_number/comments",
    owner = OWNER, repo = REPO,
    issue_number = issue_id, body = body
  )
}
