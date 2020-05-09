library(rvest)
library(data.table)
library(gh)

# --- MISCELLANEOUS UTILITIES ----
# random hex color (16777216 is 256^3=16^6)
rand_color = function() sprintf('%06X', sample(16777216L, 1L))

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
get_field = function(page, type, id, clean = TRUE, node_only = FALSE, ...) {
  out = switch(type,
    span = , td =
      html_node(page, xpath = sprintf('.//%s[@id="%s"]', type, id)),
    'tr/td' =
      html_node(page, xpath = sprintf('.//tr[@id="%s"]/td', id)),
    stop("Unrecognized 'type'")
  )
  if (node_only) return(out)
  if (clean) html_text_clean(out, ...) else html_text(out)
}

# this might be over-aggressive, e.g. there are probably some
#   public-facing emails like the R mailing lists
censor_email = function(s) {
  gsub('([a-zA-Z0-9._]+)@([a-zA-Z0-9._]+)', '\\1@<::CENSORED -- SEE ORIGINAL ON BUGZILLA::>', s)
}

# ---- GITHUB UTILITIES ----
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
