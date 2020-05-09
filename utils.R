library(rvest)
# begin a session in bugzilla & log in
bugzilla_session = function(URL = 'https://bugs.r-project.org') {
  session = html_session(URL)

  # the login form at the top of the page behind "login" button
  login = Filter(
    function(x) x$name == 'mini_login_top',
    html_form(session)
  )

  # form IDs by manual inspection
  submit_form(session, set_values(
    login[[1L]],
    Bugzilla_login = Sys.getenv('BUGZILLA_USER'),
    Bugzilla_password = Sys.getenv('BUGZILLA_PASSWORD')
  ))

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
