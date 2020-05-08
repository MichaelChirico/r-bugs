library(rvest)
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

html_text_clean = function(x, replace_newline = TRUE, replace_nbsp = TRUE, collapse_ws = TRUE) {
  text = trimws(html_text(x))
  if (replace_newline) text = gsub('\n', ' ', text, fixed = TRUE)
  if (replace_nbsp) text = gsub(intToUtf8(160L), ' ', text, fixed = TRUE)
  if (collapse_ws) text = gsub('\\s{2,}', ' ', text)
  return(text)
}
