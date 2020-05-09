# INCREMENTAL UPDATES OF ISSUES [[ONLINE]]
URL = 'https://bugs.r-project.org'
BUG_URL_FMT = file.path(URL, 'bugzilla', 'show_bug.cgi?id=%d')
OWNER = 'MichaelChirico'
REPO = 'r-bugs'

source('utils.R')
library(data.table)
library(gh)

check_credentials()
session = bugzilla_session(URL)

bug_file   = file.path('data', 'known_bugs.csv')
label_file = file.path('data', 'labels.csv')

bugDF   = fread(bug_file, colClasses = c(github_id = 'integer'))
labelDF =  fread(label_file)

