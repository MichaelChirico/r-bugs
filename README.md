# R Issues Tracker

The [R project](https://www.r-project.org/) uses [Bugzilla](https://bugs.r-project.org/bugzilla/) as a bug tracking / feature requesting platform. As noted in the [official document on bug reporting](https://www.r-project.org/bugs.html),

> due to abuse by spammers, since 2016-07-09, only "members" (including all who have previously submitted bugs) can submit new bugs on R's Bugzilla.

An unfortunate side effect of this policy is reduced transparency to the universe of R users, many of whom are insufficiently motivated to open a Bugzilla account. These users may nevertheless benefit from observing the activity on the issue tracker, perhaps to understand if an unexpected behavior has been reported by other users, or to observe how the R language updates.

This repository exists to help bridge that gap -- the official bug tracker is mirrored here to offer a read-only snapshot for general perusal.

## See https://github.com/wch/r-source for a read-only mirror of the R source code

### How this repo was set up

 1. Download all bug IDs at the time of initialization [`initialize.R`](initialize.R)
 2. Record them all as "unmirrored", then gradually start catching up the bugs incrementally [`backfill.R`](backfill.R)
 3. Maintenance mode -- record only incremental bugs & updates to bugs on a daily basis [`update.R`](update.R)
 
Built with [`rvest`](https://github.com/tidyverse/rvest), [`gh`](https://github.com/r-lib/gh), and [`data.table`](https://github.com/Rdatatable/data.table) and maintained with [GitHub Actions](https://github.com/features/actions).
