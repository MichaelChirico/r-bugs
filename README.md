# R Issues Tracker

The [R project](https://www.r-project.org/) uses [Bugzilla](https://bugs.r-project.org/bugzilla/) as a bug tracking / feature requesting platform. An unfortunate side effect of using Bugzilla is reduced transparency to the universe of R users, for many of whom the its interface is unfamiliar at a minimum and perhaps intimidating/offputting. These users may nevertheless benefit from observing the activity on the issue tracker, perhaps to understand if an unexpected behavior has been reported by other users, or to observe how the R language updates.

This repository exists to help bridge that gap -- the official bug tracker is mirrored here to offer a read-only snapshot in a format more familiar to a broader community of users.

## See https://github.com/wch/r-source for a read-only mirror of the R source code

### How this repo was set up

 1. [`initialize.R`](initialize.R) - Download all bug IDs at the time of initialization
 2. [`backfill.R`](backfill.R) - Record them all as "unmirrored", then gradually start catching up the bugs incrementally
 3. [`update.R`](update.R) - Maintenance mode -- record only incremental bugs & updates to bugs on a daily basis
 
Built with [`httr`](https://github.com/r-lib/httr), [`gh`](https://github.com/r-lib/gh), and [`data.table`](https://github.com/Rdatatable/data.table) and maintained with [GitHub Actions](https://github.com/features/actions).
