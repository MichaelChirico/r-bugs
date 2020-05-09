## See https://github.com/wch/r-source for a read-only mirror of the R source code

### How this repo was set up

 1. Download all bug IDs at the time of initialization [`initialize.R`](initialize.R)
 2. Record them all as "unmirrored", then gradually start catching up the bugs incrementally [`backfill.R`](backfill.R)
 3. Maintenance mode -- record only incremental bugs