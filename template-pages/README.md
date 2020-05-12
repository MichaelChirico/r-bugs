## Example pages

This directory saves some archived pages that were used as "archetypical" pages for designing
the web scraping scripts. If the page design changes, the templates should change.

 - `bug-001.html` is [Bugzilla #1](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=1), a test bug on Bugzilla. It has many of the fields filled out and a few comments.
 - [Bugzilla #3](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=3), [Bugzilla #52](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=52), and [Bugzilla #65](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=65) demonstrate some potential privacy issues -- there are full e-mail addresses and telephone numbers in a few different formats
 - `bug-123.html` is [Bugzilla #123](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=123). Its description uses a Bugzilla code block (lines starting with `>`), including an embedded newline
 - `bug-134.html` is [Bugzilla #134](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=134). It has some examples of using a line full of `-` which, when parsed by GitHub Markdown, interprets the previous line as a section header. A line of `-` alone would be interpreted as a horizontal line.
 - `bug-143.html` is [Bugzilla #143](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=143). This contains a mixed code block: first line starts with `> `, subsequent lines start with `+ `. The latter are interpreted as a list item without intervention.
