((magit-fetch
  (transient:magit-fetch:--unshallow . 1)
  (magit-fetch-from-upstream . 1))
 (magit-gitignore
  (magit-no-assume-unchanged . 1)
  (magit-assume-unchanged . 1))
 (magit-log
  (magit-shortlog . 1)
  (transient:magit-log:--first-parent . 1)
  (transient:magit-log:--no-merges . 1)))
