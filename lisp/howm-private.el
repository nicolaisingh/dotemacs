;; from https://web.archive.org/web/20230928205157/http://howm.osdn.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder
;; howm-test050308 でテスト
(defvar howm-private-regexp (regexp-quote "[Private]"))
(defvar howm-hide-private t)
(when (not (fboundp 'howm-item-summary)) ;; for howm-1.2.1
  (defalias 'howm-item-summary #'howm-view-item-summary))
(defadvice howm-menu-general (around hide-private (label formatter item-list)
                                     activate)
  (when howm-hide-private
    (setq item-list
          (remove-if (lambda (item)
                       (string-match howm-private-regexp
                                     (howm-item-summary item)))
                     item-list)))
  ad-do-it)

(defvar howm-toggle-private-hist nil)
;;(defvar howm-toggle-private-hist (list howm-private-regexp))

(defun howm-toggle-private (&optional arg)
  "プライベートメモを表示するか否かを切り替える.
C-u を前につけたときの動作は, 変数 `howm-toggle-private-hist' の値に依存.
・nil → プレフィクスが正の値のときオン, 0 か負ならオフ
・nil 以外 → プライベートとみなす正規表現を対話的に入力
後者の場合, `howm-toggle-private-hist' は文字列のリストとすること.
それが対話入力の初期履歴となる."
  (interactive "P")
  (cond ((null arg)
         (setq howm-hide-private (not howm-hide-private)))
        (howm-toggle-private-hist
         (setq howm-private-regexp
               (read-from-minibuffer "Regexp: "
                                     `(,howm-private-regexp . 0)
                                     nil
                                     nil
                                     'howm-toggle-private-hist)))
        (t
         (setq howm-hide-private (> (prefix-numeric-value arg) 0))))
  (howm-menu t)
  (message (if howm-hide-private
               "hide private reminders"
             "show private reminders")))

;; [Private] を含むメモを一覧でも非表示に.
;; ただし, 「一ファイル複数メモ」でも「ファイル丸ごと非表示」になったりするので注意.
;; 要 howm-test090530 以降.
(setq howm-normalizer
      (lambda (item-list)
        (when howm-hide-private
          (setq item-list
                (howm-filter-items-by-contents item-list howm-private-regexp t)))
        (howm-sort-items-by-mtime item-list)))

;; [Private] を含む項目を予定表(C-c , y)や ToDo リスト(C-c , t)でも非表示に
;; howm-test090502 でテスト
(defun howm-filter-private-maybe (postprocess)
  (when howm-hide-private
    ;; (if (howm-view-remove-by-contents howm-private-regexp) ;; メモ毎
    (if (howm-view-filter-by-summary t howm-private-regexp) ;; 項目毎
        (funcall postprocess)
      (progn
        (howm-view-kill-buffer)
        (message "No match.")))))
;; 迂遠だが, 公開関数に defadvice する方が安全
(defadvice howm-list-schedule (around hide-private activate)
  ad-do-it
  (howm-filter-private-maybe #'howm-reminder-goto-today))
(defadvice howm-list-todo (around hide-private activate)
  ad-do-it
  (howm-filter-private-maybe #'list)) ;; dummy postprocess
