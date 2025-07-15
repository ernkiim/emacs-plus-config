;; -*- lexical-binding: t; -*-

(require 'org)

(defun org-roam-agenda-todo-p ()
  "Return non-nil if current buffer has any TODO entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (eq (org-element-property :todo-type h)
          'todo))
    nil 'first-match))

(defun org-roam-agenda-update-todo-tag ()
  "Update TODO tag in the current buffer."
  (when (and (not (active-minibuffer-window))
             (org-roam-file-p))
    (org-with-point-at 1
      (if (org-agenda-has-todo-p)
          (org-roam-tag-add '("todo"))
        (when (org-collect-keywords '("filetags"))
          (org-roam-tag-remove '("todo")))))))

(defun org-roam-agenda-todo-files ()
  "Return a list of roam files containing todo tag."
  (org-roam-db-sync)
  (let ((todo-nodes (seq-filter (lambda (n)
                                  (seq-contains-p (org-roam-node-tags n) "todo"))
                                (org-roam-node-list))))
    (seq-uniq (seq-map #'org-roam-node-file todo-nodes))))

(defun org-roam-agenda-update-todo-files (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (org-roam-agenda-todo-files)))

(defun org-roam-agenda-open (&optional arg)
  "Find all todo files and open agenda"
  (interactive "P")
  (org-roam-agenda-update-todo-files)
  (org-agenda arg))

(defun org-roam-agenda-todo-list (&optional arg)
  "Find all todo files and open todo list"
  (interactive "P")
  (org-roam-agenda-update-todo-files)
  (org-todo-list arg))

(provide 'org-roam-agenda)
