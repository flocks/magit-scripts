(defun magit-scripts--is-worktree-clean ()
  "Check if git status is clean. No untracked files and no modified
files"
  (unless (magit-toplevel)
	(user-error "Not inside a .git directory"))
  (and (eq (length (magit-untracked-files)) 0)
	   (eq (length (magit-modified-files)) 0)))


(defun magit-scripts-bump (prefix)
  "Bump version of project, assuming it's handled by a package.json
file.

when PREFIX argument is given create an annotated tag

Prompt the user for the new version then git commit and git tag."
  (interactive "P")
  (when-let ((root-directory (magit-toplevel)))
	(unless (magit-scripts--is-worktree-clean)
	  (user-error "Git status not clean"))
	(unless (file-exists-p (format "%s/package.json" root-directory))
	  (user-error "No package.json file found."))
	(with-current-buffer (find-file-noselect (format "%s/package.json" root-directory))
	  (goto-char (point-min))
	  (re-search-forward "\\(\"version\"\: \"\\)\\(.+\\)\"")
	  (let* ((orig-version (match-string 2)) 
			 (version (save-match-data (read-string "version: " orig-version))))
		(when (not (string= version orig-version))
		  (replace-match (format "\\1%s\"" version))
		  (save-buffer)
		  (magit-stage-modified)
		  (magit-run-git "commit" "-m" (format "Bump to %s" version))
		  (when prefix
			(magit-run-git "tag" "-a" version "-m" version)))))))


(defun magit-scripts-show-outdated-branches (remote)
  "In magit-refs buffer display only branches that are not up-to-date
with their tracked remote branch"
  (interactive
   (list (or
		  (and current-prefix-arg (magit-read-remote "Remote"))
		  "origin")))
  (let ((inhibit-read-only t))
	(goto-char (point-min))
	(keep-lines (format "<[0-9]+%s" remote))))

(defun magit-scripts-update-branches (branches &optional remote)
  "Equivalent of doing git fetch remote remote-branch:local-branch

It's very useful to be able to be on main branch and without changing branch
to fetch and merge a remote branch into your local branch.

Works also by selecting multiple branches
"
  (interactive
   (list (or (magit-region-values 'branch t)
			 (list (read-string "Branch: ")))
		 (or
		  (and current-prefix-arg (magit-read-remote "Remote"))
		  "origin")))
  (magit-process-buffer)
  (dolist (branche branches)
	(magit-fetch-refspec remote (format "%s:%s" branche branche) nil)))

(defun magit-scripts-track-branches (branches)
  "For each remote branche in BRANCHES, create a local branch that
track the remote branch.

Basically doing git branch --track local origin/local in a loop

"
  (interactive
   (list (or (magit-region-values 'branch t)
			 (list (read-string "Branch: ")))))
  (magit-process-buffer)
  (dolist (branche branches)
	(when-let ((local (cadr (split-string branche "origin/"))))
	  (magit-git-command (format "git branch --track %s %s" local branche)))))

(provide 'magit-scripts)

