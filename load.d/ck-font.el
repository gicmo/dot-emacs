
;; set font

(setq ck-fonts
      '(("Source Code Pro" 12)
	("Inconsolata" 12)
	("Menlo" 12)))

(defun font-existsp (name size)
  (cond ((find-font (font-spec :name (concat name " for Powerline")))
	 (format "%s for Powerline-%d" name size))
	((find-font (font-spec :name name))
	 (format "%s-%d" name size))))

(defun ck-first-font (lst)
  (or (apply 'font-existsp (car lst))
      (ck-first-font (cdr lst))))

(when (display-graphic-p)
 (let ((foo-font (ck-first-font ck-fonts)))
  (set-face-attribute 'default nil :font foo-font)
  (message (concat "Setting font to: " foo-font))
  ))
