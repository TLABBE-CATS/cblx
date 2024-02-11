;;; cblx-app.el --- CBLX Aplication Cobol Swiss Army Knife  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <ETPD254@KSTEPCHP1093>
;; Keywords: data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
;(add-to-list 'load-path "~/.emacs.d")
;(require 'cblx)

;; constants 
(defconst cblx-app-select-program-id
  "SELECT id FROM programs WHERE name = '%s' AND path = '%s'")
(defconst cblx-app-select-division-id
  "SELECT id FROM divisions WHERE name = '%s' AND program_id = %d")
(defconst cblx-app-select-paragraph-id
  "SELECT id FROM paragraphs WHERE name = '%s' and division_id = %d")
(defconst cblx-app--paragraph-name-re
  "^.\\{6\\}[ ]\\{1,3\\}\\([A-Z0-9-]+\\)"
  "Detecter une étiquete de paragraphe")
(defconst cblx-app-verbs-list
  (append cobol-verbs '("END-IF" "ELSE" "." "END-PERFORM" "WHEN" "END-EVALUATE"))
  "Liste des mots clefs COBOL marquant le debut d'une nouvelle instruction ( + end-scope terminators).")
(defconst cblx-app--statement-list-re
  (concat (cobol--with-opt-whitespace-line)
	  (regexp-opt cblx-app-verbs-list)))
(defun cblx-app-program-buffer (file-path)
  "Renvoie le buffer associé au file-path."
  (let ((resp (get-file-buffer file-path))
	(expected-buffer-name (file-name-nondirectory file-path)))
    (cond ((not (null resp))  
					;  le buffer sur le fichier file-path existe : 
	   resp)
	  ((member expected-buffer-name (mapcar #'buffer-name (buffer-list))) 
					; il existe un buffer avec le même nom ..; mais sur un fichier différent 
	    (switch-to-buffer-other-window expected-buffer-name) 
	    (if (y-or-n-p (format  "Le Buffer '%s' existe déjà\nLe recharger depuis le fichier %s ?" expected-buffer-name file-path))
		(progn (kill-buffer expected-buffer-name)
		       (find-file file-path)
		       (buffer-name))))
	  (t
					;le buffer n'existe pas : on le créé
	   (buffer-name (find-file file-path))))))

(defun cblx-app-analyse (cobol-buffer)
  (let ((prev-buff (current-buffer))
	program-name program-path division-id paragraph-id)
    
    (save-excursion
      (save-restriction
	(switch-to-buffer-other-window (cblx-app-program-buffer cblx-program-path))
					; enregistrer le programme dans la database
	(setq program-path (buffer-file-name))  ; fixme : le `buffer-file-name' is nil
	(setq program-name (file-name-base (buffer-file-name)))
	(cblx-db-insert "programs"  `(:name ,program-name :path ,program-path))
	(setq cblx-program-id (cblx-db-select (format cblx-app-select-program-id program-name program-path)))
	(cblx-app--analyse-divisions (cblx-app-list-divisions))))))

(defun cblx-app--analyse-divisions (divisions)
  (mapcar (lambda 
	    (div) 
	    (let (division-id 
		  (division-name (map-elt div :name))
		  (div-start (map-elt div :region_start))
		  (div-end (map-elt div :region_end)))
	      (map-put! div :program_id cblx-program-id)
	      (cblx-db-insert "divisions" div)
	      (setq division-id (cblx-db-select 
				 (format cblx-app-select-division-id
					 division-name cblx-program-id)))      
	      (when (string= division-name "PROCEDURE")
		(let ((paragraphs (cblx-app-list-paragraphs
			    	   division-id 					
				   div-start 					
				   div-end))
		      (parag-counter 0))
		  (let ((parag-progress (make-progress-reporter
					 "Traitement des Paragraphes"
					 parag-counter (length paragraphs))))
		    (cl-loop for parag in paragraphs
			     do (progn
				  (setq parag-counter (1+ parag-counter))
				  (cblx-app--analyse-paragraph parag division-id)
				  (progress-reporter-update parag-progress parag-counter)
				  (progress-reporter-done parag-progress))))))))
	  divisions))
	      
(defun cblx-app--analyse-paragraph (parag division-id)
  (cblx-db-insert "paragraphs" parag)
  (setq paragraph-id (cblx-db-select (format cblx-app-select-paragraph-id (map-elt parag :name) division-id)))
  (let ((statements (cblx-app-list-statements parag paragraph-id)))
    (cl-loop for statement in statements
	     do (cblx-app--analyse-statement statement paragraph-id))))

(defun cblx-app--analyse-statement (statement paragraph-id)
  (map-put! statement :paragraph_id paragraph-id)
  (cblx-db-insert "statements" statement))

(defun cblx-app-list-statements (parag parag-id)
  (narrow-to-region (map-elt parag :region_start)
		    (map-elt parag :region_end))
  (goto-char (point-max))
  (let ((res '()) 
	(verb "")
	(text "")
	(indent 0)
	(last (point-max))
	region-start
	region-end
	line-number)
    (while (re-search-backward cblx-app--statement-list-re nil t)
      (setq line-number (line-number-at-pos (point) t))
      (setq verb (match-string-no-properties 1))
      (setq indent 0)
      (setq region-start (point))
      (setq region-end last)
      (setq last (1- (point)))
      (setq text (cblx-app--obtain-text region-start region-end))
      
      (push (list :verb verb
		  :text text
		  :indent indent
		  :region_start region-start
		  :region_end region-end
		  :line_number line-number)
	    res))
    res))

(defun cblx-app--obtain-text (start end &optional raw)
  (when raw (buffer-substring-no-properties start end))
  (cblx-app-normalize-cobol start end))

(defun cblx-app-normalize-cobol (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((one-liner "")
	    (strs (string-split (buffer-substring start end) "\n")))
	(cl-loop for s in strs
		 do (let ((max-len (length s)))
		      (if (not (string-match cobol--fixed-form-comment-re s))
			  (let ((code (substring s 
						 (min 11 max-len)
						 (min 72 max-len))))
			    (setq one-liner (concat one-liner code " "))))))
	(string-trim (replace-regexp-in-string "\\( \\)+" " " one-liner))))))

(defun cblx-app-list-paragraphs (division-id start end)
  (narrow-to-region start end)
  (goto-char (point-max))
  (let ((res '())
	(paragraph-name "")
	(last (point-max))
	region-start
	region-end
	line-number)
    (while (re-search-backward cblx-app--paragraph-name-re nil t)
      (setq line-number (line-number-at-pos (point) t))
      (setq name (match-string-no-properties 1))
      (setq region-start (point))
      (setq region-end last)
      (setq last (1- (point)))
      (push (list :name name
		  :region_start region-start
		  :region_end region-end
		  :line_number line-number
		  :division_id division-id) 
	    res))
    res))

(defun cblx-app-list-divisions ()
  (widen)
  (let ((result ())
	(last (point-max))
	name
	line-number
	region-start
	region-end
	)
    (goto-char last)
    (while (re-search-backward cobol--division-re nil t)
      (setq name (car (seq-filter (lambda (s) (not (string= s "")))
				  (string-split (match-string-no-properties 0) " "))))
      (setq line-number (line-number-at-pos (point) t))
      (setq region-start (point))
      (setq region-end last)
      (push (list :name name 
		  :region_start region-start 
		  :region_end region-end
		  :line_number line-number) 
	    result))
    result))

(defun cblx-app-program-status ()
  "renvoie le status du programme"
  (let* ((cobol-buffer (format "Buffer '%s'" cblx-program-buffer))
	 (cobol-path  (format "Chemin du source Cobol: %s" cblx-program-path))
	 (cobol-name (format "Fichier a analyser: %s" cblx-program-name))
	 (cobol-analyse (format "Analysed: %s "  (cblx-db-select (format cblx-app-select-program-id 
									 cobol-name cobol-path)))))
    (format "
 CBLX PROGRAM STATUS:
*===================*

%s
%s
%s
%s
" cobol-buffer cobol-path cobol-name cobol-analyse)))

(provide 'cblx-app)

;;; cblx-app.el ends here
