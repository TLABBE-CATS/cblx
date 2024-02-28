;;; cblx.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <ETPD254@KSTEPCHP1093>
;; Keywords: help, tools

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

;(add-to-list 'load-path "~/elisp/cblx")
(require 'sql)
(require 'sqlite)
(require 'hydra)
(require 'cblx-app)
(require 'cblx-db)
; constantes 
(defconst cblx-database-buffer "*cblx-database-status*"
  "Buffer to output cblx-database messages")
(defconst cblx-program-status-buffer "*cblx-program-status*"
  "Buffer to output cblx-program messages")

; variables 
;  variables Program cblx-pgm-*
(defvar cblx-program-path nil
  "'path' du fichier analysé.")
(defvar cblx-program-name nil
  "'basename' du fichier analysé.")
(defvar cblx-program-id nil
  "Id du fichier analysé.")
(defvar cblx-program-buffer nil
  "Buffer contenant les ource Cobol à analyser.")
(defvar cblx-database-status-buffer "*cbl/database-status*"
  "Buffer pour afficher les infos sur la Database")
;  variables Division cblx-div-*
(defvar cblx-division-id nil
  "Id de la division en cours.")

; variables Praragraph cblx-parag-*
(defvar cblx-parag-id nil
  "Id du paragrapgh en cours.")
(defvar cblx-parag-name nil
  "current paragraph 'name'.")

(defun cblx-status-string ()
 ; (debug)
  (format " | db-file: '%s' | db : '%s'|\n | pgm '%s' | full-path: '%s' |"
	  cblx-db-file
	  cblx-db
	  cblx-program-name
	  cblx-program-path
	  )
  )

(defun cblx-affiche-menu (menu)
  (cond ((stringp menu) (message "%s" menu))
	((listp menu) (cl-loop for e in menu do (message "%s" e)))))

; menus hydra 
(defhydra cblx-menu-main 
  (:exit t
   :color 
   teal
   :pre
   (message "pre  cblx-menu-main")
   :post 
   (message "post cblx-menu-main"))
  (format "%s
 CBLX MENU
*=========*
_S_ default setup 

_B_ Database Menu
_P_ Program Menu
_d_ Division Menu
_p_ Paragraph Menu
--------------------
_q_ Exit CBLX
 "  (cblx-status-string))
  ("S" cblx-menu-default-setup)
  ("B" cblx-menu-database/body )
  ("P" cblx-menu-program/body )
  ("d" cblx-menu-division/body)
  ("p" cblx-menu-paragraph/body )
  ("q" (progn (message "... Done")
	      nil)))

;(define-key cobol-mode-map (kbd "C-c u c") 'cblx-menu-main/body)
(keymap-global-set "C-! c" 'cblx-menu-main/body)

(defhydra cblx-menu-database 
  (:exit t
   :color 
   teal
   :pre   (message "pre  cblx-menu-database")	  
   :post  (message "post cblx-menu-database"))
  "
 DATABASE MENU
*=============*
_s_ Datatbase Staus
_p_ Purge
_S_ Setup
_i_ Ligne de commande SQL
------------------------
_q_ go back to CBLX MENU
------------------------
"
  ("s" cblx-cmd-database-status)
  ("p" cblx-cmd-database-purge)
  ("S" cblx-cmd-database-setup)
  ("i" cblx-cmd-database-inteactive)
  ("q" cblx-menu-main/body ))

; menu-programme
;  cmd-program-status            Infos ( analysed / statistics ) 
;  cmd-program-analyse           Analyse ( with clean-up )
;  cmd-program-erase             Clean upselect * from programs

(defhydra cblx-menu-program
  (:exit t
   :color
   teal 
   :pre  (message "pre  cblx-menu-program")
   :post (message "post cblx-menu-program"))
  (format "%s
 MENU PROGRAM
*============*
_s_ Program Status
_a_ Program Analysis
_e_ Program Erase
------------------------
_q_ go back to CBLX MENU
------------------------
" (cblx-status-string))
  ("s" cblx-cmd-program-status)
  ("a" cblx-cmd-program-analyse)
  ("e" cblx-cmd-program-erase)
  ("q" cblx-menu-main/body ))
 
; menu-division
;  cmd-division-infos            Infos ( analysed / Statistics )
;  cmd-division-list-paragraphs  List Paragraphs
(defhydra cblx-menu-division
  (:color
   teal 
   :pre  (message "pre  cblx-menu-division")
   :post (message "post cblx-menu-division"))
  "
 MENU DIVISION
*=============*
_s_ Division status
_l_ Paragraphs List
------------------------
_q_ go back to CBLX MENU
"
  ("s" cblx-cmd-division-status)
  ("l" cblx-cmd-division-list-paragraphs :exit t)
  ("q" cblx-menu-main/body :exit t))


(defhydra cblx-menu-paragraph
  (:exit t
   :color
   teal 
   :pre (message "pre  cblx-menu-paragraph")
   :post (message "post cblx-menu-paragraph"))
  "
 MENU PARAGRAPH
*==============*
_b_ Browse Paragraph
_s_ Paragraph status
------------------------
_q_ go back to CBLX MENU
------------------------
"
    ("b" cblx-cmd-paragraph-browse)
    ("s" cblx-cmd-paragraph-status)
    
    ("q" cblx-menu-main/body ))

; menu-paragraph
;  cmd-paragraph-status          Status ( Analysed / Statistics )  
;  cmd-paragraph-browse            

(defun cblx-menu-default-setup ()
  (interactive)
  (setq cblx-db-file "~/cblx.db")
  (setq cblx-program-path "c:/Users/etpd254/OneDrive - Groupe Crédit Agricole/etpd254/SQAD SAV/MDE/M75200.cob")
  (setq cblx-program-name "M75200")
  (setq cblx-program-buffer "M75200.cob")
  (setq cblx-db (cblx-db-connect cblx-db-file))
  (message "DEBUG : cblx-menu-default-setup
-   cblx-db-file %s
-   cblx-program-path %s
-   cblx-program-name %s
-   cblx-program-buffer %s
-   connected : %s
"	   cblx-db-file cblx-program-path 
	   cblx-program-name 
	   cblx-program-buffer
	   (sqlitep cblx-db)))




(defun cblx-cmd-database-status ()
  (interactive)
  (let ((buffer 
	 (progn 
	   (get-buffer-create cblx-database-status-buffer))))
    
   ; (display-buffer buffer '(display-buffer-at-bottom . nil))
    
    (switch-to-buffer-other-window buffer)
    (erase-buffer) 
    (insert (cblx-db-status))))   
 
                                                                     
(defun cblx-cmd-database-purge ()
  (interactive)  
  ;(cblx-affiche-menu "Purge")
(cblx-db-create-all-tables))                                 

(defun cblx-cmd-database-setup ()
  (interactive)
  (cblx-affiche-menu "Changer de fichier"))                    

(defun cblx-cmd-database-inteactive ()
  (interactive)
  (sql-sqlite cblx-db-file))
  
(defun cblx-cmd-program-status () 
  (interactive)
  (let ((buffer (get-buffer-create cblx-program-status-buffer)))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (insert (cblx-app-program-status)))
  
  (cblx-affiche-menu "Infos ( analysed / statistics )"))

(defun cblx-cmd-program-analyse ()     
  (interactive)
  (cblx-app-analyse cblx-program-buffer))

(defun cblx-cmd-program-erase ()     
  (interactive)
  (cblx-affiche-menu  "Clean up"))                        
(defun cblx-cmd-division-status ()
  (interactive)
  (cblx-affiche-menu
   "Infos ( analysed / Statistics )"))  
(defun cblx-cmd-division-list-paragraphs ()
  (interactive)
  (cblx-affiche-menu  "List Paragraphs"))                  
(defun cblx-cmd-paragraph-status ()   
  (interactive)
  (cblx-affiche-menu  "Status ( Analysed / Statistics )")) 
(defun cblx-cmd-paragraph-browse ()
  (interactive)
  (cblx-affiche-menu "Browse Paragraph"))                                           

(provide 'cblx)
;;; cblx.el ends here
