;;; cblx-db.el --- Regroups cblx definitions relative to thee database.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <ETPD254@KSTEPCHP1093>
;; Keywords: COBOL, sql, docs

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


;; (defun ppp (tbl)
;;   "Interactive command to implement various interactive debugging.
;; Argument TBL Table under test."
;;   (interactive (list (completing-read "Table: " '("programs" "divisions" "paragraphs" "ref_to" "annotations"))))
;;   (message "ppp:\n'%s'" (cblx-db--generate-create-table-statement tbl)))


;;; Code:
(require 'sql)
(require 'sqlite)

;; constants
(defconst --type-integer "INTEGER")
(defconst --type-vchar50 "VARCHAR(50)")
(defconst --type-vchar20 "VARCHAR(20)")
(defconst --type-vchar500 "VARCHAR(500)")

(defconst cblx-db-model-definition
  '(("programs" . (:columns
		   (list
		    (:name id   :type --type-integer :pk t :autincrement t)
		    (:name name :type --type-vchar20)
		    (:name path :type --type-vchar500))
		   :primary-keys (list (:name id))
		   :foreign-keys nil))
    ("divisions" . (:columns
		    (list
		     (:name id            :type --type-integer :pk t :autoincrement t)
		     (:name name          :type --type-vchar50)
		     (:name region_start  :type --type-integer)
		     (:name region_end    :type --type-integer)
		     (:name line_number   :type --type-integer)
		     (:name program_id    :type --type-integer))
		    :primary-keys (list  (:name id))
		    :foreign-keys
		    (list 
		     (:column program_id :foreign-table programs 
			      :foreign-column id :on-delete CASCADE ))))
    ("paragraphs" . (:columns 
		     (list   
		      (:name id             :type --type-integer :pk t :autoincrement t)
		      (:name name           :type --type-vchar50)
		      (:name region_start   :type --type-integer)
		      (:name region_end     :type --type-integer)	      
		      (:name line_number    :type --type-integer)
		      (:name division_id    :type --type-integer))
		     :primary-keys (list (:name id))
		     :foreign-keys
		     (list 
		      (:column division_id :foreign-table divisions 
			       :foreign-column id :on-delete CASCADE ))))
    ("statements" . (:columns 
		     (list   
		      (:name id              :type --type-integer :pk t :autoincrement t)
		      (:name verb            :type --type-vchar20)
		      (:name text            :type --type-vchar500)
		      (:name region_start    :type --type-integer)
		      (:name region_end      :type --type-integer)	      
		      (:name line_number     :type --type-integer)
		      (:name indent          :type --type-integer)
		      (:name paragraph_id    :type --type-integer))
		     :primary-keys (list (:name id) )
		     :foreign-keys
		     (list 
		      (:column paragraph_id :foreign-table paragraphs
			       :foreign-column id :on-delete CASCADE ))))))


(defvar cblx-db-tables-list (map-keys cblx-db-model-definition))

(defun cblx-db-model-table-def (table)
  "Return the definition of the TABLE according to `cblx-db-model-definition'.

Throws an ERROR if TABLE is not found. "
  (cond ((member table (map-keys cblx-db-model-definition))
	 (map-elt cblx-db-model-definition table))
	(t (throw 'error 
		  (format "ERROR : table '%s' not found in `cblx-db-model-definition'. "
			  table)))))

(defun cblx-db-model-table-attr (table attr)
  "For the given TABLE return the value analyseddddau ATTR.

Throws an error if the ATTR does'nt exists in TABLE definition
TABLE definition is given `cblx-db-model-definition' .
"
  (let ((valid-attrs (map-keys (cblx-db-model-table-def table))))
    (cond ((member attr valid-attrs)
	   (let ((value (map-elt (cblx-db-model-table-def table) attr)))
	     (cond ((listp value) (cdr value))
		   (t value))))	  
	  (t (throw 'error (format "attribute %S not in %S" attr valid-attrs))))))

(defun cblx-db-ddl-columns-def (cols)
  "Produce a string joining all 'column-definition' for the descriptions in the list COLS. "
  (string-join (mapcar 'cblx-db-ddl-column-def cols) "\n , "))

(defun cblx-db-ddl-column-def (col) 
  "Produce the column-definition for the given COL."
  (let ((def (format "%s     %s" (map-elt col :name) (eval (map-elt col :type)))))
    (cond ((and (map-elt col :pk) (map-elt col :autoincrement) ) 
	   (format "%s PRIMARY KEY AUTOINCREMENT" def))
	  ((map-elt col :pk)
	   (format "%s PRIMARY KEY" def))
	  (t def))))

(defun cblx-db-ddl-foreign-keys-def (fks)
  "Produce a string joining all the `foreign-key-definition's for the descriptions in the list FKS."
  (string-join (mapcar 'cblx-db-ddl-foreing-key fks) "\n , "))

(defun cblx-db-ddl-foreing-key (fk)
  (format "FOREIGN KEY ( %s )\n\t REFERENCES %s ( %s )\n\t ON DELETE %s"
	  (map-elt fk :column)
	  (map-elt fk :foreign-table)
	  (map-elt fk :foreign-column)
	  (map-elt fk :on-delete)))

;; variables
;;  variables Database cblx-db-*
(defvar cblx-db-file "~/cblx.db"
  "Fichier base de donnée SqLite utilisée.")
(defvar cblx-db nil
  "Connexion à la base de donnée.")

(defun cblx-db-connect (&optional db-file)
  "Connect to the database-file (default `cblx-db-file')."
  (interactive)
  (let ((db-file (or db-file cblx-db-file)))
    (setq cblx-db-file db-file)
    (setq cblx-db (sqlite-open cblx-db-file))))

(defun cblx-db-close ()
  "Close the sqlite connection."
  (interactive)
  (setq cblx-db (sqlite-close cblx-db)))

(defun cblx-db--create-table (table)
  "Creates the TABLE."
  (let ((stmt-drop (format "DROP TABLE IF EXISTS %s;" table))
	(stmt-create (cblx-db--generate-create-table-statement table)))
    (sqlite-execute cblx-db stmt-drop)
    (sqlite-execute cblx-db stmt-create)))

(defun cblx-db--generate-create-table-statement (table)
  "Evaluates to the SQL statement to create the table TABLE.

FIXME : La contrainte 'PRIMARY KEY AUTOINCREMENT' sur la definition de la colonne. pas au niveau de la table.
TODO : ceci implique qu'avec AUTOINCREMENT et PKL il n'est plus possible de faire des PK 'composites'."  
  (catch 'error
    (let* ((cols (cblx-db-model-table-attr table :columns))
	   (fks  (cblx-db-model-table-attr table :foreign-keys )))
      (if fks 
	  (format "CREATE TABLE %s (\n   %s\n , %s\n);\n"
		  table
		  (cblx-db-ddl-columns-def cols)
		  (cblx-db-ddl-foreign-keys-def fks))
	(format "CREATE TABLE %s (\n   %s\n);\n"
		table
		(cblx-db-ddl-columns-def cols)
		)))))

(defun cblx-db-create-all-tables ()
  "Create all tables defined in the `cblx-db-model-definition'."
  (mapcar 'cblx-db--create-table (map-keys cblx-db-model-definition)))

(defun cblx-db--invalid-columns (cols valid-cols)
  "Renturns list of non existent COLS in VALID-COLS."
  (message "DEBUG : cols: %S valid-cols: %S" cols valid-cols)
  (let ((invalid-cols 
	 (seq-filter (lambda (c) (not (null c)))
		     (mapcar (lambda (c) (if (not (member c valid-cols)) c nil))
			     cols))))
    (message "DEBUG : invalid-cols: %S" invalid-cols)
    invalid-cols))

(defun cblx-db-insertable-column-names (table)
  "Returns for the given TABLE the columns 'insertable' i.e not primary key.

FIXME : il est domage de réévaluer cette fonction à chaque INSERT : il n'y a qu'une poignée de tables . Memoization.
"
  (mapcar (lambda (c) (map-elt c :name))
	  (seq-filter (lambda (c) (not (map-elt c :pk)))
		      (cblx-db-model-table-attr table :columns))))

(defun cblx-utils-remove-colons-on-symbols (lst)
  "Returns for the given liist LST the symbols without ':'(colons)."
  (mapcar (lambda (sym) (let ((sym-name (symbol-name sym))) 
			  (if (string= ":" (substring sym-name 0 1))
			      (intern (substring sym-name 1))
			    (quote sym-name))))
	  lst))
(defun cblx-utils-replace-items-with-question-mark (lst)
  "Documenation TODO."
  (string-join (mapcar (lambda (item) (format "?")) lst) ", "))
  

(defun cblx-db-generate-insert (table names values)
  (let ((stmt 
	 (format "INSERT INTO %s ( %s ) VALUES ( %s );"
		 table
		 (string-join (mapcar (lambda (sym) (symbol-name sym)) names) ", ")
		 (string-join (mapcar (lambda (sym) "?") names) ", "))))
    (message "cblx-db-generate-insert <%s>" stmt)
    stmt))

(defun cblx-db-insert (table plst)   
  "Executes an insert into the TABLE for the PLST ( a PLIST )."
  (let ((insertable-cols (cblx-db-insertable-column-names table))
	(cols-names  (cblx-utils-remove-colons-on-symbols  (map-keys plst)))
	(cols-values (map-values plst)))
    (message "- table %s \n insertable-cols:  %s" table insertable-cols)
    (message " columns names     : %s" cols-names)
    (message " columns values    : %s" cols-values)
    (let ((invalid-cols-p (cblx-db--invalid-columns cols-names insertable-cols)))
      (cond ((not invalid-cols-p)
	     (message "%s "(cblx-db-generate-insert table cols-names cols-values))
	     (sqlite-execute cblx-db (cblx-db-generate-insert table cols-names cols-values) cols-values)) 
	    
	    (t (throw 'error 
		      (format "ERROR : invalid column name(s) %S in %S" 
			      invalid-cols-p cols-names)))	    
	    ))))

(defun cblx-db-select (statement)
  "Execute statement"
  (let ((res nil ))
    (pp cblx-db)
    (pp statement)
    (setq res (sqlite-select cblx-db statement))
    (pp res)
    (pp (car (car res)))
    (car (car res))))

(defun cblx-db-status ()
  "Afficher le statut de la database."
  (let ((product (format"
SQL Product : %s" (or  sql-product "undefined")))
	(program (format "
sqliteSQLite program : %s" sql-sqlite-program ) )
	(db-file (format "
Fichier base de donnée : %s" cblx-db-file))
	(connection (format "
Connection %s" (if (sqlitep cblx-db) "'OK'" "Not OK"))))
    (format "
 CBLX DATABASE STATUS:
*=====================*
%s
%s
%s
%s
 " product 
 program
 db-file
 connection)))

;; tests 

(defun ppp- (table)
  (interactive (list (completing-read "table: " cblx-db-tables-list)))
  (message "%s" (cblx-db--generate-create-table-statement table)))

(defun ppp (table)
    (interactive (list (completing-read "table: " cblx-db-tables-list)))
  (cblx-db-insert table '(:name "toto" :path "tata")))

(defun pppp ()
  (message "%S" '(name path))
  )

(provide 'cblx-db)
;;; cblx-db.el ends here
