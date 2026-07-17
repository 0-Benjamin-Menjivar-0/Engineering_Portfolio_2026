; Developed by: Benjamin Menjivar
; Mechanical Engineering Student
; California State University, Northridge

; INITIAL OBJECTIVE:
; This AutoLISP routine cleans a selected drawing area by deleting dimensions,
; leaders, text, mtext, and specific line lengths, then asks project-specific
; questions to insert the correct blocking detail DWG at a user-selected point.
;
; MAIN PURPOSE:
; - Select a highlighted area containing objects to remove.
; - Delete dimensions, leaders, multileaders, text, and mtext.
; - Delete line objects that match approved target lengths.
; - Ask whether the upper landing is at the top.
; - Ask whether blocking is on the right side.
; - Ask whether the project is California or Midwest.
; - Use those answers to choose the correct DWG block file.
; - Insert the selected block at a custom insertion point chosen every time.

; ============================================================================
; STEP 1 - Define Main Command And Load Visual LISP
; ----------------------------------------------------------------------------
; The command name typed in AutoCAD is V2.
; ============================================================================

(defun c:V2 (/ ss target tol lengths ss i ent data objtype p1 p2 len tol deleted acad doc ms ans ans1 ans2 ans3 blk pt inspt)

(vl-load-com)

; ============================================================================
; STEP 2 - Set Deletion Rules
; ----------------------------------------------------------------------------
; The tolerance and lengths list control which line objects are deleted.
; ============================================================================

(setq tol 0.01)
	(setq deleted 0)
	(setq lengths '(6.0 6.25 1.5 4.0 4.25 4.313 4.5 6.19 34.25 34.313 34.187))

	(princ "\nSelect highlighted area containing dimensions.")
	
	
	; HIGHLIGHT - Selection filter limits the routine to dimensions, leaders, text, mtext, and lines.
(setq ss
		(ssget
			'((0 . "DIMENSION,LEADER,MLEADER,TEXT,MTEXT,LINE"))
		)
	)
	
	(if ss
		(progn
			(setq i 0)

; ============================================================================
; STEP 3 - Review Selected Objects And Delete Matches
; ----------------------------------------------------------------------------
; Dimensions/text are deleted directly. Lines are measured first and deleted
; only if their length matches the approved list with included tolerances.
; ============================================================================

(while (< i (sslength ss))
				(setq ent (ssname ss i))
				(setq data (entget ent))
				(setq objtype (cdr (assoc 0 data)))
	
				(cond
					((member objtype '("DIMENSION" "LEADER" "MLEADER" "TEXT" "MTEXT"))
						(entdel ent)
						(setq deleted (1+ deleted))
					)
				
					((= objtype "LINE")
						(setq p1 (cdr (assoc 10 data)))
						(setq p2 (cdr (assoc 11 data)))
						(setq len (distance p1 p2))

						(if 
							(vl-some
								'(lambda (x)
									(<= (abs (- len x)) tol)
								)
								lengths
							)
							(progn
								(entdel ent)
								(setq deleted (1+ deleted))
							)
						)    
	`				)
				)
				(setq i (1+ i))
			)
		
			(princ (strcat "\nDeleted." (itoa deleted) "object(s)."))
		)	
		(princ "\nNothing selected.")
	)

; ============================================================================
; STEP 4 - Ask Project-Specific Questions
; ----------------------------------------------------------------------------
; initget/getkword restrict the responses to valid Y/N choices.
; ============================================================================

;; Ask Question 1

	(initget "Y N")
	(setq ans1 (getkword "\nIs Upper Landing at the top? [Y/N]: "))
	
;; Ask Question 2

	(initget "Y N")
	(setq ans2 (getkword "\nIs the Blocking on the right side? [Y/N]: "))

;; Ask Question 3

	(initget "Y N")
	(setq ans3 (getkword "\nIs this project CA? [Y/N]: "))


; ============================================================================
; STEP 5 - Decide Which DWG Block To Insert
; ----------------------------------------------------------------------------
; The cond statement works as a decision table. Each combination of Y/N answers
; points to a different block drawing file.
; ============================================================================

;; Decide which block to insert
	(cond
		((and (= ans1 "Y") (= ans2 "Y") (= ans3 "Y"))
		 (setq blk "C:\\Users\\BenMenjivar\\OneDrive - Arrow Lift\\Documents\\ULRCA.dwg")
		 (setq pt (getpoint "\nPick Insertion Point: "))
		)

		((and (= ans1 "Y") (= ans2 "N") (= ans3 "Y"))
		 (setq blk "C:\\Users\\BenMenjivar\\OneDrive - Arrow Lift\\Documents\\ULLCA.dwg")
		 (setq pt (getpoint "\nPick Insertion Point: "))
		)

		((and (= ans1 "N") (= ans2 "Y") (= ans3 "Y"))
		 (setq blk "C:\\Users\\BenMenjivar\\OneDrive - Arrow Lift\\Documents\\BLRCA.dwg")
		 (setq pt (getpoint "\nPick Insertion Point: "))
		)

		((and (= ans1 "N") (= ans2 "N") (= ans3 "Y"))
		 (setq blk "C:\\Users\\BenMenjivar\\OneDrive - Arrow Lift\\Documents\\BLLCA.dwg")
		 (setq pt (getpoint "\nPick Insertion Point: "))
		)

		((and (= ans1 "Y") (= ans2 "Y") (= ans3 "N"))
		 (setq blk "C:\\Users\\BenMenjivar\\OneDrive - Arrow Lift\\Documents\\ULRMW.dwg")
		 (setq pt (getpoint "\nPick Insertion Point: "))
		)

		((and (= ans1 "Y") (= ans2 "N") (= ans3 "N"))
		 (setq blk "C:\\Users\\BenMenjivar\\OneDrive - Arrow Lift\\Documents\\ULLMW.dwg")
		 (setq pt (getpoint "\nPick Insertion Point: "))
		)

		((and (= ans1 "N") (= ans2 "Y") (= ans3 "N"))
		 (setq blk "C:\\Users\\BenMenjivar\\OneDrive - Arrow Lift\\Documents\\BLRMW.dwg")
		 (setq pt (getpoint "\nPick Insertion Point: "))
		)

		((and (= ans1 "N") (= ans2 "N") (= ans3 "N"))
		 (setq blk "C:\\Users\\BenMenjivar\\OneDrive - Arrow Lift\\Documents\\BLLMW.dwg")
		 (setq pt (getpoint "\nPick Insertion Point: "))
		)
	)
	
; ============================================================================
; STEP 6 - Insert Selected Block At User Picked Point
; ----------------------------------------------------------------------------
; The selected DWG file is inserted into Model Space using the insertion point
; picked earlier by the user.
; ============================================================================

;; Insert the block
	
	(setq acad (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acad))
	(setq ms  (vla-get-ModelSpace doc))

	(setq inspt (vlax-3d-point (car pt) (cadr pt) (caddr pt)))

	(princ (strcat "\nBlock file: " blk))

	(vla-InsertBlock ms inspt blk 1.0 1.0 1.0 0.0)

	(princ "\nBlock inserted.")
	(princ)
)
