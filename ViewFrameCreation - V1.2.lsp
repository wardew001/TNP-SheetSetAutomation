; Define the callback function to use for the reactor
; The following function prints a message with the circle’s radius

;|
When we define an object reactor, we must identify the entity the reactor is to be attached to. So callback functions for object reactors must be defined to accept three arguments:

    The first argument identifies the object that fired the notification.
    The second argument identifies the Reactor object that called the function.
    The third argument is a list of parameters specific to the callback condition.

|;


(defun Setting()
  (setq settings
	 (list
	   (cons "FrameH" 365.0)
	   (cons "FrameW" 1024.0))
	)
  )

;|
     (setq acadObj (vlax-get-acad-object))
    (setq doc (vla-get-ActiveDocument acadObj))

    ;; Display the current setting for TILEMODE
    (alert (strcat "TILEMODE = " (itoa (vla-get-ActiveSpace doc))))
     
    ;; Changes active document to paper space
    (vla-put-ActiveSpace doc acPaperSpace)
    (alert (strcat "TILEMODE = " (itoa (vla-get-ActiveSpace doc))))
   
    ;; Changes active document to model space
    (vla-put-ActiveSpace doc acModelSpace)
    (alert (strcat "TILEMODE = " (itoa (vla-get-ActiveSpace doc))))
    |;


(defun CallBackToReactor (notifier-object reactor-object parameter-list)
  (vl-load-com)
  ; (setq rect (car (entsel)))
  ; (setq vp (car (entsel)))
  (cond
    (
     t
     (setq rect (vlax-vla-object->ename notifier-object))
     (setq vp (handent (ReturnXdataHandle rect)))         ; 
     (setq MsVp (vlax-ename->vla-object vp))

     (setq expmode (getvar 'expert))
     (setvar 'expert 5)

     (vl-cmdf "_Model")
     
     (setq PBG (car (GetRectCorners rect))
	   PHD (cadr (GetRectCorners rect)))

     (setvar 'ctab (last (GetLayoutOrdered)))

     (setq acadObj (vlax-get-acad-object))
     (setq acadDoc (vla-get-activedocument acadObj))

     ;    (setvar 'cvport (cdr (assoc 69 (entget vp)))) (getvar 'cvport ) (eq MsVp (vla-get-ActivePViewport acadDoc))

     ;    (vla-put-activepviewport acadDoc MsVp)  (vla-get-Visible MsVp) (vla-get-activepviewport acadDoc)


     
     (vla-put-ActiveSpace acadDoc acPaperSpace)
     (vla-put-DisplayLocked MsVp :vlax-false)

     (vla-ZoomExtents acadObj)
      
     (vla-put-mspace acadDoc :vlax-true)
     (vla-put-activepviewport acadDoc MsVp)  ;(setq tst (vlax-vla-object->ename (vla-get-activepviewport acadDoc)))
     

     (setq PBGasVariante (vlax-3d-point (car PBG) (cadr PBG)  0))
     (setq PHDasVariante (vlax-3d-point (car PHD) (cadr PHD)  0))
     
     ;(command "_.ZOOM" "w" PBG PHD ) Command not working for a Reactor !!!

     (vla-ZoomWindow acadObj PBGasVariante PHDasVariante)

     (vla-put-mspace acadDoc :vlax-false)
     

     ;(if Scal (vla-put-CustomScale (vlax-ename->vla-object vp2) Scal))


     ;|
     (setq VportObj (vla-item (vla-get-Viewports acadDoc) 0))
     (setq lowerLeft (vlax-safearray->list (vlax-variant-value (vla-get-center VportObj))))
     
     (setq PLL (vla-get-center VportObj))

     |;

     (setvar 'expert expmode)
     ;(command "_Model" )   : reste

     (princ "This rectangle has been modified, hence its assoctiated ViewPort has changed too")
     
    )
  )
)

(defun ListOfRectangles( / l i)
   ; Create a variable to hold the object that
   ; will be used as the owner of the reactor
  (repeat (setq i (sslength (setq ss (ssget))))
            (setq l (cons (vlax-ename->vla-object (ssname ss (setq i (1- i)))) l))
        )
  l
  )

(defun CreateReactor( rectangl )   ; (setq RectsReactor nil)
	; Create the reactor and pass it the circle object, 
	; application-specific data, and callback function.
  (if t;  (= RectsReactor nil)
    (progn
      (setq RectsReactor (vlr-object-reactor (list rectangl)    ; (vlr-pers circleReactor)
			  nil '((:vlr-objectClosed  . CallBackToReactor)))
	  )
      (vlr-pers RectsReactor)   ; make it persistent
      ) ;prgn
    ) ; if
  ) ; dfn



(defun C:LinkAviewPortWithRectInModel( / expmode) ;  PBG PHD Long Haut LayoutToUse Location0 Scal Typi

  (setq Rect0 (car (entsel)))

  ; select VP
  (setq expmode (getvar 'expert))
  (setvar 'expert 5)
  (setvar 'ctab (last (GetLayoutOrdered)))
  (setq vp (car (entsel)))
  (setvar 'expert expmode)
  (command "_Model" )

  ; Select ViewPort
  
  (AddXdataByCodeToEnt Rect0 1005 (cdr (assoc 5 (entget vp))))
  
     (setq expmode (getvar 'expert))
     (setvar 'expert 5)
     (setq PBG (car (GetRectCorners Rect0))
	   PHD (cadr (GetRectCorners Rect0)))
     (setvar 'ctab (last (GetLayoutOrdered)))

     (setq acadDoc (vla-get-activedocument (vlax-get-acad-object)))
     (setq MsVp (vlax-ename->vla-object vp))
     ; (vla-put-activepviewport acadDoc MsVp)
     (vla-put-mspace acadDoc :vlax-true)
     

     (command "._Mspace" )
     (command "_.ZOOM" "w" PBG PHD )

     (vla-put-mspace acadDoc :vlax-false)
     (command "._pspace" )

     ;(if Scal (vla-put-CustomScale (vlax-ename->vla-object vp2) Scal))

     (setvar 'expert expmode)
     (command "_Model" )

  (CreateReactor (vlax-ename->vla-object Rect0))
  
  )


(defun AddXdataByCodeToEnt(Ent Code value0)
	; Ent that will add to it
	; Code of what to add
	; Value of what to add
  (if (and  Ent  value0)
    (progn
      (setq EntId (entget Ent))
      (regapp "AppVP")

      (setq exdata                       
	     (list (list -3 (list "AppVP"                  
		    (cons Code value0)
		    )))                               
	    )

      ; Appends new data list to entity's list
      (setq newent (append EntId exdata))
      
      ; Modifies the entity with the new definition data.
      (entmod newent)
      )
    )
  )


(defun ReturnXdataHandle(ent)

  ; (setq ent ent)

  (setq elist (entget ent '("AppVP")))
  (setq exlist (assoc -3 elist))
  (setq thexdata (car (cdr exlist)))

  (setq Handl (cdr (nth 1 thexdata)))

  )





; ------------------------------------------------------------------------  ------------------------------------------------------------------------------------
; ------------------------------------------------------------------------  ------------------------------------------------------------------------------------
; ------------------------------------------------------------------------  ------------------------------------------------------------------------------------


(defun C:Mb()
  (command "_undo" "_b" )
  )


(defun GetLayoutOrdered nil       ; (GetLayoutOrdered)
 (
   (lambda ( LayoutCollection )
     (vl-sort (cons "Model" (layoutlist))
      '(lambda ( layout1 layout2 )
         (< (vla-get-TabOrder (vla-item LayoutCollection layout1))
            (vla-get-TabOrder (vla-item LayoutCollection layout2))
         )
       )
     )
   )
   (vla-get-layouts (vla-get-ActiveDocument (vlax-get-acad-object)))
 )
)



(defun PointsOfPline(Pline / n vertices) ; Pline
  ;(setq Pline  (car (entsel "\nSelect lightweight Polyline: ")))
  (repeat (setq n (+ (fix (vlax-curve-getEndParam pline)) (if (vlax-curve-isClosed pline) 0 1)))
    (setq vertices (cons (vlax-curve-getPointAtParam pline (setq n (1- n))) vertices))
  ); repeat
  vertices
); defun

(defun GetRectCorners ( ent /  obj pts x y minX minY maxX maxY leftDown rightUpper)
  
  (if (and ent (eq (cdr (assoc 0 (entget ent))) "LWPOLYLINE"))
    (progn
      ;; Get all vertices
      (setq obj (vlax-ename->vla-object ent))
      (setq pts '())
      (repeat (setq i (length (setq ListPnts (PointsOfPline ent))))
        (setq pt (nth (setq i (1- i)) ListPnts))
        (setq pts (cons pt pts))
      )

      ;; Extract X and Y separately
      (setq x (mapcar 'car pts))
      (setq y (mapcar 'cadr pts))

      ;; Compute bounding box
      (setq minX (apply 'min x))
      (setq maxX (apply 'max x))
      (setq minY (apply 'min y))
      (setq maxY (apply 'max y))

      (setq leftDown   (list minX minY 0.0))   ; leftDown  rightUpper
      (setq rightUpper (list maxX maxY 0.0))
    )
  )
  (list leftDown  rightUpper) 
)



(defun AnalysRectanglBBox(Ractangl Lin / returnThisEdge)
  ; Rectangl = BBox of a set of line
  ; Lin = Line showing direction of a Frame
  ; return the length of the edge that is in the same direction as Lin (On of edges must be in the same direction as Rectangl)
  ; return P0 the midppint of StartLine of rectalgle BBox WCS
  ; (setq Ractangl TestedFrame )
  ; (setq Lin MidLine)
  
  (setq explodeRect (vlax-variant-value (vla-Explode (vlax-ename->vla-object Ractangl))))
  (setq RectEdgesAsEnt (mapcar '(lambda(x) (vlax-vla-object->ename x)) (vlax-safearray->list explodeRect)))

  (setq FrontLines '())    ; we must have 2 lines as front ;

  (repeat (setq g (length RectEdgesAsEnt))
    (setq AnEdge (nth (setq g (- g 1)) RectEdgesAsEnt))
    (if (equal (angle (vlax-curve-getEndPoint (vlax-ename->vla-object Lin)) (vlax-curve-getStartPoint (vlax-ename->vla-object Lin)))
	       (angle (vlax-curve-getEndPoint (vlax-ename->vla-object AnEdge)) (vlax-curve-getStartPoint (vlax-ename->vla-object AnEdge)))
	       0.0001)
      (setq returnThisEdge AnEdge)
      ) ; if
    (if (equal (AngleBetweenLines Lin AnEdge) 90 0.1)
      (setq FrontLines (cons AnEdge FrontLines))
      )
    )  ;rep

  (setq aFront1 (nth 0 FrontLines))
  (setq MinDist1 (distance (trans (vlax-curve-getClosestPointTo (vlax-ename->vla-object aFront1)
				       (vlax-curve-getStartPoint (vlax-ename->vla-object Lin)))
				     0 1)
			     (trans (vlax-curve-getStartPoint (vlax-ename->vla-object Lin)) 0 1)))
  (setq aFront2 (nth 1 FrontLines))
  (setq MinDist2 (distance (trans (vlax-curve-getClosestPointTo (vlax-ename->vla-object aFront2)
				       (vlax-curve-getStartPoint (vlax-ename->vla-object Lin)))
				     0 1)
			      (trans (vlax-curve-getStartPoint (vlax-ename->vla-object Lin)) 0 1)))

  (if (< MinDist1 MinDist2) (setq BaseFront aFront1) (setq BaseFront aFront2))

  (setq StartPointOfFrame  (vlax-curve-getpointAtdist (vlax-ename->vla-object BaseFront) (* 0.5 (vla-get-length (vlax-ename->vla-object BaseFront)))))
			      
		      
		      

  (if returnThisEdge
    (setq Length0 (vla-get-length (vlax-ename->vla-object returnThisEdge)))
    (setq Length0 0)
    )

  
  (setq SsOfEdges (ListToSS RectEdgesAsEnt))
  ;(command  "_.pedit" "_multiple" SsOfEdges "" "y"  "_join" "" "")
  

  (command "erase" TestedFrame SsOfEdges "" )
  (list Length0 StartPointOfFrame)
  
  )  ;def
  
	

  
(defun ListToSS(lst)
  (if lst
    (progn
      (setq LinesAsSS (ssadd))
      (repeat (setq i (length lst))
	(setq LinesAsSS (ssadd (setq Line (nth (setq i (- i 1)) lst)) LinesAsSS)))
      ) ; prg
    ) ; if
  LinesAsSS
  )


(defun AngleBetweenLines (ent1 ent2 /   pt1 pt2 pt3 pt4 ang1 ang2 angBetween)
  ;; Select first line
  ;(setq ent1 (car (entsel "\nSelect first line: ")))
  ;; Select second line
  ;(setq ent2 (car (entsel "\nSelect second line: ")))

  (if (and ent1 ent2)
    (progn
      ;; Get line points
      (setq pt1 (cdr (assoc 10 (entget ent1))) ; start point of line 1
            pt2 (cdr (assoc 11 (entget ent1))) ; end point of line 1
            pt3 (cdr (assoc 10 (entget ent2))) ; start point of line 2
            pt4 (cdr (assoc 11 (entget ent2))) ; end point of line 2
      )
      ;; Calculate angles from X-axis
      (setq ang1 (angle pt1 pt2))
      (setq ang2 (angle pt3 pt4))

      ;; Calculate angle between lines
      (setq angBetween (abs (- ang1 ang2)))

      ;; Normalize to acute angle (0 to pi)
      (if (> angBetween pi)
        (setq angBetween (- (* 2 pi) angBetween))
      )

      ;; Convert radians to degrees
      (setq angBetweenDeg (* angBetween (/ 180.0 pi)))
    )
    (princ "\nInvalid selection.")
  )
  angBetweenDeg
)

(defun AverageAngleOfaPlin(Plin0)

  ; (setq Plin0 (car (entsel)))
  
  (setq sumX 0.0)
  (setq sumY 0.0)
  (setq totalLen 0.0)

  (setq PointsOfPline0 (PointsOfPline Plin0))

  (setq i 0)
  (repeat (- (length PointsOfPline0) 1)

    (setq p1 (nth i PointsOfPline0)
	  p2 (nth (+ i 1) PointsOfPline0))
    

    ;; delta X and delta Y
    (setq dx (- (car p2) (car p1)))
    (setq dy (- (cadr p2) (cadr p1)))

    ;; segment length
    
    (setq segLen (distance p1 p2))

    ;; segment angle
    (setq angl (atan dy dx))

    ;; accumulate weighted components
    (setq sumX (+ sumX (* (cos angl) segLen)))
    (setq sumY (+ sumY (* (sin angl) segLen)))
    (setq totalLen (+ totalLen segLen))
    (setq i (1+ i))
  )
  (setq CalculatedA (atan sumY sumX))
  ; (command "line" (getpoint) (strcat "@" "1024" "<" (rtos CalculatedA 2 2)) "")
  )
  

(defun AverageAngle(Lines0)

  ; (setq Line0 (SStoList (ssget)))
  
  (setq sumX 0.0)
  (setq sumY 0.0)
  (setq totalLen 0.0)

  (repeat (setq i (length Lines0))

    (setq Line (nth (setq i (- i 1)) Lines0))

    (setq p1 (vlax-curve-getStartPoint (vlax-ename->vla-object Line))
	  p2 (vlax-curve-getEndPoint (vlax-ename->vla-object Line)))
    

    ;; delta X and delta Y
    (setq dx (- (car p2) (car p1)))
    (setq dy (- (cadr p2) (cadr p1)))

    ;; segment length
    
    (setq segLen (vla-get-length (vlax-ename->vla-object Line)))

    ;; segment angle
    (setq angl (atan dy dx))

    ;; accumulate weighted components
    (setq sumX (+ sumX (* (cos angl) segLen)))
    (setq sumY (+ sumY (* (sin angl) segLen)))
    (setq totalLen (+ totalLen segLen))
  )
  (setq CalculatedA (atan sumY sumX))
  
  )

(defun DrawFrame(Pini W H Angle0)

  ; (setq Pini P0ofFrame W Width H H Angle0 CalculatedA)

  (command "line" Pini (strcat "@" (rtos W 2 2) "<" (rtos Angle0 2 2)) "")
  (setq MidLine0 (entlast))

  
  (setq Offseted1 (vla-offset (vlax-ename->vla-object MidLine0) (/ H 2)))

  
  (setq SaffArrayOfObjects (vlax-safearray->list (vlax-variant-value Offseted1)))
  (setq SaffArrayOfObjects (mapcar 'vlax-vla-object->ename SaffArrayOfObjects))
  (setq OffsetedLine1 (car SaffArrayOfObjects))

  
  (setq Offseted2 (vla-offset (vlax-ename->vla-object MidLine0) (/ H -2)))

  
  (setq SaffArrayOfObjects (vlax-safearray->list (vlax-variant-value Offseted2)))
  (setq SaffArrayOfObjects (mapcar 'vlax-vla-object->ename SaffArrayOfObjects))
  (setq OffsetedLine2 (car SaffArrayOfObjects))

  
  (command "line" (vlax-curve-getStartPoint (vlax-ename->vla-object OffsetedLine1))
	   (vlax-curve-getStartPoint (vlax-ename->vla-object OffsetedLine2)) "" )
  (setq Frnt1 (entlast))
  (command "line" (vlax-curve-getEndPoint (vlax-ename->vla-object OffsetedLine1))
	   (vlax-curve-getEndPoint (vlax-ename->vla-object OffsetedLine2)) "" )
  (setq Frnt2 (entlast))

  (setq SsOfEdges2 (ListToSs (list OffsetedLine2 OffsetedLine1 Frnt2 Frnt1)))

  (command  "_.pedit" "_multiple" SsOfEdges2 "" "y"  "_join" "" "") (setq Frame_i (entlast))

  (command "erase" MidLine0 "" )

  Frame_i

  )


(defun Inter2entitiesNone(ent1 ent2)
  (setq Inter (vlax-safearray->list
		(vlax-variant-value
		  (vla-IntersectWith (vlax-ename->vla-object ent1) (vlax-ename->vla-object ent2) acExtendNone)
		  )
		)
	  )
  )
  

(defun NextPoint(CurrentFrame Pln)

  ; (setq CurrentFrame Frame0 Pln pln)
  (setq Inter (vl-catch-all-apply 'Inter2entitiesNone (list plin Apln)))

  )
  




(defun GetFrameForThisPoint0(Mainpln P0)

  ;(setq Lines (ssget '((0 . "LINE"))))
  (setq Mainpln pln
	P0 P0i)

  (setq Width (cdr (assoc "FrameW" (Setting))))
  (setq H (cdr (assoc "FrameH" (Setting))))

  (setq PointTesteur (vlax-curve-getpointAtdist (vlax-ename->vla-object Mainpln) Width))   ; Test if we can get the best fit Frame from P0 to This PointTesteur
  ; If So, then Ok, go to the next Frame,
  ; If No, check what's not making it fitted (Augmente Abscisse of PointTesteur ... )
  
  (command "breakatpoint" Mainpln PointTesteur)
  
  (setq Line0 Mainpln)
  (setq Mainpln (entlast))

  (setq explodedObjects (vlax-variant-value (vla-Explode (vlax-ename->vla-object Line0))))
  (setq LineAsEnt (mapcar '(lambda(x) (vlax-vla-object->ename x)) (vlax-safearray->list explodedObjects)))

  (setq CalculatedA (AverageAngle LineAsEnt))

  (command "line" P0 (strcat "@" (rtos Width 2 2) "<" (rtos CalculatedA 2 2)) "")
  (setq MidLine (entlast))
  
  (setvar "osmode" 0)

  (command "ucs" "ob" MidLine)                 ; heeeeeeeeeere

  (setq LinesAsSS (ListToSS LineAsEnt))        ; (setq LinesAsSS (ssget)) (setq Line0 (car (entsel)))

  (setq BndBx (LM:ssboundingbox LinesAsSS))

  ; (setq BndBx (LM:boundingbox (vlax-ename->vla-object Line0))) (car (entsel))
  
  (setq NewUCS (mapcar '(lambda(x) (trans x 0 1)) BndBx))      ; (setq NewUCS BndBx)
  (command "rectang" (car NewUCS) (cadr NewUCS) )
  (setq TestedFrame (entlast))

  (setq P0ofFrame (cadr (AnalysRectanglBBox TestedFrame MidLine)))   ; (setq P0ofFrame StartPointOfFrame) (list Length0 StartPointOfFrame)
  (command "ucs" "w")
  (setq Fram0 (DrawFrame P0ofFrame Width H CalculatedA))

  (command "erase" MidLine TestedFrame LinesAsSS "" )

  (list Fram0 Mainpln)
  
)


(defun GetFrameForThisPoint(Mainpln P0)

  ;(setq Lines (ssget '((0 . "LINE"))))
  ;(setq Mainpln (car (entsel)) (setq Inter  (getpoint))
  
  ;(setq Mainpln pln P0 P0i) pln P0i

  (setq Width (cdr (assoc "FrameW" (Setting))))
  (setq H (cdr (assoc "FrameH" (Setting))))

  (command "circle" P0 Width ) (setq Crcl (entlast))

  (setq Inter (vl-catch-all-apply 'Inter2entitiesNone (list Crcl Mainpln)))
  (if (null (vl-catch-all-error-p Inter))
    (setq PointTesteur Inter)
    (setq PointTesteur (vlax-curve-getpointAtdist (vlax-ename->vla-object Mainpln) Width))
    )
  
  (command "breakatpoint" Mainpln PointTesteur)
  
  (setq Line0 Mainpln)
  (setq Mainpln (entlast))

  (setq CalculatedA (AverageAngleOfaPlin Line0))

  (command "line" P0 (strcat "@" (rtos Width 2 2) "<" (rtos CalculatedA 2 2)) "")
  (setq MidLine (entlast))
  
  (setvar "osmode" 0)

  (command "ucs" "ob" MidLine)              

  (setq NewUCS (BoundinBoxUcs Line0 ))

  ; (setq Line0AssSS (ssadd)) (setq Line0AssSS (ssadd Line0 Line0AssSS))
  ; (setq BndBx (LM:ssboundingbox Line0AssSS))
  ;(setq NewUCS (mapcar '(lambda(x) (trans x 0 1)) BndBx))      ; (setq NewUCS BndBx)
  
  (command "rectang" (car NewUCS) (cadr NewUCS) )
  
  (setq TestedFrame (entlast))

     ; (setq P0ofFrame StartPointOfFrame) (list Length0 StartPointOfFrame)
  (command "ucs" "w")

  (setq P0ofFrame (cadr (AnalysRectanglBBox TestedFrame MidLine)))
  (setq Fram0 (DrawFrame P0ofFrame Width H CalculatedA))

  (command "erase" MidLine TestedFrame LinesAsSS Crcl "" )

  (setq NextPoint0 PointTesteur)

  (list Fram0 Mainpln NextPoint0)
  
)


(defun BoundinBoxUcs(e)

  (setq ePoints (PointsOfPline e))
  (setq ePointsUcs (mapcar '(lambda(x) (trans x 0 1)) ePoints))
  
  (setq MinX (apply 'min (mapcar '(lambda(P) (car P)) ePointsUcs)))
  (setq MaxX (apply 'max (mapcar '(lambda(P) (car P)) ePointsUcs)))
  (setq MinY (apply 'min (mapcar '(lambda(P) (cadr P)) ePointsUcs)))
  (setq MaxY (apply 'max (mapcar '(lambda(P) (cadr P)) ePointsUcs)))

  (setq Corner1 (list MinX MinY))
  (setq Corner2 (list MaxX MaxY))

  ;(command "rectang" Corner1 Corner2 )

  (list Corner1 Corner2)

  )


(defun C:TestViewFrames()

  (command "_undo" "_m")

  (setvar 'AUNITS  3)

  
  (setq Alignement (car (entsel "\nSelect an Alignement")))
  (while (/= (cdr (assoc 0 (entget Alignement))) "AECC_ALIGNMENT" )
    (princ)
    (princ "selected element is not a civil 3D Alignement")
    (setq Alignement (car (entsel "Select an Alignement")))
    )


  (command "copy" Alignement "" '(0 0) '(0 0) )
  (command "explode" Alignement "" ) (setq BlockAlignement (entlast))

  (setq explodedObjects (vlax-variant-value (vla-Explode (vlax-ename->vla-object BlockAlignement))))
  (setq explodedObjects2 (mapcar '(lambda(x) (vlax-vla-object->ename x)) (vlax-safearray->list explodedObjects)))
  (setq explodedObjects3 (ListToSS explodedObjects2))

  (command  "_.pedit" "_multiple" explodedObjects3 "" "y"  "_join" "" "")
  (setq pln (entlast))


 
  (setq P0i (vlax-curve-getStartPoint (vlax-ename->vla-object pln)))


  (setq Frame0AndPln (GetFrameForThisPoint pln P0i))
  (setq Frame0 (car Frame0AndPln)
	pln (cadr Frame0AndPln)
	P0i (last Frame0AndPln))

  (while

    (setq Frame0AndPln (GetFrameForThisPoint pln P0i))
    (setq Frame0 (car Frame0AndPln)
	  pln (cadr Frame0AndPln)
	  P0i (last Frame0AndPln))
    
    )

  
  )
  
  
    
    



;; Bounding Box  -  Lee Mac
;; Returns the point list describing the rectangular frame bounding the supplied object.
;; obj - [vla] VLA-Object (setq obj (vlax-ename->vla-object (car (entsel)))) 

(defun LM:boundingbox ( obj / a b lst )
    (if
        (and
            (vlax-method-applicable-p obj 'getboundingbox)
            (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'a 'b))))
            (setq lst (mapcar 'vlax-safearray->list (list a b)))
        )
        (mapcar '(lambda ( a ) (mapcar '(lambda ( b ) ((eval b) lst)) a))
           '(
                (caar   cadar)
                (caadr  cadar)
                (caadr cadadr)
                (caar  cadadr)
            )
        )
    )
)

;; Selection Set Bounding Box  -  Lee Mac
;; Returns a list of the lower-left and upper-right WCS coordinates of a
;; rectangular frame bounding all objects in a supplied selection set.
;; sel - [sel] Selection set for which to return bounding box

(defun LM:ssboundingbox ( sel / idx llp ls1 ls2 obj urp )
    (repeat (setq idx (sslength sel))
        (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        (if (and (vlax-method-applicable-p obj 'getboundingbox)
                 (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'llp 'urp))))
            )
            (setq ls1 (cons (trans (vlax-safearray->list llp) 0 1) ls1)
                  ls2 (cons (trans (vlax-safearray->list urp) 0 1) ls2)
            )
        )
    )
    (if (and ls1 ls2)
        (mapcar '(lambda ( a b ) (apply 'mapcar (cons a b))) '(min max) (list ls1 ls2))
    )
)

