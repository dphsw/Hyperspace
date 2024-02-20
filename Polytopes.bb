Global sqrtthird#=Sqr(1.0/3.0)
Global minussqrtthird#=0-sqrtthird
Global ihconsta#=Sqr(2.0/(5.0+Sqr(5.0)))
Global ihconstb#=Sqr(1.0-(2.0/(5.0+Sqr(5.0))))
Global quartsqrtfive#=Sqr(5.0)/4.0
Global phi#=(Sqr(5.0)+1.0)/2.0
Global recipphi#=2.0/(Sqr(5.0)+1.0)
;Global ddcR1#=Sqr((4.0-(((2.0*Sin(72.0))/(phi*phi))*((2.0*Sin(72.0))/(phi*phi))))/(2.0+phi))
;0.97962991749423856907680539550188;0.97;Sin(72.0)/(phi*Cos(54.0));R
;Global ddcr2#=(phi*Sqr(4.0-((phi+2.0)*(ddcr1*ddcr1))))/(2.0*Sin(72.0));r
;Global ddcD1#=Sqr(1.0-(ddcr2*ddcr2));D
;Global ddcd2#=Sqr(1.0-(ddcR1*ddcR1));d
Global dodeca1#=(1.0+(2.0*phi))/(Sqr(3.0)*(1.0+phi))
Global dodeca2#=phi/(Sqr(3.0)*(1.0+phi))
Global root1_11#=Sqr(1.0/11.0)
Global roothalf#=Sqr(0.5)
Global truncubec#=Sqr((3.0+(2.0*Sqr(2.0)))/(7.0+(4.0*Sqr(2.0))))
Global truncubet#=1.0/Sqr(7.0+(4.0*Sqr(2.0)))
Global truntessc#=Sqr((3.0+(2.0*Sqr(2.0)))/(10.0+(6.0*Sqr(2.0))))
Global truntesst#=1.0/Sqr(10.0+(6.0*Sqr(2.0)))
Global dispent#=Sqr(5.0/24.0)
Global dtruntess#=truntessc/Sqr((((3.0*truntessc)+truntesst)/4.0)*(((3.0*truntessc)+truntesst)/4.0)*4.0)
Global ticosa#=Sqr(9.0/7.0)/(6.0)
Global xsoprism#=1.0/Sqr(0.5+(2.0*(Sin(67.5)^2.0)))
Global soprism#=xsoprism/2.0
Global lsoprism#=xsoprism*Sin(67.5)*Sqr(2)
Global ssoprism#=xsoprism*Sin(67.5)
Global dpthsf#=1.625039840137490743113886910444
Global dpth1#=0.5*dpthsf
Global dpth2#=(0.5/(1+Sqr(2)))*dpthsf
Global rcoh#=Sqr(2)-1.0
Global pthsf#=1/Sqr(0.5+(2*((0.5*rcoh)^2)))
Global pth1#=0.5*pthsf
Global pth2#=pth1*rcoh

Function RenderPolytope(Dimension,Polytope)
	
	EraseHyperspace
	HyperSpaceSize=1.1
	
	If Polytope=0 Then
		; Axes
		CreatePoint(0,0,0,0)
		CreatePoint(0,-1,0,0)
		CreatePoint(0,1,0,0)
		CreateLine(0,-1,0,0,0,1,0,0,"a","","")
		If Dimension>1 Then
			CreatePoint(0,0,-1,0)
			CreatePoint(0,0,1,0)
			CreateLine(0,0,-1,0,0,0,1,0,"b","","")
		End If
		If Dimension>2 Then
			CreatePoint(0,0,0,-1)
			CreatePoint(0,0,0,1)
			CreateLine(0,0,0,-1,0,0,0,1,"c","","")
		End If
		If Dimension>3 Then
			CreatePoint(-1,0,0,0)
			CreatePoint(1,0,0,0)
			CreateLine(-1,0,0,0,1,0,0,0,"d","","")
		End If
		Cells1=Dimension
	End If
	
	Select Dimension
;		Case 2 ; Plane
;		
;			If Polytope>2 Then
;				For l=1 To Polytope
;					deg#=(Float(l)/Float(Polytope))*360.0
;					lastdeg#=(Float(l-1)/Float(Polytope))*360.0
;					CreatePoint(0,Cos(deg),Sin(deg),0)
;					CreateLine(0,Cos(deg),Sin(deg),0,0,Cos(lastdeg),Sin(lastdeg),0,Chr(96+l),0,0)
;				Next
;				Cells1=Polytope
;			End If
;		
		Case 2 ; Surfaces
		
			Select Polytope
				Case 1 ; Klein Bottle
					; Bottle Opening
					For k1=0 To 345 Step 15
						For k2=0 To 40 Step 4
							r#=Float(k2+10)/100.0
							h#=Sin(ACos(5*(r-0.3)))*0.2
							CreatePoint(0,0.5+h,r*Cos(k1),r*Sin(k1))
							CreateLine(0,0.5+h,r*Cos(k1),r*Sin(k1),0,0.5+h,r*Cos(k1+15),r*Sin(k1+15),"a","","")
							If k2>0 Then
								prevr#=Float(k2+6)/100.0
								prevh#=Sin(ACos(5*(prevr-0.3)))*0.2
								CreateLine(0,0.5+h,r*Cos(k1),r*Sin(k1),0,0.5+prevh,prevr*Cos(k1),prevr*Sin(k1),"a","","")
							End If
						Next
					Next
					; Straight part of tube
					For k2=1 To 4
						For k1=0 To 345 Step 15
							CreatePoint(0,0.7-(Float(k2)*0.15),0.1*Cos(k1),0.1*Sin(k1))
						Next
					Next
			
			End Select

		Case 3 ; Space
		
			Select Polytope
				Case 1 ; Tetrahedron
					;Create Vertices
					CreatePoint(0,-sqrtthird,-sqrtthird,-sqrtthird)
					CreatePoint(0,-sqrtthird,sqrtthird,sqrtthird)
					CreatePoint(0,sqrtthird,-sqrtthird,sqrtthird)
					CreatePoint(0,sqrtthird,sqrtthird,-sqrtthird)
					
					;Create Edges
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,"ab","","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,"ac","","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,"bc","","")
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,"da","","")
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,"db","","")
					CreateLine(0,sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,"dc","","")
					
					Cells1=4
					
				Case 2 ; Cube,Hexahedron
					;Create Vertices
					CreatePoint(0,sqrtthird,sqrtthird,sqrtthird)
					CreatePoint(0,sqrtthird,sqrtthird,-sqrtthird)
					CreatePoint(0,sqrtthird,-sqrtthird,sqrtthird)
					CreatePoint(0,sqrtthird,-sqrtthird,-sqrtthird)
					CreatePoint(0,-sqrtthird,sqrtthird,sqrtthird)
					CreatePoint(0,-sqrtthird,sqrtthird,-sqrtthird)
					CreatePoint(0,-sqrtthird,-sqrtthird,sqrtthird)
					CreatePoint(0,-sqrtthird,-sqrtthird,-sqrtthird)
					
					;Create Edges
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,"ac","","")
					CreateLine(0,-sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,"ad","","")
					CreateLine(0,-sqrtthird,sqrtthird,-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,"bc","","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,-sqrtthird,-sqrtthird,"bd","","")
	
					CreateLine(0,sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,"ae","","")
					CreateLine(0,-sqrtthird,-sqrtthird,sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,"af","","")
					CreateLine(0,sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,"be","","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,sqrtthird,-sqrtthird,"bf","","")
	
					CreateLine(0,sqrtthird,sqrtthird,-sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,"ce","","")
					CreateLine(0,-sqrtthird,sqrtthird,-sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,"cf","","")
					CreateLine(0,sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,"de","","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,-sqrtthird,sqrtthird,"df","","")
					
					Cells1=6
	
				Case 3 ; Octahedron
					;Create Vertices
					CreatePoint(0,1,0,0)
					CreatePoint(0,-1,0,0)
					CreatePoint(0,0,1,0)
					CreatePoint(0,0,-1,0)
					CreatePoint(0,0,0,1)
					CreatePoint(0,0,0,-1)
					
					;Create Edges
					CreateLine(0,1,0,0,0,0,1,0,"ae","","")
					CreateLine(0,0,1,0,0,-1,0,0,"bf","","")
					CreateLine(0,-1,0,0,0,0,-1,0,"cg","","")
					CreateLine(0,0,-1,0,0,1,0,0,"dh","","")
					
					CreateLine(0,1,0,0,0,0,0,1,"ah","","")
					CreateLine(0,0,0,1,0,-1,0,0,"bg","","")
					CreateLine(0,-1,0,0,0,0,0,-1,"cf","","")
					CreateLine(0,0,0,-1,0,1,0,0,"de","","")
				
					CreateLine(0,0,1,0,0,0,0,1,"ab","","")
					CreateLine(0,0,0,1,0,0,-1,0,"gh","","")
					CreateLine(0,0,-1,0,0,0,0,-1,"cd","","")
					CreateLine(0,0,0,-1,0,0,1,0,"ef","","")
					
					Cells1=8
					
				Case 4 ; Dodecahedron
					CreatePoint(0,sqrtthird,sqrtthird,sqrtthird)
					CreatePoint(0,sqrtthird,sqrtthird,-sqrtthird)
					CreatePoint(0,sqrtthird,-sqrtthird,sqrtthird)
					CreatePoint(0,sqrtthird,-sqrtthird,-sqrtthird)
					CreatePoint(0,-sqrtthird,sqrtthird,sqrtthird)
					CreatePoint(0,-sqrtthird,sqrtthird,-sqrtthird)
					CreatePoint(0,-sqrtthird,-sqrtthird,sqrtthird)
					CreatePoint(0,-sqrtthird,-sqrtthird,-sqrtthird)
					
					CreatePoint(0,0,dodeca1,dodeca2)
					CreatePoint(0,0,dodeca1,-dodeca2)
					CreatePoint(0,0,-dodeca1,dodeca2)
					CreatePoint(0,0,-dodeca1,-dodeca2)
					
					CreatePoint(0,dodeca2,0,dodeca1)
					CreatePoint(0,dodeca2,0,-dodeca1)
					CreatePoint(0,-dodeca2,0,dodeca1)
					CreatePoint(0,-dodeca2,0,-dodeca1)

					CreatePoint(0,dodeca1,dodeca2,0)
					CreatePoint(0,dodeca1,-dodeca2,0)
					CreatePoint(0,-dodeca1,dodeca2,0)
					CreatePoint(0,-dodeca1,-dodeca2,0)
					
					;FindEdges((2.0*(1.0+phi+(phi*phi)))/(3.0*((1.0+phi)*(1.0+phi))))
					CreateLine(0,0,dodeca1,dodeca2,0,0,dodeca1,-dodeca2,"ef","","")
					CreateLine(0,0,dodeca1,dodeca2,0,sqrtthird,sqrtthird,sqrtthird,"af","","")
					CreateLine(0,0,dodeca1,dodeca2,0,-sqrtthird,sqrtthird,sqrtthird,"ae","","")
					CreateLine(0,0,dodeca1,-dodeca2,0,sqrtthird,sqrtthird,-sqrtthird,"fk","","")
					CreateLine(0,0,dodeca1,-dodeca2,0,-sqrtthird,sqrtthird,-sqrtthird,"ek","","")
					
					CreateLine(0,0,-dodeca1,dodeca2,0,0,-dodeca1,-dodeca2,"gh","","")
					CreateLine(0,0,-dodeca1,dodeca2,0,sqrtthird,-sqrtthird,sqrtthird,"bh","","")
					CreateLine(0,0,-dodeca1,dodeca2,0,-sqrtthird,-sqrtthird,sqrtthird,"bg","","")
					CreateLine(0,0,-dodeca1,-dodeca2,0,sqrtthird,-sqrtthird,-sqrtthird,"hl","","")
					CreateLine(0,0,-dodeca1,-dodeca2,0,-sqrtthird,-sqrtthird,-sqrtthird,"gl","","")
					
					CreateLine(0,dodeca2,0,dodeca1,0,-dodeca2,0,dodeca1,"ab","","")
					CreateLine(0,dodeca2,0,dodeca1,0,sqrtthird,sqrtthird,sqrtthird,"ad","","")
					CreateLine(0,dodeca2,0,dodeca1,0,sqrtthird,-sqrtthird,sqrtthird,"bd","","")
					CreateLine(0,-dodeca2,0,dodeca1,0,-sqrtthird,sqrtthird,sqrtthird,"ac","","")
					CreateLine(0,-dodeca2,0,dodeca1,0,-sqrtthird,-sqrtthird,sqrtthird,"bc","","")
					
					CreateLine(0,dodeca2,0,-dodeca1,0,-dodeca2,0,-dodeca1,"kl","","")
					CreateLine(0,dodeca2,0,-dodeca1,0,sqrtthird,sqrtthird,-sqrtthird,"jk","","")
					CreateLine(0,dodeca2,0,-dodeca1,0,sqrtthird,-sqrtthird,-sqrtthird,"jl","","")
					CreateLine(0,-dodeca2,0,-dodeca1,0,-sqrtthird,sqrtthird,-sqrtthird,"ik","","")
					CreateLine(0,-dodeca2,0,-dodeca1,0,-sqrtthird,-sqrtthird,-sqrtthird,"il","","")
					
					CreateLine(0,dodeca1,dodeca2,0,0,dodeca1,-dodeca2,0,"dj","","")
					CreateLine(0,dodeca1,dodeca2,0,0,sqrtthird,sqrtthird,sqrtthird,"df","","")
					CreateLine(0,dodeca1,dodeca2,0,0,sqrtthird,sqrtthird,-sqrtthird,"fj","","")
					CreateLine(0,dodeca1,-dodeca2,0,0,sqrtthird,-sqrtthird,sqrtthird,"dh","","")
					CreateLine(0,dodeca1,-dodeca2,0,0,sqrtthird,-sqrtthird,-sqrtthird,"hj","","")
					
					CreateLine(0,-dodeca1,dodeca2,0,0,-dodeca1,-dodeca2,0,"ci","","")
					CreateLine(0,-dodeca1,dodeca2,0,0,-sqrtthird,sqrtthird,sqrtthird,"ce","","")
					CreateLine(0,-dodeca1,dodeca2,0,0,-sqrtthird,sqrtthird,-sqrtthird,"ei","","")
					CreateLine(0,-dodeca1,-dodeca2,0,0,-sqrtthird,-sqrtthird,sqrtthird,"cg","","")
					CreateLine(0,-dodeca1,-dodeca2,0,0,-sqrtthird,-sqrtthird,-sqrtthird,"gi","","")
					
					Cells1=12
					
				Case 5 ; Icosahedron
					;Create Vertices
					CreatePoint(0,ihconsta,ihconstb,0)
					CreatePoint(0,-ihconsta,ihconstb,0)
					CreatePoint(0,ihconsta,-ihconstb,0)
					CreatePoint(0,-ihconsta,-ihconstb,0)

					CreatePoint(0,ihconstb,0,ihconsta)
					CreatePoint(0,-ihconstb,0,ihconsta)
					CreatePoint(0,ihconstb,0,-ihconsta)
					CreatePoint(0,-ihconstb,0,-ihconsta)

					CreatePoint(0,0,ihconsta,ihconstb)
					CreatePoint(0,0,-ihconsta,ihconstb)
					CreatePoint(0,0,ihconsta,-ihconstb)
					CreatePoint(0,0,-ihconsta,-ihconstb)
					
					;Create Edges
					CreateLine(0,-ihconsta,ihconstb,0,0,ihconsta,ihconstb,0,"af","","");1-#1
					CreateLine(0,ihconsta,-ihconstb,0,0,-ihconsta,-ihconstb,0,"kr","","");2-#2
					CreateLine(0,ihconstb,0,-ihconsta,0,ihconstb,0,ihconsta,"hi","","");3-#3
					CreateLine(0,-ihconstb,0,ihconsta,0,-ihconstb,0,-ihconsta,"mn","","");4-#4
					CreateLine(0,0,-ihconsta,ihconstb,0,0,ihconsta,ihconstb,"pt","","");5-#5
					CreateLine(0,0,ihconsta,-ihconstb,0,0,-ihconsta,-ihconstb,"cd","","");6-#6

					CreateLine(0,-ihconsta,ihconstb,0,0,0,ihconsta,ihconstb,"fo","","");1-#5
					CreateLine(0,ihconsta,-ihconstb,0,0,0,-ihconsta,-ihconstb,"jk","","");2-#6
					CreateLine(0,ihconstb,0,-ihconsta,0,ihconsta,ihconstb,0,"bh","","");3-#1
					CreateLine(0,-ihconstb,0,ihconsta,0,-ihconsta,-ihconstb,0,"ms","","");4-#2
					CreateLine(0,0,-ihconsta,ihconstb,0,ihconstb,0,ihconsta,"pq","","");5-#3
					CreateLine(0,0,ihconsta,-ihconstb,0,-ihconstb,0,-ihconsta,"de","","");6-#4

					CreateLine(0,-ihconsta,ihconstb,0,0,-ihconstb,0,-ihconsta,"en","","");1-#4
					CreateLine(0,ihconsta,-ihconstb,0,0,ihconstb,0,ihconsta,"iq","","");2-#3
					CreateLine(0,ihconstb,0,-ihconsta,0,0,-ihconsta,-ihconstb,"cj","","");3-#6
					CreateLine(0,-ihconstb,0,ihconsta,0,0,ihconsta,ihconstb,"ot","","");4-#5
					CreateLine(0,0,-ihconsta,ihconstb,0,-ihconsta,-ihconstb,0,"rs","","");5-#2
					CreateLine(0,0,ihconsta,-ihconstb,0,ihconsta,ihconstb,0,"ab","","");6-#1

					CreateLine(0,-ihconsta,ihconstb,0,0,0,ihconsta,-ihconstb,"ae","","");1-6
					CreateLine(0,ihconsta,-ihconstb,0,0,0,-ihconsta,ihconstb,"qr","","");2-5
					CreateLine(0,ihconstb,0,-ihconsta,0,0,ihconsta,-ihconstb,"bc","","");3-6
					CreateLine(0,-ihconstb,0,ihconsta,0,0,-ihconsta,ihconstb,"st","","");4-5
					CreateLine(0,-ihconsta,ihconstb,0,0,-ihconstb,0,ihconsta,"no","","");1-4
					CreateLine(0,ihconsta,-ihconstb,0,0,ihconstb,0,-ihconsta,"ij","","");2-3

					CreateLine(0,ihconsta,ihconstb,0,0,0,ihconsta,ihconstb,"fg","","");#1-#5
					CreateLine(0,-ihconsta,-ihconstb,0,0,-ihconstb,0,-ihconsta,"lm","","");#2-#4
					CreateLine(0,ihconstb,0,ihconsta,0,ihconsta,ihconstb,0,"gh","","");#3-#1
					CreateLine(0,-ihconstb,0,-ihconsta,0,0,-ihconsta,-ihconstb,"dl","","");#4-#6
					CreateLine(0,0,ihconsta,ihconstb,0,ihconstb,0,ihconsta,"gp","","");#5-#3
					CreateLine(0,0,-ihconsta,-ihconstb,0,-ihconsta,-ihconstb,0,"kl","","");#6-#2

					Cells1=20

				Case 6 ; Small Stellated Dodecahedron
					;Create Vertices
					CreatePoint(0,ihconsta,ihconstb,0)
					CreatePoint(0,-ihconsta,ihconstb,0)
					CreatePoint(0,ihconsta,-ihconstb,0)
					CreatePoint(0,-ihconsta,-ihconstb,0)

					CreatePoint(0,ihconstb,0,ihconsta)
					CreatePoint(0,-ihconstb,0,ihconsta)
					CreatePoint(0,ihconstb,0,-ihconsta)
					CreatePoint(0,-ihconstb,0,-ihconsta)

					CreatePoint(0,0,ihconsta,ihconstb)
					CreatePoint(0,0,-ihconsta,ihconstb)
					CreatePoint(0,0,ihconsta,-ihconstb)
					CreatePoint(0,0,-ihconsta,-ihconstb)
					
					;Create Edges
					CreateLine(0,-ihconsta,ihconstb,0,0,ihconstb,0,-ihconsta,"fg","","");1-3
					CreateLine(0,-ihconsta,ihconstb,0,0,0,-ihconsta,-ihconstb,"fj","","");1-#6
					CreateLine(0,-ihconsta,ihconstb,0,0,-ihconsta,-ihconstb,0,"dj","","");1-#2
					CreateLine(0,-ihconsta,ihconstb,0,0,0,-ihconsta,ihconstb,"dk","","");1-5
					CreateLine(0,-ihconsta,ihconstb,0,0,ihconstb,0,ihconsta,"gk","","");1-#3
					
					CreateLine(0,ihconsta,-ihconstb,0,0,-ihconstb,0,ihconsta,"eh","","");2-4
					CreateLine(0,ihconsta,-ihconstb,0,0,0,ihconsta,ihconstb,"ei","","");2-#5
					CreateLine(0,ihconsta,-ihconstb,0,0,ihconsta,ihconstb,0,"ci","","");2-#1
					CreateLine(0,ihconsta,-ihconstb,0,0,0,ihconsta,-ihconstb,"cl","","");2-6
					CreateLine(0,ihconsta,-ihconstb,0,0,-ihconstb,0,-ihconsta,"hl","","");2-#4
					
					CreateLine(0,ihconstb,0,-ihconsta,0,-ihconsta,-ihconstb,0,"bl","","");3-#2
					CreateLine(0,0,-ihconsta,-ihconstb,0,0,-ihconsta,ihconstb,"bh","","");#6-5
					CreateLine(0,-ihconsta,-ihconstb,0,0,ihconstb,0,ihconsta,"be","","");#2-#3
					CreateLine(0,0,-ihconsta,ihconstb,0,ihconstb,0,-ihconsta,"bi","","");5-3
					CreateLine(0,ihconstb,0,ihconsta,0,0,-ihconsta,-ihconstb,"bc","","");#3-#6
					
					CreateLine(0,-ihconstb,0,ihconsta,0,ihconsta,ihconstb,0,"ak","","");4-#1
					CreateLine(0,0,ihconsta,ihconstb,0,0,ihconsta,-ihconstb,"ag","","");#5-6
					CreateLine(0,ihconsta,ihconstb,0,0,-ihconstb,0,-ihconsta,"af","","");#1-#4
					CreateLine(0,0,ihconsta,-ihconstb,0,-ihconstb,0,ihconsta,"aj","","");6-4
					CreateLine(0,-ihconstb,0,-ihconsta,0,0,ihconsta,ihconstb,"ad","","");#4-#5
					
					CreateLine(0,ihconsta,ihconstb,0,0,0,-ihconsta,-ihconstb,"cf","","");#1-#6
					CreateLine(0,0,ihconsta,ihconstb,0,ihconstb,0,-ihconsta,"gi","","");#5-3
					CreateLine(0,-ihconstb,0,ihconsta,0,ihconstb,0,ihconsta,"ek","","");4-#3
					CreateLine(0,-ihconstb,0,-ihconsta,0,0,-ihconsta,ihconstb,"dh","","");#4-5
					CreateLine(0,0,ihconsta,-ihconstb,0,-ihconsta,-ihconstb,0,"jl","","");6-#2

					CreateLine(0,ihconsta,ihconstb,0,0,0,-ihconsta,ihconstb,"ik","","");#1-5
					CreateLine(0,0,ihconsta,ihconstb,0,-ihconsta,-ihconstb,0,"de","","");#5-#2
					CreateLine(0,-ihconstb,0,ihconsta,0,0,-ihconsta,-ihconstb,"hj","","");4-#6
					CreateLine(0,-ihconstb,0,-ihconsta,0,ihconstb,0,-ihconsta,"fl","","");#4-3
					CreateLine(0,0,ihconsta,-ihconstb,0,ihconstb,0,ihconsta,"cg","","");6-#3
					
					Cells1=12

				Case 7 ; Great Dodecahedron
					;Create Vertices
					CreatePoint(0,ihconsta,ihconstb,0)
					CreatePoint(0,-ihconsta,ihconstb,0)
					CreatePoint(0,ihconsta,-ihconstb,0)
					CreatePoint(0,-ihconsta,-ihconstb,0)

					CreatePoint(0,ihconstb,0,ihconsta)
					CreatePoint(0,-ihconstb,0,ihconsta)
					CreatePoint(0,ihconstb,0,-ihconsta)
					CreatePoint(0,-ihconstb,0,-ihconsta)

					CreatePoint(0,0,ihconsta,ihconstb)
					CreatePoint(0,0,-ihconsta,ihconstb)
					CreatePoint(0,0,ihconsta,-ihconstb)
					CreatePoint(0,0,-ihconsta,-ihconstb)
					
					;Create Edges
					;Actually, as far as edges are concerned, this is identical to the icosahedron.
					;In fact, the edges of the Great Icosahedron are identical to those of the Small Stellated Dodecahedron too.
					
				Case 8 ; 
			End Select
					
		Case 4 ; Hyperspace
	
			Select Polytope
				Case 1 ; Simplex,Pentachoron
					;Create Vertices
					CreatePoint(1,0,0,0);A
					CreatePoint(-0.25,-quartsqrtfive,-quartsqrtfive,-quartsqrtfive);B
					CreatePoint(-0.25,quartsqrtfive,quartsqrtfive,-quartsqrtfive);C
					CreatePoint(-0.25,quartsqrtfive,-quartsqrtfive,quartsqrtfive);D
					CreatePoint(-0.25,-quartsqrtfive,quartsqrtfive,quartsqrtfive);E
					
					;Create Edges
					CreateLine(1,0,0,0,-0.25,-quartsqrtfive,-quartsqrtfive,-quartsqrtfive,"cde","","");AB
					CreateLine(1,0,0,0,-0.25,quartsqrtfive,quartsqrtfive,-quartsqrtfive,"bde","","");AC
					CreateLine(1,0,0,0,-0.25,quartsqrtfive,-quartsqrtfive,quartsqrtfive,"bce","","");AD
					CreateLine(1,0,0,0,-0.25,-quartsqrtfive,quartsqrtfive,quartsqrtfive,"bcd","","");AE
					
		CreateLine(-0.25,-quartsqrtfive,-quartsqrtfive,-quartsqrtfive,-0.25,quartsqrtfive,quartsqrtfive,-quartsqrtfive,"ade","","");BC
		CreateLine(-0.25,-quartsqrtfive,-quartsqrtfive,-quartsqrtfive,-0.25,quartsqrtfive,-quartsqrtfive,quartsqrtfive,"ace","","");BD
		CreateLine(-0.25,-quartsqrtfive,-quartsqrtfive,-quartsqrtfive,-0.25,-quartsqrtfive,quartsqrtfive,quartsqrtfive,"acd","","");BE

		CreateLine(-0.25,quartsqrtfive,quartsqrtfive,-quartsqrtfive,-0.25,quartsqrtfive,-quartsqrtfive,quartsqrtfive,"abe","","");CD
		CreateLine(-0.25,quartsqrtfive,quartsqrtfive,-quartsqrtfive,-0.25,-quartsqrtfive,quartsqrtfive,quartsqrtfive,"abd","","");CE

		CreateLine(-0.25,quartsqrtfive,-quartsqrtfive,quartsqrtfive,-0.25,-quartsqrtfive,quartsqrtfive,quartsqrtfive,"abc","","");DE
				
				Cells1=5
				
				Case 2 ; Tesseract,Octachoron
					;Create Points
					For w#=-0.5 To 0.5
						For x#=-0.5 To 0.5
							For y#=-0.5 To 0.5
								For z#=-0.5 To 0.5
									CreatePoint(w,x,y,z)
								Next
							Next
						Next
					Next
					
					;Create Lines
;					For w=-0.5 To 0.5
;						For x=-0.5 To 0.5
;							For y=-0.5 To 0.5
;								CreateLine(w,x,y,-0.5,w,x,y,0.5,"","","")
;							Next
;							For z=-0.5 To 0.5
;								CreateLine(w,x,-0.5,z,w,x,0.5,z,"","","")
;							Next
;						Next
;						For y=-0.5 To 0.5
;							For z=-0.5 To 0.5
;								CreateLine(w,-0.5,y,z,w,0.5,y,z,"","","")
;							Next
;						Next
;					Next
;					For x=-0.5 To 0.5
;						For y=-0.5 To 0.5
;							For z=-0.5 To 0.5
;								CreateLine(-0.5,x,y,z,0.5,x,y,z,"","","")
;							Next
;						Next
;					Next
					CreateLine(-0.5,-0.5,-0.5,-0.5,0.5,-0.5,-0.5,-0.5,"ceg","","")
					CreateLine(-0.5,-0.5,-0.5,0.5,0.5,-0.5,-0.5,0.5,"cfg","","")
					CreateLine(-0.5,-0.5,0.5,-0.5,0.5,-0.5,0.5,-0.5,"deg","","")
					CreateLine(-0.5,-0.5,0.5,0.5,0.5,-0.5,0.5,0.5,"dfg","","")
					CreateLine(-0.5,0.5,-0.5,-0.5,0.5,0.5,-0.5,-0.5,"ceh","","")
					CreateLine(-0.5,0.5,-0.5,0.5,0.5,0.5,-0.5,0.5,"cfh","","")
					CreateLine(-0.5,0.5,0.5,-0.5,0.5,0.5,0.5,-0.5,"deh","","")
					CreateLine(-0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,"dfh","","")

					CreateLine(-0.5,-0.5,-0.5,-0.5,-0.5,0.5,-0.5,-0.5,"ace","","")
					CreateLine(-0.5,-0.5,-0.5,0.5,-0.5,0.5,-0.5,0.5,"acf","","")
					CreateLine(-0.5,-0.5,0.5,-0.5,-0.5,0.5,0.5,-0.5,"ade","","")
					CreateLine(-0.5,-0.5,0.5,0.5,-0.5,0.5,0.5,0.5,"adf","","")
					CreateLine(0.5,-0.5,-0.5,-0.5,0.5,0.5,-0.5,-0.5,"bce","","")
					CreateLine(0.5,-0.5,-0.5,0.5,0.5,0.5,-0.5,0.5,"bcf","","")
					CreateLine(0.5,-0.5,0.5,-0.5,0.5,0.5,0.5,-0.5,"bde","","")
					CreateLine(0.5,-0.5,0.5,0.5,0.5,0.5,0.5,0.5,"bdf","","")

					CreateLine(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,0.5,-0.5,"aeg","","")
					CreateLine(-0.5,-0.5,-0.5,0.5,-0.5,-0.5,0.5,0.5,"afg","","")
					CreateLine(-0.5,0.5,-0.5,-0.5,-0.5,0.5,0.5,-0.5,"aeh","","")
					CreateLine(-0.5,0.5,-0.5,0.5,-0.5,0.5,0.5,0.5,"afh","","")
					CreateLine(0.5,-0.5,-0.5,-0.5,0.5,-0.5,0.5,-0.5,"beg","","")
					CreateLine(0.5,-0.5,-0.5,0.5,0.5,-0.5,0.5,0.5,"bfg","","")
					CreateLine(0.5,0.5,-0.5,-0.5,0.5,0.5,0.5,-0.5,"beh","","")
					CreateLine(0.5,0.5,-0.5,0.5,0.5,0.5,0.5,0.5,"bfh","","")

					CreateLine(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,0.5,"acg","","")
					CreateLine(-0.5,-0.5,0.5,-0.5,-0.5,-0.5,0.5,0.5,"adg","","")
					CreateLine(-0.5,0.5,-0.5,-0.5,-0.5,0.5,-0.5,0.5,"ach","","")
					CreateLine(-0.5,0.5,0.5,-0.5,-0.5,0.5,0.5,0.5,"adh","","")
					CreateLine(0.5,-0.5,-0.5,-0.5,0.5,-0.5,-0.5,0.5,"bcg","","")
					CreateLine(0.5,-0.5,0.5,-0.5,0.5,-0.5,0.5,0.5,"bdg","","")
					CreateLine(0.5,0.5,-0.5,-0.5,0.5,0.5,-0.5,0.5,"bch","","")
					CreateLine(0.5,0.5,0.5,-0.5,0.5,0.5,0.5,0.5,"bdh","","")
					
					Cells1=8
					
				Case 3 ; 16-cell,Hexadecachoron
					CreatePoint(0,0,0,-1)
					CreatePoint(0,0,0,1)
					CreatePoint(0,0,-1,0)
					CreatePoint(0,0,1,0)
					CreatePoint(0,-1,0,0)
					CreatePoint(0,1,0,0)
					CreatePoint(-1,0,0,0)
					CreatePoint(1,0,0,0)
					
					CreateLine(1,0,0,0,0,1,0,0,"acdf","","")
					CreateLine(0,1,0,0,-1,0,0,0,"eikm","","")
					CreateLine(-1,0,0,0,0,-1,0,0,"gnop","","")
					CreateLine(0,-1,0,0,1,0,0,0,"bhjl","","")
					
					CreateLine(1,0,0,0,0,0,1,0,"abdj","","")
					CreateLine(0,0,1,0,-1,0,0,0,"egin","","")
					CreateLine(-1,0,0,0,0,0,-1,0,"kmop","","")
					CreateLine(0,0,-1,0,1,0,0,0,"cfhl","","")
					
					CreateLine(1,0,0,0,0,0,0,1,"abch","","")
					CreateLine(0,0,0,1,-1,0,0,0,"egko","","")
					CreateLine(-1,0,0,0,0,0,0,-1,"imnp","","")
					CreateLine(0,0,0,-1,1,0,0,0,"dfjl","","")
					
					CreateLine(0,1,0,0,0,0,1,0,"adei","","")
					CreateLine(0,0,1,0,0,-1,0,0,"bgjn","","")
					CreateLine(0,-1,0,0,0,0,-1,0,"hlop","","")
					CreateLine(0,0,-1,0,0,1,0,0,"cfkm","","")
					
					CreateLine(0,1,0,0,0,0,0,1,"acek","","")
					CreateLine(0,0,0,1,0,-1,0,0,"bgho","","")
					CreateLine(0,-1,0,0,0,0,0,-1,"jlnp","","")
					CreateLine(0,0,0,-1,0,1,0,0,"dfim","","")
					
					CreateLine(0,0,1,0,0,0,0,1,"abeg","","")
					CreateLine(0,0,0,1,0,0,-1,0,"chko","","")
					CreateLine(0,0,-1,0,0,0,0,-1,"flmp","","")
					CreateLine(0,0,0,-1,0,0,1,0,"dijn","","")
					
					Cells1=16
					
				Case 4 ; 24-cell, Icositetrachoron
					For w#=-0.5 To 0.5
						For x#=-0.5 To 0.5
							For y#=-0.5 To 0.5
								For z#=-0.5 To 0.5
									CreatePoint(w,x,y,z)
								Next
							Next
						Next
					Next
					CreatePoint(0,0,0,-1)
					CreatePoint(0,0,0,1)
					CreatePoint(0,0,-1,0)
					CreatePoint(0,0,1,0)
					CreatePoint(0,-1,0,0)
					CreatePoint(0,1,0,0)
					CreatePoint(-1,0,0,0)
					CreatePoint(1,0,0,0)
					
					CreateLine(-0.5,-0.5,-0.5,-0.5,0.5,-0.5,-0.5,-0.5,"nqr","","")
					CreateLine(-0.5,-0.5,-0.5,0.5,0.5,-0.5,-0.5,0.5,"mor","","")
					CreateLine(-0.5,-0.5,0.5,-0.5,0.5,-0.5,0.5,-0.5,"lpq","","")
					CreateLine(-0.5,-0.5,0.5,0.5,0.5,-0.5,0.5,0.5,"kop","","")
					CreateLine(-0.5,0.5,-0.5,-0.5,0.5,0.5,-0.5,-0.5,"ijn","","")
					CreateLine(-0.5,0.5,-0.5,0.5,0.5,0.5,-0.5,0.5,"him","","")
					CreateLine(-0.5,0.5,0.5,-0.5,0.5,0.5,0.5,-0.5,"gjl","","")
					CreateLine(-0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,"ghk","","")

					CreateLine(-0.5,-0.5,-0.5,-0.5,-0.5,0.5,-0.5,-0.5,"nvw","","")
					CreateLine(-0.5,-0.5,-0.5,0.5,-0.5,0.5,-0.5,0.5,"msw","","")
					CreateLine(-0.5,-0.5,0.5,-0.5,-0.5,0.5,0.5,-0.5,"ltv","","")
					CreateLine(-0.5,-0.5,0.5,0.5,-0.5,0.5,0.5,0.5,"kst","","")
					CreateLine(0.5,-0.5,-0.5,-0.5,0.5,0.5,-0.5,-0.5,"efn","","")
					CreateLine(0.5,-0.5,-0.5,0.5,0.5,0.5,-0.5,0.5,"cem","","")
					CreateLine(0.5,-0.5,0.5,-0.5,0.5,0.5,0.5,-0.5,"bfl","","")
					CreateLine(0.5,-0.5,0.5,0.5,0.5,0.5,0.5,0.5,"bck","","")

					CreateLine(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,0.5,-0.5,"qvx","","")
					CreateLine(-0.5,-0.5,-0.5,0.5,-0.5,-0.5,0.5,0.5,"osx","","")
					CreateLine(-0.5,0.5,-0.5,-0.5,-0.5,0.5,0.5,-0.5,"juv","","")
					CreateLine(-0.5,0.5,-0.5,0.5,-0.5,0.5,0.5,0.5,"hsu","","")
					CreateLine(0.5,-0.5,-0.5,-0.5,0.5,-0.5,0.5,-0.5,"dfq","","")
					CreateLine(0.5,-0.5,-0.5,0.5,0.5,-0.5,0.5,0.5,"cdo","","")
					CreateLine(0.5,0.5,-0.5,-0.5,0.5,0.5,0.5,-0.5,"afj","","")
					CreateLine(0.5,0.5,-0.5,0.5,0.5,0.5,0.5,0.5,"ach","","")

					CreateLine(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,0.5,"rwx","","")
					CreateLine(-0.5,-0.5,0.5,-0.5,-0.5,-0.5,0.5,0.5,"ptx","","")
					CreateLine(-0.5,0.5,-0.5,-0.5,-0.5,0.5,-0.5,0.5,"iuw","","")
					CreateLine(-0.5,0.5,0.5,-0.5,-0.5,0.5,0.5,0.5,"gtu","","")
					CreateLine(0.5,-0.5,-0.5,-0.5,0.5,-0.5,-0.5,0.5,"der","","")
					CreateLine(0.5,-0.5,0.5,-0.5,0.5,-0.5,0.5,0.5,"bdp","","")
					CreateLine(0.5,0.5,-0.5,-0.5,0.5,0.5,-0.5,0.5,"aei","","")
					CreateLine(0.5,0.5,0.5,-0.5,0.5,0.5,0.5,0.5,"abg","","")
					
					CreateLine(1,0,0,0,0.5,0.5,0.5,0.5,"abc","","")
					CreateLine(1,0,0,0,0.5,0.5,0.5,-0.5,"abf","","")
					CreateLine(1,0,0,0,0.5,0.5,-0.5,0.5,"ace","","")
					CreateLine(1,0,0,0,0.5,0.5,-0.5,-0.5,"aef","","")
					CreateLine(1,0,0,0,0.5,-0.5,0.5,0.5,"bcd","","")
					CreateLine(1,0,0,0,0.5,-0.5,0.5,-0.5,"bdf","","")
					CreateLine(1,0,0,0,0.5,-0.5,-0.5,0.5,"cde","","")
					CreateLine(1,0,0,0,0.5,-0.5,-0.5,-0.5,"def","","")

					CreateLine(-1,0,0,0,-0.5,0.5,0.5,0.5,"stu","","")
					CreateLine(-1,0,0,0,-0.5,0.5,0.5,-0.5,"tuv","","")
					CreateLine(-1,0,0,0,-0.5,0.5,-0.5,0.5,"suw","","")
					CreateLine(-1,0,0,0,-0.5,0.5,-0.5,-0.5,"uvw","","")
					CreateLine(-1,0,0,0,-0.5,-0.5,0.5,0.5,"stx","","")
					CreateLine(-1,0,0,0,-0.5,-0.5,0.5,-0.5,"tvx","","")
					CreateLine(-1,0,0,0,-0.5,-0.5,-0.5,0.5,"swx","","")
					CreateLine(-1,0,0,0,-0.5,-0.5,-0.5,-0.5,"vwx","","")
					
					CreateLine(0,1,0,0,0.5,0.5,0.5,0.5,"agh","","")
					CreateLine(0,1,0,0,0.5,0.5,0.5,-0.5,"agj","","")
					CreateLine(0,1,0,0,0.5,0.5,-0.5,0.5,"ahi","","")
					CreateLine(0,1,0,0,0.5,0.5,-0.5,-0.5,"aij","","")
					CreateLine(0,1,0,0,-0.5,0.5,0.5,0.5,"ghu","","")
					CreateLine(0,1,0,0,-0.5,0.5,0.5,-0.5,"gju","","")
					CreateLine(0,1,0,0,-0.5,0.5,-0.5,0.5,"hiu","","")
					CreateLine(0,1,0,0,-0.5,0.5,-0.5,-0.5,"iju","","")

					CreateLine(0,-1,0,0,0.5,-0.5,0.5,0.5,"dop","","")
					CreateLine(0,-1,0,0,0.5,-0.5,0.5,-0.5,"dpq","","")
					CreateLine(0,-1,0,0,0.5,-0.5,-0.5,0.5,"dor","","")
					CreateLine(0,-1,0,0,0.5,-0.5,-0.5,-0.5,"dqr","","")
					CreateLine(0,-1,0,0,-0.5,-0.5,0.5,0.5,"opx","","")
					CreateLine(0,-1,0,0,-0.5,-0.5,0.5,-0.5,"pqx","","")
					CreateLine(0,-1,0,0,-0.5,-0.5,-0.5,0.5,"orx","","")
					CreateLine(0,-1,0,0,-0.5,-0.5,-0.5,-0.5,"qrx","","")

					CreateLine(0,0,1,0,0.5,0.5,0.5,0.5,"bgk","","")
					CreateLine(0,0,1,0,0.5,0.5,0.5,-0.5,"bgl","","")
					CreateLine(0,0,1,0,0.5,-0.5,0.5,0.5,"bkp","","")
					CreateLine(0,0,1,0,0.5,-0.5,0.5,-0.5,"blp","","")
					CreateLine(0,0,1,0,-0.5,0.5,0.5,0.5,"gkt","","")
					CreateLine(0,0,1,0,-0.5,0.5,0.5,-0.5,"glt","","")
					CreateLine(0,0,1,0,-0.5,-0.5,0.5,0.5,"kpt","","")
					CreateLine(0,0,1,0,-0.5,-0.5,0.5,-0.5,"lpt","","")

					CreateLine(0,0,-1,0,0.5,0.5,-0.5,0.5,"eim","","")
					CreateLine(0,0,-1,0,0.5,0.5,-0.5,-0.5,"ein","","")
					CreateLine(0,0,-1,0,0.5,-0.5,-0.5,0.5,"emr","","")
					CreateLine(0,0,-1,0,0.5,-0.5,-0.5,-0.5,"enr","","")
					CreateLine(0,0,-1,0,-0.5,0.5,-0.5,0.5,"imw","","")
					CreateLine(0,0,-1,0,-0.5,0.5,-0.5,-0.5,"inw","","")
					CreateLine(0,0,-1,0,-0.5,-0.5,-0.5,0.5,"mrw","","")
					CreateLine(0,0,-1,0,-0.5,-0.5,-0.5,-0.5,"nrw","","")

					CreateLine(0,0,0,1,0.5,0.5,0.5,0.5,"chk","","")
					CreateLine(0,0,0,1,0.5,0.5,-0.5,0.5,"chm","","")
					CreateLine(0,0,0,1,0.5,-0.5,0.5,0.5,"cko","","")
					CreateLine(0,0,0,1,0.5,-0.5,-0.5,0.5,"cmo","","")
					CreateLine(0,0,0,1,-0.5,0.5,0.5,0.5,"hks","","")
					CreateLine(0,0,0,1,-0.5,0.5,-0.5,0.5,"hms","","")
					CreateLine(0,0,0,1,-0.5,-0.5,0.5,0.5,"kos","","")
					CreateLine(0,0,0,1,-0.5,-0.5,-0.5,0.5,"mos","","")

					CreateLine(0,0,0,-1,0.5,0.5,0.5,-0.5,"fjl","","")
					CreateLine(0,0,0,-1,0.5,0.5,-0.5,-0.5,"fjn","","")
					CreateLine(0,0,0,-1,0.5,-0.5,0.5,-0.5,"flq","","")
					CreateLine(0,0,0,-1,0.5,-0.5,-0.5,-0.5,"fnq","","")
					CreateLine(0,0,0,-1,-0.5,0.5,0.5,-0.5,"jlv","","")
					CreateLine(0,0,0,-1,-0.5,0.5,-0.5,-0.5,"jnv","","")
					CreateLine(0,0,0,-1,-0.5,-0.5,0.5,-0.5,"lqv","","")
					CreateLine(0,0,0,-1,-0.5,-0.5,-0.5,-0.5,"nqv","","")

					;a++00,b+0+0,c+00+,d+-00,e+0-0,f+00-,g0++0,h0+0+,i0+-0,j0+0-,k00++,l00+-
					;m00-+,n00--,o0-0+,p0-+0,q0-0-,r0--0,s-00+,t-0+0,u-+00,v-00-,w-0-0,x--00
					
					Cells1=24
					
				Case 5 ; Hecatonicosachoron (120-cell)
					ED#=Sqr(8)
					CreatePermutationPoints(2.0/ED,2.0/ED,0,0)
					CreatePermutationPoints(2.0/ED,-2.0/ED,0,0)
					CreatePermutationPoints(-2.0/ED,-2.0/ED,0,0)

					CreatePermutationPoints(Sqr(5)/ED,1.0/ED,1.0/ED,1.0/ED)
					CreatePermutationPoints(Sqr(5)/ED,1.0/ED,1.0/ED,-1.0/ED)
					CreatePermutationPoints(Sqr(5)/ED,1.0/ED,-1.0/ED,-1.0/ED)
					CreatePermutationPoints(Sqr(5)/ED,-1.0/ED,-1.0/ED,-1.0/ED)
					CreatePermutationPoints(-Sqr(5)/ED,1.0/ED,1.0/ED,1.0/ED)
					CreatePermutationPoints(-Sqr(5)/ED,1.0/ED,1.0/ED,-1.0/ED)
					CreatePermutationPoints(-Sqr(5)/ED,1.0/ED,-1.0/ED,-1.0/ED)
					CreatePermutationPoints(-Sqr(5)/ED,-1.0/ED,-1.0/ED,-1.0/ED)
					
					CreatePermutationPoints(phi/ED,phi/ED,phi/ED,(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,phi/ED,-phi/ED,(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,-phi/ED,-phi/ED,(recipphi^2)/ED)
					CreatePermutationPoints(-phi/ED,-phi/ED,-phi/ED,(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,phi/ED,phi/ED,-(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,phi/ED,-phi/ED,-(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,-phi/ED,-phi/ED,-(recipphi^2)/ED)
					CreatePermutationPoints(-phi/ED,-phi/ED,-phi/ED,-(recipphi^2)/ED)
					
					CreatePermutationPoints((phi^2)/ED,recipphi/ED,recipphi/ED,recipphi/ED)
					CreatePermutationPoints((phi^2)/ED,-recipphi/ED,recipphi/ED,recipphi/ED)
					CreatePermutationPoints((phi^2)/ED,-recipphi/ED,-recipphi/ED,recipphi/ED)
					CreatePermutationPoints((phi^2)/ED,-recipphi/ED,-recipphi/ED,-recipphi/ED)
					CreatePermutationPoints(-(phi^2)/ED,recipphi/ED,recipphi/ED,recipphi/ED)
					CreatePermutationPoints(-(phi^2)/ED,-recipphi/ED,recipphi/ED,recipphi/ED)
					CreatePermutationPoints(-(phi^2)/ED,-recipphi/ED,-recipphi/ED,recipphi/ED)
					CreatePermutationPoints(-(phi^2)/ED,-recipphi/ED,-recipphi/ED,-recipphi/ED)
					
					CreateEvenPermutationPoints((phi^2)/ED,1/ED,(recipphi^2)/ED,0)
					CreateEvenPermutationPoints((phi^2)/ED,1/ED,-(recipphi^2)/ED,0)
					CreateEvenPermutationPoints((phi^2)/ED,-1/ED,(recipphi^2)/ED,0)
					CreateEvenPermutationPoints((phi^2)/ED,-1/ED,-(recipphi^2)/ED,0)
					CreateEvenPermutationPoints(-(phi^2)/ED,1/ED,(recipphi^2)/ED,0)
					CreateEvenPermutationPoints(-(phi^2)/ED,1/ED,-(recipphi^2)/ED,0)
					CreateEvenPermutationPoints(-(phi^2)/ED,-1/ED,(recipphi^2)/ED,0)
					CreateEvenPermutationPoints(-(phi^2)/ED,-1/ED,-(recipphi^2)/ED,0)
					
					CreateEvenPermutationPoints(Sqr(5)/ED,phi/ED,recipphi/ED,0)
					CreateEvenPermutationPoints(Sqr(5)/ED,phi/ED,-recipphi/ED,0)
					CreateEvenPermutationPoints(Sqr(5)/ED,-phi/ED,recipphi/ED,0)
					CreateEvenPermutationPoints(Sqr(5)/ED,-phi/ED,-recipphi/ED,0)
					CreateEvenPermutationPoints(-Sqr(5)/ED,phi/ED,recipphi/ED,0)
					CreateEvenPermutationPoints(-Sqr(5)/ED,phi/ED,-recipphi/ED,0)
					CreateEvenPermutationPoints(-Sqr(5)/ED,-phi/ED,recipphi/ED,0)
					CreateEvenPermutationPoints(-Sqr(5)/ED,-phi/ED,-recipphi/ED,0)
					
					CreateEvenPermutationPoints(2.0/ED,phi/ED,1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,phi/ED,1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,phi/ED,-1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,phi/ED,-1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,-phi/ED,1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,-phi/ED,1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,-phi/ED,-1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,-phi/ED,-1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,phi/ED,1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,phi/ED,1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,phi/ED,-1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,phi/ED,-1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,-phi/ED,1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,-phi/ED,1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,-phi/ED,-1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,-phi/ED,-1.0/ED,-recipphi/ED)
					
					FindEdges(2.0*(recipphi^2)/ED)
					
				Case 6 ; Hexacosichoron (600-cell)
					CreateEvenPermutationPoints(phi/2.0,0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,-0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,-0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,-0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,-0.5,-recipphi/2.0,0)
					
					For w#=-0.5 To 0.5
						For x#=-0.5 To 0.5
							For y#=-0.5 To 0.5
								For z#=-0.5 To 0.5
									CreatePoint(w,x,y,z)
								Next
							Next
						Next
					Next
				
					CreatePoint(0,0,0,-1)
					CreatePoint(0,0,0,1)
					CreatePoint(0,0,-1,0)
					CreatePoint(0,0,1,0)
					CreatePoint(0,-1,0,0)
					CreatePoint(0,1,0,0)
					CreatePoint(-1,0,0,0)
					CreatePoint(1,0,0,0)
					
					FindEdges(recipphi);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					
			End Select
			
		Case 5 ; Space - Semiregular
			
			Select Polytope
				Case 1 ; Truncated Tetrahedron
					CreatePoint(0,3.0*root1_11,root1_11,root1_11)
					CreatePoint(0,root1_11,3.0*root1_11,root1_11)
					CreatePoint(0,root1_11,root1_11,3.0*root1_11)
					
					CreatePoint(0,3.0*root1_11,-root1_11,-root1_11)
					CreatePoint(0,root1_11,-3.0*root1_11,-root1_11)
					CreatePoint(0,root1_11,-root1_11,-3.0*root1_11)
					
					CreatePoint(0,-3.0*root1_11,root1_11,-root1_11)
					CreatePoint(0,-root1_11,3.0*root1_11,-root1_11)
					CreatePoint(0,-root1_11,root1_11,-3.0*root1_11)
					
					CreatePoint(0,-3.0*root1_11,-root1_11,root1_11)
					CreatePoint(0,-root1_11,-3.0*root1_11,root1_11)
					CreatePoint(0,-root1_11,-root1_11,3.0*root1_11)
					
					CreateLine(0,3.0*root1_11,root1_11,root1_11,0,root1_11,3.0*root1_11,root1_11,"d","a","")
					CreateLine(0,root1_11,3.0*root1_11,root1_11,0,root1_11,root1_11,3.0*root1_11,"b","a","")
					CreateLine(0,root1_11,root1_11,3.0*root1_11,0,3.0*root1_11,root1_11,root1_11,"c","a","")
					
					CreateLine(0,3.0*root1_11,-root1_11,-root1_11,0,root1_11,-3.0*root1_11,-root1_11,"c","b","")
					CreateLine(0,root1_11,-3.0*root1_11,-root1_11,0,root1_11,-root1_11,-3.0*root1_11,"a","b","")
					CreateLine(0,root1_11,-root1_11,-3.0*root1_11,0,3.0*root1_11,-root1_11,-root1_11,"d","b","")
					
					CreateLine(0,-3.0*root1_11,root1_11,-root1_11,0,-root1_11,3.0*root1_11,-root1_11,"b","c","")
					CreateLine(0,-root1_11,3.0*root1_11,-root1_11,0,-root1_11,root1_11,-3.0*root1_11,"d","c","")
					CreateLine(0,-root1_11,root1_11,-3.0*root1_11,0,-3.0*root1_11,root1_11,-root1_11,"a","c","")
					
					CreateLine(0,-3.0*root1_11,-root1_11,root1_11,0,-root1_11,-3.0*root1_11,root1_11,"a","d","")
					CreateLine(0,-root1_11,-3.0*root1_11,root1_11,0,-root1_11,-root1_11,3.0*root1_11,"c","d","")
					CreateLine(0,-root1_11,-root1_11,3.0*root1_11,0,-3.0*root1_11,-root1_11,root1_11,"b","d","")
					
					CreateLine(0,3.0*root1_11,root1_11,root1_11,0,3.0*root1_11,-root1_11,-root1_11,"cd","","")
					CreateLine(0,root1_11,3.0*root1_11,root1_11,0,-root1_11,3.0*root1_11,-root1_11,"bd","","")
					CreateLine(0,root1_11,root1_11,3.0*root1_11,0,-root1_11,-root1_11,3.0*root1_11,"bc","","")
					
					CreateLine(0,-3.0*root1_11,root1_11,-root1_11,0,-3.0*root1_11,-root1_11,root1_11,"ab","","")
					CreateLine(0,root1_11,-3.0*root1_11,-root1_11,0,-root1_11,-3.0*root1_11,root1_11,"ac","","")
					CreateLine(0,root1_11,-root1_11,-3.0*root1_11,0,-root1_11,root1_11,-3.0*root1_11,"ad","","")
					
					Cells1=4
					Cells2=4
					
				Case 2 ; Truncated Cube
				
					CreatePoint(0,truncubec,truncubec,truncubet)
					CreatePoint(0,truncubec,truncubet,truncubec)
					CreatePoint(0,truncubet,truncubec,truncubec)
				
					CreatePoint(0,truncubec,truncubec,-truncubet)
					CreatePoint(0,truncubec,truncubet,-truncubec)
					CreatePoint(0,truncubet,truncubec,-truncubec)
				
					CreatePoint(0,truncubec,-truncubec,truncubet)
					CreatePoint(0,truncubec,-truncubet,truncubec)
					CreatePoint(0,truncubet,-truncubec,truncubec)
				
					CreatePoint(0,truncubec,-truncubec,-truncubet)
					CreatePoint(0,truncubec,-truncubet,-truncubec)
					CreatePoint(0,truncubet,-truncubec,-truncubec)
				
					CreatePoint(0,-truncubec,truncubec,truncubet)
					CreatePoint(0,-truncubec,truncubet,truncubec)
					CreatePoint(0,-truncubet,truncubec,truncubec)
				
					CreatePoint(0,-truncubec,truncubec,-truncubet)
					CreatePoint(0,-truncubec,truncubet,-truncubec)
					CreatePoint(0,-truncubet,truncubec,-truncubec)
				
					CreatePoint(0,-truncubec,-truncubec,truncubet)
					CreatePoint(0,-truncubec,-truncubet,truncubec)
					CreatePoint(0,-truncubet,-truncubec,truncubec)
				
					CreatePoint(0,-truncubec,-truncubec,-truncubet)
					CreatePoint(0,-truncubec,-truncubet,-truncubec)
					CreatePoint(0,-truncubet,-truncubec,-truncubec)
					
					CreateLine(0,truncubec,truncubec,truncubet,0,truncubec,truncubet,truncubec,"e","a","")
					CreateLine(0,truncubec,truncubet,truncubec,0,truncubet,truncubec,truncubec,"a","a","")
					CreateLine(0,truncubet,truncubec,truncubec,0,truncubec,truncubec,truncubet,"c","a","")
				
					CreateLine(0,truncubec,truncubec,-truncubet,0,truncubec,truncubet,-truncubec,"e","b","")
					CreateLine(0,truncubec,truncubet,-truncubec,0,truncubet,truncubec,-truncubec,"b","b","")
					CreateLine(0,truncubet,truncubec,-truncubec,0,truncubec,truncubec,-truncubet,"c","b","")
				
					CreateLine(0,truncubec,-truncubec,truncubet,0,truncubec,-truncubet,truncubec,"e","c","")
					CreateLine(0,truncubec,-truncubet,truncubec,0,truncubet,-truncubec,truncubec,"a","c","")
					CreateLine(0,truncubet,-truncubec,truncubec,0,truncubec,-truncubec,truncubet,"d","c","")
				
					CreateLine(0,truncubec,-truncubec,-truncubet,0,truncubec,-truncubet,-truncubec,"e","d","")
					CreateLine(0,truncubec,-truncubet,-truncubec,0,truncubet,-truncubec,-truncubec,"b","d","")
					CreateLine(0,truncubet,-truncubec,-truncubec,0,truncubec,-truncubec,-truncubet,"d","d","")
				
					CreateLine(0,-truncubec,truncubec,truncubet,0,-truncubec,truncubet,truncubec,"f","e","")
					CreateLine(0,-truncubec,truncubet,truncubec,0,-truncubet,truncubec,truncubec,"a","e","")
					CreateLine(0,-truncubet,truncubec,truncubec,0,-truncubec,truncubec,truncubet,"c","e","")
				
					CreateLine(0,-truncubec,truncubec,-truncubet,0,-truncubec,truncubet,-truncubec,"f","f","")
					CreateLine(0,-truncubec,truncubet,-truncubec,0,-truncubet,truncubec,-truncubec,"b","f","")
					CreateLine(0,-truncubet,truncubec,-truncubec,0,-truncubec,truncubec,-truncubet,"c","f","")
				
					CreateLine(0,-truncubec,-truncubec,truncubet,0,-truncubec,-truncubet,truncubec,"f","g","")
					CreateLine(0,-truncubec,-truncubet,truncubec,0,-truncubet,-truncubec,truncubec,"a","g","")
					CreateLine(0,-truncubet,-truncubec,truncubec,0,-truncubec,-truncubec,truncubet,"d","g","")
				
					CreateLine(0,-truncubec,-truncubec,-truncubet,0,-truncubec,-truncubet,-truncubec,"f","h","")
					CreateLine(0,-truncubec,-truncubet,-truncubec,0,-truncubet,-truncubec,-truncubec,"b","h","")
					CreateLine(0,-truncubet,-truncubec,-truncubec,0,-truncubec,-truncubec,-truncubet,"d","h","")
				
					CreateLine(0,truncubec,truncubec,-truncubet,0,truncubec,truncubec,truncubet,"ce","","")
					CreateLine(0,truncubec,-truncubec,-truncubet,0,truncubec,-truncubec,truncubet,"de","","")
					CreateLine(0,-truncubec,truncubec,-truncubet,0,-truncubec,truncubec,truncubet,"cf","","")
					CreateLine(0,-truncubec,-truncubec,-truncubet,0,-truncubec,-truncubec,truncubet,"df","","")
				
					CreateLine(0,truncubec,truncubet,truncubec,0,truncubec,-truncubet,truncubec,"ae","","")
					CreateLine(0,truncubec,truncubet,-truncubec,0,truncubec,-truncubet,-truncubec,"be","","")
					CreateLine(0,-truncubec,truncubet,truncubec,0,-truncubec,-truncubet,truncubec,"af","","")
					CreateLine(0,-truncubec,truncubet,-truncubec,0,-truncubec,-truncubet,-truncubec,"bf","","")
				
					CreateLine(0,truncubet,truncubec,truncubec,0,-truncubet,truncubec,truncubec,"ac","","")
					CreateLine(0,truncubet,truncubec,-truncubec,0,-truncubet,truncubec,-truncubec,"bc","","")
					CreateLine(0,truncubet,-truncubec,truncubec,0,-truncubet,-truncubec,truncubec,"ad","","")
					CreateLine(0,truncubet,-truncubec,-truncubec,0,-truncubet,-truncubec,-truncubec,"bd","","")
					
					Cells1=6
					Cells2=8
				
				Case 3 ; Cuboctahedron
					CreatePoint(0,0,roothalf,roothalf)
					CreatePoint(0,0,roothalf,-roothalf)
					CreatePoint(0,0,-roothalf,roothalf)
					CreatePoint(0,0,-roothalf,-roothalf)
					CreatePoint(0,roothalf,0,roothalf)
					CreatePoint(0,roothalf,0,-roothalf)
					CreatePoint(0,-roothalf,0,roothalf)
					CreatePoint(0,-roothalf,0,-roothalf)
					CreatePoint(0,roothalf,roothalf,0)
					CreatePoint(0,roothalf,-roothalf,0)
					CreatePoint(0,-roothalf,roothalf,0)
					CreatePoint(0,-roothalf,-roothalf,0)
					
					CreateLine(0,0,roothalf,roothalf,0,roothalf,0,roothalf,"a","a","")
					CreateLine(0,roothalf,0,roothalf,0,0,-roothalf,roothalf,"a","b","")
					CreateLine(0,0,-roothalf,roothalf,0,-roothalf,0,roothalf,"a","c","")
					CreateLine(0,-roothalf,0,roothalf,0,0,roothalf,roothalf,"a","d","")
					
					CreateLine(0,0,roothalf,-roothalf,0,roothalf,0,-roothalf,"b","e","")
					CreateLine(0,roothalf,0,-roothalf,0,0,-roothalf,-roothalf,"b","f","")
					CreateLine(0,0,-roothalf,-roothalf,0,-roothalf,0,-roothalf,"b","g","")
					CreateLine(0,-roothalf,0,-roothalf,0,0,roothalf,-roothalf,"b","h","")
					
					CreateLine(0,0,roothalf,roothalf,0,roothalf,roothalf,0,"c","a","")
					CreateLine(0,roothalf,roothalf,0,0,0,roothalf,-roothalf,"c","e","")
					CreateLine(0,0,roothalf,-roothalf,0,-roothalf,roothalf,0,"c","h","")
					CreateLine(0,-roothalf,roothalf,0,0,0,roothalf,roothalf,"c","d","")
					
					CreateLine(0,0,-roothalf,roothalf,0,roothalf,-roothalf,0,"d","b","")
					CreateLine(0,roothalf,-roothalf,0,0,0,-roothalf,-roothalf,"d","f","")
					CreateLine(0,0,-roothalf,-roothalf,0,-roothalf,-roothalf,0,"d","g","")
					CreateLine(0,-roothalf,-roothalf,0,0,0,-roothalf,roothalf,"d","c","")
					
					CreateLine(0,roothalf,0,roothalf,0,roothalf,roothalf,0,"e","a","")
					CreateLine(0,roothalf,roothalf,0,0,roothalf,0,-roothalf,"e","e","")
					CreateLine(0,roothalf,0,-roothalf,0,roothalf,-roothalf,0,"e","f","")
					CreateLine(0,roothalf,-roothalf,0,0,roothalf,0,roothalf,"e","b","")
					
					CreateLine(0,-roothalf,0,roothalf,0,-roothalf,roothalf,0,"f","d","")
					CreateLine(0,-roothalf,roothalf,0,0,-roothalf,0,-roothalf,"f","h","")
					CreateLine(0,-roothalf,0,-roothalf,0,-roothalf,-roothalf,0,"f","g","")
					CreateLine(0,-roothalf,-roothalf,0,0,-roothalf,0,roothalf,"f","c","")
					
					Cells1=6
					Cells2=8
					
				Case 4 ; Icosadodecahedron
					CreatePoint(0,0,0,1)
					CreatePoint(0,0,0,-1)
					CreatePoint(0,0,1,0)
					CreatePoint(0,0,-1,0)
					CreatePoint(0,1,0,0)
					CreatePoint(0,-1,0,0)

					CreatePoint(0,ihconsta/(2*ihconstb),(ihconsta+ihconstb)/(2*ihconstb),0.5)
					CreatePoint(0,0.5,ihconsta/(2*ihconstb),(ihconsta+ihconstb)/(2*ihconstb))
					CreatePoint(0,(ihconsta+ihconstb)/(2*ihconstb),0.5,ihconsta/(2*ihconstb))
					CreatePoint(0,ihconsta/(2*ihconstb),(ihconsta+ihconstb)/(2*ihconstb),-0.5)
					CreatePoint(0,0.5,ihconsta/(2*ihconstb),-(ihconsta+ihconstb)/(2*ihconstb))
					CreatePoint(0,(ihconsta+ihconstb)/(2*ihconstb),0.5,-ihconsta/(2*ihconstb))
					CreatePoint(0,ihconsta/(2*ihconstb),-(ihconsta+ihconstb)/(2*ihconstb),0.5)
					CreatePoint(0,0.5,-ihconsta/(2*ihconstb),(ihconsta+ihconstb)/(2*ihconstb))
					CreatePoint(0,(ihconsta+ihconstb)/(2*ihconstb),-0.5,ihconsta/(2*ihconstb))
					CreatePoint(0,ihconsta/(2*ihconstb),-(ihconsta+ihconstb)/(2*ihconstb),-0.5)
					CreatePoint(0,0.5,-ihconsta/(2*ihconstb),-(ihconsta+ihconstb)/(2*ihconstb))
					CreatePoint(0,(ihconsta+ihconstb)/(2*ihconstb),-0.5,-ihconsta/(2*ihconstb))
					CreatePoint(0,-ihconsta/(2*ihconstb),(ihconsta+ihconstb)/(2*ihconstb),0.5)
					CreatePoint(0,-0.5,ihconsta/(2*ihconstb),(ihconsta+ihconstb)/(2*ihconstb))
					CreatePoint(0,-(ihconsta+ihconstb)/(2*ihconstb),0.5,ihconsta/(2*ihconstb))
					CreatePoint(0,-ihconsta/(2*ihconstb),(ihconsta+ihconstb)/(2*ihconstb),-0.5)
					CreatePoint(0,-0.5,ihconsta/(2*ihconstb),-(ihconsta+ihconstb)/(2*ihconstb))
					CreatePoint(0,-(ihconsta+ihconstb)/(2*ihconstb),0.5,-ihconsta/(2*ihconstb))
					CreatePoint(0,-ihconsta/(2*ihconstb),-(ihconsta+ihconstb)/(2*ihconstb),0.5)
					CreatePoint(0,-0.5,-ihconsta/(2*ihconstb),(ihconsta+ihconstb)/(2*ihconstb))
					CreatePoint(0,-(ihconsta+ihconstb)/(2*ihconstb),-0.5,ihconsta/(2*ihconstb))
					CreatePoint(0,-ihconsta/(2*ihconstb),-(ihconsta+ihconstb)/(2*ihconstb),-0.5)
					CreatePoint(0,-0.5,-ihconsta/(2*ihconstb),-(ihconsta+ihconstb)/(2*ihconstb))
					CreatePoint(0,-(ihconsta+ihconstb)/(2*ihconstb),-0.5,-ihconsta/(2*ihconstb))
					
					FindEdges(ihconsta/ihconstb)

;					CreatePoint(0,-ihconsta/ihconstb,(ihconsta+ihconstb)/2*ihconstb,0.5)
;					CreatePoint(0,ihconsta/ihconstb,-(ihconsta+ihconstb)/2*ihconstb,-0.5)
;					CreatePoint(0,(ihconsta+ihconstb)/2*ihconstb,0.5,-ihconsta/ihconstb)
;					CreatePoint(0,-(ihconsta+ihconstb)/2*ihconstb,-0.5,ihconsta/ihconstb)
;					CreatePoint(0,0.5,-ihconsta/ihconstb,(ihconsta+ihconstb)/2*ihconstb)
;					CreatePoint(0,-0.5,ihconsta/ihconstb,-(ihconsta+ihconstb)/2*ihconstb)
;
;					CreatePoint(0,-(ihconsta+ihconstb)/2*ihconstb,-0.5,-ihconsta/ihconstb)
;					CreateLine(0,-ihconsta,ihconstb,0,0,-ihconstb,0,-ihconsta,"en","","");1-#4
;					CreateLine(0,ihconsta,-ihconstb,0,0,ihconstb,0,ihconsta,"iq","","");2-#3
;					CreateLine(0,ihconstb,0,-ihconsta,0,0,-ihconsta,-ihconstb,"cj","","");3-#6
;					CreateLine(0,-ihconstb,0,ihconsta,0,0,ihconsta,ihconstb,"ot","","");4-#5
;					CreateLine(0,0,-ihconsta,ihconstb,0,-ihconsta,-ihconstb,0,"rs","","");5-#2
;					CreateLine(0,0,ihconsta,-ihconstb,0,ihconsta,ihconstb,0,"ab","","");6-#1
;
;					CreateLine(0,-ihconsta,ihconstb,0,0,0,ihconsta,-ihconstb,"ae","","");1-6
;					CreateLine(0,ihconsta,-ihconstb,0,0,0,-ihconsta,ihconstb,"qr","","");2-5
;					CreateLine(0,ihconstb,0,-ihconsta,0,0,ihconsta,-ihconstb,"bc","","");3-6
;					CreateLine(0,-ihconstb,0,ihconsta,0,0,-ihconsta,ihconstb,"st","","");4-5
;					CreateLine(0,-ihconsta,ihconstb,0,0,-ihconstb,0,ihconsta,"no","","");1-4
;					CreateLine(0,ihconsta,-ihconstb,0,0,ihconstb,0,-ihconsta,"ij","","");2-3
;
;					CreateLine(0,ihconsta,ihconstb,0,0,0,ihconsta,ihconstb,"fg","","");#1-#5
;					CreateLine(0,-ihconsta,-ihconstb,0,0,-ihconstb,0,-ihconsta,"lm","","");#2-#4
;					CreateLine(0,ihconstb,0,ihconsta,0,ihconsta,ihconstb,0,"gh","","");#3-#1
;					CreateLine(0,-ihconstb,0,-ihconsta,0,0,-ihconsta,-ihconstb,"dl","","");#4-#6
;					CreateLine(0,0,ihconsta,ihconstb,0,ihconstb,0,ihconsta,"gp","","");#5-#3
;					CreateLine(0,0,-ihconsta,-ihconstb,0,-ihconsta,-ihconstb,0,"kl","","");#6-#2

				
			End Select
			
		Case 6 ; Hyperspace - Semi-regular
		
			Select Polytope
			
				Case 1 ; Dispentachoron
					CreatePoint(Sqr(24.0)/8.0,-dispent,-dispent,-dispent)
					CreatePoint(Sqr(24.0)/8.0,-dispent,dispent,dispent)
					CreatePoint(Sqr(24.0)/8.0,dispent,-dispent,dispent)
					CreatePoint(Sqr(24.0)/8.0,dispent,dispent,-dispent)
					CreatePoint(-1.0/Sqr(6.0),-Sqr(5.0/6.0),0,0)
					CreatePoint(-1.0/Sqr(6.0),0,-Sqr(5.0/6.0),0)
					CreatePoint(-1.0/Sqr(6.0),0,0,-Sqr(5.0/6.0))
					CreatePoint(-1.0/Sqr(6.0),Sqr(5.0/6.0),0,0)
					CreatePoint(-1.0/Sqr(6.0),0,Sqr(5.0/6.0),0)
					CreatePoint(-1.0/Sqr(6.0),0,0,Sqr(5.0/6.0))
					
					CreateLine(-1.0/Sqr(6.0),Sqr(5.0/6.0),0,0,-1.0/Sqr(6.0),0,-Sqr(5.0/6.0),0,"ae","c","")
					CreateLine(-1.0/Sqr(6.0),Sqr(5.0/6.0),0,0,-1.0/Sqr(6.0),0,0,-Sqr(5.0/6.0),"ae","d","")
					CreateLine(-1.0/Sqr(6.0),Sqr(5.0/6.0),0,0,-1.0/Sqr(6.0),0,Sqr(5.0/6.0),0,"ab","d","")
					CreateLine(-1.0/Sqr(6.0),Sqr(5.0/6.0),0,0,-1.0/Sqr(6.0),0,0,Sqr(5.0/6.0),"ab","c","")
					CreateLine(-1.0/Sqr(6.0),0,0,-Sqr(5.0/6.0),-1.0/Sqr(6.0),0,-Sqr(5.0/6.0),0,"ae","a","")
					CreateLine(-1.0/Sqr(6.0),0,Sqr(5.0/6.0),0,-1.0/Sqr(6.0),0,0,-Sqr(5.0/6.0),"ad","d","")
					CreateLine(-1.0/Sqr(6.0),0,0,Sqr(5.0/6.0),-1.0/Sqr(6.0),0,Sqr(5.0/6.0),0,"ab","b","")
					CreateLine(-1.0/Sqr(6.0),0,-Sqr(5.0/6.0),0,-1.0/Sqr(6.0),0,0,Sqr(5.0/6.0),"ac","c","")
					CreateLine(-1.0/Sqr(6.0),-Sqr(5.0/6.0),0,0,-1.0/Sqr(6.0),0,-Sqr(5.0/6.0),0,"ac","a","")
					CreateLine(-1.0/Sqr(6.0),-Sqr(5.0/6.0),0,0,-1.0/Sqr(6.0),0,0,-Sqr(5.0/6.0),"ad","a","")
					CreateLine(-1.0/Sqr(6.0),-Sqr(5.0/6.0),0,0,-1.0/Sqr(6.0),0,Sqr(5.0/6.0),0,"ad","b","")
					CreateLine(-1.0/Sqr(6.0),-Sqr(5.0/6.0),0,0,-1.0/Sqr(6.0),0,0,Sqr(5.0/6.0),"ac","b","")

					CreateLine(Sqr(24.0)/8.0,-dispent,-dispent,-dispent,-1.0/Sqr(6.0),-Sqr(5.0/6.0),0,0,"cd","a","")
					CreateLine(Sqr(24.0)/8.0,-dispent,-dispent,-dispent,-1.0/Sqr(6.0),0,-Sqr(5.0/6.0),0,"ce","a","")
					CreateLine(Sqr(24.0)/8.0,-dispent,-dispent,-dispent,-1.0/Sqr(6.0),0,0,-Sqr(5.0/6.0),"de","a","")
					
					CreateLine(Sqr(24.0)/8.0,-dispent,dispent,dispent,-1.0/Sqr(6.0),-Sqr(5.0/6.0),0,0,"cd","b","")
					CreateLine(Sqr(24.0)/8.0,-dispent,dispent,dispent,-1.0/Sqr(6.0),0,Sqr(5.0/6.0),0,"bd","b","")
					CreateLine(Sqr(24.0)/8.0,-dispent,dispent,dispent,-1.0/Sqr(6.0),0,0,Sqr(5.0/6.0),"bc","b","")
					
					CreateLine(Sqr(24.0)/8.0,dispent,-dispent,dispent,-1.0/Sqr(6.0),Sqr(5.0/6.0),0,0,"be","c","")
					CreateLine(Sqr(24.0)/8.0,dispent,-dispent,dispent,-1.0/Sqr(6.0),0,-Sqr(5.0/6.0),0,"ce","c","")
					CreateLine(Sqr(24.0)/8.0,dispent,-dispent,dispent,-1.0/Sqr(6.0),0,0,Sqr(5.0/6.0),"bc","c","")
					
					CreateLine(Sqr(24.0)/8.0,dispent,dispent,-dispent,-1.0/Sqr(6.0),Sqr(5.0/6.0),0,0,"be","d","")
					CreateLine(Sqr(24.0)/8.0,dispent,dispent,-dispent,-1.0/Sqr(6.0),0,Sqr(5.0/6.0),0,"bd","d","")
					CreateLine(Sqr(24.0)/8.0,dispent,dispent,-dispent,-1.0/Sqr(6.0),0,0,-Sqr(5.0/6.0),"de","d","")
					
					CreateLine(Sqr(24.0)/8.0,-dispent,-dispent,-dispent,Sqr(24.0)/8.0,-dispent,dispent,dispent,"cd","e","")
					CreateLine(Sqr(24.0)/8.0,-dispent,-dispent,-dispent,Sqr(24.0)/8.0,dispent,-dispent,dispent,"ce","e","")
					CreateLine(Sqr(24.0)/8.0,-dispent,-dispent,-dispent,Sqr(24.0)/8.0,dispent,dispent,-dispent,"de","e","")
					
					CreateLine(Sqr(24.0)/8.0,-dispent,dispent,dispent,Sqr(24.0)/8.0,dispent,dispent,-dispent,"bd","e","")
					CreateLine(Sqr(24.0)/8.0,dispent,-dispent,dispent,Sqr(24.0)/8.0,-dispent,dispent,dispent,"bc","e","")
					CreateLine(Sqr(24.0)/8.0,dispent,dispent,-dispent,Sqr(24.0)/8.0,dispent,-dispent,dispent,"be","e","")
					
					Cells1=5
					Cells2=5
					
				Case 2 ; Truncated Tesseract
					CreatePoint(truntessc,truntessc,truntessc,truntesst)
					CreatePoint(truntessc,truntessc,truntesst,truntessc)
					CreatePoint(truntessc,truntesst,truntessc,truntessc)
					CreatePoint(truntesst,truntessc,truntessc,truntessc)
					CreatePoint(truntessc,truntessc,truntessc,-truntesst)
					CreatePoint(truntessc,truntessc,truntesst,-truntessc)
					CreatePoint(truntessc,truntesst,truntessc,-truntessc)
					CreatePoint(truntesst,truntessc,truntessc,-truntessc)
					CreatePoint(truntessc,truntessc,-truntessc,truntesst)
					CreatePoint(truntessc,truntessc,-truntesst,truntessc)
					CreatePoint(truntessc,truntesst,-truntessc,truntessc)
					CreatePoint(truntesst,truntessc,-truntessc,truntessc)
					CreatePoint(truntessc,truntessc,-truntessc,-truntesst)
					CreatePoint(truntessc,truntessc,-truntesst,-truntessc)
					CreatePoint(truntessc,truntesst,-truntessc,-truntessc)
					CreatePoint(truntesst,truntessc,-truntessc,-truntessc)
					CreatePoint(truntessc,-truntessc,truntessc,truntesst)
					CreatePoint(truntessc,-truntessc,truntesst,truntessc)
					CreatePoint(truntessc,-truntesst,truntessc,truntessc)
					CreatePoint(truntesst,-truntessc,truntessc,truntessc)
					CreatePoint(truntessc,-truntessc,truntessc,-truntesst)
					CreatePoint(truntessc,-truntessc,truntesst,-truntessc)
					CreatePoint(truntessc,-truntesst,truntessc,-truntessc)
					CreatePoint(truntesst,-truntessc,truntessc,-truntessc)
					CreatePoint(truntessc,-truntessc,-truntessc,truntesst)
					CreatePoint(truntessc,-truntessc,-truntesst,truntessc)
					CreatePoint(truntessc,-truntesst,-truntessc,truntessc)
					CreatePoint(truntesst,-truntessc,-truntessc,truntessc)
					CreatePoint(truntessc,-truntessc,-truntessc,-truntesst)
					CreatePoint(truntessc,-truntessc,-truntesst,-truntessc)
					CreatePoint(truntessc,-truntesst,-truntessc,-truntessc)
					CreatePoint(truntesst,-truntessc,-truntessc,-truntessc)
					CreatePoint(-truntessc,truntessc,truntessc,truntesst)
					CreatePoint(-truntessc,truntessc,truntesst,truntessc)
					CreatePoint(-truntessc,truntesst,truntessc,truntessc)
					CreatePoint(-truntesst,truntessc,truntessc,truntessc)
					CreatePoint(-truntessc,truntessc,truntessc,-truntesst)
					CreatePoint(-truntessc,truntessc,truntesst,-truntessc)
					CreatePoint(-truntessc,truntesst,truntessc,-truntessc)
					CreatePoint(-truntesst,truntessc,truntessc,-truntessc)
					CreatePoint(-truntessc,truntessc,-truntessc,truntesst)
					CreatePoint(-truntessc,truntessc,-truntesst,truntessc)
					CreatePoint(-truntessc,truntesst,-truntessc,truntessc)
					CreatePoint(-truntesst,truntessc,-truntessc,truntessc)
					CreatePoint(-truntessc,truntessc,-truntessc,-truntesst)
					CreatePoint(-truntessc,truntessc,-truntesst,-truntessc)
					CreatePoint(-truntessc,truntesst,-truntessc,-truntessc)
					CreatePoint(-truntesst,truntessc,-truntessc,-truntessc)
					CreatePoint(-truntessc,-truntessc,truntessc,truntesst)
					CreatePoint(-truntessc,-truntessc,truntesst,truntessc)
					CreatePoint(-truntessc,-truntesst,truntessc,truntessc)
					CreatePoint(-truntesst,-truntessc,truntessc,truntessc)
					CreatePoint(-truntessc,-truntessc,truntessc,-truntesst)
					CreatePoint(-truntessc,-truntessc,truntesst,-truntessc)
					CreatePoint(-truntessc,-truntesst,truntessc,-truntessc)
					CreatePoint(-truntesst,-truntessc,truntessc,-truntessc)
					CreatePoint(-truntessc,-truntessc,-truntessc,truntesst)
					CreatePoint(-truntessc,-truntessc,-truntesst,truntessc)
					CreatePoint(-truntessc,-truntesst,-truntessc,truntessc)
					CreatePoint(-truntesst,-truntessc,-truntessc,truntessc)
					CreatePoint(-truntessc,-truntessc,-truntessc,-truntesst)
					CreatePoint(-truntessc,-truntessc,-truntesst,-truntessc)
					CreatePoint(-truntessc,-truntesst,-truntessc,-truntessc)
					CreatePoint(-truntesst,-truntessc,-truntessc,-truntessc)
					
					CreateLine(truntessc,truntessc,truntessc,truntesst,truntessc,truntessc,truntesst,truntessc,"ab","a","")
					CreateLine(truntessc,truntessc,truntessc,truntesst,truntessc,truntesst,truntessc,truntessc,"ac","a","")
					CreateLine(truntessc,truntessc,truntessc,truntesst,truntesst,truntessc,truntessc,truntessc,"bc","a","")
					CreateLine(truntessc,truntessc,truntesst,truntessc,truntessc,truntesst,truntessc,truntessc,"ad","a","")
					CreateLine(truntessc,truntessc,truntesst,truntessc,truntesst,truntessc,truntessc,truntessc,"bd","a","")
					CreateLine(truntessc,truntesst,truntessc,truntessc,truntesst,truntessc,truntessc,truntessc,"cd","a","")
					
					CreateLine(truntessc,truntessc,truntessc,-truntesst,truntessc,truntessc,truntesst,-truntessc,"ab","b","")
					CreateLine(truntessc,truntessc,truntessc,-truntesst,truntessc,truntesst,truntessc,-truntessc,"ac","b","")
					CreateLine(truntessc,truntessc,truntessc,-truntesst,truntesst,truntessc,truntessc,-truntessc,"bc","b","")
					CreateLine(truntessc,truntessc,truntesst,-truntessc,truntessc,truntesst,truntessc,-truntessc,"ae","b","")
					CreateLine(truntessc,truntessc,truntesst,-truntessc,truntesst,truntessc,truntessc,-truntessc,"be","b","")
					CreateLine(truntessc,truntesst,truntessc,-truntessc,truntesst,truntessc,truntessc,-truntessc,"ce","b","")
					
					CreateLine(truntessc,truntessc,-truntessc,truntesst,truntessc,truntessc,-truntesst,truntessc,"ab","c","")
					CreateLine(truntessc,truntessc,-truntessc,truntesst,truntessc,truntesst,-truntessc,truntessc,"af","c","")
					CreateLine(truntessc,truntessc,-truntessc,truntesst,truntesst,truntessc,-truntessc,truntessc,"bf","c","")
					CreateLine(truntessc,truntessc,-truntesst,truntessc,truntessc,truntesst,-truntessc,truntessc,"ad","c","")
					CreateLine(truntessc,truntessc,-truntesst,truntessc,truntesst,truntessc,-truntessc,truntessc,"bd","c","")
					CreateLine(truntessc,truntesst,-truntessc,truntessc,truntesst,truntessc,-truntessc,truntessc,"df","c","")
					
					CreateLine(truntessc,truntessc,-truntessc,-truntesst,truntessc,truntessc,-truntesst,-truntessc,"ab","d","")
					CreateLine(truntessc,truntessc,-truntessc,-truntesst,truntessc,truntesst,-truntessc,-truntessc,"af","d","")
					CreateLine(truntessc,truntessc,-truntessc,-truntesst,truntesst,truntessc,-truntessc,-truntessc,"bf","d","")
					CreateLine(truntessc,truntessc,-truntesst,-truntessc,truntessc,truntesst,-truntessc,-truntessc,"ae","d","")
					CreateLine(truntessc,truntessc,-truntesst,-truntessc,truntesst,truntessc,-truntessc,-truntessc,"be","d","")
					CreateLine(truntessc,truntesst,-truntessc,-truntessc,truntesst,truntessc,-truntessc,-truntessc,"ef","d","")
					
					CreateLine(truntessc,-truntessc,truntessc,truntesst,truntessc,-truntessc,truntesst,truntessc,"ag","e","")
					CreateLine(truntessc,-truntessc,truntessc,truntesst,truntessc,-truntesst,truntessc,truntessc,"ac","e","")
					CreateLine(truntessc,-truntessc,truntessc,truntesst,truntesst,-truntessc,truntessc,truntessc,"cg","e","")
					CreateLine(truntessc,-truntessc,truntesst,truntessc,truntessc,-truntesst,truntessc,truntessc,"ad","e","")
					CreateLine(truntessc,-truntessc,truntesst,truntessc,truntesst,-truntessc,truntessc,truntessc,"dg","e","")
					CreateLine(truntessc,-truntesst,truntessc,truntessc,truntesst,-truntessc,truntessc,truntessc,"cd","e","")
					
					CreateLine(truntessc,-truntessc,truntessc,-truntesst,truntessc,-truntessc,truntesst,-truntessc,"ag","f","")
					CreateLine(truntessc,-truntessc,truntessc,-truntesst,truntessc,-truntesst,truntessc,-truntessc,"ac","f","")
					CreateLine(truntessc,-truntessc,truntessc,-truntesst,truntesst,-truntessc,truntessc,-truntessc,"cg","f","")
					CreateLine(truntessc,-truntessc,truntesst,-truntessc,truntessc,-truntesst,truntessc,-truntessc,"ae","f","")
					CreateLine(truntessc,-truntessc,truntesst,-truntessc,truntesst,-truntessc,truntessc,-truntessc,"eg","f","")
					CreateLine(truntessc,-truntesst,truntessc,-truntessc,truntesst,-truntessc,truntessc,-truntessc,"ce","f","")
					
					CreateLine(truntessc,-truntessc,-truntessc,truntesst,truntessc,-truntessc,-truntesst,truntessc,"ag","g","")
					CreateLine(truntessc,-truntessc,-truntessc,truntesst,truntessc,-truntesst,-truntessc,truntessc,"af","g","")
					CreateLine(truntessc,-truntessc,-truntessc,truntesst,truntesst,-truntessc,-truntessc,truntessc,"fg","g","")
					CreateLine(truntessc,-truntessc,-truntesst,truntessc,truntessc,-truntesst,-truntessc,truntessc,"ad","g","")
					CreateLine(truntessc,-truntessc,-truntesst,truntessc,truntesst,-truntessc,-truntessc,truntessc,"dg","g","")
					CreateLine(truntessc,-truntesst,-truntessc,truntessc,truntesst,-truntessc,-truntessc,truntessc,"df","g","")
					
					CreateLine(truntessc,-truntessc,-truntessc,-truntesst,truntessc,-truntessc,-truntesst,-truntessc,"ag","h","")
					CreateLine(truntessc,-truntessc,-truntessc,-truntesst,truntessc,-truntesst,-truntessc,-truntessc,"af","h","")
					CreateLine(truntessc,-truntessc,-truntessc,-truntesst,truntesst,-truntessc,-truntessc,-truntessc,"fg","h","")
					CreateLine(truntessc,-truntessc,-truntesst,-truntessc,truntessc,-truntesst,-truntessc,-truntessc,"ae","h","")
					CreateLine(truntessc,-truntessc,-truntesst,-truntessc,truntesst,-truntessc,-truntessc,-truntessc,"eg","h","")
					CreateLine(truntessc,-truntesst,-truntessc,-truntessc,truntesst,-truntessc,-truntessc,-truntessc,"ef","h","")

					CreateLine(-truntessc,truntessc,truntessc,truntesst,-truntessc,truntessc,truntesst,truntessc,"bh","i","")
					CreateLine(-truntessc,truntessc,truntessc,truntesst,-truntessc,truntesst,truntessc,truntessc,"ch","i","")
					CreateLine(-truntessc,truntessc,truntessc,truntesst,-truntesst,truntessc,truntessc,truntessc,"bc","i","")
					CreateLine(-truntessc,truntessc,truntesst,truntessc,-truntessc,truntesst,truntessc,truntessc,"dh","i","")
					CreateLine(-truntessc,truntessc,truntesst,truntessc,-truntesst,truntessc,truntessc,truntessc,"bd","i","")
					CreateLine(-truntessc,truntesst,truntessc,truntessc,-truntesst,truntessc,truntessc,truntessc,"cd","i","")
					
					CreateLine(-truntessc,truntessc,truntessc,-truntesst,-truntessc,truntessc,truntesst,-truntessc,"bh","j","")
					CreateLine(-truntessc,truntessc,truntessc,-truntesst,-truntessc,truntesst,truntessc,-truntessc,"ch","j","")
					CreateLine(-truntessc,truntessc,truntessc,-truntesst,-truntesst,truntessc,truntessc,-truntessc,"bc","j","")
					CreateLine(-truntessc,truntessc,truntesst,-truntessc,-truntessc,truntesst,truntessc,-truntessc,"eh","j","")
					CreateLine(-truntessc,truntessc,truntesst,-truntessc,-truntesst,truntessc,truntessc,-truntessc,"be","j","")
					CreateLine(-truntessc,truntesst,truntessc,-truntessc,-truntesst,truntessc,truntessc,-truntessc,"ce","j","")
					
					CreateLine(-truntessc,truntessc,-truntessc,truntesst,-truntessc,truntessc,-truntesst,truntessc,"bh","k","")
					CreateLine(-truntessc,truntessc,-truntessc,truntesst,-truntessc,truntesst,-truntessc,truntessc,"fh","k","")
					CreateLine(-truntessc,truntessc,-truntessc,truntesst,-truntesst,truntessc,-truntessc,truntessc,"bf","k","")
					CreateLine(-truntessc,truntessc,-truntesst,truntessc,-truntessc,truntesst,-truntessc,truntessc,"dh","k","")
					CreateLine(-truntessc,truntessc,-truntesst,truntessc,-truntesst,truntessc,-truntessc,truntessc,"bd","k","")
					CreateLine(-truntessc,truntesst,-truntessc,truntessc,-truntesst,truntessc,-truntessc,truntessc,"df","k","")
					
					CreateLine(-truntessc,truntessc,-truntessc,-truntesst,-truntessc,truntessc,-truntesst,-truntessc,"bh","l","")
					CreateLine(-truntessc,truntessc,-truntessc,-truntesst,-truntessc,truntesst,-truntessc,-truntessc,"fh","l","")
					CreateLine(-truntessc,truntessc,-truntessc,-truntesst,-truntesst,truntessc,-truntessc,-truntessc,"bf","l","")
					CreateLine(-truntessc,truntessc,-truntesst,-truntessc,-truntessc,truntesst,-truntessc,-truntessc,"eh","l","")
					CreateLine(-truntessc,truntessc,-truntesst,-truntessc,-truntesst,truntessc,-truntessc,-truntessc,"be","l","")
					CreateLine(-truntessc,truntesst,-truntessc,-truntessc,-truntesst,truntessc,-truntessc,-truntessc,"ef","l","")
					
					CreateLine(-truntessc,-truntessc,truntessc,truntesst,-truntessc,-truntessc,truntesst,truntessc,"gh","m","")
					CreateLine(-truntessc,-truntessc,truntessc,truntesst,-truntessc,-truntesst,truntessc,truntessc,"ch","m","")
					CreateLine(-truntessc,-truntessc,truntessc,truntesst,-truntesst,-truntessc,truntessc,truntessc,"cg","m","")
					CreateLine(-truntessc,-truntessc,truntesst,truntessc,-truntessc,-truntesst,truntessc,truntessc,"dh","m","")
					CreateLine(-truntessc,-truntessc,truntesst,truntessc,-truntesst,-truntessc,truntessc,truntessc,"dg","m","")
					CreateLine(-truntessc,-truntesst,truntessc,truntessc,-truntesst,-truntessc,truntessc,truntessc,"cd","m","")
					
					CreateLine(-truntessc,-truntessc,truntessc,-truntesst,-truntessc,-truntessc,truntesst,-truntessc,"gh","n","")
					CreateLine(-truntessc,-truntessc,truntessc,-truntesst,-truntessc,-truntesst,truntessc,-truntessc,"ch","n","")
					CreateLine(-truntessc,-truntessc,truntessc,-truntesst,-truntesst,-truntessc,truntessc,-truntessc,"cg","n","")
					CreateLine(-truntessc,-truntessc,truntesst,-truntessc,-truntessc,-truntesst,truntessc,-truntessc,"eh","n","")
					CreateLine(-truntessc,-truntessc,truntesst,-truntessc,-truntesst,-truntessc,truntessc,-truntessc,"eg","n","")
					CreateLine(-truntessc,-truntesst,truntessc,-truntessc,-truntesst,-truntessc,truntessc,-truntessc,"ce","n","")
					
					CreateLine(-truntessc,-truntessc,-truntessc,truntesst,-truntessc,-truntessc,-truntesst,truntessc,"gh","o","")
					CreateLine(-truntessc,-truntessc,-truntessc,truntesst,-truntessc,-truntesst,-truntessc,truntessc,"fh","o","")
					CreateLine(-truntessc,-truntessc,-truntessc,truntesst,-truntesst,-truntessc,-truntessc,truntessc,"fg","o","")
					CreateLine(-truntessc,-truntessc,-truntesst,truntessc,-truntessc,-truntesst,-truntessc,truntessc,"dh","o","")
					CreateLine(-truntessc,-truntessc,-truntesst,truntessc,-truntesst,-truntessc,-truntessc,truntessc,"dg","o","")
					CreateLine(-truntessc,-truntesst,-truntessc,truntessc,-truntesst,-truntessc,-truntessc,truntessc,"df","o","")
					
					CreateLine(-truntessc,-truntessc,-truntessc,-truntesst,-truntessc,-truntessc,-truntesst,-truntessc,"gh","p","")
					CreateLine(-truntessc,-truntessc,-truntessc,-truntesst,-truntessc,-truntesst,-truntessc,-truntessc,"fh","p","")
					CreateLine(-truntessc,-truntessc,-truntessc,-truntesst,-truntesst,-truntessc,-truntessc,-truntessc,"fg","p","")
					CreateLine(-truntessc,-truntessc,-truntesst,-truntessc,-truntessc,-truntesst,-truntessc,-truntessc,"eh","p","")
					CreateLine(-truntessc,-truntessc,-truntesst,-truntessc,-truntesst,-truntessc,-truntessc,-truntessc,"eg","p","")
					CreateLine(-truntessc,-truntesst,-truntessc,-truntessc,-truntesst,-truntessc,-truntessc,-truntessc,"ef","p","")
					
					CreateLine(truntessc,truntessc,truntessc,truntesst,truntessc,truntessc,truntessc,-truntesst,"abc","","")
					CreateLine(truntessc,truntessc,-truntessc,truntesst,truntessc,truntessc,-truntessc,-truntesst,"abf","","")
					CreateLine(truntessc,-truntessc,truntessc,truntesst,truntessc,-truntessc,truntessc,-truntesst,"acg","","")
					CreateLine(truntessc,-truntessc,-truntessc,truntesst,truntessc,-truntessc,-truntessc,-truntesst,"afg","","")
					CreateLine(-truntessc,truntessc,truntessc,truntesst,-truntessc,truntessc,truntessc,-truntesst,"bch","","")
					CreateLine(-truntessc,truntessc,-truntessc,truntesst,-truntessc,truntessc,-truntessc,-truntesst,"bfh","","")
					CreateLine(-truntessc,-truntessc,truntessc,truntesst,-truntessc,-truntessc,truntessc,-truntesst,"cgh","","")
					CreateLine(-truntessc,-truntessc,-truntessc,truntesst,-truntessc,-truntessc,-truntessc,-truntesst,"fgh","","")
					
					CreateLine(truntessc,truntessc,truntesst,truntessc,truntessc,truntessc,-truntesst,truntessc,"abd","","")
					CreateLine(truntessc,truntessc,truntesst,-truntessc,truntessc,truntessc,-truntesst,-truntessc,"abe","","")
					CreateLine(truntessc,-truntessc,truntesst,truntessc,truntessc,-truntessc,-truntesst,truntessc,"adg","","")
					CreateLine(truntessc,-truntessc,truntesst,-truntessc,truntessc,-truntessc,-truntesst,-truntessc,"aeg","","")
					CreateLine(-truntessc,truntessc,truntesst,truntessc,-truntessc,truntessc,-truntesst,truntessc,"bdh","","")
					CreateLine(-truntessc,truntessc,truntesst,-truntessc,-truntessc,truntessc,-truntesst,-truntessc,"beh","","")
					CreateLine(-truntessc,-truntessc,truntesst,truntessc,-truntessc,-truntessc,-truntesst,truntessc,"dgh","","")
					CreateLine(-truntessc,-truntessc,truntesst,-truntessc,-truntessc,-truntessc,-truntesst,-truntessc,"egh","","")
					
					CreateLine(truntessc,truntesst,truntessc,truntessc,truntessc,-truntesst,truntessc,truntessc,"acd","","")
					CreateLine(truntessc,truntesst,truntessc,-truntessc,truntessc,-truntesst,truntessc,-truntessc,"ace","","")
					CreateLine(truntessc,truntesst,-truntessc,truntessc,truntessc,-truntesst,-truntessc,truntessc,"adf","","")
					CreateLine(truntessc,truntesst,-truntessc,-truntessc,truntessc,-truntesst,-truntessc,-truntessc,"aef","","")
					CreateLine(-truntessc,truntesst,truntessc,truntessc,-truntessc,-truntesst,truntessc,truntessc,"cdh","","")
					CreateLine(-truntessc,truntesst,truntessc,-truntessc,-truntessc,-truntesst,truntessc,-truntessc,"ceh","","")
					CreateLine(-truntessc,truntesst,-truntessc,truntessc,-truntessc,-truntesst,-truntessc,truntessc,"dfh","","")
					CreateLine(-truntessc,truntesst,-truntessc,-truntessc,-truntessc,-truntesst,-truntessc,-truntessc,"efh","","")
					
					CreateLine(truntesst,truntessc,truntessc,truntessc,-truntesst,truntessc,truntessc,truntessc,"bcd","","")
					CreateLine(truntesst,truntessc,truntessc,-truntessc,-truntesst,truntessc,truntessc,-truntessc,"bce","","")
					CreateLine(truntesst,truntessc,-truntessc,truntessc,-truntesst,truntessc,-truntessc,truntessc,"bdf","","")
					CreateLine(truntesst,truntessc,-truntessc,-truntessc,-truntesst,truntessc,-truntessc,-truntessc,"bef","","")
					CreateLine(truntesst,-truntessc,truntessc,truntessc,-truntesst,-truntessc,truntessc,truntessc,"cdg","","")
					CreateLine(truntesst,-truntessc,truntessc,-truntessc,-truntesst,-truntessc,truntessc,-truntessc,"ceg","","")
					CreateLine(truntesst,-truntessc,-truntessc,truntessc,-truntesst,-truntessc,-truntessc,truntessc,"dfg","","")
					CreateLine(truntesst,-truntessc,-truntessc,-truntessc,-truntesst,-truntessc,-truntessc,-truntessc,"efg","","")
					
					Cells1=8
					Cells2=16
					
				Case 3 ; Rectified Tesseract
				
					CreatePoint(0,sqrtthird,sqrtthird,sqrtthird)
					CreatePoint(0,sqrtthird,sqrtthird,-sqrtthird)
					CreatePoint(0,sqrtthird,-sqrtthird,sqrtthird)
					CreatePoint(0,sqrtthird,-sqrtthird,-sqrtthird)
					CreatePoint(0,-sqrtthird,sqrtthird,sqrtthird)
					CreatePoint(0,-sqrtthird,sqrtthird,-sqrtthird)
					CreatePoint(0,-sqrtthird,-sqrtthird,sqrtthird)
					CreatePoint(0,-sqrtthird,-sqrtthird,-sqrtthird)
					
					CreatePoint(sqrtthird,0,sqrtthird,sqrtthird)
					CreatePoint(sqrtthird,0,sqrtthird,-sqrtthird)
					CreatePoint(sqrtthird,0,-sqrtthird,sqrtthird)
					CreatePoint(sqrtthird,0,-sqrtthird,-sqrtthird)
					CreatePoint(-sqrtthird,0,sqrtthird,sqrtthird)
					CreatePoint(-sqrtthird,0,sqrtthird,-sqrtthird)
					CreatePoint(-sqrtthird,0,-sqrtthird,sqrtthird)
					CreatePoint(-sqrtthird,0,-sqrtthird,-sqrtthird)
					
					CreatePoint(sqrtthird,sqrtthird,0,sqrtthird)
					CreatePoint(sqrtthird,sqrtthird,0,-sqrtthird)
					CreatePoint(sqrtthird,-sqrtthird,0,sqrtthird)
					CreatePoint(sqrtthird,-sqrtthird,0,-sqrtthird)
					CreatePoint(-sqrtthird,sqrtthird,0,sqrtthird)
					CreatePoint(-sqrtthird,sqrtthird,0,-sqrtthird)
					CreatePoint(-sqrtthird,-sqrtthird,0,sqrtthird)
					CreatePoint(-sqrtthird,-sqrtthird,0,-sqrtthird)
					
					CreatePoint(sqrtthird,sqrtthird,sqrtthird,0)
					CreatePoint(sqrtthird,sqrtthird,-sqrtthird,0)
					CreatePoint(sqrtthird,-sqrtthird,sqrtthird,0)
					CreatePoint(sqrtthird,-sqrtthird,-sqrtthird,0)
					CreatePoint(-sqrtthird,sqrtthird,sqrtthird,0)
					CreatePoint(-sqrtthird,sqrtthird,-sqrtthird,0)
					CreatePoint(-sqrtthird,-sqrtthird,sqrtthird,0)
					CreatePoint(-sqrtthird,-sqrtthird,-sqrtthird,0)
					
					CreateLine(0,sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,sqrtthird,"cd","a","")
					CreateLine(0,sqrtthird,sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,"bd","a","")
					CreateLine(0,sqrtthird,sqrtthird,sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,"bc","a","")
					CreateLine(0,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,sqrtthird,sqrtthird,"cd","i","")
					CreateLine(0,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,"bd","i","")
					CreateLine(0,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,"bc","i","")

					CreateLine(0,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,-sqrtthird,"ch","b","")
					CreateLine(0,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,-sqrtthird,"bh","b","")
					CreateLine(0,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,"bc","b","")
					CreateLine(0,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,-sqrtthird,"ch","j","")
					CreateLine(0,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,-sqrtthird,"bh","j","")
					CreateLine(0,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,"bc","j","")
					
					CreateLine(0,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,-sqrtthird,sqrtthird,"gd","c","")
					CreateLine(0,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,"bd","c","")
					CreateLine(0,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,"bg","c","")
					CreateLine(0,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,-sqrtthird,sqrtthird,"gd","k","")
					CreateLine(0,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,"bd","k","")
					CreateLine(0,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,"bg","k","")

					CreateLine(0,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,-sqrtthird,-sqrtthird,"gh","d","")
					CreateLine(0,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,-sqrtthird,"bh","d","")
					CreateLine(0,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,"bg","d","")
					CreateLine(0,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,-sqrtthird,"gh","l","")
					CreateLine(0,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,-sqrtthird,"bh","l","")
					CreateLine(0,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,"bg","l","")
					
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,sqrtthird,"cd","e","")
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,sqrtthird,"df","e","")
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,"cf","e","")
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,sqrtthird,sqrtthird,"cd","m","")
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,"df","m","")
					CreateLine(0,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,"cf","m","")

					CreateLine(0,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,-sqrtthird,"ch","f","")
					CreateLine(0,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,-sqrtthird,"fh","f","")
					CreateLine(0,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,"cf","f","")
					CreateLine(0,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,-sqrtthird,"ch","n","")
					CreateLine(0,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,"fh","n","")
					CreateLine(0,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,"cf","n","")
					
					CreateLine(0,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,-sqrtthird,sqrtthird,"dg","g","")
					CreateLine(0,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,sqrtthird,"df","g","")
					CreateLine(0,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,"fg","g","")
					CreateLine(0,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,-sqrtthird,sqrtthird,"dg","o","")
					CreateLine(0,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,"df","o","")
					CreateLine(0,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,"fg","o","")

					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,-sqrtthird,-sqrtthird,"gh","h","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,-sqrtthird,"fh","h","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,"fg","h","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,-sqrtthird,"gh","p","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,"fh","p","")
					CreateLine(0,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,"fg","p","")
					
					CreateLine(sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,"ad","a","")
					CreateLine(sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,"ac","a","")
					CreateLine(sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,sqrtthird,"ad","e","")
					CreateLine(sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,"ac","e","")

					CreateLine(sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,-sqrtthird,"ah","b","")
					CreateLine(sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,"ac","b","")
					CreateLine(sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,-sqrtthird,"ah","f","")
					CreateLine(sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,"ac","f","")

					CreateLine(sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,sqrtthird,"ad","c","")
					CreateLine(sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,"ag","c","")
					CreateLine(sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,sqrtthird,"ad","g","")
					CreateLine(sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,"ag","g","")

					CreateLine(sqrtthird,0,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,-sqrtthird,"ah","d","")
					CreateLine(sqrtthird,0,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,"ag","d","")
					CreateLine(sqrtthird,0,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,-sqrtthird,"ah","h","")
					CreateLine(sqrtthird,0,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,"ag","h","")

					CreateLine(-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,"ed","i","")
					CreateLine(-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,"ce","i","")
					CreateLine(-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,"ed","m","")
					CreateLine(-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,"ce","m","")

					CreateLine(-sqrtthird,0,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,-sqrtthird,"eh","j","")
					CreateLine(-sqrtthird,0,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,"ce","j","")
					CreateLine(-sqrtthird,0,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,"eh","n","")
					CreateLine(-sqrtthird,0,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,"ce","n","")

					CreateLine(-sqrtthird,0,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,sqrtthird,"ed","k","")
					CreateLine(-sqrtthird,0,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,"eg","k","")
					CreateLine(-sqrtthird,0,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,sqrtthird,"ed","o","")
					CreateLine(-sqrtthird,0,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,"eg","o","")

					CreateLine(-sqrtthird,0,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,-sqrtthird,"eh","l","")
					CreateLine(-sqrtthird,0,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,"eg","l","")
					CreateLine(-sqrtthird,0,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,-sqrtthird,"eh","p","")
					CreateLine(-sqrtthird,0,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,"eg","p","")
					
					CreateLine(sqrtthird,sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,"ab","a","")
					CreateLine(sqrtthird,sqrtthird,0,sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,"ab","c","")
					
					CreateLine(sqrtthird,sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,sqrtthird,0,"ab","b","")
					CreateLine(sqrtthird,sqrtthird,0,-sqrtthird,sqrtthird,sqrtthird,-sqrtthird,0,"ab","d","")
					
					CreateLine(sqrtthird,-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,"af","e","")
					CreateLine(sqrtthird,-sqrtthird,0,sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,"af","g","")
					
					CreateLine(sqrtthird,-sqrtthird,0,-sqrtthird,sqrtthird,-sqrtthird,sqrtthird,0,"af","f","")
					CreateLine(sqrtthird,-sqrtthird,0,-sqrtthird,sqrtthird,-sqrtthird,-sqrtthird,0,"af","h","")
					
					CreateLine(-sqrtthird,sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,"be","i","")
					CreateLine(-sqrtthird,sqrtthird,0,sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,"be","k","")
					
					CreateLine(-sqrtthird,sqrtthird,0,-sqrtthird,-sqrtthird,sqrtthird,sqrtthird,0,"be","j","")
					CreateLine(-sqrtthird,sqrtthird,0,-sqrtthird,-sqrtthird,sqrtthird,-sqrtthird,0,"be","l","")
					
					CreateLine(-sqrtthird,-sqrtthird,0,sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,"ef","m","")
					CreateLine(-sqrtthird,-sqrtthird,0,sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,"ef","o","")
					
					CreateLine(-sqrtthird,-sqrtthird,0,-sqrtthird,-sqrtthird,-sqrtthird,sqrtthird,0,"ef","n","")
					CreateLine(-sqrtthird,-sqrtthird,0,-sqrtthird,-sqrtthird,-sqrtthird,-sqrtthird,0,"ef","p","")
					
					Cells1=8
					Cells2=16
					
				Case 4 ; Snub Icosatetrachoron
				
					CreateEvenPermutationPoints(phi/2.0,0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,-0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,-0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,-0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,-0.5,-recipphi/2.0,0)
				
					FindEdges(recipphi)
					
				Case 5 ; Truncated Icosatetrachoron
				
					CreatePermutationPoints(ticosa,3*ticosa,3*ticosa,3*ticosa)	
					CreatePermutationPoints(ticosa,3*ticosa,3*ticosa,-3*ticosa)	
					CreatePermutationPoints(ticosa,3*ticosa,-3*ticosa,-3*ticosa)	
					CreatePermutationPoints(ticosa,-3*ticosa,-3*ticosa,-3*ticosa)	
					CreatePermutationPoints(-ticosa,3*ticosa,3*ticosa,3*ticosa)	
					CreatePermutationPoints(-ticosa,3*ticosa,3*ticosa,-3*ticosa)	
					CreatePermutationPoints(-ticosa,3*ticosa,-3*ticosa,-3*ticosa)	
					CreatePermutationPoints(-ticosa,-3*ticosa,-3*ticosa,-3*ticosa)	

					CreatePermutationPoints(4*ticosa,2*ticosa,2*ticosa,2*ticosa)	
					CreatePermutationPoints(4*ticosa,2*ticosa,2*ticosa,-2*ticosa)	
					CreatePermutationPoints(4*ticosa,2*ticosa,-2*ticosa,-2*ticosa)	
					CreatePermutationPoints(4*ticosa,-2*ticosa,-2*ticosa,-2*ticosa)	
					CreatePermutationPoints(-4*ticosa,2*ticosa,2*ticosa,2*ticosa)	
					CreatePermutationPoints(-4*ticosa,2*ticosa,2*ticosa,-2*ticosa)	
					CreatePermutationPoints(-4*ticosa,2*ticosa,-2*ticosa,-2*ticosa)	
					CreatePermutationPoints(-4*ticosa,-2*ticosa,-2*ticosa,-2*ticosa)	

					CreatePermutationPoints(5*ticosa,ticosa,ticosa,ticosa)	
					CreatePermutationPoints(5*ticosa,ticosa,ticosa,-ticosa)	
					CreatePermutationPoints(5*ticosa,ticosa,-ticosa,-ticosa)	
					CreatePermutationPoints(5*ticosa,-ticosa,-ticosa,-ticosa)	
					CreatePermutationPoints(-5*ticosa,ticosa,ticosa,ticosa)	
					CreatePermutationPoints(-5*ticosa,ticosa,ticosa,-ticosa)	
					CreatePermutationPoints(-5*ticosa,ticosa,-ticosa,-ticosa)	
					CreatePermutationPoints(-5*ticosa,-ticosa,-ticosa,-ticosa)	
					
					FindEdges(Sqr(1.0/7.0))
					
				Case 6 ; Square-Octagonal Duoprism
				
					CreatePoint(soprism,lsoprism,0,soprism)
					CreatePoint(soprism,ssoprism,ssoprism,soprism)
					CreatePoint(soprism,0,lsoprism,soprism)
					CreatePoint(soprism,-ssoprism,ssoprism,soprism)
					CreatePoint(soprism,-lsoprism,0,soprism)
					CreatePoint(soprism,-ssoprism,-ssoprism,soprism)
					CreatePoint(soprism,0,-lsoprism,soprism)
					CreatePoint(soprism,ssoprism,-ssoprism,soprism)
					
					CreatePoint(soprism,lsoprism,0,-soprism)
					CreatePoint(soprism,ssoprism,ssoprism,-soprism)
					CreatePoint(soprism,0,lsoprism,-soprism)
					CreatePoint(soprism,-ssoprism,ssoprism,-soprism)
					CreatePoint(soprism,-lsoprism,0,-soprism)
					CreatePoint(soprism,-ssoprism,-ssoprism,-soprism)
					CreatePoint(soprism,0,-lsoprism,-soprism)
					CreatePoint(soprism,ssoprism,-ssoprism,-soprism)
					
					CreatePoint(-soprism,lsoprism,0,soprism)
					CreatePoint(-soprism,ssoprism,ssoprism,soprism)
					CreatePoint(-soprism,0,lsoprism,soprism)
					CreatePoint(-soprism,-ssoprism,ssoprism,soprism)
					CreatePoint(-soprism,-lsoprism,0,soprism)
					CreatePoint(-soprism,-ssoprism,-ssoprism,soprism)
					CreatePoint(-soprism,0,-lsoprism,soprism)
					CreatePoint(-soprism,ssoprism,-ssoprism,soprism)
					
					CreatePoint(-soprism,lsoprism,0,-soprism)
					CreatePoint(-soprism,ssoprism,ssoprism,-soprism)
					CreatePoint(-soprism,0,lsoprism,-soprism)
					CreatePoint(-soprism,-ssoprism,ssoprism,-soprism)
					CreatePoint(-soprism,-lsoprism,0,-soprism)
					CreatePoint(-soprism,-ssoprism,-ssoprism,-soprism)
					CreatePoint(-soprism,0,-lsoprism,-soprism)
					CreatePoint(-soprism,ssoprism,-ssoprism,-soprism)
					
					FindEdges(2.0*soprism)
					
				Case 7 ; Diprismatotesseractihexadecachoron
				
					CreatePermutationPoints(dpth1,dpth2,dpth2,dpth2)
					CreatePermutationPoints(dpth1,dpth2,dpth2,-dpth2)
					CreatePermutationPoints(dpth1,dpth2,-dpth2,-dpth2)
					CreatePermutationPoints(dpth1,-dpth2,-dpth2,-dpth2)
					CreatePermutationPoints(-dpth1,dpth2,dpth2,dpth2)
					CreatePermutationPoints(-dpth1,dpth2,dpth2,-dpth2)
					CreatePermutationPoints(-dpth1,dpth2,-dpth2,-dpth2)
					CreatePermutationPoints(-dpth1,-dpth2,-dpth2,-dpth2)
					
					FindEdges(2.0*dpth2)
					
				Case 8 ; Prismatotesseractihexadecachoron
					
					CreatePermutationPoints(pth1,pth1,pth2,pth2)
					CreatePermutationPoints(pth1,pth1,pth2,-pth2)
					CreatePermutationPoints(pth1,pth1,-pth2,-pth2)
					CreatePermutationPoints(pth1,-pth1,pth2,pth2)
					CreatePermutationPoints(pth1,-pth1,pth2,-pth2)
					CreatePermutationPoints(pth1,-pth1,-pth2,-pth2)
					CreatePermutationPoints(-pth1,-pth1,pth2,pth2)
					CreatePermutationPoints(-pth1,-pth1,pth2,-pth2)
					CreatePermutationPoints(-pth1,-pth1,-pth2,-pth2)
					
					FindEdges(2.0*pth2)
					
				Case 9 ; Truncated Hexacosichoron
					
					;Make a Hexacosichoron
					CreateEvenPermutationPoints(phi/2.0,0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,-0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,-0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,-0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,-0.5,-recipphi/2.0,0)
					
					For w#=-0.5 To 0.5
						For x#=-0.5 To 0.5
							For y#=-0.5 To 0.5
								For z#=-0.5 To 0.5
									CreatePoint(w,x,y,z)
								Next
							Next
						Next
					Next
				
					CreatePoint(0,0,0,-1)
					CreatePoint(0,0,0,1)
					CreatePoint(0,0,-1,0)
					CreatePoint(0,0,1,0)
					CreatePoint(0,-1,0,0)
					CreatePoint(0,1,0,0)
					CreatePoint(-1,0,0,0)
					CreatePoint(1,0,0,0)
					
					FindEdges(recipphi);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					
					;Now truncate it
					
					Delete Each Point
					For l.LineType=Each LineType
						CreatePoint((l\a\w+l\a\w+l\b\w)/3.0,(l\a\x+l\a\x+l\b\x)/3.0,(l\a\y+l\a\y+l\b\y)/3.0,(l\a\z+l\a\z+l\b\z)/3.0)
						CreatePoint((l\a\w+l\b\w+l\b\w)/3.0,(l\a\x+l\b\x+l\b\x)/3.0,(l\a\y+l\b\y+l\b\y)/3.0,(l\a\z+l\b\z+l\b\z)/3.0)
					;	CreatePoint((l\a\w+l\b\w)/2.0,(l\a\x+l\b\x)/2.0,(l\a\y+l\b\y)/2.0,(l\a\z+l\b\z)/2.0)
					Next
					Delete Each LineType
					FindEdges(recipphi/3.0)
					
				Case 10 ; Truncated Hecatonicosachoron
				
					;Make a Hecatonicosachoron
					ED#=Sqr(8)
					CreatePermutationPoints(2.0/ED,2.0/ED,0,0)
					CreatePermutationPoints(2.0/ED,-2.0/ED,0,0)
					CreatePermutationPoints(-2.0/ED,-2.0/ED,0,0)

					CreatePermutationPoints(Sqr(5)/ED,1.0/ED,1.0/ED,1.0/ED)
					CreatePermutationPoints(Sqr(5)/ED,1.0/ED,1.0/ED,-1.0/ED)
					CreatePermutationPoints(Sqr(5)/ED,1.0/ED,-1.0/ED,-1.0/ED)
					CreatePermutationPoints(Sqr(5)/ED,-1.0/ED,-1.0/ED,-1.0/ED)
					CreatePermutationPoints(-Sqr(5)/ED,1.0/ED,1.0/ED,1.0/ED)
					CreatePermutationPoints(-Sqr(5)/ED,1.0/ED,1.0/ED,-1.0/ED)
					CreatePermutationPoints(-Sqr(5)/ED,1.0/ED,-1.0/ED,-1.0/ED)
					CreatePermutationPoints(-Sqr(5)/ED,-1.0/ED,-1.0/ED,-1.0/ED)
					
					CreatePermutationPoints(phi/ED,phi/ED,phi/ED,(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,phi/ED,-phi/ED,(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,-phi/ED,-phi/ED,(recipphi^2)/ED)
					CreatePermutationPoints(-phi/ED,-phi/ED,-phi/ED,(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,phi/ED,phi/ED,-(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,phi/ED,-phi/ED,-(recipphi^2)/ED)
					CreatePermutationPoints(phi/ED,-phi/ED,-phi/ED,-(recipphi^2)/ED)
					CreatePermutationPoints(-phi/ED,-phi/ED,-phi/ED,-(recipphi^2)/ED)
					
					CreatePermutationPoints((phi^2)/ED,recipphi/ED,recipphi/ED,recipphi/ED)
					CreatePermutationPoints((phi^2)/ED,-recipphi/ED,recipphi/ED,recipphi/ED)
					CreatePermutationPoints((phi^2)/ED,-recipphi/ED,-recipphi/ED,recipphi/ED)
					CreatePermutationPoints((phi^2)/ED,-recipphi/ED,-recipphi/ED,-recipphi/ED)
					CreatePermutationPoints(-(phi^2)/ED,recipphi/ED,recipphi/ED,recipphi/ED)
					CreatePermutationPoints(-(phi^2)/ED,-recipphi/ED,recipphi/ED,recipphi/ED)
					CreatePermutationPoints(-(phi^2)/ED,-recipphi/ED,-recipphi/ED,recipphi/ED)
					CreatePermutationPoints(-(phi^2)/ED,-recipphi/ED,-recipphi/ED,-recipphi/ED)
					
					CreateEvenPermutationPoints((phi^2)/ED,1/ED,(recipphi^2)/ED,0)
					CreateEvenPermutationPoints((phi^2)/ED,1/ED,-(recipphi^2)/ED,0)
					CreateEvenPermutationPoints((phi^2)/ED,-1/ED,(recipphi^2)/ED,0)
					CreateEvenPermutationPoints((phi^2)/ED,-1/ED,-(recipphi^2)/ED,0)
					CreateEvenPermutationPoints(-(phi^2)/ED,1/ED,(recipphi^2)/ED,0)
					CreateEvenPermutationPoints(-(phi^2)/ED,1/ED,-(recipphi^2)/ED,0)
					CreateEvenPermutationPoints(-(phi^2)/ED,-1/ED,(recipphi^2)/ED,0)
					CreateEvenPermutationPoints(-(phi^2)/ED,-1/ED,-(recipphi^2)/ED,0)
					
					CreateEvenPermutationPoints(Sqr(5)/ED,phi/ED,recipphi/ED,0)
					CreateEvenPermutationPoints(Sqr(5)/ED,phi/ED,-recipphi/ED,0)
					CreateEvenPermutationPoints(Sqr(5)/ED,-phi/ED,recipphi/ED,0)
					CreateEvenPermutationPoints(Sqr(5)/ED,-phi/ED,-recipphi/ED,0)
					CreateEvenPermutationPoints(-Sqr(5)/ED,phi/ED,recipphi/ED,0)
					CreateEvenPermutationPoints(-Sqr(5)/ED,phi/ED,-recipphi/ED,0)
					CreateEvenPermutationPoints(-Sqr(5)/ED,-phi/ED,recipphi/ED,0)
					CreateEvenPermutationPoints(-Sqr(5)/ED,-phi/ED,-recipphi/ED,0)
					
					CreateEvenPermutationPoints(2.0/ED,phi/ED,1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,phi/ED,1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,phi/ED,-1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,phi/ED,-1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,-phi/ED,1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,-phi/ED,1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,-phi/ED,-1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(2.0/ED,-phi/ED,-1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,phi/ED,1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,phi/ED,1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,phi/ED,-1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,phi/ED,-1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,-phi/ED,1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,-phi/ED,1.0/ED,-recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,-phi/ED,-1.0/ED,recipphi/ED)
					CreateEvenPermutationPoints(-2.0/ED,-phi/ED,-1.0/ED,-recipphi/ED)
					
					FindEdges(2.0*(recipphi^2)/ED)
				
					;Now truncate it
					
					r1#=1.0/(2.0+(Sin(108)/Sin(36)))
					r2#=1.0-r1
					
					Delete Each Point
					For l.LineType=Each LineType
						CreatePoint((l\a\w*r1)+(l\b\w*r2),(l\a\x*r1)+(l\b\x*r2),(l\a\y*r1)+(l\b\y*r2),(l\a\z*r1)+(l\b\z*r2))
						CreatePoint((l\a\w*r2)+(l\b\w*r1),(l\a\x*r2)+(l\b\x*r1),(l\a\y*r2)+(l\b\y*r1),(l\a\z*r2)+(l\b\z*r1))
					Next
					Delete Each LineType
					FindEdges((2.0*(recipphi^2)/ED)*(1-(2.0*r1)))
				
				Case 11 ; Tetracontaoctachoron
					
					r#=3.0-Sqr(8)
					
					CreatePermutationPoints(0.75-(0.25*r),0.25-(0.75*r),0.25+(0.25*r),0.25+(0.25*r))
					CreatePermutationPoints(0.75-(0.25*r),0.25-(0.75*r),0.25+(0.25*r),-0.25-(0.25*r))
					CreatePermutationPoints(0.75-(0.25*r),0.25-(0.75*r),-0.25-(0.25*r),-0.25-(0.25*r))
					CreatePermutationPoints(0.75-(0.25*r),-0.25+(0.75*r),0.25+(0.25*r),0.25+(0.25*r))
					CreatePermutationPoints(0.75-(0.25*r),-0.25+(0.75*r),0.25+(0.25*r),-0.25-(0.25*r))
					CreatePermutationPoints(0.75-(0.25*r),-0.25+(0.75*r),-0.25-(0.25*r),-0.25-(0.25*r))
					CreatePermutationPoints(-0.75+(0.25*r),0.25-(0.75*r),0.25+(0.25*r),0.25+(0.25*r))
					CreatePermutationPoints(-0.75+(0.25*r),0.25-(0.75*r),0.25+(0.25*r),-0.25-(0.25*r))
					CreatePermutationPoints(-0.75+(0.25*r),0.25-(0.75*r),-0.25-(0.25*r),-0.25-(0.25*r))
					CreatePermutationPoints(-0.75+(0.25*r),-0.25+(0.75*r),0.25+(0.25*r),0.25+(0.25*r))
					CreatePermutationPoints(-0.75+(0.25*r),-0.25+(0.75*r),0.25+(0.25*r),-0.25-(0.25*r))
					CreatePermutationPoints(-0.75+(0.25*r),-0.25+(0.75*r),-0.25-(0.25*r),-0.25-(0.25*r))
					
					CreatePermutationPoints(0.5+(0.5*r),0.5-(0.5*r),0.5-(0.5*r),0)
					CreatePermutationPoints(0.5+(0.5*r),0.5-(0.5*r),-0.5+(0.5*r),0)
					CreatePermutationPoints(0.5+(0.5*r),-0.5+(0.5*r),-0.5+(0.5*r),0)
					CreatePermutationPoints(-0.5-(0.5*r),0.5-(0.5*r),0.5-(0.5*r),0)
					CreatePermutationPoints(-0.5-(0.5*r),0.5-(0.5*r),-0.5+(0.5*r),0)
					CreatePermutationPoints(-0.5-(0.5*r),-0.5+(0.5*r),-0.5+(0.5*r),0)
					
					FindEdges(r*Sqr(2.0))
					
				Case 12 ; Grand Antiprism
				
					GrandAntiprismSafeguard=True
					
					
					
					;Follow procedure for creating 600-cell, while excluding certain vertices
										CreateEvenPermutationPoints(phi/2.0,0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,-0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(phi/2.0,-0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,0.5,-recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,-0.5,recipphi/2.0,0)
					CreateEvenPermutationPoints(-phi/2.0,-0.5,-recipphi/2.0,0)
					
					For w#=-0.5 To 0.5
						For x#=-0.5 To 0.5
							For y#=-0.5 To 0.5
								For z#=-0.5 To 0.5
									CreatePoint(w,x,y,z)
								Next
							Next
						Next
					Next
				
					CreatePoint(0,0,0,-1)
					CreatePoint(0,0,0,1)
					CreatePoint(0,0,-1,0)
					CreatePoint(0,0,1,0)
					CreatePoint(0,-1,0,0)
					CreatePoint(0,1,0,0)
					CreatePoint(-1,0,0,0)
					CreatePoint(1,0,0,0)
					
					FindEdges(recipphi);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					
					
					
					GrandAntiprismSafeguard=False
				
			End Select
		
		Case 7 ; Hyperspace - Complex Graphs
		
			Select Polytope
			
				Case 1 ; Graph (y+wi)=(x+zi)^2
					For x#=-1 To 1 Step 0.05
						For z#=-1 To 1 Step 0.05
							y#=(x*x)-(z*z)
							w#=2*x*z
							If Sqr((w*w)+(x*x)+(y*y)+(z*z))<=2
							;If y=>-1 And y<=1 Then
							;	If w=>-1 And w<=1 Then
									CreatePoint(w/2,x/2,y/2,z/2)
							;	End If
							End If
						Next
					Next
				
				Case 2 ; Graph (y+wi)=(x+zi)^3
					For x#=-1 To 1 Step 0.05
						For z#=-1 To 1 Step 0.05
							y#=(x*x*x)-(3*x*z*z)
							w#=(3*x*x*z)-(z*z*z)
							If Sqr((w*w)+(x*x)+(y*y)+(z*z))<=2
							;If y=>-1 And y<=1 Then
							;	If w=>-1 And w<=1 Then
									CreatePoint(w/2,x/2,y/2,z/2)
							;	End If
							End If
						Next
					Next
				
				Case 3 ; Graph (y+wi)=1/(x+zi)
					For x#=-1 To 1 Step 0.05
						For z#=-1 To 1 Step 0.05
							t#=Sqr((x*x)+(z*z))
							y#=x/t
							w#=-z/t
							If Sqr((w*w)+(x*x)+(y*y)+(z*z))<=2
							;If y=>-1 And y<=1 Then
							;	If w=>-1 And w<=1 Then
									CreatePoint(w/2,x/2,y/2,z/2)
							;	End If
							End If
						Next
					Next
				
				Case 4 ; Graph (y+wi)=sin(x+zi)
					For x#=-2 To 2 Step 0.1
						For z#=-2 To 2 Step 0.1
							y#=x
							w#=z
							ty#=(x*x*x)-(3*x*z*z)
							tw#=(3*x*x*z)-(z*z*z)
							y=y-(ty/6)
							w=w-(tw/6)
							ty#=(x*x*x*x*x)-(10*x*x*x*z*z)+(5*x*z*z*z*z)
							tw#=(5*x*x*x*x*z)-(10*x*x*z*z*z)+(z*z*z*z*z)
							y=y+(ty/120)
							w=w+(tw/120)
							ty#=(1*x*x*x*x*x*x*x)-(21*x*x*x*x*x*z*z)+(35*x*x*x*z*z*z*z)-(7*x*z*z*z*z*z*z)
							tw#=(7*x*x*x*x*x*x*z)-(35*x*x*x*x*z*z*z)+(21*x*x*z*z*z*z*z)-(1*z*z*z*z*z*z*z)
							y=y-(ty/5040)
							w=w-(tw/5040)
							
							If Sqr((w*w)+(x*x)+(y*y)+(z*z))<=4
							;If y=>-1 And y<=1 Then
							;	If w=>-1 And w<=1 Then
									CreatePoint(w/4,x/4,y/4,z/4)
							;	End If
							End If
						Next
					Next
				
				Case 5 ; Graph (y+wi)=cos(x+zi)
					For x#=-2 To 2 Step 0.1
						For z#=-2 To 2 Step 0.1
							y#=1
							w#=1
							ty#=(x*x)-(z*z)
							tw#=(2*x*z)
							y=y-(ty/2)
							w=w-(tw/2)
							ty#=(1*x*x*x*x)-(6*x*x*z*z)+(1*z*z*z*z)
							tw#=(4*x*x*x*z)-(4*x*z*z*z)
							y=y+(ty/24)
							w=w+(tw/24)
							ty#=(1*x*x*x*x*x*x)-(15*x*x*x*x*z*z)+(15*x*x*z*z*z*z)-(1*z*z*z*z*z*z)
							tw#=(6*x*x*x*x*x*z)-(20*x*x*x*z*z*z)+(6*x*z*z*z*z*z)
							y=y-(ty/720)
							w=w-(tw/720)
							
							If Sqr((w*w)+(x*x)+(y*y)+(z*z))<=4
							;If y=>-1 And y<=1 Then
							;	If w=>-1 And w<=1 Then
									CreatePoint(w/4,x/4,y/4,z/4)
							;	End If
							End If
						Next
					Next
				
				Case 6 ; Graph (y+wi)=e^(x+zi)
					For x#=-2 To 2 Step 0.1
						For z#=-2 To 2 Step 0.1
							y#=1+x
							w#=1+z
							ty#=(x*x)-(z*z)
							tw#=(2*x*z)
							y=y+(ty/2)
							w=w+(tw/2)
							ty#=(x*x*x)-(3*x*z*z)
							tw#=(3*x*x*z)-(z*z*z)
							y=y+(ty/6)
							w=w+(tw/6)
							ty#=(1*x*x*x*x)-(6*x*x*z*z)+(1*z*z*z*z)
							tw#=(4*x*x*x*z)-(4*x*z*z*z)
							y=y+(ty/24)
							w=w+(tw/24)
							ty#=(x*x*x*x*x)-(10*x*x*x*z*z)+(5*x*z*z*z*z)
							tw#=(5*x*x*x*x*z)-(10*x*x*z*z*z)+(z*z*z*z*z)
							y=y+(ty/120)
							w=w+(tw/120)
							ty#=(1*x*x*x*x*x*x)-(15*x*x*x*x*z*z)+(15*x*x*z*z*z*z)-(1*z*z*z*z*z*z)
							tw#=(6*x*x*x*x*x*z)-(20*x*x*x*z*z*z)+(6*x*z*z*z*z*z)
							y=y+(ty/720)
							w=w+(tw/720)
							ty#=(1*x*x*x*x*x*x*x)-(21*x*x*x*x*x*z*z)+(35*x*x*x*z*z*z*z)-(7*x*z*z*z*z*z*z)
							tw#=(7*x*x*x*x*x*x*z)-(35*x*x*x*x*z*z*z)+(21*x*x*z*z*z*z*z)-(1*z*z*z*z*z*z*z)
							y=y+(ty/5040)
							w=w+(tw/5040)
							ty#=(1*x*x*x*x*x*x*x*x)-(28*x*x*x*x*x*x*z*z)+(70*x*x*x*x*z*z*z*z)-(28*x*x*z*z*z*z*z*z)+(1*z*z*z*z*z*z*z*z)
							tw#=(8*x*x*x*x*x*x*x*z)-(56*x*x*x*x*x*z*z*z)+(56*x*x*x*z*z*z*z*z)-(8*x*z*z*z*z*z*z*z)
							y=y+(ty/40320)
							w=w+(tw/40320)
							ty#=(1*x*x*x*x*x*x*x*x*x)-(36*x*x*x*x*x*x*x*z*z)+(126*x*x*x*x*x*z*z*z*z)-(84*x*x*x*z*z*z*z*z*z*z)+(9*x*z*z*z*z*z*z*z*z)
							tw#=(9*x*x*x*x*x*x*x*x*z)-(84*x*x*x*x*x*x*z*z*z)+(126*x*x*x*x*z*z*z*z*z)-(36*x*x*z*z*z*z*z*z*z)+(1*z*z*z*z*z*z*z*z*z)
							y=y+(ty/362880)
							w=w+(tw/362880)
							
							If Sqr((w*w)+(x*x)+(y*y)+(z*z))<=4
							;If y=>-1 And y<=1 Then
							;	If w=>-1 And w<=1 Then
									CreatePoint(w/4,x/4,y/4,z/4)
							;	End If
							End If
						Next
					Next
				
			End Select
			
		Case 8 ; Hyperspace - semi-regular duals
		
			Select Polytope
			
				Case 2 ; Dual of Truncated Tesseract
				
					CreatePermutationPoints(dtruntess,0,0,0)
					CreatePermutationPoints(-dtruntess,0,0,0)
					CreatePoint(0.5,0.5,0.5,0.5)
					CreatePoint(-0.5,-0.5,-0.5,-0.5)
					CreatePermutationPoints(-0.5,0.5,0.5,0.5)
					CreatePermutationPoints(-0.5,-0.5,0.5,0.5)
					CreatePermutationPoints(-0.5,-0.5,-0.5,0.5)
					
					CreateLine(dtruntess,0,0,0,0,dtruntess,0,0,"","","")
					CreateLine(dtruntess,0,0,0,0,-dtruntess,0,0,"","","")
					CreateLine(-dtruntess,0,0,0,0,-dtruntess,0,0,"","","")
					CreateLine(-dtruntess,0,0,0,0,dtruntess,0,0,"","","")
					
					CreateLine(dtruntess,0,0,0,0,0,dtruntess,0,"","","")
					CreateLine(dtruntess,0,0,0,0,0,-dtruntess,0,"","","")
					CreateLine(-dtruntess,0,0,0,0,0,-dtruntess,0,"","","")
					CreateLine(-dtruntess,0,0,0,0,0,dtruntess,0,"","","")
					
					CreateLine(dtruntess,0,0,0,0,0,0,dtruntess,"","","")
					CreateLine(dtruntess,0,0,0,0,0,0,-dtruntess,"","","")
					CreateLine(-dtruntess,0,0,0,0,0,0,-dtruntess,"","","")
					CreateLine(-dtruntess,0,0,0,0,0,0,dtruntess,"","","")
					
					CreateLine(0,dtruntess,0,0,0,0,dtruntess,0,"","","")
					CreateLine(0,dtruntess,0,0,0,0,-dtruntess,0,"","","")
					CreateLine(0,-dtruntess,0,0,0,0,-dtruntess,0,"","","")
					CreateLine(0,-dtruntess,0,0,0,0,dtruntess,0,"","","")
					
					CreateLine(0,dtruntess,0,0,0,0,0,dtruntess,"","","")
					CreateLine(0,dtruntess,0,0,0,0,0,-dtruntess,"","","")
					CreateLine(0,-dtruntess,0,0,0,0,0,-dtruntess,"","","")
					CreateLine(0,-dtruntess,0,0,0,0,0,dtruntess,"","","")
					
					CreateLine(0,0,dtruntess,0,0,0,0,dtruntess,"","","")
					CreateLine(0,0,dtruntess,0,0,0,0,-dtruntess,"","","")
					CreateLine(0,0,-dtruntess,0,0,0,0,-dtruntess,"","","")
					CreateLine(0,0,-dtruntess,0,0,0,0,dtruntess,"","","")
					
					CreateLine(dtruntess,0,0,0,0.5,0.5,0.5,0.5,"","","")
					CreateLine(dtruntess,0,0,0,0.5,0.5,0.5,-0.5,"","","")
					CreateLine(dtruntess,0,0,0,0.5,0.5,-0.5,0.5,"","","")
					CreateLine(dtruntess,0,0,0,0.5,0.5,-0.5,-0.5,"","","")
					CreateLine(dtruntess,0,0,0,0.5,-0.5,0.5,0.5,"","","")
					CreateLine(dtruntess,0,0,0,0.5,-0.5,0.5,-0.5,"","","")
					CreateLine(dtruntess,0,0,0,0.5,-0.5,-0.5,0.5,"","","")
					CreateLine(dtruntess,0,0,0,0.5,-0.5,-0.5,-0.5,"","","")
					
					CreateLine(-dtruntess,0,0,0,-0.5,0.5,0.5,0.5,"","","")
					CreateLine(-dtruntess,0,0,0,-0.5,0.5,0.5,-0.5,"","","")
					CreateLine(-dtruntess,0,0,0,-0.5,0.5,-0.5,0.5,"","","")
					CreateLine(-dtruntess,0,0,0,-0.5,0.5,-0.5,-0.5,"","","")
					CreateLine(-dtruntess,0,0,0,-0.5,-0.5,0.5,0.5,"","","")
					CreateLine(-dtruntess,0,0,0,-0.5,-0.5,0.5,-0.5,"","","")
					CreateLine(-dtruntess,0,0,0,-0.5,-0.5,-0.5,0.5,"","","")
					CreateLine(-dtruntess,0,0,0,-0.5,-0.5,-0.5,-0.5,"","","")
					
					CreateLine(0,dtruntess,0,0,0.5,0.5,0.5,0.5,"","","")
					CreateLine(0,dtruntess,0,0,0.5,0.5,0.5,-0.5,"","","")
					CreateLine(0,dtruntess,0,0,0.5,0.5,-0.5,0.5,"","","")
					CreateLine(0,dtruntess,0,0,0.5,0.5,-0.5,-0.5,"","","")
					CreateLine(0,dtruntess,0,0,-0.5,0.5,0.5,0.5,"","","")
					CreateLine(0,dtruntess,0,0,-0.5,0.5,0.5,-0.5,"","","")
					CreateLine(0,dtruntess,0,0,-0.5,0.5,-0.5,0.5,"","","")
					CreateLine(0,dtruntess,0,0,-0.5,0.5,-0.5,-0.5,"","","")
					
					CreateLine(0,-dtruntess,0,0,0.5,-0.5,0.5,0.5,"","","")
					CreateLine(0,-dtruntess,0,0,0.5,-0.5,0.5,-0.5,"","","")
					CreateLine(0,-dtruntess,0,0,0.5,-0.5,-0.5,0.5,"","","")
					CreateLine(0,-dtruntess,0,0,0.5,-0.5,-0.5,-0.5,"","","")
					CreateLine(0,-dtruntess,0,0,-0.5,-0.5,0.5,0.5,"","","")
					CreateLine(0,-dtruntess,0,0,-0.5,-0.5,0.5,-0.5,"","","")
					CreateLine(0,-dtruntess,0,0,-0.5,-0.5,-0.5,0.5,"","","")
					CreateLine(0,-dtruntess,0,0,-0.5,-0.5,-0.5,-0.5,"","","")
					
					CreateLine(0,0,dtruntess,0,0.5,0.5,0.5,0.5,"","","")
					CreateLine(0,0,dtruntess,0,0.5,0.5,0.5,-0.5,"","","")
					CreateLine(0,0,dtruntess,0,0.5,-0.5,0.5,0.5,"","","")
					CreateLine(0,0,dtruntess,0,0.5,-0.5,0.5,-0.5,"","","")
					CreateLine(0,0,dtruntess,0,-0.5,0.5,0.5,0.5,"","","")
					CreateLine(0,0,dtruntess,0,-0.5,0.5,0.5,-0.5,"","","")
					CreateLine(0,0,dtruntess,0,-0.5,-0.5,0.5,0.5,"","","")
					CreateLine(0,0,dtruntess,0,-0.5,-0.5,0.5,-0.5,"","","")
					
					CreateLine(0,0,-dtruntess,0,0.5,0.5,-0.5,0.5,"","","")
					CreateLine(0,0,-dtruntess,0,0.5,0.5,-0.5,-0.5,"","","")
					CreateLine(0,0,-dtruntess,0,0.5,-0.5,-0.5,0.5,"","","")
					CreateLine(0,0,-dtruntess,0,0.5,-0.5,-0.5,-0.5,"","","")
					CreateLine(0,0,-dtruntess,0,-0.5,0.5,-0.5,0.5,"","","")
					CreateLine(0,0,-dtruntess,0,-0.5,0.5,-0.5,-0.5,"","","")
					CreateLine(0,0,-dtruntess,0,-0.5,-0.5,-0.5,0.5,"","","")
					CreateLine(0,0,-dtruntess,0,-0.5,-0.5,-0.5,-0.5,"","","")
					
					CreateLine(0,0,0,dtruntess,0.5,0.5,0.5,0.5,"","","")
					CreateLine(0,0,0,dtruntess,0.5,0.5,-0.5,0.5,"","","")
					CreateLine(0,0,0,dtruntess,0.5,-0.5,0.5,0.5,"","","")
					CreateLine(0,0,0,dtruntess,0.5,-0.5,-0.5,0.5,"","","")
					CreateLine(0,0,0,dtruntess,-0.5,0.5,0.5,0.5,"","","")
					CreateLine(0,0,0,dtruntess,-0.5,0.5,-0.5,0.5,"","","")
					CreateLine(0,0,0,dtruntess,-0.5,-0.5,0.5,0.5,"","","")
					CreateLine(0,0,0,dtruntess,-0.5,-0.5,-0.5,0.5,"","","")
					
					CreateLine(0,0,0,-dtruntess,0.5,0.5,0.5,-0.5,"","","")
					CreateLine(0,0,0,-dtruntess,0.5,0.5,-0.5,-0.5,"","","")
					CreateLine(0,0,0,-dtruntess,0.5,-0.5,0.5,-0.5,"","","")
					CreateLine(0,0,0,-dtruntess,0.5,-0.5,-0.5,-0.5,"","","")
					CreateLine(0,0,0,-dtruntess,-0.5,0.5,0.5,-0.5,"","","")
					CreateLine(0,0,0,-dtruntess,-0.5,0.5,-0.5,-0.5,"","","")
					CreateLine(0,0,0,-dtruntess,-0.5,-0.5,0.5,-0.5,"","","")
					CreateLine(0,0,0,-dtruntess,-0.5,-0.5,-0.5,-0.5,"","","")
					
			End Select
		
	End Select
End Function