;Stereoscope 4D
;By David Hudson

Type Vector
	Field w#,x#,y#,z#
End Type

Type Point
	Field P.Vector
End Type

Type LeftViewPoint
	Field P.Vector
End Type

Type RightViewPoint
	Field P.Vector
End Type

Type LineType
	Field a.Vector
	Field b.Vector
	Field Cell1$
	Field Cell2$
	Field Cell3$
	Field Highlighted
End Type

Type LeftViewLine
	Field a.Vector
	Field b.Vector
	Field Highlighted
End Type

Type RightViewLine
	Field a.Vector
	Field b.Vector
	Field Highlighted
End Type

Type Matrix
	Field a1#,a2#,a3#,a4#
	Field b1#,b2#,b3#,b4#
	Field c1#,c2#,c3#,c4#
	Field d1#,d2#,d3#,d4#
End Type

Include "polytopes.bb"

Dim TempArrayMatrix#(4,4)

Global CellTypeHighlighted
Global CellNumberHighlighted
Global Cells1
Global Cells2
Global Cells3

Global GrandAntiprismSafeGuard=False

Global VOrigin.Vector
Global MIdentity.Matrix
Global ZPerspective#=0.1
Global WPerspective#=0.1
Global CrossViewingAngle#=2 ;In zx plane
Global MLeftRotation.Matrix
Global MRightRotation.Matrix
MIdentity=New Matrix
SetMatrix(MIdentity,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)
MLeftRotation.Matrix=New Matrix
SetRotationMatrix(MLeftRotation,4,2,0-CrossViewingAngle)
MRightRotation.Matrix=New Matrix
SetRotationMatrix(MRightRotation,4,2,CrossViewingAngle)

Global RotationAmount#=0.5
Global RotationQ.Matrix=New Matrix
Global RotationA.Matrix=New Matrix
Global RotationW.Matrix=New Matrix
Global RotationS.Matrix=New Matrix
Global RotationE.Matrix=New Matrix
Global RotationD.Matrix=New Matrix
Global RotationR.Matrix=New Matrix
Global RotationF.Matrix=New Matrix
Global RotationT.Matrix=New Matrix
Global RotationG.Matrix=New Matrix
Global RotationY.Matrix=New Matrix
Global RotationH.Matrix=New Matrix
SetRotationMatrices

SeedRnd MilliSecs()
Global XRes,YRes

XRes=640
YRes=480

Global ColourRes
Global HighlightColour
ColourRes=256
Dim ColourGrid(ColourRes)
Dim CGR(ColourRes)
Dim CGG(ColourRes)
Dim CGB(ColourRes)

Global HyperspaceSize#,TotalPoints

Graphics XRes,YRes,32,2
;1=Full Screen, 2=Window

Global ThickPoints=0
Global ThickLines=0
Global ViewSize=200
Global Separation=10
Global LeftViewImage
Global RightViewImage
Global BackgroundImage
LeftViewImage=CreateImage(ViewSize,ViewSize)
RightViewImage=CreateImage(ViewSize,ViewSize)
BackgroundImage=CreateImage(XRes,YRes)
Dim ZViewLeft#(1,1)
Dim ZViewRight#(1,1)

Global RenderingDimension=4
Global SelectedColourScheme=1

InitialiseRenderingColours(SelectedColourScheme)

HyperspaceSize=1.1

RenderPolytope(4,2)

Repeat
	DrawScreen
	Respond
Until KeyDown(1)





Function DrawScreen()
	CreateStereoscope
	ClearViews
	DrawPoints
	DrawLines
	ShowStereoscope
End Function

Function InitialiseRenderingColours(ColourScheme)
	RCI=CreateImage(ColourRes,1)
	SetBuffer ImageBuffer(RCI)
	If ColourScheme=1 Then ;Red-Blue
		For l=1 To ColourRes
			If l<ColourRes/2 Then
				R=255
				B=255*l/(ColourRes/2)
			Else
				B=255
				R=255*(ColourRes-l)/(ColourRes/2)
			End If
			If R<0 Then R=0
			If B<0 Then B=0
			If R>255 Then R=255
			If B>255 Then B=255
			Color R,0,B
			Plot l-1,0
			CGR(l-1)=R
			CGG(l-1)=0
			CGB(l-1)=B
		Next
	End If
	If ColourScheme=2 Then ;Rainbow
		SetBuffer ImageBuffer(RCI)
		For x=1 To ColourRes
			If x<(ColourRes/5) Then
				CR=255
				CG=Int(255.0*(Float(x)/(Float(ColourRes)/5.0)))
				CB=0
			Else If x<((ColourRes*2)/5) Then
				CR=Int(255-(255.0*(Float(x-(Float(ColourRes)*0.2))/(Float(ColourRes)/5.0))))
				CG=255
				CB=0
			Else If x<((ColourRes*3)/5) Then
				CR=0
				CG=255
				CB=Int(255.0*(Float(x-(Float(ColourRes)*0.4))/(Float(ColourRes)/5.0)))
			Else If x<((ColourRes*4)/5) Then
				CR=0
				CG=Int(255-(255.0*(Float(x-(Float(ColourRes)*0.6))/(Float(ColourRes)/5.0))))
				CB=255
			Else
				CR=Int(255.0*(Float(x-(Float(ColourRes)*0.8))/(Float(ColourRes)/5.0)))
				CG=0
				CB=255
			End If
			If CR<0 Then CR=0
			If CG<0 Then CG=0
			If CB<0 Then CB=0
			If CR>255 Then CR=255
			If CG>255 Then CG=255
			If CB>255 Then CB=255
			Color CR,CG,CB
			Plot x-1,0
			CGR(x-1)=CR
			CGG(x-1)=CG
			CGB(x-1)=CB
		Next
	End If
	LockBuffer ImageBuffer(RCI)
	For l=1 To ColourRes
		ColourGrid(l-1)=ReadPixelFast(l-1,0)
	Next
	UnlockBuffer ImageBuffer(RCI)
	SaveImage(RCI,"ColourScheme"+Str(ColourScheme)+".bmp");;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	Color 255,255,255
	Plot 0,0
	LockBuffer ImageBuffer(RCI)
	HighlightColour=ReadPixelFast(0,0)
	UnlockBuffer ImageBuffer(RCI)
	
	FreeImage RCI
	
	DrawBackground(ColourScheme)
End Function

Function CreateStereoscope()
	For L.LeftViewPoint=Each LeftViewPoint
		Delete L\P
	Next
	For R.RightViewPoint=Each RightViewPoint
		Delete R\P
	Next
	For LL.LeftViewLine=Each LeftViewLine
		Delete LL\a
		Delete LL\b
	Next
	For RL.RightViewLine=Each RightViewLine
		Delete RL\a
		Delete RL\b
	Next
	Delete Each LeftViewPoint
	Delete Each RightViewPoint
	Delete Each LeftViewLine
	Delete Each RightViewLine
	For P.Point=Each Point
		L.LeftViewPoint=New LeftViewPoint
		L\P=New Vector
		SetVector(L\P,P\P\w,P\P\x,P\P\y,P\P\z)
		R.RightViewPoint=New RightViewPoint
		R\P=New Vector
		SetVector(R\P,P\P\w,P\P\x,P\P\y,P\P\z)
		MultiplyVector(R\P,MRightRotation,R\P)
		MultiplyVector(L\P,MLeftRotation,L\P)
	Next
	For LT.LineType=Each LineType
		LL.LeftViewLine=New LeftViewLine
		LL\a=New Vector
		LL\b=New Vector
		SetVector(LL\a,LT\a\w,LT\a\x,LT\a\y,LT\a\z)
		SetVector(LL\b,LT\b\w,LT\b\x,LT\b\y,LT\b\z)
		LL\Highlighted=LT\Highlighted
		RL.RightViewLine=New RightViewLine
		RL\a=New Vector
		RL\b=New Vector
		SetVector(RL\a,LT\a\w,LT\a\x,LT\a\y,LT\a\z)
		SetVector(RL\b,LT\b\w,LT\b\x,LT\b\y,LT\b\z)
		RL\Highlighted=LT\Highlighted
		MultiplyVector(RL\a,MRightRotation,RL\a)
		MultiplyVector(LL\a,MLeftRotation,LL\a)
		MultiplyVector(RL\b,MRightRotation,RL\b)
		MultiplyVector(LL\b,MLeftRotation,LL\b)
	Next
	For L=Each LeftViewPoint
		L\P\x=L\P\x+(L\P\x*L\P\z*zPerspective)
		L\P\x=L\P\x+(L\P\x*L\P\w*wPerspective)
		L\P\y=L\P\y+(L\P\y*L\P\z*zPerspective)
		L\P\y=L\P\y+(L\P\y*L\P\w*wPerspective)
	Next
	For R=Each RightViewPoint
		R\P\x=R\P\x+(R\P\x*R\P\z*zPerspective)
		R\P\x=R\P\x+(R\P\x*R\P\w*wPerspective)
		R\P\y=R\P\y+(R\P\y*R\P\z*zPerspective)
		R\P\y=R\P\y+(R\P\y*R\P\w*wPerspective)
	Next
	For LL=Each LeftViewLine
		LL\a\x=LL\a\x+(LL\a\x*LL\a\z*zPerspective)
		LL\a\x=LL\a\x+(LL\a\x*LL\a\w*wPerspective)
		LL\a\y=LL\a\y+(LL\a\y*LL\a\z*zPerspective)
		LL\a\y=LL\a\y+(LL\a\y*LL\a\w*wPerspective)
		LL\b\x=LL\b\x+(LL\b\x*LL\b\z*zPerspective)
		LL\b\x=LL\b\x+(LL\b\x*LL\b\w*wPerspective)
		LL\b\y=LL\b\y+(LL\b\y*LL\b\z*zPerspective)
		LL\b\y=LL\b\y+(LL\b\y*LL\b\w*wPerspective)
	Next
	For RL=Each RightViewLine
		RL\a\x=RL\a\x+(RL\a\x*RL\a\z*zPerspective)
		RL\a\x=RL\a\x+(RL\a\x*RL\a\w*wPerspective)
		RL\a\y=RL\a\y+(RL\a\y*RL\a\z*zPerspective)
		RL\a\y=RL\a\y+(RL\a\y*RL\a\w*wPerspective)
		RL\b\x=RL\b\x+(RL\b\x*RL\b\z*zPerspective)
		RL\b\x=RL\b\x+(RL\b\x*RL\b\w*wPerspective)
		RL\b\y=RL\b\y+(RL\b\y*RL\b\z*zPerspective)
		RL\b\y=RL\b\y+(RL\b\y*RL\b\w*wPerspective)
	Next
End Function

Function ClearViews()
	ClsColor 0,0,0
	SetBuffer ImageBuffer(LeftViewImage)
	Cls
	SetBuffer ImageBuffer(RightViewImage)
	Cls
	Dim ZViewLeft#(ViewSize,ViewSize)
	Dim ZViewRight#(ViewSize,ViewSize)
End Function

Function DrawPoints()
	SetBuffer ImageBuffer(LeftViewImage)
	LockBuffer ImageBuffer(LeftViewImage)
	For L.LeftViewPoint=Each LeftViewPoint
		x=((((L\P\x/HyperspaceSize)+1.0)/2.0)*ViewSize)
		y=(((1.0-(L\P\y/HyperspaceSize))/2.0)*ViewSize)
		If x>-1 And x<ViewSize Then
			If y>-1 And y<ViewSize Then
				h=((((L\P\w/HyperspaceSize)+1.0)/2.0)*ColourRes)
				If h>-1 And h<ColourRes Then
					If L\P\z+HyperspaceSize>ZViewLeft(x,y) Then
						WritePixelFast x,y,ColourGrid(h)
						ZViewLeft(x,y)=L\P\z+HyperspaceSize
					End If
					If ThickPoints>0 Then
						If L\P\z+HyperspaceSize>ZViewLeft(x-1,y) Then
							WritePixelFast x-1,y,ColourGrid(h)
							ZViewLeft(x-1,y)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x+1,y) Then
							WritePixelFast x+1,y,ColourGrid(h)
							ZViewLeft(x+1,y)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x,y-1) Then
							WritePixelFast x,y-1,ColourGrid(h)
							ZViewLeft(x,y-1)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x,y+1) Then
							WritePixelFast x,y+1,ColourGrid(h)
							ZViewLeft(x,y+1)=L\P\z+HyperspaceSize
						End If
					End If
					If ThickPoints>1 Then
						If L\P\z+HyperspaceSize>ZViewLeft(x-1,y-1) Then
							WritePixelFast x-1,y-1,ColourGrid(h)
							ZViewLeft(x-1,y-1)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x+1,y-1) Then
							WritePixelFast x+1,y-1,ColourGrid(h)
							ZViewLeft(x+1,y-1)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x-1,y+1) Then
							WritePixelFast x-1,y+1,ColourGrid(h)
							ZViewLeft(x-1,y+1)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x+1,y+1) Then
							WritePixelFast x+1,y+1,ColourGrid(h)
							ZViewLeft(x+1,y+1)=L\P\z+HyperspaceSize
						End If
					End If
					If ThickPoints>2 Then
						If L\P\z+HyperspaceSize>ZViewLeft(x-2,y) Then
							WritePixelFast x-2,y,ColourGrid(h)
							ZViewLeft(x-2,y)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x+2,y) Then
							WritePixelFast x+2,y,ColourGrid(h)
							ZViewLeft(x+2,y)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x,y-2) Then
							WritePixelFast x,y-2,ColourGrid(h)
							ZViewLeft(x,y-2)=L\P\z+HyperspaceSize
						End If
						If L\P\z+HyperspaceSize>ZViewLeft(x,y+2) Then
							WritePixelFast x,y+2,ColourGrid(h)
							ZViewLeft(x,y+2)=L\P\z+HyperspaceSize
						End If
					End If
				End If
			End If
		End If
	Next
	UnlockBuffer ImageBuffer(LeftViewImage)

	SetBuffer ImageBuffer(RightViewImage)
	LockBuffer ImageBuffer(RightViewImage)
	For R.RightViewPoint=Each RightViewPoint
		x=((((R\P\x/HyperspaceSize)+1.0)/2.0)*ViewSize)
		y=(((1.0-(R\P\y/HyperspaceSize))/2.0)*ViewSize)
		If x>-1 And x<ViewSize Then
			If y>-1 And y<ViewSize Then
				h=((((R\P\w/HyperspaceSize)+1.0)/2.0)*ColourRes)
				If h>-1 And h<ColourRes Then
					If R\P\z+HyperspaceSize>ZViewRight(x,y) Then
						WritePixelFast x,y,ColourGrid(h)
						ZViewRight(x,y)=R\P\z+HyperspaceSize
					End If
					If ThickPoints>0 Then
						If R\P\z+HyperspaceSize>ZViewRight(x-1,y) Then
							WritePixelFast x-1,y,ColourGrid(h)
							ZViewRight(x-1,y)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x+1,y) Then
							WritePixelFast x+1,y,ColourGrid(h)
							ZViewRight(x+1,y)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x,y-1) Then
							WritePixelFast x,y-1,ColourGrid(h)
							ZViewRight(x,y-1)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x,y+1) Then
							WritePixelFast x,y+1,ColourGrid(h)
							ZViewRight(x,y+1)=R\P\z+HyperspaceSize
						End If
					End If
					If ThickPoints>1 Then
						If R\P\z+HyperspaceSize>ZViewRight(x-1,y-1) Then
							WritePixelFast x-1,y-1,ColourGrid(h)
							ZViewRight(x-1,y-1)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x+1,y-1) Then
							WritePixelFast x+1,y-1,ColourGrid(h)
							ZViewRight(x+1,y-1)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x-1,y+1) Then
							WritePixelFast x-1,y+1,ColourGrid(h)
							ZViewRight(x-1,y+1)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x+1,y+1) Then
							WritePixelFast x+1,y+1,ColourGrid(h)
							ZViewRight(x+1,y+1)=R\P\z+HyperspaceSize
						End If
					End If
					If ThickPoints>2 Then
						If R\P\z+HyperspaceSize>ZViewRight(x-2,y) Then
							WritePixelFast x-2,y,ColourGrid(h)
							ZViewRight(x-2,y)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x+2,y) Then
							WritePixelFast x+2,y,ColourGrid(h)
							ZViewRight(x+2,y)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x,y-2) Then
							WritePixelFast x,y-2,ColourGrid(h)
							ZViewRight(x,y-2)=R\P\z+HyperspaceSize
						End If
						If R\P\z+HyperspaceSize>ZViewRight(x,y+2) Then
							WritePixelFast x,y+2,ColourGrid(h)
							ZViewRight(x,y+2)=R\P\z+HyperspaceSize
						End If
					End If
				End If
			End If
		End If
	Next
	UnlockBuffer ImageBuffer(RightViewImage)
End Function

Function DrawLines()
	If ThickLines=-1 Then Return
	SetBuffer ImageBuffer(LeftViewImage)
	LockBuffer ImageBuffer(LeftViewImage)
	For L.LeftViewLine=Each LeftViewLine
		x1=((((L\a\x/HyperspaceSize)+1.0)/2.0)*ViewSize)
		y1=(((1.0-(L\a\y/HyperspaceSize))/2.0)*ViewSize)
		z1#=L\a\z+HyperspaceSize
		h1=((((L\a\w/HyperspaceSize)+1.0)/2.0)*ColourRes)
		x2=((((L\b\x/HyperspaceSize)+1.0)/2.0)*ViewSize)
		y2=(((1.0-(L\b\y/HyperspaceSize))/2.0)*ViewSize)
		z2#=L\b\z+HyperspaceSize
		h2=((((L\b\w/HyperspaceSize)+1.0)/2.0)*ColourRes)
		xd=x2-x1
		yd=y2-y1
		zd#=z2-z1
		hd=h2-h1
		lh=L\Highlighted
		If Abs(xd)>Abs(yd) Then
			If x2>x1 Then inc=1 Else inc=-1
			x=x1
			Repeat
				y=y1+(((x-x1)*yd)/xd)
				z#=z1+((Float(x-x1)*zd)/Float(xd))
				h=h1+(((x-x1)*hd)/xd)
				If x>-1 And x<ViewSize Then
					If y>-1 And y<ViewSize Then
						If h>-1 And h<ColourRes Then
							If z>ZViewLeft(x,y) Then
								WritePixelFast x,y,ColourGrid(h)
								If lh Then
									If ThickLines>1 Then
										WritePixelFast x,y,HighlightColour
									Else
										If Rnd(0,1)<0.5 Then WritePixelFast x,y,HighlightColour
									End If
								End If
								ZViewLeft(x,y)=z
							End If
							If ThickLines>0 Then
								If z>ZViewLeft(x,y-1) Then
									WritePixelFast x,y-1,ColourGrid(h)
									ZViewLeft(x,y-1)=z
								End If
							End If
							If ThickLines>1 Then
								If z>ZViewLeft(x,y+1) Then
									WritePixelFast x,y+1,ColourGrid(h)
									ZViewLeft(x,y+1)=z
								End If
							End If
							If ThickLines>2 Then
								If z>ZViewLeft(x,y-2) Then
									WritePixelFast x,y-2,ColourGrid(h)
									ZViewLeft(x,y-2)=z
								End If
								If z>ZViewLeft(x,y+2) Then
									WritePixelFast x,y+2,ColourGrid(h)
									ZViewLeft(x,y+2)=z
								End If
							End If
						End If
					End If
				End If
				x=x+inc
			Until x*inc>x2*inc
		Else
			If yd=0 Then
				If x1>-1 And x1<ViewSize Then
					If y1>-1 And y1<ViewSize Then
						If h1>-1 And h1<ColourRes Then
							If z1>ZViewLeft(x,y) Then
								WritePixelFast x1,y1,ColourGrid(h1)
								If lh Then
									If ThickLines>1 Then
										WritePixelFast x1,y1,HighlightColour
									Else
										If Rnd(0,1)<0.5 Then WritePixelFast x1,y1,HighlightColour
									End If
								End If
								ZViewLeft(x,y)=z1
							End If
							If ThickLines>0 Then
								If z>ZViewLeft(x-1,y) Then
									WritePixelFast x-1,y,ColourGrid(h)
									ZViewLeft(x-1,y)=z
								End If
								If z>ZViewLeft(x,y-1) Then
									WritePixelFast x,y-1,ColourGrid(h)
									ZViewLeft(x,y-1)=z
								End If
							End If
							If ThickLines>1 Then
								If z>ZViewLeft(x+1,y) Then
									WritePixelFast x+1,y,ColourGrid(h)
									ZViewLeft(x+1,y)=z
								End If
								If z>ZViewLeft(x,y+1) Then
									WritePixelFast x,y+1,ColourGrid(h)
									ZViewLeft(x,y+1)=z
								End If
							End If
							If ThickLines>2 Then
								If z>ZViewLeft(x-2,y) Then
									WritePixelFast x-2,y,ColourGrid(h)
									ZViewLeft(x-2,y)=z
								End If
								If z>ZViewLeft(x,y-2) Then
									WritePixelFast x,y-2,ColourGrid(h)
									ZViewLeft(x,y-2)=z
								End If
								If z>ZViewLeft(x+2,y) Then
									WritePixelFast x+2,y,ColourGrid(h)
									ZViewLeft(x+2,y)=z
								End If
								If z>ZViewLeft(x,y+2) Then
									WritePixelFast x,y+2,ColourGrid(h)
									ZViewLeft(x,y+2)=z
								End If
							End If
						End If
					End If
				End If
			Else
				If y2>y1 Then inc=1 Else inc=-1
				y=y1
				Repeat
					x=x1+(((y-y1)*xd)/yd)
					z#=z1+((Float(y-y1)*zd)/Float(yd))
					h=h1+(((y-y1)*hd)/yd)
					If x>-1 And x<ViewSize Then
						If y>-1 And y<ViewSize Then
							If h>-1 And h<ColourRes Then
								If z>ZViewLeft(x,y) Then
									WritePixelFast x,y,ColourGrid(h)
									If lh Then
										If ThickLines>1 Then
											WritePixelFast x,y,HighlightColour
										Else
											If Rnd(0,1)<0.5 Then WritePixelFast x,y,HighlightColour
										End If
									End If
									ZViewLeft(x,y)=z
								End If
								If ThickLines>0 Then
									If z>ZViewLeft(x-1,y) Then
										WritePixelFast x-1,y,ColourGrid(h)
										ZViewLeft(x-1,y)=z
									End If
								End If
								If ThickLines>1 Then
									If z>ZViewLeft(x+1,y) Then
										WritePixelFast x+1,y,ColourGrid(h)
										ZViewLeft(x+1,y)=z
									End If
								End If
								If ThickLines>2 Then
									If z>ZViewLeft(x-2,y) Then
										WritePixelFast x-2,y,ColourGrid(h)
										ZViewLeft(x-2,y)=z
									End If
									If z>ZViewLeft(x+2,y) Then
										WritePixelFast x+2,y,ColourGrid(h)
										ZViewLeft(x+2,y)=z
									End If
								End If
							End If
						End If
					End If
					y=y+inc
				Until y*inc>y2*inc ;So it works regardless of the sign of inc
			End If
		End If
	Next
	UnlockBuffer ImageBuffer(LeftViewImage)

	SetBuffer ImageBuffer(RightViewImage)
	LockBuffer ImageBuffer(RightViewImage)
	For R.RightViewLine=Each RightViewLine
		x1=((((R\a\x/HyperspaceSize)+1.0)/2.0)*ViewSize)
		y1=(((1.0-(R\a\y/HyperspaceSize))/2.0)*ViewSize)
		z1=R\a\z+HyperspaceSize
		h1=((((R\a\w/HyperspaceSize)+1.0)/2.0)*ColourRes)
		x2=((((R\b\x/HyperspaceSize)+1.0)/2.0)*ViewSize)
		y2=(((1.0-(R\b\y/HyperspaceSize))/2.0)*ViewSize)
		z2=R\a\z+HyperspaceSize
		h2=((((R\b\w/HyperspaceSize)+1.0)/2.0)*ColourRes)
		xd=x2-x1
		yd=y2-y1
		zd=z2-z1
		hd=h2-h1
		lh=R\Highlighted
		If Abs(xd)>Abs(yd) Then
			If x2>x1 Then inc=1 Else inc=-1
			x=x1
			Repeat
				y=y1+(((x-x1)*yd)/xd)
				z#=z1+((Float(x-x1)*zd)/Float(xd))
				h=h1+(((x-x1)*hd)/xd)
				If x>-1 And x<ViewSize Then
					If y>-1 And y<ViewSize Then
						If h>-1 And h<ColourRes Then
							If z>ZViewRight(x,y) Then
								WritePixelFast x,y,ColourGrid(h)
								If lh Then
									If ThickLines>1 Then
										WritePixelFast x,y,HighlightColour
									Else
										If Rnd(0,1)<0.5 Then WritePixelFast x,y,HighlightColour
									End If
								End If
								ZViewRight(x,y)=z
							End If
							If ThickLines>0 Then
								If z>ZViewRight(x,y-1) Then
									WritePixelFast x,y-1,ColourGrid(h)
									ZViewRight(x,y-1)=z
								End If
							End If
							If ThickLines>1 Then
								If z>ZViewRight(x,y+1) Then
									WritePixelFast x,y+1,ColourGrid(h)
									ZViewRight(x,y+1)=z
								End If
							End If
							If ThickLines>2 Then
								If z>ZViewRight(x,y-2) Then
									WritePixelFast x,y-2,ColourGrid(h)
									ZViewRight(x,y-2)=z
								End If
								If z>ZViewRight(x,y+2) Then
									WritePixelFast x,y+2,ColourGrid(h)
									ZViewRight(x,y+2)=z
								End If
							End If
						End If
					End If
				End If
				x=x+inc
			Until x*inc>x2*inc
		Else
			If yd=0 Then
				If x1>-1 And x1<ViewSize Then
					If y1>-1 And y1<ViewSize Then
						If h1>-1 And h1<ColourRes Then
							If z1>ZViewRight(x,y) Then
								WritePixelFast x1,y1,ColourGrid(h1)
								If lh Then
									If ThickLines>1 Then
										WritePixelFast x1,y1,HighlightColour
									Else
										If Rnd(0,1)<0.5 Then WritePixelFast x1,y1,HighlightColour
									End If
								End If
								ZViewRight(x,y)=z1
							End If
							If ThickLines>0 Then
								If z>ZViewRight(x-1,y) Then
									WritePixelFast x-1,y,ColourGrid(h)
									ZViewRight(x-1,y)=z
								End If
								If z>ZViewRight(x,y-1) Then
									WritePixelFast x,y-1,ColourGrid(h)
									ZViewRight(x,y-1)=z
								End If
							End If
							If ThickLines>1 Then
								If z>ZViewRight(x+1,y) Then
									WritePixelFast x+1,y,ColourGrid(h)
									ZViewRight(x+1,y)=z
								End If
								If z>ZViewRight(x,y+1) Then
									WritePixelFast x,y+1,ColourGrid(h)
									ZViewRight(x,y+1)=z
								End If
							End If
							If ThickLines>2 Then
								If z>ZViewRight(x-2,y) Then
									WritePixelFast x-2,y,ColourGrid(h)
									ZViewRight(x-2,y)=z
								End If
								If z>ZViewRight(x,y-2) Then
									WritePixelFast x,y-2,ColourGrid(h)
									ZViewRight(x,y-2)=z
								End If
								If z>ZViewRight(x+2,y) Then
									WritePixelFast x+2,y,ColourGrid(h)
									ZViewRight(x+2,y)=z
								End If
								If z>ZViewRight(x,y+2) Then
									WritePixelFast x,y+2,ColourGrid(h)
									ZViewRight(x,y+2)=z
								End If
							End If
						End If
					End If
				End If
			Else
				If y2>y1 Then inc=1 Else inc=-1
				y=y1
				Repeat
					x=x1+(((y-y1)*xd)/yd)
					z#=z1+((Float(y-y1)*zd)/Float(yd))
					h=h1+(((y-y1)*hd)/yd)
					If x>-1 And x<ViewSize Then
						If y>-1 And y<ViewSize Then
							If h>-1 And h<ColourRes Then
								If z>ZViewRight(x,y) Then
									WritePixelFast x,y,ColourGrid(h)
									If lh Then
										If ThickLines>1 Then
											WritePixelFast x,y,HighlightColour
										Else
											If Rnd(0,1)<0.5 Then WritePixelFast x,y,HighlightColour
										End If
									End If
									ZViewRight(x,y)=z
								End If
								If ThickLines>0 Then
									If z>ZViewRight(x-1,y) Then
										WritePixelFast x-1,y,ColourGrid(h)
										ZViewRight(x-1,y)=z
									End If
								End If
								If ThickLines>1 Then
									If z>ZViewRight(x+1,y) Then
										WritePixelFast x+1,y,ColourGrid(h)
										ZViewRight(x+1,y)=z
									End If
								End If
								If ThickLines>2 Then
									If z>ZViewRight(x-2,y) Then
										WritePixelFast x-2,y,ColourGrid(h)
										ZViewRight(x-2,y)=z
									End If
									If z>ZViewRight(x+2,y) Then
										WritePixelFast x+2,y,ColourGrid(h)
										ZViewRight(x+2,y)=z
									End If
								End If
							End If
						End If
					End If
					y=y+inc
				Until y*inc>y2*inc ;So it works regardless of the sign of inc
			End If
		End If
	Next
	UnlockBuffer ImageBuffer(RightViewImage)
End Function

Function ShowStereoscope()
	SetBuffer BackBuffer()
	DrawBlock BackGroundImage,0,0
	DrawBlock LeftViewImage,((XRes-Separation)/2)-ViewSize,(YRes-ViewSize)/2
	DrawBlock RightViewImage,((XRes+Separation)/2),(YRes-ViewSize)/2
;	Color 0,0,0
;	Oval ((XRes-Separation)/2)-ViewSize,(YRes-ViewSize)/2,ViewSize,ViewSize,True
;	Oval ((XRes+Separation)/2),(YRes-ViewSize)/2,ViewSize,ViewSize,True
;	DrawImage LeftViewImage,((XRes-Separation)/2)-ViewSize,(YRes-ViewSize)/2
;	DrawImage RightViewImage,((XRes+Separation)/2),(YRes-ViewSize)/2
	Flip
;	DebugLog MilliSecs()
End Function	

Function DrawBackground(ColorScheme)
	SetBuffer ImageBuffer(BackgroundImage)
	ClsColor 50,50,50
	Cls
	Color 255,255,255
	Text 320,10,"Hyperspace",True,True
	Text 320,25,"By David Hudson",True,True
	Text 80,50,"Keys",True,True
	Text 80,65,"Rotate in Plane",True,True
	Text 80,80,"Rotate around Plane",True,True
	Text 180,50,"Q/A",True,True
	Text 180,65,"xy",True,True
	Text 180,80,"wz",True,True
	Text 210,50,"W/S",True,True
	Text 210,65,"yz",True,True
	Text 210,80,"wx",True,True
	Text 240,50,"E/D",True,True
	Text 240,65,"zx",True,True
	Text 240,80,"wy",True,True
	Text 270,50,"R/F",True,True
	Text 270,65,"wx",True,True
	Text 270,80,"yz",True,True
	Text 300,50,"T/G",True,True
	Text 300,65,"wy",True,True
	Text 300,80,"xz",True,True
	Text 330,50,"Y/H",True,True
	Text 330,65,"wz",True,True
	Text 330,80,"xy",True,True
	Text 420,50,"z/x",True,True
	Text 420,65,"Alter stereogram",True,True
	Text 420,80,"angle ("+StrFloat(CrossViewingAngle,1)+Chr(176)+")",True,True
	Text 550,50,"+/-",True,True
	Text 550,65,"Alter viewing",True,True
	Text 550,80,"size ("+Str(ViewSize)+")",True,True
	Text 20,YRes-84,"1",True,True
	Text 20,YRes-70,"2",True,True
	Text 20,YRes-56,"3",True,True
	If RenderingDimension=2 Then
		Text 40,YRes-84,"Klein Bottle",False,True
;		Text 40,YRes-70,"Square",False,True
;		Text 40,YRes-56,"Pentagon",False,True
;		Text 20,YRes-42,"4",True,True
;		Text 40,YRes-42,"Hexagon",False,True
;		Text 20,YRes-28,"5",True,True
;		Text 40,YRes-28,"Heptagon",False,True
;		Text 20,YRes-14,"6",True,True
;		Text 40,YRes-14,"Octagon",False,True
	End If
	If RenderingDimension=3 Then
		Text 40,YRes-84,"Tetrahedron",False,True
		Text 40,YRes-70,"Cube",False,True
		Text 40,YRes-56,"Octahedron",False,True
		Text 20,YRes-42,"4",True,True
		Text 40,YRes-42,"Dodecahedron",False,True
		Text 20,YRes-28,"5",True,True
		Text 40,YRes-28,"Icosahedron",False,True
		Text 20,YRes-14,"6",True,True
		Text 40,YRes-14,"Small Stellated Dodecahedron",False,True
	End If
	If RenderingDimension=4 Then
		Text 40,YRes-84,"Pentachoron",False,True
		Text 40,YRes-70,"Tesseract",False,True
		Text 40,YRes-56,"Hexidecachoron",False,True
		Text 20,YRes-42,"4",True,True
		Text 40,YRes-42,"Icositetrachoron",False,True
		Text 20,YRes-28,"5",True,True
		Text 40,YRes-28,"Hecatonicosachoron",False,True
		Text 20,YRes-14,"6",True,True
		Text 40,YRes-14,"Hexacosichoron",False,True
	End If
	If RenderingDimension=5 Then
		Text 40,YRes-84,"Truncated Tetrahedron",False,True
		Text 40,YRes-70,"Truncated Cube",False,True
		Text 40,YRes-56,"Cuboctahedron",False,True
		Text 20,YRes-42,"4",True,True
		Text 40,YRes-42,"Icosidodecahedron",False,True
	End If
	If RenderingDimension=6 Then
		Text 40,YRes-84,"Dispentachoron",False,True
		Text 40,YRes-70,"Truncated Tesseract",False,True
		Text 40,YRes-56,"Rectified Tesseract",False,True
		Text 20,YRes-42,"4",True,True
		Text 40,YRes-42,"Snub Icositetrachoron",False,True
		Text 20,YRes-28,"5",True,True
		Text 40,YRes-28,"Truncated Icositetrachoron",False,True
		Text 20,YRes-14,"6",True,True
		Text 40,YRes-14,"Square-Octagonal Duoprism",False,True
	End If
	If RenderingDimension=7 Then
		Text 40,YRes-84,"Graph (y+wi)=(x+zi)^2",False,True
		Text 40,YRes-70,"Graph (y+wi)=(x+zi)^3",False,True
		Text 40,YRes-56,"Graph (y+wi)=1/(x+zi)",False,True
		Text 20,YRes-42,"4",True,True
		Text 40,YRes-42,"Graph (y+wi)=sin(x+zi)",False,True
		Text 20,YRes-28,"5",True,True
		Text 40,YRes-28,"Graph (y+wi)=cos(x+zi)",False,True
		Text 20,YRes-14,"6",True,True
		Text 40,YRes-14,"Graph (y+wi)=e^(x+zi)",False,True
	End If
	If RenderingDimension=8 Then
		;Text 40,YRes-84,"Dispentachoron",False,True
		Text 40,YRes-70,"Truncated Tesseract",False,True
		;Text 40,YRes-56,"Rectified Tesseract",False,True
		;Text 20,YRes-42,"4",True,True
		;Text 40,YRes-42,"Snub Icositetrachoron",False,True
	End If
	Text 240,YRes-84,"F2 Surfaces",False,True
	Text 240,YRes-70,"F3 Regular Polyhedra",False,True
	Text 240,YRes-56,"F4 Regular Polychora",False,True
	Text 240,YRes-42,"F5 Semi-Regular Polyhedra",False,True
	Text 240,YRes-28,"F6 Semi-Regular Polychora",False,True
	Text 240,YRes-14,"F7 Complex Graphs",False,True
	Text 420,YRes-84,"P - Point Thickness ("+Str(ThickPoints+1)+")",False,True
	Text 420,YRes-70,"L - Line Thickness ("+Str(ThickLines+1)+")",False,True
End Function

Function StrFloat$(f#,dp)
	t$=Str$(f)
	l=0
	Repeat
		l=l+1
	Until Mid(t,l,1)="."
	If dp=0 Then Return Left(t,l-1)
	Return Left(t,l+dp)
End Function

Function SetRotationMatrices()
	SetRotationMatrix(RotationQ,2,3,RotationAmount)
	SetRotationMatrix(RotationA,2,3,0-RotationAmount)
	SetRotationMatrix(RotationW,3,4,RotationAmount)
	SetRotationMatrix(RotationS,3,4,0-RotationAmount)
	SetRotationMatrix(RotationE,4,2,RotationAmount)
	SetRotationMatrix(RotationD,4,2,0-RotationAmount)
	SetRotationMatrix(RotationR,1,2,RotationAmount)
	SetRotationMatrix(RotationF,1,2,0-RotationAmount)
	SetRotationMatrix(RotationT,1,3,RotationAmount)
	SetRotationMatrix(RotationG,1,3,0-RotationAmount)
	SetRotationMatrix(RotationY,1,4,RotationAmount)
	SetRotationMatrix(RotationH,1,4,0-RotationAmount)
End Function

Function Respond()
	If KeyDown(16) Then MultiplyAll(RotationQ)
	If KeyDown(30) Then MultiplyAll(RotationA)
	If KeyDown(17) Then MultiplyAll(RotationW)
	If KeyDown(31) Then MultiplyAll(RotationS)
	If KeyDown(18) Then MultiplyAll(RotationE)
	If KeyDown(32) Then MultiplyAll(RotationD)
	If KeyDown(19) Then MultiplyAll(RotationR)
	If KeyDown(33) Then MultiplyAll(RotationF)
	If KeyDown(20) Then MultiplyAll(RotationT)
	If KeyDown(34) Then MultiplyAll(RotationG)
	If KeyDown(21) Then MultiplyAll(RotationY)
	If KeyDown(35) Then MultiplyAll(RotationH)
	If KeyHit(44) Then ChangeViewingAngle(CrossViewingAngle-0.5)		
	If KeyHit(45) Then ChangeViewingAngle(CrossViewingAngle+0.5)
	If KeyDown(78) And ViewSize<300 Then ChangeViewSize(ViewSize+1)
	If KeyDown(74) And ViewSize>50 Then ChangeViewSize(ViewSize-1)
	If KeyHit(60) Then ChangeRenderingDimension(2)
	If KeyHit(61) Then ChangeRenderingDimension(3)
	If KeyHit(62) Then ChangeRenderingDimension(4)
	If KeyHit(63) Then ChangeRenderingDimension(5)
	If KeyHit(64) Then ChangeRenderingDimension(6)
	If KeyHit(65) Then ChangeRenderingDimension(7)
	If KeyHit(66) Then ChangeRenderingDimension(8)
	If KeyHit(23) Then MakeIrregular()
	If KeyHit(25) Then
		ThickPoints=ThickPoints+1
		If ThickPoints>3 Then ThickPoints=0
		DrawBackground(SelectedColourScheme)
	End If
	If KeyHit(38) Then
		ThickLines=ThickLines+1
		If ThickLines>3 Then ThickLines=-1
		DrawBackground(SelectedColourScheme)
	End If
	If KeyHit(79) Then HighlightNextCell(1)
	If KeyHit(80) Then HighlightNextCell(2)
	If KeyHit(81) Then HighlightNextCell(3)
	If KeyHit(82) Then HighlightCell(0,"")
	If RenderingDimension=2 Then
		If KeyHit(2) Then RenderPolytope(2,1)
		;If KeyHit(3) Then RenderPolytope(2,4)
		;If KeyHit(4) Then RenderPolytope(2,5)
		;If KeyHit(5) Then RenderPolytope(2,6)
		;If KeyHit(6) Then RenderPolytope(2,7)
		;If KeyHit(7) Then RenderPolytope(2,8)
	End If
	If RenderingDimension=3 Then
		If KeyHit(2) Then RenderPolytope(3,1)
		If KeyHit(3) Then RenderPolytope(3,2)
		If KeyHit(4) Then RenderPolytope(3,3)
		If KeyHit(5) Then RenderPolytope(3,4)
		If KeyHit(6) Then RenderPolytope(3,5)
		If KeyHit(7) Then RenderPolytope(3,6)
	End If
	If RenderingDimension=4 Then
		If KeyHit(2) Then RenderPolytope(4,1)
		If KeyHit(3) Then RenderPolytope(4,2)
		If KeyHit(4) Then RenderPolytope(4,3)
		If KeyHit(5) Then RenderPolytope(4,4)
		If KeyHit(6) Then RenderPolytope(4,5)
		If KeyHit(7) Then RenderPolytope(4,6)
	End If
	If RenderingDimension=5 Then
		If KeyHit(2) Then RenderPolytope(5,1)
		If KeyHit(3) Then RenderPolytope(5,2)
		If KeyHit(4) Then RenderPolytope(5,3)
		If KeyHit(5) Then RenderPolytope(5,4)
	End If
	If RenderingDimension=6 Then
		extra=0
		If KeyDown(42) Then extra=11
		If KeyHit(2) Then RenderPolytope(6,1+extra)
		If KeyHit(3) Then RenderPolytope(6,2)
		If KeyHit(4) Then RenderPolytope(6,3)
		If KeyHit(5) Then RenderPolytope(6,4)
		If KeyHit(6) Then RenderPolytope(6,5)
		If KeyHit(7) Then RenderPolytope(6,6)
		If KeyHit(8) Then RenderPolytope(6,7)
		If KeyHit(9) Then RenderPolytope(6,8)
		If KeyHit(10) Then RenderPolytope(6,9)
		If KeyHit(11) Then RenderPolytope(6,10)
		If KeyHit(12) Then RenderPolytope(6,11)
	End If
	If RenderingDimension=7 Then
		If KeyHit(2) Then RenderPolytope(7,1)
		If KeyHit(3) Then RenderPolytope(7,2)
		If KeyHit(4) Then RenderPolytope(7,3)
		If KeyHit(5) Then RenderPolytope(7,4)
		If KeyHit(6) Then RenderPolytope(7,5)
		If KeyHit(7) Then RenderPolytope(7,6)
	End If
	If RenderingDimension=8 Then
		;If KeyHit(2) Then RenderPolytope(7,1)
		If KeyHit(3) Then RenderPolytope(8,2)
		;If KeyHit(4) Then RenderPolytope(7,3)
		;If KeyHit(5) Then RenderPolytope(7,4)
	End If
End Function

Function ChangeRenderingDimension(NewRD)
	RenderingDimension=NewRD
	DrawBackground(SelectedColourScheme)
End Function

Function ChangeViewingAngle(NewAngle#)
	CrossViewingAngle=NewAngle
	SetRotationMatrix(MLeftRotation,4,2,0-CrossViewingAngle)
	SetRotationMatrix(MRightRotation,4,2,CrossViewingAngle)
	DrawBackground(SelectedColourScheme)
End Function

Function ChangeViewSize(NewSize)
	ViewSize=NewSize
	DrawBackground(SelectedColourScheme)
	FreeImage(LeftViewImage)
	FreeImage(RightViewImage)
	LeftViewImage=CreateImage(ViewSize,ViewSize)
	RightViewImage=CreateImage(ViewSize,ViewSize)
End Function

Function EraseHyperspace()
	For P.Point=Each Point
		Delete P\P
	Next
	Delete Each Point
	For L.LineType=Each LineType
		Delete L\a
		Delete L\b
	Next
	Delete Each LineType
	Cells1=0
	Cells2=0
	Cells3=0
	CellTypeHighlighted=0
	CellNumberHighlighted=0
End Function

Function CreatePoint(w#,x#,y#,z#)

	If GrandAntiprismSafeguard Then
		If Abs(w)<0.0001 And Abs(y)<0.0001 Then
			If (x*x)+(z*z)=1 Then Return
		End If
		If Abs(x)<0.0001 And Abs(z)<0.0001 Then
			If (w*w)+(y*y)=1 Then Return
		End If
	End If

	P.Point=New Point
	P\P=New Vector
	SetPoint(P,w,x,y,z)
End Function

Function SetPoint(P.Point,w#,x#,y#,z#)
	P\P\w=w
	P\P\x=x
	P\P\y=y
	P\P\z=z
End Function

Function CreatePointIfAbsent(w#,x#,y#,z#)
	For P.Point=Each Point
		If P\P\w=w And P\P\x=x And P\P\y=y And P\P\z=z Then Return
	Next
	CreatePoint(w,x,y,z)
	;P.Point=New Point
	;P\P=New Vector
	;SetPoint(P,w,x,y,z)
	;DebugLog "Created Vertex : ("+Str(P\P\w)+","+Str(P\P\x)+","+Str(P\P\y)+","+Str(P\P\z)+")"
End Function


Function CreatePermutationPoints(w#,x#,y#,z#)
	CreatePointIfAbsent(w,x,y,z)
	CreatePointIfAbsent(w,x,z,y)
	CreatePointIfAbsent(w,y,x,z)
	CreatePointIfAbsent(w,y,z,x)
	CreatePointIfAbsent(w,z,x,y)
	CreatePointIfAbsent(w,z,y,x)
	CreatePointIfAbsent(x,w,y,z)
	CreatePointIfAbsent(x,w,z,y)
	CreatePointIfAbsent(x,y,w,z)
	CreatePointIfAbsent(x,y,z,w)
	CreatePointIfAbsent(x,z,w,y)
	CreatePointIfAbsent(x,z,y,w)
	CreatePointIfAbsent(y,w,x,z)
	CreatePointIfAbsent(y,w,z,x)
	CreatePointIfAbsent(y,x,w,z)
	CreatePointIfAbsent(y,x,z,w)
	CreatePointIfAbsent(y,z,w,x)
	CreatePointIfAbsent(y,z,x,w)
	CreatePointIfAbsent(z,w,x,y)
	CreatePointIfAbsent(z,w,y,x)
	CreatePointIfAbsent(z,x,w,y)
	CreatePointIfAbsent(z,x,y,w)
	CreatePointIfAbsent(z,y,w,x)
	CreatePointIfAbsent(z,y,x,w)
End Function

Function CreateEvenPermutationPoints(w#,x#,y#,z#)
	CreatePoint(w,x,y,z)
;	If w<>x And x<>y Then CreatePoint(x,y,w,z)
;	If w<>x And x<>y Then CreatePoint(y,w,x,z)
;	If w<>x And x<>z Then CreatePoint(z,w,y,x)
;	If w<>x And x<>z Then CreatePoint(x,z,y,w)
;	If w<>y And y<>z Then CreatePoint(z,x,w,y)
;	If w<>y And y<>z Then CreatePoint(y,x,z,w)
;	If x<>y And y<>z Then CreatePoint(w,y,z,x)
;	If x<>y And y<>z Then CreatePoint(w,z,x,y)
;	If w<>x And x<>y And y<>z Then CreatePoint(x,w,z,y)
;	If w<>x And x<>y And y<>z Then CreatePoint(y,z,w,x)
;	If w<>x And x<>y And y<>z Then CreatePoint(z,y,x,w)
	CreatePointIfAbsent(x,y,w,z)
	CreatePointIfAbsent(y,w,x,z)
	CreatePointIfAbsent(z,w,y,x)
	CreatePointIfAbsent(x,z,y,w)
	CreatePointIfAbsent(z,x,w,y)
	CreatePointIfAbsent(y,x,z,w)
	CreatePointIfAbsent(w,y,z,x)
	CreatePointIfAbsent(w,z,x,y)
	CreatePointIfAbsent(x,w,z,y)
	CreatePointIfAbsent(y,z,w,x)
	CreatePointIfAbsent(z,y,x,w)
End Function

Function SetVector(P.Vector,w#,x#,y#,z#)
	P\w=w
	P\x=x
	P\y=y
	P\z=z
End Function

Function CreateLine(w1#,x1#,y1#,z1#,w2#,x2#,y2#,z2#,C1$,C2$,C3$)
	L.LineType=New LineType
	L\a=New Vector
	L\b=New Vector
	SetLine(L,w1,x1,y1,z1,w2,x2,y2,z2,C1,C2,C3)
End Function

Function SetLine(L.LineType,w1#,x1#,y1#,z1#,w2#,x2#,y2#,z2#,C1$,C2$,C3$)
	L\a\w=w1
	L\a\x=x1
	L\a\y=y1
	L\a\z=z1
	L\b\w=w2
	L\b\x=x2
	L\b\y=y2
	L\b\z=z2
	L\Cell1=C1
	L\Cell2=C2
	L\Cell3=C3
End Function

Function HighLightCell(Number,Letter$)
	For L.LineType=Each LineType
		L\Highlighted=False
		If Number=0 Then C$=" "
		If Number=1 Then C$=L\Cell1
		If Number=2 Then C$=L\Cell2
		If Number=3 Then C$=L\Cell3
		cl=Len(C)
		For sl=1 To cl
			If Mid(C,sl,1)=Letter Then L\Highlighted=True
		Next
	Next
End Function

Function HighlightNextCell(Number)
	If Number=CellTypeHighlighted Then
		CellNumberHighlighted=CellNumberHighlighted+1
	Else
		CellNumberHighlighted=1
		CellTypeHighLighted=Number
	End If
	If Number=1 And CellNumberHighlighted>Cells1 Then CellNumberHighlighted=1
	If Number=1 And Cells1=0 Then CellTypeHighlighted=0
	If Number=2 And CellNumberHighlighted>Cells2 Then CellNumberHighlighted=1
	If Number=2 And Cells2=0 Then CellTypeHighlighted=0
	If Number=3 And CellNumberHighlighted>Cells3 Then CellNumberHighlighted=1
	If Number=3 And Cells3=0 Then CellTypeHighlighted=0
	HighlightCell(CellTypeHighlighted,Chr(CellNumberHighlighted+96))
End Function

Function CreateMatrix(a1#,a2#,a3#,a4#,b1#,b2#,b3#,b4#,c1#,c2#,c3#,c4#,d1#,d2#,d3#,d4#)
	M.Matrix=New Matrix
	SetMatrix(M,a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4)
End Function

Function SetMatrix(M.Matrix,a1#,a2#,a3#,a4#,b1#,b2#,b3#,b4#,c1#,c2#,c3#,c4#,d1#,d2#,d3#,d4#)
	M\a1=a1
	M\a2=a2
	M\a3=a3
	M\a4=a4
	M\b1=b1
	M\b2=b2
	M\b3=b3
	M\b4=b4
	M\c1=c1
	M\c2=c2
	M\c3=c3
	M\c4=c4
	M\d1=d1
	M\d2=d2
	M\d3=d3
	M\d4=d4
End Function

Function SetRotationMatrix(M.Matrix,plane1,plane2,degrees#)
	;In space, x axis=yz plane, y axis=zx plane, z axis=xy plane
	For x=1 To 4
		For y=1 To 4
			TempArrayMatrix(x,y)=0
			If x=y Then TempArrayMatrix(x,y)=1
		Next
	Next
	TempArrayMatrix(plane1,plane1)=Cos(degrees)
	TempArrayMatrix(plane2,plane2)=Cos(degrees)
	TempArrayMatrix(plane1,plane2)=0-Sin(degrees)
	TempArrayMatrix(plane2,plane1)=Sin(degrees)
	For x=1 To 4
		For y=1 To 4
			DebugLog TempArrayMatrix(x,y)
		Next
	Next
	SetMatrixToTemp(M)
End Function

Function SetMatrixToTemp(M.Matrix)
	M\a1=TempArrayMatrix(1,1)
	M\a2=TempArrayMatrix(2,1)
	M\a3=TempArrayMatrix(3,1)
	M\a4=TempArrayMatrix(4,1)
	M\b1=TempArrayMatrix(1,2)
	M\b2=TempArrayMatrix(2,2)
	M\b3=TempArrayMatrix(3,2)
	M\b4=TempArrayMatrix(4,2)
	M\c1=TempArrayMatrix(1,3)
	M\c2=TempArrayMatrix(2,3)
	M\c3=TempArrayMatrix(3,3)
	M\c4=TempArrayMatrix(4,3)
	M\d1=TempArrayMatrix(1,4)
	M\d2=TempArrayMatrix(2,4)
	M\d3=TempArrayMatrix(3,4)
	M\d4=TempArrayMatrix(4,4)
End Function

Function VectorsEqual(A.Vector,B.Vector)
	test=True
	precision#=0.0001
	If Abs(A\w-B\w)>precision Then test=False
	If Abs(A\x-B\x)>precision Then test=False
	If Abs(A\y-B\y)>precision Then test=False
	If Abs(A\z-B\z)>precision Then test=False
	Return test
End Function

Function MultiplyVector(V.Vector,M.Matrix,VR.Vector)
;NB column:row
	a#=(V\w*M\a1)+(V\x*M\a2)+(V\y*M\a3)+(V\z*M\a4)
	b#=(V\w*M\b1)+(V\x*M\b2)+(V\y*M\b3)+(V\z*M\b4)
	c#=(V\w*M\c1)+(V\x*M\c2)+(V\y*M\c3)+(V\z*M\c4)
	d#=(V\w*M\d1)+(V\x*M\d2)+(V\y*M\d3)+(V\z*M\d4)
	VR\w=a
	VR\x=b
	VR\y=c
	VR\z=d
End Function

Function MultiplyAll(M.Matrix)
	For P.Point=Each Point
		MultiplyVector(P\P,M,P\P)
	Next
	For L.LineType=Each LineType
		MultiplyVector(L\a,M,L\a)
		MultiplyVector(L\b,M,L\b)
	Next
End Function

Function FindEdges(EdgeLength#)
	EdgesFound=0
	P1.Point=First Point
	Repeat
		P2.Point=P1
		Repeat
			P2=After P2
			If Abs(PointsDistance(P1,P2)-EdgeLength)<0.0001 Then
				EdgesFound=EdgesFound+1
				CreateLine(P1\P\w,P1\P\x,P1\P\y,P1\P\z,P2\P\w,P2\P\x,P2\P\y,P2\P\z,"","","")
			End If
		Until P2=Last Point
		P1=After P1
	Until P1=Last Point
End Function

Function FindCells(CellNumber,S1,S2,S3,S4,S5)
	CellsFound=0
	If S1=3 Then
		If S2=3 Then
			If S3=3 Then
				If S4=0 Then
					;Tetrahedron
					L1.LineType=First LineType
					Repeat
						L2.LineType=L1
						Repeat
							L2=After L2
							If LinesTouch(L1,L2) Then
							
							End If
						Until L2=Last LineType
						L1=After L1
					Until L1=Last LineType
				End If
			End If
		End If
	End If	
End Function

Function PointsDistance#(P1.Point,P2.Point)
	Return Sqr(((P1\P\w-P2\P\w)^2)+((P1\P\x-P2\P\x)^2)+((P1\P\y-P2\P\y)^2)+((P1\P\z-P2\P\z)^2))
End Function

Function LinesTouch(L1.LineType,L2.LineType)
	If L1\a\w=L2\a\w And L1\a\x=L2\a\x And L1\a\y=L2\a\y And L1\a\z=L2\a\z Then Return True
	If L1\a\w=L2\b\w And L1\a\x=L2\b\x And L1\a\y=L2\b\y And L1\a\z=L2\b\z Then Return True
	If L1\b\w=L2\a\w And L1\b\x=L2\a\x And L1\b\y=L2\a\y And L1\b\z=L2\a\z Then Return True
	If L1\b\w=L2\b\w And L1\b\x=L2\b\x And L1\b\y=L2\b\y And L1\b\z=L2\b\z Then Return True
	Return False
End Function

Function MakeIrregular()
	l.LineType=First LineType
	d#=Sqr(((l\a\w-l\b\w)*(l\a\w-l\b\w))+((l\a\x-l\b\x)*(l\a\x-l\b\x))+((l\a\y-l\b\y)*(l\a\y-l\b\y))+((l\a\z-l\b\z)*(l\a\z-l\b\z)))
	d=d/2.0
	
	d=d*0.5 ; Just because the effect seemed a bit much
	
	HyperSpaceSize=HyperSpaceSize+d
	For p.Point=Each Point
		pd#=Rnd(0,d)
		wm#=Rnd(-1,1)
		xm#=Rnd(-1,1)
		ym#=Rnd(-1,1)
		zm#=Rnd(-1,1)
		tm#=Sqr((wm*wm)+(xm*xm)+(ym*ym)+(zm*zm))
		wm=(wm*pd)/tm
		xm=(xm*pd)/tm
		ym=(ym*pd)/tm
		zm=(zm*pd)/tm
		For l.LineType=Each LineType
			If VectorsEqual(p\p,l\a) Then
				l\a\w=l\a\w+wm
				l\a\x=l\a\x+xm
				l\a\y=l\a\y+ym
				l\a\z=l\a\z+zm
			End If
			If VectorsEqual(p\p,l\b) Then
				l\b\w=l\b\w+wm
				l\b\x=l\b\x+xm
				l\b\y=l\b\y+ym
				l\b\z=l\b\z+zm
			End If
		Next
		p\p\w=p\p\w+wm
		p\p\x=p\p\x+xm
		p\p\y=p\p\y+ym
		p\p\z=p\p\z+zm
	Next
End Function



























