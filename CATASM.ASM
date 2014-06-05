; Catacomb Source Code
; Copyright (C) 1993-2014 Flat Rock Software
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License along
; with this program; if not, write to the Free Software Foundation, Inc.,
; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

	MASM

	locals
	.MODEL	TPASCAL

	.DATA

	EXTRN	pics,xormask:WORD
	EXTRN	oldtiles, view, background, originx, originy: WORD
	EXTRN	priority: BYTE

objtype	STRUC

active	db	?
class 	db	?
x	db	?
y	db	?
stage	db	?
delay	db	?
dir	db	?
hp	db	?
oldx	db	?
oldy	db	?
oldtile dw	?
filler1	db	?,?,?,?

think 	db	?
contact db	?
solid	db	?
firstchar dw	?
psize	db	?
stages	db	?
dirmask db	?
speed 	dw	?
hitpoints db	?
damage 	db	?
points 	dw	?
filler2	db	?,?

objtype	ENDS

	EXTRN	obj, altobj: objtype

obp	dw	?		;temporary for turbo's BP


;
; rle STUFF
;

MinCnt	=	3-1	;min count for repeat
MaxCnt	=	127+MinCnt	;max count (128+ = non repeat string)

handle		dw	?	;file i/o requirements are here!
flength1	dw	?
flength2	dw	?
buf1		dw	?
buf2		dw	?
foff1		dw	?
foff2		dw	?

; We store our memory allocation results here

RLESeg	dw	?	;RLE packed buf (src/dest)
SrcSeg	dw	?	;original source data

RLEIdx	dw	?	;indexes for read/write bufs
SRCIdx	dw	?

SrcLen	dw	2 dup (?)	;length of source & dest bufs
RLELen	dw	2 dup (?)

ByteCnt	dw	?	;# of sequential bytes
LastByte	db	?	;last byte found
DifCnt	dw	?	;# of different bytes

EndByte	dw	?

	.CODE

;========================================================================

;{=========================================}
;{                                         }
;{ DRAWOBJ                                 }
;{ Draws the object to TILES in the proper }
;{ direction and state.                    }
;{                                         }
;{=========================================}
;
;Procedure DrawObj;
;var
;  objpri,bsize,tilenum:integer;
;Begin
;  tilenum:=obj.firstchar + obj.size * obj.size
;  *((integer(obj.dir) and obj.dirmask) * obj.stages + obj.stage);
;  obj.oldtile:= tilenum;
;  obj.oldx:=obj.x;
;  obj.oldy:=obj.y;
;  objpri:=priority[tilenum];
;  For y:=obj.y to obj.y+obj.size-1 do
;    for x:=obj.x to obj.x+obj.size-1 do
;      Begin
;	if priority[view[y,x]]<=objpri then
;	  view[y,x]:=tilenum;
;	inc(tilenum);
;      end;
;End;
;
;
squares	db	0,1,4,9,16,25,36,49,64

table86 dw    0,  86, 172, 258, 344, 430, 516, 602, 688, 774, 860, 946,1032,1118
    dw 1204,1290,1376,1462,1548,1634,1720,1806,1892,1978,2064,2150,2236,2322
    dw 2408,2494,2580,2666,2752,2838,2924,3010,3096,3182,3268,3354,3440,3526
    dw 3612,3698,3784,3870,3956,4042,4128,4214,4300,4386,4472,4558,4644,4730
    dw 4816,4902,4988,5074,5160,5246,5332,5418,5504,5590,5676,5762,5848,5934
    dw 6020,6106,6192,6278,6364,6450,6536,6622,6708,6794,6880,6966,7052,7138
    dw 7224,7310,7396


DrawObj	PROC   NEAR
	PUBLIC DrawObj

	mov	al,obj.dir
	and	al,obj.dirmask
	mul	obj.stages
	add	al,obj.stage
	mov	cl,al

	mov	bl,obj.psize
	xor	bh,bh
	mov	al,cs:[squares+bx]

	mul	cl

	add	ax,obj.firstchar

	mov	SI,ax
	mov	obj.oldtile,SI		;SI holds the first tile to put in

	mov	dl,[priority+SI]	;entire object has same priority
					;priority is saved in DL

	mov	bl,obj.y
	mov	obj.oldy,bl
	xor	bh,bh
	shl	bx,1
	mov	ax,cs:[table86+bx] ;View is 86*86
	mov	bl,obj.x
	mov     obj.oldx,bl
	xor	bh,bh
	add	ax,bx		;calculate origin's offset in VIEW
	shl	ax,1		;becuase view is WORD width
	mov	di,ax		;DI will point into VIEW

	mov	al,obj.psize	;throughout loop
	xor	ah,ah

	mov	dh,al		;do this many lines
@@yloop:
	mov	cx,ax		;do this many characters / line

@@xloop:
	mov     bx,[view+DI]
	cmp	dl,[priority+bx] ;check tiles priority level
	jb	@@next		;don't draw if lower than what's there
	mov	[view+di],si

@@next:
	inc	si
	inc	di
	inc	di
	loop	@@xloop

	sub	di,ax
	sub	di,ax
	add	di,86*2		;position destination at start of next line

	dec	dh		;any more lines to do?
	jnz	@@yloop

	ret

DrawObj	ENDP

;========================================================================

;{=======================================}
;{                                       }
;{ ERASEOBJ                              }
;{ Erases the current object by copying  }
;{ the background onto the view where the}
;{ object is standing                    }
;{                                       }
;{=======================================}
;
;Procedure EraseObj;
;var
;  objpri,bsize,tilenum:integer;
;Begin
;  tilenum:=obj.oldtile;
;  For y:=obj.oldy to obj.oldy+obj.size-1 do
;    for x:=obj.oldx to obj.oldx+obj.size-1 do
;      Begin
;	if view[y,x]=tilenum then
;	  view[y,x]:=background[y,x];
;	inc(tilenum);
;      end;
;End;
;
EraseObj PROC   NEAR
	PUBLIC EraseObj

	mov	SI,obj.oldtile ;only erase chars that match what was
				;drawn by the last drawobj
	mov	bl,obj.oldy
	xor	bh,bh
	shl	bx,1
	mov	ax,cs:[table86+bx] ;View is 86*86
	mov	bl,obj.oldx
	xor	bh,bh
	add	ax,bx		;calculate origin's offset in VIEW
	shl	ax,1		;becuase view is WORD width
	mov	di,ax		;DI will point into VIEW

	mov	al,obj.psize	;throughout loop
	xor	ah,ah

	mov	dh,al		;do this many lines
@@yloop:
	mov	cx,ax		;do this many characters / line

@@xloop:
	cmp     si,[view+DI]
	jne	@@next		;don't erase if its not part of the shape
	mov	bx,[background+di]
	mov	[view+di],bx	;erase it

@@next:
	inc	si
	inc	di
	inc	di
	loop	@@xloop

	sub	di,ax
	sub	di,ax
	add	di,86*2		;position destination at start of next line

	dec	dh		;any more lines to do?
	jnz	@@yloop

	ret

EraseObj	ENDP

;========================================================================

;{====================}
;{                    }
;{ DoAll              }
;{ The main play loop }
;{                    }
;{====================}
;
;Procedure Doall;
;begin
;  Repeat  {until leveldone or playdone}
;    For objecton:=numobj downto 0 do
;      Begin
;	move (o[objecton],obj.active,sizeof(o[objecton]) );
;	if obj.class<>nothing then {class=nothing means it was killed}
;	  Begin
;	    move (ObjDef[obj.class],obj.think,sizeof(objdef[obj.class]) );
;	    if obj.active then
;	      DoActive
;	    else
;	      DoInactive;
;	  end;
;      end;
;   refresh;
;   inc (frameon);
;  until leveldone or playdone;
;end;

	.DATA

	EXTRN	o:objtype
	EXTRN   ObjDef:WORD	;actually the second half of objtype record
	EXTRN	frameon:WORD
	EXTRN	numobj:WORD
	EXTRN	objecton:WORD
	EXTRN	leveldone:BYTE
	EXTRN	playdone:BYTE

	.CODE

	EXTRN	refresh:NEAR
	EXTRN	DoActive:NEAR
	EXTRN	DoInactive:NEAR

DoAll	PROC	NEAR
	PUBLIC	DoAll

@@repeat:
	mov	ax,[numobj]
	mov	[objecton],ax
@@forobjecton:
	mov	si,[objecton]
	shl	si,1
	shl	si,1
	shl	si,1
	shl	si,1		;o[] is 16 bytes wide
	add	si,OFFSET o
	mov	di,OFFSET obj
	push	ds
	pop	es
	movsw
	movsw
	movsw
	movsw
	movsw
	movsw
	movsw
	movsw			;copy 16 bytes

	mov     al,[obj.class]
	or	al,al
	jz	@@next
	xor	ah,ah
	mov	si,ax
	shl	si,1
	shl	si,1
	shl	si,1
	shl	si,1		;objdef is 16 bytes wide
	add	si,OFFSET objdef
	movsw
	movsw
	movsw
	movsw
	movsw
	movsw
	movsw
	movsw			;copy second 16 bytes into obj

	mov	al,[obj.active]
	or	al,al
	jnz	@@isactive
	call	DoInactive
	jmp	@@next
@@isactive:
	call	DoActive

@@next:
	mov	al,[leveldone]	; check end
	or	al,al
	jnz	@@done
	mov	al,[playdone]
	or	al,al
	jnz	@@done

	dec	[objecton]
	jns	@@forobjecton	; END for

	call	refresh
	inc	[frameon]

	mov	al,[leveldone]	; UNTIL
	or	al,al
	jnz	@@done
	mov	al,[playdone]
	or	al,al
	jz	@@repeat

@@done:
	ret

DoAll	ENDP

;========================================================================

;=================================================
;
; Init RND generator
; if randomize is false, the counter is set to 0
;
;=================================================

InitRnd	PROC	NEAR randomize:byte
	public	initrnd

	mov	al,randomize
	cmp	al,0
	jne	@@timeit		;if randomize is true, really random

	mov	dx,0			;set to a definate value
	mov	cx,dx
	mov	[cs:lastrnd],dx
	jmp	@@setit

@@timeit:
	mov	ah,2ch
	int	21h			;GetSystemTime

@@setit:
	mov	[cs:RndArray],dx
	mov	[cs:RndArray+2],cx
	mov	ax,17*2
	mov	[cs:indexi],ax
	mov	ax,5*2
	mov	[cs:indexj],ax

	push	ds
	push	es
	mov	ax,cs
	mov	ds,ax
	mov	es,ax
	mov	di,offset RndArray
	mov	si,offset baseRndArray
	mov	cx,17
	cld
	rep	movsw		;set up the table (which is constantly changed)
	pop	es
	pop	ds


	mov	ax,0ffffh
	push	ax
	call	Random			;warm up generator!
	mov	ax,0ffffh
	push	ax
	call	Random
	ret

initrnd	ENDP

;=================================================
;
; Return a random # between 0-?
; Exit : AX = 0-max value
;
;=================================================
random	PROC	NEAR maxval:WORD
	public	random

	mov	ax,maxval

	push	ax			;save max value
;
; create a mask to cut down on the # of SUBTRACTS!
;
	mov	dx,0ffffh		;full-mask
@@0:	shl	ax,1
	jc	@@0a
	shr	dx,1
	jmp	@@0
@@0a:
	pop	ax
	push	ax
	mov	bx,[cs:indexi]		;this routine was converted from
	mov	si,[cs:indexj]		;the Random macro on Merlin GS
	mov	ax,[cs:RndArray-2+bx]
	adc	ax,[cs:RndArray-2+si]
	mov	[cs:RndArray-2+bx],ax
	add	ax,[cs:LastRnd]
	mov	[cs:LastRnd],ax
	dec	bx
	dec	bx
	jne	@@1
	mov	bx,17*2
@@1:
	dec	si
	dec	si
	jne	@@2
	mov	si,17*2
@@2:
	mov	[cs:indexi],bx
	mov	[cs:indexj],si
	pop	cx                      ;loop -- returns value in range
	and	ax,dx			;AND our mask!
@@3:
	cmp	ax,cx			;SUBTRACT to be within range
	jbe	@@4
	shr	ax,1
@@4:
	ret

random	ENDP


;
; Random # Generator vars
;
indexi		dw	?	;Rnd#Generator
indexj		dw	?
LastRnd		dw	?
RndArray	dw	17 dup (?)

baseRndArray	dw	1,1,2,3,5,8,13,21,54,75,129,204
   		dw	323,527,850,1377,2227


;===========================================================================

;========
;
; WAITVBL
;
;========

WaitVBL	PROC	NEAR
	PUBLIC	WaitVBL

	mov	dx,3dah
waitvbl1:
	in	al,dx
	and	al,00001000b	;look for vbl
	jnz	waitvbl1

waitvbl2:
	in	al,dx
	and	al,00001000b	;look for vbl
	jz	waitvbl2

	ret

WaitVBL	ENDP

;=======================================================================

;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
;
; Move EGA tiles into EGA memory at "EGATileLoc"!
;
;ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
EGAMove	PROC	NEAR
	PUBLIC	EGAMove

	mov	dx,3c4h 	;turn MapMask register on!
	mov	al,2
	out	dx,al
	inc	dx

	mov	ax,0A200h
	mov	es,ax	;ES:DI = just after screen in latch memory
	xor	di,di

	mov	ax,WORD PTR [pics+2]
	mov	ds,ax
	xor	si,si	;DS:SI = start of tiles!

	cld
	mov	cx,2048

@@0:
	mov	ah,1b	;start at bitplane 0
	mov	bx,di

@@2:
	mov	al,ah
	out	dx,al	;select new bitplane!

	movsb
	movsb
	movsb
	movsb
	movsb
	movsb
	movsb
	movsb

	mov	di,bx

	shl	ah,1
	test	ah,1111b
	jnz	@@2	;do all bitplanes in shape

	add	di,8

	loop	@@0	;do all tiles

	mov	al,1111b
	out	dx,al	;select all bitplanes

	mov	ax,SEG DATA	;reset DATA segment
	mov	ds,ax

	ret

EGAMove	ENDP

;=======================================================================

;============
;
; CGAcharout
;
;============

CgaCharOut PROC	NEAR xcoordinate:WORD, ycoordinate:WORD, char:WORD
	PUBLIC	CgaCharOut

	mov	si,char
	shl	si,1
	shl	si,1
	shl	si,1
	shl	si,1		;char * 16 = tile's offset in PICS

	mov	di,xcoordinate
	shl	di,1		;x * 2 + ylookup[y] = screen location

	mov	bx,ycoordinate
	shl	bx,1
	add	di,cs:CGAylookup[bx]	;BX is pointer into YLOOKUP


	mov	bx,[xormask]	;so chars can be inverted

	mov	ax,[pics+2]
	mov	ds,ax		;segment of tile pictures (PARA'd)

	mov	ax,0B800h	;segment of screen memory
	mov	es,ax

	cld

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	add	di,1FFEh
	lodsw
	xor	ax,bx
	stosw
	sub	di,1FB2h
	lodsw
	xor	ax,bx
	stosw
	add	di,1FFEh
	lodsw
	xor	ax,bx
	stosw
	sub	di,1FB2h
	lodsw
	xor	ax,bx
	stosw
	add	di,1FFEh
	lodsw
	xor	ax,bx
	stosw
	sub	di,1FB2h
	lodsw
	xor	ax,bx
	stosw
	add	di,1FFEh
	lodsw
	xor	ax,bx
	stosw

	mov	ax,SEG DATA
	mov	ds,ax		;restore turbo's data segment
	ret
CgaCharOut ENDP


;=======================================================================

;============
;
; EGAcharout
;
;============

EgaCharOut PROC	NEAR xcoordinate:WORD, ycoordinate:WORD, char:WORD
	PUBLIC	EgaCharOut

	mov	si,char
	mov	bx,[xormask]	;so chars can be inverted
	or	bx,bx		;set flags
	je	notinv
	cmp	si,32
	jne	notspc
	mov	si,129
	jmp	notinv
notspc: add	si,32		;make the other set...

notinv:
	shl	si,1
	shl	si,1
	shl	si,1		;char * 8 = tile's offset in PICS
	add	si,2000h	;because the screen is in first part

	mov	di,xcoordinate	;x * + ylookup[y] = screen location

	mov	bx,ycoordinate
	shl	bx,1
	add	di,cs:EGAylookup[bx]	;BX is pointer into YLOOKUP

	mov	cx,ds
	mov	ax,0A000h	;segment of screen memory
	mov	es,ax
	mov	ds,ax		;tile pictures are also in screen memory

	mov	ax,105h
	mov	dx,3ceh		;set write mode 1
	out	dx,ax

	cld

	lodsb			;load in a row of the tile's picture
	stosb
	add	di,39
	lodsb			;load in a row of the tile's picture
	stosb
	add	di,39
	lodsb			;load in a row of the tile's picture
	stosb
	add	di,39
	lodsb			;load in a row of the tile's picture
	stosb
	add	di,39
	lodsb			;load in a row of the tile's picture
	stosb
	add	di,39
	lodsb			;load in a row of the tile's picture
	stosb
	add	di,39
	lodsb			;load in a row of the tile's picture
	stosb
	add	di,39
	lodsb			;load in a row of the tile's picture
	stosb

	mov	ax,SEG DATA
	mov	ds,cx		;restore turbo's data segment
	ret
EgaCharOut ENDP


;=======================================================================

;============
;
; VGAcharout
;
;============

VgaCharOut PROC	NEAR xcoordinate:WORD, ycoordinate:WORD, char:WORD
	PUBLIC	VgaCharOut

	mov	si,char
	shl	si,1
	shl	si,1
	shl	si,1
	shl	si,1
	shl	si,1
	shl	si,1		;char * 64 = tile's offset in PICS

	mov	di,xcoordinate
	shl	di,1
	shl	di,1
	shl	di,1		;x * 8 + ylookup[y] = screen location

	mov	bx,ycoordinate
	shl	bx,1
	add	di,cs:VGAylookup[bx]	;BX is pointer into YLOOKUP


	mov	bx,[xormask]	;so chars can be inverted

	mov	ax,[pics+2]
	mov	ds,ax		;segment of tile pictures (PARA'd)

	mov	ax,0A000h	;segment of screen memory
	mov	es,ax

	cld

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw

	add	di,312

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw

	add	di,312

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw

	add	di,312

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw

	add	di,312

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw

	add	di,312

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw

	add	di,312

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw

	add	di,312

	lodsw			;load in a row of the tile's picture
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw
	lodsw
	xor	ax,bx
	stosw

	mov	ax,SEG DATA
	mov	ds,ax		;restore turbo's data segment
	ret
VgaCharOut ENDP

;===========================================================================

;========
;
; YLOOKUP has the offsets from screen mem of each screen line (multiples of 8)
;
;========

CGAylookup	label	word
EGAylookup	label	word	;they are the same because of CGA's interleave
	dw 320*0,320*1,320*2,320*3,320*4,320*5,320*6,320*7
	dw 320*8,320*9,320*10,320*11,320*12,320*13,320*14,320*15
	dw 320*16,320*17,320*18,320*19,320*20,320*21,320*22,320*23,320*24

VGAylookup	label	word
	dw 2560*0,2560*1,2560*2,2560*3,2560*4,2560*5,2560*6,2560*7
	dw 2560*8,2560*9,2560*10,2560*11,2560*12,2560*13,2560*14,2560*15
	dw 2560*16,2560*17,2560*18,2560*19,2560*20,2560*21,2560*22,2560*23
	dw 2560*24


;=======================================================================

;=========
;
; CGAREFRESH redraws the tiles that have changed in the tiled screen area
;
;=========

CgaRefresh PROC	NEAR
	PUBLIC	CgaRefresh

	mov	obp,bp		;save off turbo's BP

	mov	ax,0B800h	;start of CGA memory
	mov	es,ax
	cld			;just in case
	mov	cx,OFFSET @@next	;so it can be JMPd to

	mov	ax,originy
	mov	bl,86		;View is 86*86 so no clipping
	mul	bl
	add	ax,originx	;calculate origin's offset in VIEW
	shl	ax,1		;becuase view is WORD width
	mov	bp,ax		;BP will point into VIEW
	mov	dx,ax
	add	dx,48		;when BP=DX, one row has been filled

	xor	bx,bx		;fast mov bx,0

@@check:
	mov	ax,view[bp]	;load the current tile
	cmp	ax,oldtiles[bx]	;compare it with the old tile
	jne	@@drawone		;if different, redraw
@@next:
	add	bx,2		;next oldtile
	add	bp,2		;next view tile
	cmp	bp,dx		;has an entire row from VIEW been drawn?
	jne	@@check

	cmp	bx,24*24*2	;have all tiles been drawn?
	je	@@done
	add	bp,124		;point it to the start of the next row
	add	dx,172		;point to end of next row
	jmp	@@check

@@done:
	mov	bp,obp		;restore turbo's BP
	ret

@@drawone:
	mov	oldtiles[bx],ax	;store the tile back to the oldtiles
	mov	di,word ptr cs:CGAtileloc[bx]	;set di to screen address
	shl	ax,1		;character number * 16 = start of data
	shl	ax,1
	shl	ax,1
	shl	ax,1
	mov	si,ax
	mov	ds,[pics+2]	;segment of pics (para aligned)
	movsw			;load in a row of the tile's picture
	add	di,1FFEh
	movsw
	sub	di,1FB2h
	movsw
	add	di,1FFEh
	movsw
	sub	di,1FB2h
	movsw
	add	di,1FFEh
	movsw
	sub	di,1FB2h
	movsw
	add	di,1FFEh
	movsw
	mov	ax,SEG DATA
	mov	ds,ax		;restore turbo's data segment
	jmp	CX		;CX holds OFFSET NEXT


CgaRefresh ENDP


;=======================================================================

;=========
;
; EGAREFRESH redraws the tiles that have changed in the tiled screen area
;
;=========

EgaRefresh PROC	NEAR
	PUBLIC	EgaRefresh

	mov	obp,bp		;save off turbo's BP

	mov	ax,105h
	mov	dx,3ceh		;set write mode 1
	out	dx,ax

	mov	ax,0A000h	;start of EGA memory
	mov	es,ax
	cld			;just in case
	mov	cx,OFFSET @@next	;so it can be JMPd to

	mov	ax,originy
	mov	bl,86		;View is 86*86 so no clipping
	mul	bl
	add	ax,originx	;calculate origin's offset in VIEW
	shl	ax,1		;becuase view is WORD width
	mov	bp,ax		;BP will point into VIEW
	mov	dx,ax
	add	dx,48		;when BP=DX, one row has been filled

	xor	bx,bx		;fast mov bx,0

@@check:
	mov	ax,view[bp]	;load the current tile
	cmp	ax,oldtiles[bx]	;compare it with the old tile
	jne	@@drawone		;if different, redraw
@@next:
	add	bx,2		;next oldtile
	add	bp,2		;next view tile
	cmp	bp,dx		;has an entire row from VIEW been drawn?
	jne	@@check

	cmp	bx,24*24*2	;have all tiles been drawn?
	je	@@done
	add	bp,124		;point it to the start of the next row
	add	dx,172		;point to end of next row
	jmp	@@check

@@done:
	mov	bp,obp		;restore turbo's BP
	ret

@@drawone:
	mov	oldtiles[bx],ax	;store the tile back to the oldtiles
	mov	di,word ptr cs:EGAtileloc[bx]	;set di to screen address
	shl	ax,1		;character number * 8 = start of data
	shl	ax,1
	shl	ax,1
	add	ax,2000h	;because the ega pics are in same bank as screen
	mov	si,ax

	mov	ax,es
	mov	ds,ax		;pics are just later in screen memory

	movsb			;load in a row of the tile's picture
	add	di,39
	movsb
	add	di,39
	movsb
	add	di,39
	movsb
	add	di,39
	movsb
	add	di,39
	movsb
	add	di,39
	movsb
	add	di,39
	movsb

	mov	ax,SEG DATA
	mov	ds,ax		;restore turbo's data segment
	jmp	CX		;CX holds OFFSET NEXT


EgaRefresh ENDP


;=======================================================================

;=========
;
; VGAREFRESH redraws the tiles that have changed in the tiled screen area
;
;=========

VgaRefresh PROC	NEAR
	PUBLIC	VgaRefresh

	mov	obp,bp		;save off turbo's BP

	mov	ax,0A000h	;start of VGA memory
	mov	es,ax
	cld			;just in case
	mov	cx,312		;added to DI after each row

	mov	ax,originy
	mov	bl,86		;View is 86*86 so no clipping
	mul	bl
	add	ax,originx	;calculate origin's offset in VIEW
	shl	ax,1		;becuase view is WORD width
	mov	bp,ax		;BP will point into VIEW
	mov	dx,ax
	add	dx,48		;when BP=DX, one row has been filled

	xor	bx,bx		;fast mov bx,0

@@check:
	mov	ax,view[bp]	;load the current tile
	cmp	ax,oldtiles[bx]	;compare it with the old tile
	jne	@@drawone		;if different, redraw
@@next:
	add	bx,2		;next oldtile
	add	bp,2		;next view tile
	cmp	bp,dx		;has an entire row from VIEW been drawn?
	jne	@@check

	cmp	bx,24*24*2	;have all tiles been drawn?
	je	@@done
	add	bp,124		;point it to the start of the next row
	add	dx,172		;point to end of next row
	jmp	@@check

@@done:
	mov	bp,obp		;restore turbo's BP
	ret

@@drawone:
	mov	oldtiles[bx],ax	;store the tile back to the oldtiles
	mov	di,cs:VGAtileloc[bx]	;set di to screen address
	cmp	ax,1023
	ja	@@drawhigh	;because the VGA pics take up two segments
	shl	ax,1		;character number * 64 = start of data
	shl	ax,1
	shl	ax,1
	shl	ax,1
	shl	ax,1
	shl	ax,1
	mov	si,ax
	mov	ds,[pics+2]	;segment of pics (para aligned)

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw


	mov	ax,SEG DATA
	mov	ds,ax		;restore turbo's data segment
	jmp	@@next

@@drawhigh:
	sub	ax,1024
	shl	ax,1		;character number * 64 = start of data
	shl	ax,1
	shl	ax,1
	shl	ax,1
	shl	ax,1
	shl	ax,1
	mov	si,ax
	mov	ax,[pics+2]	;segment of pics (para aligned)
	add	ax,1000h	;64k further than other pics
	mov	ds,ax

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw

	add	di,cx

	movsw			;load in a row of the tile's picture
	movsw
	movsw
	movsw


	mov	ax,SEG DATA
	mov	ds,ax		;restore turbo's data segment
	jmp	@@next


VgaRefresh ENDP


;=======================================================================

;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
;
; Name:	VideoID
;
; Function:	Detects the presence of various video subsystems
;
; int VideoID;
;
; Subsystem ID values:
; 	 0  = (none)
; 	 1  = MDA
; 	 2  = CGA
; 	 3  = EGA
; 	 4  = MCGA
; 	 5  = VGA
; 	80h = HGC
; 	81h = HGC+
; 	82h = Hercules InColor
;
;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
;
; Equates
;
;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
VIDstruct	STRUC		; corresponds to C data structure

Video0Type	DB	?	; first subsystem type
Display0Type	DB	? 	; display attached to first subsystem

Video1Type	DB	?	; second subsystem type
Display1Type	DB	?	; display attached to second subsystem

VIDstruct	ENDS


Device0	EQU	word ptr Video0Type[di]
Device1	EQU	word ptr Video1Type[di]


MDA	EQU	1	; subsystem types
CGA	EQU	2
EGA	EQU	3
MCGA	EQU	4
VGA	EQU	5
HGC	EQU	80h
HGCPlus	EQU	81h
InColor	EQU	82h

MDADisplay	EQU	1	; display types
CGADisplay	EQU	2
EGAColorDisplay	EQU	3
PS2MonoDisplay	EQU	4
PS2ColorDisplay	EQU	5

TRUE	EQU	1
FALSE	EQU	0

;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
;
; Program
;
;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

Results	VIDstruct <>	;results go here!

EGADisplays	DB	CGADisplay	; 0000b, 0001b	(EGA switch values)
	DB	EGAColorDisplay	; 0010b, 0011b
	DB	MDADisplay	; 0100b, 0101b
	DB	CGADisplay	; 0110b, 0111b
	DB	EGAColorDisplay	; 1000b, 1001b
	DB	MDADisplay	; 1010b, 1011b

DCCtable	DB	0,0	; translate table for INT 10h func 1Ah
	DB	MDA,MDADisplay
	DB	CGA,CGADisplay
	DB	0,0
	DB	EGA,EGAColorDisplay
	DB	EGA,MDADisplay
	DB	0,0
	DB	VGA,PS2MonoDisplay
	DB	VGA,PS2ColorDisplay
	DB	0,0
	DB	MCGA,EGAColorDisplay
	DB	MCGA,PS2MonoDisplay
	DB	MCGA,PS2ColorDisplay

TestSequence	DB	TRUE	; this list of flags and addresses
	DW	FindPS2	;  determines the order in which this
			;  program looks for the various
EGAflag	DB	?	;  subsystems
	DW	FindEGA

CGAflag	DB	?
	DW	FindCGA

Monoflag	DB	?
	DW	FindMono

NumberOfTests	EQU	($-TestSequence)/3


	PUBLIC	VideoID
VideoID	PROC	near

	push	bp	; preserve caller registers
	mov	bp,sp
	push	ds
	push	si
	push	di

	push	cs
	pop	ds
	ASSUME	DS:@Code

; initialize the data structure that will contain the results

	lea	di,Results	; DS:DI -> start of data structure

	mov	Device0,0	; zero these variables
	mov	Device1,0

; look for the various subsystems using the subroutines whose addresses are
; tabulated in TestSequence; each subroutine sets flags in TestSequence
; to indicate whether subsequent subroutines need to be called

	mov	byte ptr CGAflag,TRUE
	mov	byte ptr EGAflag,TRUE
	mov	byte ptr Monoflag,TRUE

	mov	cx,NumberOfTests
	mov	si,offset TestSequence

@@L01:	lodsb		; AL := flag
	test	al,al
	lodsw		; AX := subroutine address
	jz	@@L02	; skip subroutine if flag is false

	push	si
	push	cx
	call	ax	; call subroutine to detect subsystem
	pop	cx
	pop	si

@@L02:	loop	@@L01

; determine which subsystem is active

	call	FindActive

	mov	al,Results.Video0Type
	mov	ah,0	; was:  Results.Display0Type

	pop	di	; restore caller registers and return
	pop	si
	pop	ds
	mov	sp,bp
	pop	bp
	ret

VideoID	ENDP


;
; FindPS2
;
; This subroutine uses INT 10H function 1Ah to determine the video BIOS
; Display Combination Code (DCC) for each video subsystem present.
;

FindPS2	PROC	near

	mov	ax,1A00h
	int	10h	; call video BIOS for info

	cmp	al,1Ah
	jne	@@L13	; exit if function not supported (i.e.,
			;  no MCGA or VGA in system)

; convert BIOS DCCs into specific subsystems & displays

	mov	cx,bx
	xor	bh,bh	; BX := DCC for active subsystem

	or	ch,ch
	jz	@@L11	; jump if only one subsystem present

	mov	bl,ch	; BX := inactive DCC
	add	bx,bx
	mov	ax,[bx+offset DCCtable]

	mov	Device1,ax

	mov	bl,cl
	xor	bh,bh	; BX := active DCC

@@L11:	add	bx,bx
	mov	ax,[bx+offset DCCtable]

	mov	Device0,ax

; reset flags for subsystems that have been ruled out

	mov	byte ptr CGAflag,FALSE
	mov	byte ptr EGAflag,FALSE
	mov	byte ptr Monoflag,FALSE

	lea	bx,Video0Type[di]  ; if the BIOS reported an MDA ...
	cmp	byte ptr [bx],MDA
	je	@@L12

	lea	bx,Video1Type[di]
	cmp	byte ptr [bx],MDA
	jne	@@L13

@@L12:	mov	word ptr [bx],0    ; ... Hercules can't be ruled out
	mov	byte ptr Monoflag,TRUE

@@L13:	ret

FindPS2	ENDP


;
; FindEGA
;
; Look for an EGA.  This is done by making a call to an EGA BIOS function
;  which doesn't exist in the default (MDA, CGA) BIOS.

FindEGA	PROC	near	; Caller:	AH = flags
			; Returns:	AH = flags
			;		Video0Type and
			;		 Display0Type updated

	mov	bl,10h	; BL := 10h (return EGA info)
	mov	ah,12h	; AH := INT 10H function number
	int	10h	; call EGA BIOS for info
			; if EGA BIOS is present,
			;  BL <> 10H
			;  CL = switch setting
	cmp	bl,10h
	je	@@L22	; jump if EGA BIOS not present

	mov	al,cl
	shr	al,1	; AL := switches/2
	mov	bx,offset EGADisplays
	xlat		; determine display type from switches
	mov	ah,al	; AH := display type
	mov	al,EGA	; AL := subystem type
	call	FoundDevice

	cmp	ah,MDADisplay
	je	@@L21	; jump if EGA has a monochrome display

	mov	CGAflag,FALSE	; no CGA if EGA has color display
	jmp	short @@L22

@@L21:	mov	Monoflag,FALSE	; EGA has a mono display, so MDA and
			;  Hercules are ruled out
@@L22:	ret

FindEGA	ENDP

;
; FindCGA
;
; This is done by looking for the CGA's 6845 CRTC at I/O port 3D4H.
;
FindCGA	PROC	near	; Returns:	VIDstruct updated

	mov	dx,3D4h	; DX := CRTC address port
	call	Find6845
	jc	@@L31	; jump if not present

	mov	al,CGA
	mov	ah,CGADisplay
	call	FoundDevice

@@L31:	ret

FindCGA	ENDP

;
; FindMono
;
; This is done by looking for the MDA's 6845 CRTC at I/O port 3B4H.  If
; a 6845 is found, the subroutine distinguishes between an MDA
; and a Hercules adapter by monitoring bit 7 of the CRT Status byte.
; This bit changes on Hercules adapters but does not change on an MDA.
;
; The various Hercules adapters are identified by bits 4 through 6 of
; the CRT Status value:
;
; 000b = HGC
; 001b = HGC+
; 101b = InColor card
;

FindMono	PROC	near	; Returns:	VIDstruct updated

	mov	dx,3B4h	; DX := CRTC address port
	call	Find6845
	jc	@@L44	; jump if not present

	mov	dl,0BAh	; DX := 3BAh (status port)
	in	al,dx
	and	al,80h
	mov	ah,al	; AH := bit 7 (vertical sync on HGC)

	mov	cx,8000h	; do this 32768 times
@@L41:	in	al,dx
	and	al,80h	; isolate bit 7
	cmp	ah,al
	loope	@@L41	; wait for bit 7 to change
	jne	@@L42	; if bit 7 changed, it's a Hercules

	mov	al,MDA	; if bit 7 didn't change, it's an MDA
	mov	ah,MDADisplay
	call	FoundDevice
	jmp	short @@L44

@@L42:	in	al,dx
	mov	dl,al	; DL := value from status port
	and	dl,01110000b	; mask bits 4 thru 6

	mov	ah,MDADisplay	; assume it's a monochrome display

	mov	al,HGCPlus	; look for an HGC+
	cmp	dl,00010000b
	je	@@L43	; jump if it's an HGC+

	mov	al,HGC	; look for an InColor card or HGC
	cmp	dl,01010000b
	jne	@@L43	; jump if it's not an InColor card

	mov	al,InColor	; it's an InColor card
	mov	ah,EGAColorDisplay

@@L43:	call	FoundDevice

@@L44:	ret

FindMono	ENDP

;
; Find6845
;
; This routine detects the presence of the CRTC on a MDA, CGA or HGC.
; The technique is to write and read register 0Fh of the chip (cursor
; low).  If the same value is read as written, assume the chip is
; present at the specified port addr.
;

Find6845	PROC	near	; Caller:  DX = port addr
			; Returns: cf set if not present
	mov	al,0Fh
	out	dx,al	; select 6845 reg 0Fh (Cursor Low)
	inc	dx
	in	al,dx	; AL := current Cursor Low value
	mov	ah,al	; preserve in AH
	mov	al,66h	; AL := arbitrary value
	out	dx,al	; try to write to 6845

	mov	cx,100h
@@L51:	loop	@@L51	; wait for 6845 to respond

	in	al,dx
	xchg	ah,al	; AH := returned value
			; AL := original value
	out	dx,al	; restore original value

	cmp	ah,66h	; test whether 6845 responded
	je	@@L52	; jump if it did (cf is reset)

	stc		; set carry flag if no 6845 present

@@L52:	ret

Find6845	ENDP


;
; FindActive
;
; This subroutine stores the currently active device as Device0.  The
; current video mode determines which subsystem is active.
;

FindActive	PROC	near

	cmp	word ptr Device1,0
	je	@@L63	; exit if only one subsystem

	cmp	Video0Type[di],4	; exit if MCGA or VGA present
	jge	@@L63	;  (INT 10H function 1AH
	cmp	Video1Type[di],4	;  already did the work)
	jge	@@L63

	mov	ah,0Fh
	int	10h	; AL := current BIOS video mode

	and	al,7
	cmp	al,7	; jump if monochrome
	je	@@L61	;  (mode 7 or 0Fh)

	cmp	Display0Type[di],MDADisplay
	jne	@@L63	; exit if Display0 is color
	jmp	short @@L62

@@L61:	cmp	Display0Type[di],MDADisplay
	je	@@L63	; exit if Display0 is monochrome

@@L62:	mov	ax,Device0	; make Device0 currently active
	xchg	ax,Device1
	mov	Device0,ax

@@L63:	ret

FindActive	ENDP


;
; FoundDevice
;
; This routine updates the list of subsystems.
;

FoundDevice	PROC	near	; Caller:    AH = display #
			;	     AL = subsystem #
			; Destroys:  BX
	lea	bx,Video0Type[di]
	cmp	byte ptr [bx],0
	je	@@L71	; jump if 1st subsystem

	lea	bx,Video1Type[di]	; must be 2nd subsystem

@@L71:	mov	[bx],ax	; update list entry
	ret

FoundDevice	ENDP


;=========
;
; TILELOC has the offsets from $B800 of all 24*24 tiles
;
;=========

CGAtileloc	label	word
    dw    0,   2,   4,   6,   8,  10,  12,  14,  16,  18,  20,  22,  24,  26
    dw   28,  30,  32,  34,  36,  38,  40,  42,  44,  46, 320, 322, 324, 326
    dw  328, 330, 332, 334, 336, 338, 340, 342, 344, 346, 348, 350, 352, 354
    dw  356, 358, 360, 362, 364, 366, 640, 642, 644, 646, 648, 650, 652, 654
    dw  656, 658, 660, 662, 664, 666, 668, 670, 672, 674, 676, 678, 680, 682
    dw  684, 686, 960, 962, 964, 966, 968, 970, 972, 974, 976, 978, 980, 982
    dw  984, 986, 988, 990, 992, 994, 996, 998,1000,1002,1004,1006,1280,1282
    dw 1284,1286,1288,1290,1292,1294,1296,1298,1300,1302,1304,1306,1308,1310
    dw 1312,1314,1316,1318,1320,1322,1324,1326,1600,1602,1604,1606,1608,1610
    dw 1612,1614,1616,1618,1620,1622,1624,1626,1628,1630,1632,1634,1636,1638
    dw 1640,1642,1644,1646,1920,1922,1924,1926,1928,1930,1932,1934,1936,1938
    dw 1940,1942,1944,1946,1948,1950,1952,1954,1956,1958,1960,1962,1964,1966
    dw 2240,2242,2244,2246,2248,2250,2252,2254,2256,2258,2260,2262,2264,2266
    dw 2268,2270,2272,2274,2276,2278,2280,2282,2284,2286,2560,2562,2564,2566
    dw 2568,2570,2572,2574,2576,2578,2580,2582,2584,2586,2588,2590,2592,2594
    dw 2596,2598,2600,2602,2604,2606,2880,2882,2884,2886,2888,2890,2892,2894
    dw 2896,2898,2900,2902,2904,2906,2908,2910,2912,2914,2916,2918,2920,2922
    dw 2924,2926,3200,3202,3204,3206,3208,3210,3212,3214,3216,3218,3220,3222
    dw 3224,3226,3228,3230,3232,3234,3236,3238,3240,3242,3244,3246,3520,3522
    dw 3524,3526,3528,3530,3532,3534,3536,3538,3540,3542,3544,3546,3548,3550
    dw 3552,3554,3556,3558,3560,3562,3564,3566,3840,3842,3844,3846,3848,3850
    dw 3852,3854,3856,3858,3860,3862,3864,3866,3868,3870,3872,3874,3876,3878
    dw 3880,3882,3884,3886,4160,4162,4164,4166,4168,4170,4172,4174,4176,4178
    dw 4180,4182,4184,4186,4188,4190,4192,4194,4196,4198,4200,4202,4204,4206
    dw 4480,4482,4484,4486,4488,4490,4492,4494,4496,4498,4500,4502,4504,4506
    dw 4508,4510,4512,4514,4516,4518,4520,4522,4524,4526,4800,4802,4804,4806
    dw 4808,4810,4812,4814,4816,4818,4820,4822,4824,4826,4828,4830,4832,4834
    dw 4836,4838,4840,4842,4844,4846,5120,5122,5124,5126,5128,5130,5132,5134
    dw 5136,5138,5140,5142,5144,5146,5148,5150,5152,5154,5156,5158,5160,5162
    dw 5164,5166,5440,5442,5444,5446,5448,5450,5452,5454,5456,5458,5460,5462
    dw 5464,5466,5468,5470,5472,5474,5476,5478,5480,5482,5484,5486,5760,5762
    dw 5764,5766,5768,5770,5772,5774,5776,5778,5780,5782,5784,5786,5788,5790
    dw 5792,5794,5796,5798,5800,5802,5804,5806,6080,6082,6084,6086,6088,6090
    dw 6092,6094,6096,6098,6100,6102,6104,6106,6108,6110,6112,6114,6116,6118
    dw 6120,6122,6124,6126,6400,6402,6404,6406,6408,6410,6412,6414,6416,6418
    dw 6420,6422,6424,6426,6428,6430,6432,6434,6436,6438,6440,6442,6444,6446
    dw 6720,6722,6724,6726,6728,6730,6732,6734,6736,6738,6740,6742,6744,6746
    dw 6748,6750,6752,6754,6756,6758,6760,6762,6764,6766,7040,7042,7044,7046
    dw 7048,7050,7052,7054,7056,7058,7060,7062,7064,7066,7068,7070,7072,7074
    dw 7076,7078,7080,7082,7084,7086,7360,7362,7364,7366,7368,7370,7372,7374
    dw 7376,7378,7380,7382,7384,7386,7388,7390,7392,7394,7396,7398,7400,7402
    dw 7404,7406

EGAtileloc	label	word
    dw    0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13
    dw   14,  15,  16,  17,  18,  19,  20,  21,  22,  23, 320, 321, 322, 323
    dw  324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337
    dw  338, 339, 340, 341, 342, 343, 640, 641, 642, 643, 644, 645, 646, 647
    dw  648, 649, 650, 651, 652, 653, 654, 655, 656, 657, 658, 659, 660, 661
    dw  662, 663, 960, 961, 962, 963, 964, 965, 966, 967, 968, 969, 970, 971
    dw  972, 973, 974, 975, 976, 977, 978, 979, 980, 981, 982, 983,1280,1281
    dw 1282,1283,1284,1285,1286,1287,1288,1289,1290,1291,1292,1293,1294,1295
    dw 1296,1297,1298,1299,1300,1301,1302,1303,1600,1601,1602,1603,1604,1605
    dw 1606,1607,1608,1609,1610,1611,1612,1613,1614,1615,1616,1617,1618,1619
    dw 1620,1621,1622,1623,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929
    dw 1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943
    dw 2240,2241,2242,2243,2244,2245,2246,2247,2248,2249,2250,2251,2252,2253
    dw 2254,2255,2256,2257,2258,2259,2260,2261,2262,2263,2560,2561,2562,2563
    dw 2564,2565,2566,2567,2568,2569,2570,2571,2572,2573,2574,2575,2576,2577
    dw 2578,2579,2580,2581,2582,2583,2880,2881,2882,2883,2884,2885,2886,2887
    dw 2888,2889,2890,2891,2892,2893,2894,2895,2896,2897,2898,2899,2900,2901
    dw 2902,2903,3200,3201,3202,3203,3204,3205,3206,3207,3208,3209,3210,3211
    dw 3212,3213,3214,3215,3216,3217,3218,3219,3220,3221,3222,3223,3520,3521
    dw 3522,3523,3524,3525,3526,3527,3528,3529,3530,3531,3532,3533,3534,3535
    dw 3536,3537,3538,3539,3540,3541,3542,3543,3840,3841,3842,3843,3844,3845
    dw 3846,3847,3848,3849,3850,3851,3852,3853,3854,3855,3856,3857,3858,3859
    dw 3860,3861,3862,3863,4160,4161,4162,4163,4164,4165,4166,4167,4168,4169
    dw 4170,4171,4172,4173,4174,4175,4176,4177,4178,4179,4180,4181,4182,4183
    dw 4480,4481,4482,4483,4484,4485,4486,4487,4488,4489,4490,4491,4492,4493
    dw 4494,4495,4496,4497,4498,4499,4500,4501,4502,4503,4800,4801,4802,4803
    dw 4804,4805,4806,4807,4808,4809,4810,4811,4812,4813,4814,4815,4816,4817
    dw 4818,4819,4820,4821,4822,4823,5120,5121,5122,5123,5124,5125,5126,5127
    dw 5128,5129,5130,5131,5132,5133,5134,5135,5136,5137,5138,5139,5140,5141
    dw 5142,5143,5440,5441,5442,5443,5444,5445,5446,5447,5448,5449,5450,5451
    dw 5452,5453,5454,5455,5456,5457,5458,5459,5460,5461,5462,5463,5760,5761
    dw 5762,5763,5764,5765,5766,5767,5768,5769,5770,5771,5772,5773,5774,5775
    dw 5776,5777,5778,5779,5780,5781,5782,5783,6080,6081,6082,6083,6084,6085
    dw 6086,6087,6088,6089,6090,6091,6092,6093,6094,6095,6096,6097,6098,6099
    dw 6100,6101,6102,6103,6400,6401,6402,6403,6404,6405,6406,6407,6408,6409
    dw 6410,6411,6412,6413,6414,6415,6416,6417,6418,6419,6420,6421,6422,6423
    dw 6720,6721,6722,6723,6724,6725,6726,6727,6728,6729,6730,6731,6732,6733
    dw 6734,6735,6736,6737,6738,6739,6740,6741,6742,6743,7040,7041,7042,7043
    dw 7044,7045,7046,7047,7048,7049,7050,7051,7052,7053,7054,7055,7056,7057
    dw 7058,7059,7060,7061,7062,7063,7360,7361,7362,7363,7364,7365,7366,7367
    dw 7368,7369,7370,7371,7372,7373,7374,7375,7376,7377,7378,7379,7380,7381
    dw 7382,7383

VGAtileloc	label	word
    dw    0,   8,  16,  24,  32,  40,  48,  56,  64,  72,  80,  88,  96, 104
    dw  112, 120, 128, 136, 144, 152, 160, 168, 176, 184,2560,2568,2576,2584
    dw 2592,2600,2608,2616,2624,2632,2640,2648,2656,2664,2672,2680,2688,2696
    dw 2704,2712,2720,2728,2736,2744,5120,5128,5136,5144,5152,5160,5168,5176
    dw 5184,5192,5200,5208,5216,5224,5232,5240,5248,5256,5264,5272,5280,5288
    dw 5296,5304,7680,7688,7696,7704,7712,7720,7728,7736,7744,7752,7760,7768
    dw 7776,7784,7792,7800,7808,7816,7824,7832,7840,7848,7856,7864,10240,10248
    dw 10256,10264,10272,10280,10288,10296,10304,10312,10320,10328,10336,10344,10352,10360
    dw 10368,10376,10384,10392,10400,10408,10416,10424,12800,12808,12816,12824,12832,12840
    dw 12848,12856,12864,12872,12880,12888,12896,12904,12912,12920,12928,12936,12944,12952
    dw 12960,12968,12976,12984,15360,15368,15376,15384,15392,15400,15408,15416,15424,15432
    dw 15440,15448,15456,15464,15472,15480,15488,15496,15504,15512,15520,15528,15536,15544
    dw 17920,17928,17936,17944,17952,17960,17968,17976,17984,17992,18000,18008,18016,18024
    dw 18032,18040,18048,18056,18064,18072,18080,18088,18096,18104,20480,20488,20496,20504
    dw 20512,20520,20528,20536,20544,20552,20560,20568,20576,20584,20592,20600,20608,20616
    dw 20624,20632,20640,20648,20656,20664,23040,23048,23056,23064,23072,23080,23088,23096
    dw 23104,23112,23120,23128,23136,23144,23152,23160,23168,23176,23184,23192,23200,23208
    dw 23216,23224,25600,25608,25616,25624,25632,25640,25648,25656,25664,25672,25680,25688
    dw 25696,25704,25712,25720,25728,25736,25744,25752,25760,25768,25776,25784,28160,28168
    dw 28176,28184,28192,28200,28208,28216,28224,28232,28240,28248,28256,28264,28272,28280
    dw 28288,28296,28304,28312,28320,28328,28336,28344,30720,30728,30736,30744,30752,30760
    dw 30768,30776,30784,30792,30800,30808,30816,30824,30832,30840,30848,30856,30864,30872
    dw 30880,30888,30896,30904,-32256,-32248,-32240,-32232,-32224,-32216,-32208,-32200,-32192,-32184
    dw -32176,-32168,-32160,-32152,-32144,-32136,-32128,-32120,-32112,-32104,-32096,-32088,-32080,-32072
    dw -29696,-29688,-29680,-29672,-29664,-29656,-29648,-29640,-29632,-29624,-29616,-29608,-29600,-29592
    dw -29584,-29576,-29568,-29560,-29552,-29544,-29536,-29528,-29520,-29512,-27136,-27128,-27120,-27112
    dw -27104,-27096,-27088,-27080,-27072,-27064,-27056,-27048,-27040,-27032,-27024,-27016,-27008,-27000
    dw -26992,-26984,-26976,-26968,-26960,-26952,-24576,-24568,-24560,-24552,-24544,-24536,-24528,-24520
    dw -24512,-24504,-24496,-24488,-24480,-24472,-24464,-24456,-24448,-24440,-24432,-24424,-24416,-24408
    dw -24400,-24392,-22016,-22008,-22000,-21992,-21984,-21976,-21968,-21960,-21952,-21944,-21936,-21928
    dw -21920,-21912,-21904,-21896,-21888,-21880,-21872,-21864,-21856,-21848,-21840,-21832,-19456,-19448
    dw -19440,-19432,-19424,-19416,-19408,-19400,-19392,-19384,-19376,-19368,-19360,-19352,-19344,-19336
    dw -19328,-19320,-19312,-19304,-19296,-19288,-19280,-19272,-16896,-16888,-16880,-16872,-16864,-16856
    dw -16848,-16840,-16832,-16824,-16816,-16808,-16800,-16792,-16784,-16776,-16768,-16760,-16752,-16744
    dw -16736,-16728,-16720,-16712,-14336,-14328,-14320,-14312,-14304,-14296,-14288,-14280,-14272,-14264
    dw -14256,-14248,-14240,-14232,-14224,-14216,-14208,-14200,-14192,-14184,-14176,-14168,-14160,-14152
    dw -11776,-11768,-11760,-11752,-11744,-11736,-11728,-11720,-11712,-11704,-11696,-11688,-11680,-11672
    dw -11664,-11656,-11648,-11640,-11632,-11624,-11616,-11608,-11600,-11592,-9216,-9208,-9200,-9192
    dw -9184,-9176,-9168,-9160,-9152,-9144,-9136,-9128,-9120,-9112,-9104,-9096,-9088,-9080
    dw -9072,-9064,-9056,-9048,-9040,-9032,-6656,-6648,-6640,-6632,-6624,-6616,-6608,-6600
    dw -6592,-6584,-6576,-6568,-6560,-6552,-6544,-6536,-6528,-6520,-6512,-6504,-6496,-6488
    dw -6480,-6472


;==========================================================
; RLE compression in assemlby.  C source by Shawn M. Regan
; Written by Lane Roath, Copyright (c) 1990 IFD & Softdisk
;
;----------------------------------------------------------
;
;09-Aug-90 0.00	Start coding, converting source C in Dr. Dobb's
;
;==========================================================

	IDEAL

	ASSUME	ds:@DATA

;==========================================================
;
;	Initialize our variables & buffers
;
;==========================================================

PROC	InitRLE		;internal

	mov	[ByteCnt],0
	mov	[DifCnt],1
	mov	[RLELen],0
	mov	[RLELen+2],0
	mov	SI,[SrcIdx]	;init Source Index (SrcIdx)
	mov	DI,[RLEIdx]	; & dest index (RLEIdx)

	ret
ENDP


;==========================================================
;
;	Output a compression string (repeated bytes)
;
;==========================================================


PROC	OutputRep

	push	SI
	push	ES	;save needed registers
	push	AX

	mov	ES,[RLESeg]	;get dest buffer seg
	mov	AX,[ByteCnt]
	sub	AL,2	;get byte count - 3 (already -1)
	stosb

	mov	AL,[LastByte]	; & output byte to repeat
	stosb

	mov	SI,[SrcIdx]
	inc	SI	;update source index (length + 1)
	add	SI,[ByteCnt]
	mov	[SrcIdx],SI	; & save for next output

	add	[RLELen],2	;update RLE length
	adc	[RLELen+2],0

	mov	[ByteCnt],0	;we output everything!

	pop	AX
	pop	ES	;restore registers

	mov	BL,[ES:SI]
	mov	[LastByte],BL	;this is 'last' byte!

	pop	SI

	ret

	ENDP

;==========================================================
;
;	Output a unique string with count byte preceeding
;
;==========================================================


PROC	OutputDif

	push	ES	;save important registers
	push	AX
	push	SI

	mov	ES,[RLESeg]	;get dest buffer seg
	mov	AX,[DifCnt]
	add	AL,7Fh	; & save byte count - 1 with hi bit set
	stosb

	mov	CX,[DifCnt]	;get # of bytes to copy

	mov	AX,CX	;# of bytes read (count is -1)
	inc	AX
	add	[RLELen],AX	;update RLE length
	adc	[RLELen+2],0

	mov	SI,[SrcIdx]	; & starting position in source
	push	DS
	mov	DS,[SrcSeg]	;set source seg
	REP	movsb
	pop	DS	; & copy the string

	mov	[DifCnt],0	;we output everything!

	mov	[SrcIdx],SI	;save for next output

	pop	SI
	pop	AX
	pop	ES	;restore registers

	ret

	ENDP

;==========================================================
;
;	EXPAND a RLE compressed file segment
;
;==========================================================


PROC	Expand

	call	InitRLE	;init our vars & buffers

	xchg	SI,DI	;swap SI and DI

	mov	ES,[SrcSeg]	;point to source buffer
	xor	AX,AX
	mov	DX,1
@@get:
	cmp	DI,0FF00h	;don't bog us down!
	jb	@@ok

	mov	AX,DI
	shr	AX,1
	shr	AX,1	;offset / 16
	shr	AX,1
	shr	AX,1
	mov	BX,ES	; + segment register
	add	BX,AX
	mov	ES,BX	; = normalized pointer
	and	DI,0Fh

	mov	AX,SI
	shr	AX,1
	shr	AX,1	;offset / 16
	shr	AX,1
	shr	AX,1
	add	[RLESeg],AX	; + segment register = normalized ptr
	and	SI,0Fh
@@ok:
	sub	[SrcLen],DX	;subtract out # of chars
	sbb	[SrcLen+2],0
	jc	@@Done	;exit if we are done!
	xor	DX,DX
@@0:
	push	DS
	mov	DS,[RLESeg]	;point to RLE buffer
	lodsb		; & get code byte
	cmp	AL,80h
	jb	@@rpt	;repeat code?
	sub	AL,7Fh
	mov	CX,AX	; no- string length = code - $80 + 1
	mov	DX,AX

	REP	movsb	;copy string
	pop	DS
	jmp	SHORT @@get	; & do some more
@@rpt:
	add	AL,3	;# of bytes = code + 3
	mov	CX,AX
	mov	DX,AX
	lodsb		;get byte to repeat
	REP	stosb
	pop	DS	; & store that many bytes
	jmp	SHORT @@get
@@Done:
	ret

	ENDP

;==========================================================
;
;	COMPRESS data buffer using RLE
;
;==========================================================

PROC	Compress

	call	InitRLE	;init vars

	mov	ES,[SrcSeg]	;get first byte of data
	mov	AL,[ES:SI]

	mov	[LastByte],AL	; & set as last byte
@@get:
	cmp	SI,0FF00h	;don't bog us down!
	jb	@@ok

	call	@@Done	;send any patial strings!

	mov	AX,SI
	shr	AX,1
	shr	AX,1	;offset / 16
	shr	AX,1
	shr	AX,1
	mov	BX,ES	; + segment register
	add	BX,AX
	mov	ES,BX	; = normalized pointer
	and	SI,0Fh

	mov	AX,DI
	shr	AX,1
	shr	AX,1	;offset / 16
	shr	AX,1
	shr	AX,1
	add	[RLESeg],AX	; + segment register = normalized ptr
	and	DI,0Fh
@@ok:
	sub	[SrcLen],1	;subtract out # of chars
	sbb	[SrcLen+2],0
	jc	@@Done	;exit if we are done!
@@0:
	inc	SI	;get next byte
	mov	AL,[ES:SI]
	cmp	AL,[LastByte]
	jne	@@dif	;same as last byte?

	inc	[ByteCnt]	; yes- count it
	cmp	[ByteCnt],MinCnt
	jb	@@get

	cmp	[DifCnt],1	;ok, got a rep string...already have dif?
	jbe	@@same
	dec	[DifCnt]
	call	OutputDif	; yes- output that string first
@@same:
	mov	[DifCnt],0	;force even if not output!
	cmp	[ByteCnt],MaxCnt-1
	jb	@@get	;have we gotten too big?

	call	OutputRep	; yes, save string & continue

	inc	SI
	cmp	AL,[ES:SI]
	je	@@get

	inc	[SrcIdx]	; & handle special if not re-repeating
	dec	[ByteCnt]	;need this for >127 byte strings!
	jmp	SHORT @@get
;-------------------------------------------------------------
@@dif:
	mov	CX,[ByteCnt]	;any dups yet?
	jcxz	@@cnt

	cmp	CX,MinCnt	;repeats before now?
	jb	@@add

	call	OutputRep	; yes- save repeat bytes
	jmp	SHORT @@cnt
@@add:
	add	[DifCnt],CX	;add bad repeats to dif count
	mov	[ByteCnt],0	; & no more dupes!
@@cnt:
	inc	[DifCnt]	;count byte as different
	mov	[LastByte],AL

	cmp	[DifCnt],MaxCnt-MinCnt	;still in range?
	jb	@@goget

	call	OutputDif	; no- output string & continue
@@goget:
	jmp	NEAR @@get
;-------------------------------------------------------------
@@Done:
	mov	CX,[ByteCnt]	;any reps left?
	jcxz	@@1

	cmp	CX,MinCnt	; yes- enough to output?
	jb	@@2

	cmp	[DifCnt],1	; yes, any difs before these?
	jb	@@3

	call	OutputDif	; yes- output them
@@3:
	call	OutputRep	;output rep bytes
	jmp	SHORT @@4
@@2:
	add	[DifCnt],CX	;update dif count w/small rep
@@1:
	mov	CX,[DifCnt]	;anything to output?
	jcxz	@@4
	call	OutputDif	;output dif string
@@4:
	ret

	ENDP

;====================================================
;
;	C interface to compress file
;
;====================================================

	PUBLIC	RLECompress

PROC	RLECompress NEAR Source:DWORD,_Length:DWORD,Destination:DWORD

	mov	AX,[WORD Source]
	mov	[SrcIdx],AX
	mov	AX,[WORD Source+2]
	mov	[SrcSeg],AX	;set source buffer

	mov	AX,[WORD Destination]
	mov	[RLEIdx],AX
	mov	AX,[WORD Destination+2]
	mov	[RLESeg],AX	; & destination buffer

	mov	AX,[WORD _Length]
	mov	[SrcLen],AX	;save length
	mov	AX,[WORD _Length+2]
	mov	[SrcLen+2],AX

	call	Compress	;do the compression

	mov	AX,[RLELen]	;return length of compressed file
	mov	DX,[RLELen+2]

	ret

	ENDP

;====================================================
;
;	C interface to expand file
;
;====================================================

	PUBLIC	RLEExpand

PROC	RLEExpand NEAR Source:DWORD,Destination:DWORD,_Length:DWORD

	mov	AX,[WORD Source]
	mov	[RLEIdx],AX
	mov	AX,[WORD Source+2]
	mov	[RLESeg],AX	;set source buffer

	mov	AX,[WORD Destination]
	mov	[SrcIdx],AX
	mov	AX,[WORD Destination+2]
	mov	[SrcSeg],AX	; & destination buffer

	mov	AX,[WORD _Length]
	mov	[SrcLen],AX	;save length
	mov	AX,[WORD _Length+2]
	mov	[SrcLen+2],AX

	call	Expand	;do the expansion

	ret

	ENDP


	END


