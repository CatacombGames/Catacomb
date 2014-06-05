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

	  .MODEL TPASCAL

; this is just a hacked up version of the C library...


;
;offsets into .SPK file at segment SPKRfile, offset 0
;
;sound records each take up 16 bytes, and start at $10, and continue to $3F0

snd_start	equ	0
snd_priority	equ	2
snd_samples	equ	3
snd_name	equ	4

	  .DATA
	  extrn SoundOn:BYTE, SoundData:DWORD

SoundMode	dw	?       ;0=SPKR, 1= adlib...
OldInt8		dd	?	;StartupSPK saves here, Shutdown restores
ExtraInts	db	?	;number of PlaySPKR's to a regular int 8
Intcount	db	?	;counter for extraints, call OldInt8 at 0

SndPtr		dw	?	;Pointer to frequency of current sound
SndPriority	db	?	;current sound's priority

          .CODE

SPKactive dw    0                   ;set non zero when started


;=======================================================================

;========
;
; StartupSound
;
; Sets up the new INT 8 ISR and various internal pointers.
; Assumes that the calling program has pointer SOUNDDATA to something
; meaningful...
;
;========

StartupSound PROC FAR
	PUBLIC	StartupSound

	test	cs:[SPKactive],0FFFFh	;see if library is active
	jne	@@started		;library was allready started

@@start:
	call	FAR PTR StopSound	;make sure nothing is playing
	mov	ax,0
	mov	es,ax
	mov	ax,es:[32]	;get the old intterupt 8 vector
	mov	word ptr [oldint8],ax
	mov	ax,es:[34]
	mov	word ptr [oldint8+2],ax

	mov	ax,1
	mov	[extraints],al		;the timer is still going at the
	mov	[intcount],al		;normal rate now

	mov	WORD PTR es:[32],OFFSET UpdateSPKR ;point int 8 to UpdateSPKR
	mov	WORD PTR es:[34],SEG UpdateSPKR
	mov	ax,WORD PTR [SoundData+2]

	inc	[SPKactive]	;sound routines are now active

@@started:
	mov	al,1
	mov	[soundon],al
	ret

StartupSound	ENDP

;=======================================================================

;========
;
; ShutdownSound
;
;========

ShutdownSound PROC FAR
	PUBLIC ShutdownSound

	mov	ax,[SPKactive]
	cmp	ax,0
	je	@@done		;sound library wasn't started...

	mov	ax,0
	mov	es,ax
	mov	ax,word ptr [Oldint8]	;get the old intterupt 8 vector
	mov	es:[32],ax	;point int 8 back to BIOS
	mov	ax,word ptr [Oldint8+2]
	mov	es:[34],ax

	mov	al,36h		;tell the timer chip we are going to
	out	43h,al		;change the speed of timer 0
	mov	al,0		;system expects 0000 for rate
	out	40h,al		;low
	out	40h,al		;high

	mov	[SPKactive],0	;sound routines are now inactive

	in	al,61h		;get peripheral (speaker) port value
	and	al,11111101b	;turn speaker off
	out	61h,al

@@done:
	ret

ShutdownSound ENDP

;=========================================================================

;===========
;
; PlaySound (soundnum)
;
; If the sound's priority is >= the current priority, SoundPtr, SndPriority,
; and the timer speed are changed
;
;===========

PlaySound1  PROC  FAR playnum:WORD
	PUBLIC PlaySound1

	mov	ax,WORD PTR [SoundData+2]
	mov	es,ax		;point es: to the spkr file

	mov	si,playnum   	;index into the sound headers
	shl	si,1
	shl	si,1
	shl	si,1
	shl	si,1

	mov	al,es:[si+snd_Priority]	;priority table (one byte each)
	cmp	al,[SndPriority]
	jb	@@playdone	;current sound has higher priority
	mov	[SndPriority],al
	mov	ax,es:[si+snd_Start]	;offset in .SPK file
	mov	[SndPtr],ax	;store it in the sound playing table

	mov	bl,es:[si+snd_samples]	;samples / regular timer tick (1-255)
	mov	[extraints],bl	;an OldInt8 will be called after this
	mov	[intcount],bl	;many UpdateSPKR times have been called

	cmp	bl,1		;sample rate of 0 or 1 = 0000 for timer
	ja	@@oktodiv
	xor	bx,bx
	jmp	@@settimer

@@oktodiv:
	xor	bh,bh
	xor	ax,ax
	mov	dx,1
	div	bx		;$10000 / samples = timer rate

	mov	bx,ax

@@settimer:
	mov	al,36h		;tell the timer chip we are going to
	out	43h,al		;change the speed of timer 0

	mov	al,bl		;low byte of sample rate
	out	40h,al
	mov	al,bh		;high byte of sample rate
	out	40h,al

@@playdone:
	ret

PlaySound1 ENDP


;======================================================================

;===========
;
; StopSound
;
;===========

StopSound  PROC  FAR
	PUBLIC	StopSound

	xor	ax,ax		;set to 0
	mov	[SndPtr],ax
	mov	[SndPriority],al

	in	al,61h		;get peripheral (speaker) port value
	and	al,11111101b	;turn speaker off
	out	61h,al

	ret
StopSound  ENDP

;======================================================================

;========
;
; WaitendSound
; Just waits around until the current sound stops playing
;
;========

WaitendSound PROC FAR
	PUBLIC WaitendSound

	pushf
	call FAR PTR UpdateSPKR	;in case a sound was just started and hasn't
				;been hit by an INT yet
@@wait:
	mov	ax,[sndptr]
	cmp	ax,0		;when the ptr is 0, nothing is on
	jne	@@wait

	ret

WaitendSound ENDP

;=========================================================================

;========
;
; UpdateSPKR
; only called by interrupt $8!
;
;=========

UpdateSPKR PROC  FAR

	push	ax
	push	bx
	push	cx
	push	si
	push	ds
	push	es

	mov	ax,SEG @DATA
	mov	ds,ax		;ds to this data segment
	mov	ax,WORD PTR [SoundData+2]
	mov	es,ax		;es to sound file

	mov	al,20h
	out	20h,al		;we got the interrupt here

	dec	[intcount]	;see if it is time for a BIOS int 8...
	jne	@@dosounds

	mov	al,[extraints]
	mov	[intcount],al	;reset interrupt counter

	pushf			;so the IRET from bios returns right
	call	[OldInt8]	;call the old BIOS timer routine

@@dosounds:
;
; play the speaker
;
	mov	si,[SndPtr]
	cmp	si,0
	je	@@nosound	;nothing playing

	mov	bx,es:[si]
	inc	[SndPtr]
	inc	[SndPtr]

	cmp	bx,0
	je	@@nosound	;a zero frequency is no sound, but don't stop

	cmp	bx,-1		;a -1 frequency is end of sound
	jne	@@playfreq

	xor	ax,ax
	mov	[SndPtr],ax
	mov	[SndPriority],al

	mov	al,36h		;tell the timer chip we are going to
	out	43h,al		;change the speed of timer 0

	mov	al,0		;back to normal clock speed
	out	40h,al
	out	40h,al

	inc	al
	mov	[extraints],al  ;one bios int / int8
	mov	[intcount],al

@@nosound:
	in	al,61h		;get peripheral (speaker) port value
	and	al,11111100b	;turn speaker off
	out	61h,al
	jmp	@@doneplay

@@playfreq:
	test	[soundon],0FFh	;if soundon=0, don't play anything
	je	@@nosound

	mov	al,10110110b	;write to channel 2 (speaker) timer
	out	43h,al
	mov	al,bl
	out	42h,al		;low byte
	mov	al,bh
	out	42h,al		;high byte

	in	al,61h		;get peripheral (speaker) port value
	or	al,00000011b	;turn speaker on to timer
	out	61h,al

@@doneplay:
	pop	es
	pop	ds
	pop	si
	pop	cx
	pop	bx
	pop	ax

	iret


UpdateSPKR ENDP

;==========================================================================

          END
