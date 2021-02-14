;
_custom			=		$dff000		; base address of custom chips
_SysBase		=		$04

; Exec
OpenLibrary		=		-552
CloseLibrary		=		-414
RemLibrary		=		-402
AllocMem		=		-198
FreeMem			=		-210
Disable			=		-120
Enable			=		-126

; Gfx
OwnBlitter		=		-456
DisownBlitter		=		-462
gb_copinit		=		38		; system copper list

; Joyport
getjoyport		=		-30

bm_BytesPerRow		=		0		; width of background bitmap
bm_Rows			=		2		; height of background bitmap
bm_Depth		=		5		; number of bitplanes
bm_Planes0		=		6		; pointer to bitplanes
bm_Planes1		=		10
bm_Planes2		=		14
bm_Planes3		=		18
bm_Planes4		=		22
bm_Planes5		=		26

bob_OldY		=		0		; previous y coordinate
bob_OldX		=		2		; previous x coordinate
bob_State		=		4		; bob state: visible / invisible
bob_Y			=		6		; current y coordinate
bob_X			=		8		; current x coordinate
bob_Height		=		10		; height in lines
bob_Width		=		12		; width in words
bob_Depth		=		14		; number of bitplanes
bob_Image		=		16		; pointer to bitplanes
bob_ShadowMask		=		20		; pointer to shadowmask
bob_CollMask		=		24		; pointer to collision mask
bob_vy			=		28		; current y velocity
bob_vx			=		30		; current x velocity
bob_anim		=		32		; pointer to anim structure

img_height		=		0		; bob image height
img_width		=		2		; bob image width
img_depth		=		4		; bob image bitplanes
img_offset		=		6		; bob image offset from imageprt
img_shadow		=		10		; pointer to bob image shadowmask

blt_source		=		$00		; $00 - source address
blt_dest		=		$04		; $04 - destination adress
blt_srcmod		=		$08		; $08 - source modulo
blt_destmod		=		$0a		; $0a - destination modulo
blt_width		=		$0c		; $0c - width in words
blt_height		=		$0e		; $0e - height in lines
blt_srcshift		=		$10		; $10 - source shift
blt_fwm			=		$12		; $12 - first word mask
blt_lwm			=		$14		; $14 - last word mask

MEMF_CHIP		=		2
MEMF_CLEAR		=		$10000

BLTSIZE			=		$058		; bit 15 - bit 6 : height in lines / bit 5 - bit 0 : width in words
							; height = 1024 : bit 15 - bit 6 = 0 / width = 64 : bit 5 - bit 0 = 0
								
BLTAPTH			=		$050		; source address A (hi word)
BLTAPTL			=		$052		; source address A (lo word)

BLTBPTH			=		$04c		; source address B (hi word)
BLTBPTL			=		$04e		; source address B (lo word)

BLTCPTH			=		$048		; source address C (hi word)
BLTCPTL			=		$04a		; source address C (lo word)

BLTDPTH			=		$054		; destination address (hi word)
BLTDPTL			=		$056		; destination address (hi word)

BLTCON0			=		$040		; blitter control register 0
							; bit 15 - bit 12 : shift distance for source A
							; bit 11 - bit 8 : enable DMA channel for A - D
							; bit 7 - 0 : minterms
								
BLTCON1			=		$042		; blitter control register 1
							; bit 15 - bit 12 : shift distance for source B
							; bit 11 - bit 5 : unused
							; bit 4,3,2 : EFE, IFE, FCI
							; bit 1 : 0 = ascending mode / 1 = descending mode
							; bit 0 : 0 = copy mode / 1 = line mode
								
BLTAFWM			=		$044		; first word mask for source A
BLTALWM			=		$046		; last word mask for source A

BLTAMOD			=		$064		; modulo for source A

BLTBMOD			=		$062		; modulo for source B

BLTCMOD			=		$060		; modulo for source C

BLTDMOD			=		$066		; modulo for source D

BLTADAT			=		$074		; blitter source A data register
BLTBDAT			=		$072		; blitter source B data register

DMACONR			=		$002		; DMA control register (read)
DMACON			=		$096

POTINP			=		$016		; Pot pin data read

COP1LCH			=		$080		; address of copper list
COPJMP1			=		$088		; write address to copper program counter

SPR0PTH			=		$120

INTENAR			=		$01c		; read interrupt enable register
INTENA			=		$09a		; write interrupt enable
INTREQ			=		$09c		; interrupt request

JOYTEST			=		$03c		; write to all 4 joystick-mouse counters at once
JOY0DAT			=		$00a		; joystick-mouse 0 data
JOY1DAT			=		$00c		; joystick-mouse 1 data

CIAAPRA			=		$bfe001

JPB_JOY_UP		=		3
JPB_JOY_DOWN		=		2
JPB_JOY_LEFT		=		1
JPB_JOY_RIGHT		=		0
JPB_BUTTON_RED		=		22

;
; BEGIN
;
			move.l		_SysBase,a6
			jsr		Disable(a6)
			
			bsr		openlibs
			bsr		initimages		; allocate memory for images
			bsr		initbitmaps		; init bitmap structures
			bsr		initbobs		; init bob structures
			bsr		initmasks		; init bob shadowmasks
			bsr		buildcopperlist		; init copperlist and allocate memory for copperlist
			
			move.w		#$0020,DMACON(a6)	; disable sprite DMA
			move.l		#sprites,SPR0PTH(a6)	; no mouse pointer
			
			move.l		_GfxBase,a6
			jsr		OwnBlitter(a6)
;
; mainloop
;			
			move.l		#_custom,a6
			move.l		$6c,level3int		; save old IRQ routine (COPER, autovector level 3)
			move.w		INTENAR(a6),intena_old	; save old INTENAR register
			move.l		#swapbitmap,$6c		; implement own IRQ routine
			move.w		#$7fff,INTENA(a6)	; disable all interrupts
			move.w		#$c010,INTENA(a6)	; enable only COPER interrupt

mainloop		move.l		#_custom,a6
			btst.b		#$0a,POTINP(a6)		; test for right mouse button
			beq		mainexit

			bsr		movebobs		; move bobs
			bsr		animatebobs		; next animation frame
			bsr		joy			; read joyport and move cursor bob
			bsr		writebob		; write bobs to invisible bitmap
			bsr		swapdbuftable		; swap bitmaps during COPER interrupt
			bsr		resetbackground		; copy background bitmap to invisible bitmap
			bra		mainloop

mainexit		move.l			#_custom,a6
			or.w		#$8000,intena_old	; set bit 15
			move.w		#$7fff,INTENA(a6)	; disable all interrupts
			move.w		intena_old,INTENA(a6)	; reenable old interrupts
			move.l		level3int,$6c		; restore old IRQ routine
;
; end mainloop
;			
			lea		allbobs,a0
			bsr		disablebobs		; disable all bobs

			move.l		_GfxBase,a6
			jsr		DisownBlitter(a6)

			bsr		waitbutton
			bsr		restorecopper
			bsr		freeall
			bsr		closelibs
			
			move.l		_SysBase,a6
			jsr		Enable(a6)
			
			rts					; END
;
; initcolor (a0 = colortable, a1 = colorbuffer)
;
initcolor		move.w		#$0180,d1		; start at COLOR00
			move.w		#31,d0			; COLOR00 ... COLOR31
colorloop		move.w		d1,(a1)+		; add color register to copper list
			move.w		(a0)+,(a1)+		; add color value to copper list
			add.w		#2,d1			; next color register
			dbra		d0,colorloop		; loop until COLOR31
			rts
;
; initcopper (a0 = bitmap structure, a1 = copperbuffer)
;
initcopper		move.w		bm_BytesPerRow(a0),d1	; bitmap width (max. 40 bytes = 320 pixel)
			sub.w		#40,d1
			move.w		#$0108,(a1)+		; odd planes modulo to copper list
			move.w		d1,(a1)+		; MOVE $0000,BPL1MOD
			move.w		#$010a,(a1)+		; even planes modulo to copper list
			move.w		d1,(a1)+		; MOVE $0000,BPL2MOD
			move.b		bm_Depth(a0),d1			
			lsl.w		#8,d1			; shift to bits 14-12 for BPLCON0
			lsl.w		#5,d1
			lsr.w		#1,d1
			add.w		#$0200,d1		; set bit 9 (COLOR_ON) for BPLCON0
			move.w		#$0100,(a1)+		; bit plane control register 0 to copper list
			move.w		d1,(a1)+		; MOVE $x200,BPLCON0 (x = depth)
			moveq		#5,d0			; bitplane counter
			move.w		#$00e0,d1
			add.l		#bm_Planes0,a0
copperloop		move.w		d1,(a1)+		; BPLxPTH
			move.w		(a0)+,(a1)+		; address of bitplane x to copper list
			add.w		#2,d1
			move.w		d1,(a1)+		; BPLxPTL
			move.w		(a0)+,(a1)+		; address of bitplane x to copper list
			add.w		#2,d1
			dbra		d0,copperloop		; loop for all bitplanes
			rts
;
; resetbackground
;
resetbackground		lea		dbuf_table,a3
			move.l		8(a3),a0
			move.l		4(a3),a1
			bsr		copybitmap		; prepare copy of background bitmap
			rts
;
; copybitmap (a0 = source bitmap structure, a1 = destination bitmap structure)
;
copybitmap		move.l		#_custom,a6
			move.l		#$09f00000,BLTCON0(a6)	; enable DMA for A and D / minterms LF code (D = A)
			move.l		#$ffffffff,BLTAFWM(a6)	; set first word mask and last word mask
			clr.l		BLTAMOD(a6)		; no modulo for A
			move.w		bm_BytesPerRow(a0),d5	; width of bitmap in bytes
			lsr.w		#1,d5			; width of bitmap in words
			move.w		bm_Rows(a0),d6		; height of bitmap
			and.w		#$03ff,d6		; for special case : height = 1024
			lsl.w		#6,d6			; shift height to bits 15-6
			and.w		#$003f,d5		; for special case : width = 64
			add.w		d6,d5			; ready for BLTSIZE
			clr.w		d0
			move.b		bm_Depth(a0),d0		; bitmap depth
			subq		#1,d0			; minus 1 for loop
			add.l		#bm_Planes0,a0		; bitplane 0 of source
			add.l		#bm_Planes0,a1		; bitplane 0 of destination
cpbmploop		move.l		(a0)+,BLTAPTH(a6)	; address of source A
			move.l		(a1)+,BLTDPTH(a6)	; address of destination D
			move.w		d5,BLTSIZE(a6)		; start blitter
cpbmwait		btst		#$0e,DMACONR(a6)	; wait for blitter copy finish
			bne		cpbmwait
			dbra		d0,cpbmploop		; loop through all bitplanes
			rts
;
; swapbitmaptable
;
swapdbuftable		lea		dbuf_table,a3
			move.l		(a3),a0
			move.l		4(a3),(a3)
			move.l		a0,4(a3)		; swap visible and invisible bitmap in table
			move.w		#1,bmp_swap		; set swap flag
swapwait		tst.w		bmp_swap		; wait for COPER interrupt
			bne		swapwait
			rts
;
; swapbitmap (called from COPER interrupt)
;
swapbitmap		move.l		#_custom,a6
			move.w		#$0010,INTREQ(a6)	; clear COPER interrupt
			movem.l		d0-d7/a0-a6,-(sp)
			
			tst.w		bmp_swap		; test for swap flag
			beq		swapend
			move.l		dbuf_table,a0
			move.l		coplistptr,a1
			add.l		#144,a1			; offset to copperbuffer (bitplane commands)
			bsr		initcopper		; initialize new copper list for swaped bitmap
			bsr		cylce			; cycle background COLOR04...COLOR11
			clr.w		bmp_swap		; clear swap flag
swapend			movem.l		(sp)+,d0-d7/a0-a6
			
			rte
			
;
; movebobs
;
movebobs		lea		allbobs,a1
			move.w		(a1)+,d2		; number of bobs
			tst.w		d2
			beq		movebobsend
			subq.w		#1,d2			; number of bobs - 1 for loop
movebobsloop		move.l		(a1)+,a0		; bob structure
			
			bsr		moveit

movebobsnext		dbra		d2,movebobsloop
			
movebobsend		rts
;
; moveit (a0 = bob structure)
;
moveit			move.l		bob_Y(a0),bob_OldY(a0)
			
			cmp.w		#304,bob_X(a0)
			blt		chk_x
			neg.w		bob_vx(a0)
			
chk_x			cmp.w		#0,bob_X(a0)
			bge		mov_x
			neg.w		bob_vx(a0)
			
mov_x			move.w		bob_vx(a0),d0
			add.w		d0,bob_X(a0)
			
			cmp.w		#240,bob_Y(a0)
			blt		chk_y
			neg.w		bob_vy(a0)
			
chk_y			cmp.w		#0,bob_Y(a0)
			bge		mov_y
			neg.w		bob_vy(a0)
			
mov_y			move.w		bob_vy(a0),d0
			add.w		d0,bob_Y(a0)
			
			rts

;
; bitmapcopy (a0 = source bitmap structure, a1 = destination bitmap structure)
;
bitmapcopy		move.l		#_custom,a6		; base address of custom chips
			move.l		#$01f00000,BLTCON0(a6)	; use DMA A and D, minterm D=A, clear BLTCON1
			move.l		#$ffffffff,BLTAFWM(a6)	; first and last word mask
			clr.l		BLTAMOD(a6)		; no modulo
			move.w		bm_BytesPerRow(a0),d5	; width in bytes
			lsr.w		#1,d5			; convert to width in words
			move.w 		bm_Rows(a0),d6		; height in lines
			and.w		#$03ff,d6		; for special case : height = 1024
			lsl.w		#6,d6			; shift height to bits 15-6
			and.w		#$003f,d5		; for special case : width = 64
			add.w		d6,d5			; width in bits 5-0
			clr.w		d0			; clear high byte
			move.b		bm_Depth(a0),d0		; depth in low byte
			subq		#1,d0			; decrease by one for loop
			add.l		#bm_Planes0,a0		; address of source bitmap
			add.l		#bm_Planes0,a1		; address of destination bitmap
bitmaploop		move.l		(a0)+,BLTAPTH(a6)	; source address to blitter register A
			move.l		(a1)+,BLTDPTH(a6)	; destination address to blitter register D
			move.w		d5,BLTSIZE(a6)		; set width and height
			
bltwait2		btst		#$0e,DMACONR(a6)	; wait for blitter operation finish
			bne		bltwait2
			dbra		d0,bitmaploop		; loop through all planes
			rts

;
; bitmapfill (a0 = bitmap structure, d0 = pattern, d1 = bitplane)
;
bitmapfill		move.l		#_custom,a6		; base address of custom chips
			move.w		d0,BLTADAT(a6)		; fill pattern
			move.l		#$01f00000,BLTCON0(a6)	; use DMA A and D, minterm D=A, clear BLTCON1
			move.l		#$ffffffff,BLTAFWM(a6)	; first and last word mask
			clr.l		BLTAMOD(a6)		; no modulo
			move.w		bm_BytesPerRow(a0),d5	; width in bytes
			lsr.w		#1,d5			; convert to width in words
			move.w		bm_Rows(a0),d6		; height in lines
			and.w		#$03ff,d6		; for special case : height = 1024
			lsl.w		#6,d6			; shift height to bits 15-6
			and.w		#$003f,d5		; for special case : width = 64
			add.w		d6,d5			; width in bits 5-0
			clr.w		d0			; clear high byte
			move.b		bm_Depth(a0),d0		; depth in low byte
			subq		#1,d0			; decrease by one for loop
			add.l		#bm_Planes0,a0		; address of destination bitmap
			
			cmp.w		#$0,d1			; bitplane = 0 : all bitplanes
			bgt		oneplane		; bitplane > 0 : specified bitplane

fillloop		move.l		(a0)+,BLTDPTH(a6)	; destination address to blitter register D
			move.w		d5,BLTSIZE(a6)		; set width and height
			
fillwait		btst		#$0e,DMACONR(a6)	; wait for blitter operation finish
			bne		fillwait
			dbra		d0,fillloop		; loop through all planes
			rts

oneplane		subq		#1,d1			; decrease by one
			lsl.l		#2,d1			; multiply by 4
			add.l		d1,a0			; add to address of destination bitmap
			
			move.l		(a0),BLTDPTH(a6)	; destination address to blitter register D
			move.w		d5,BLTSIZE(a6)		; set width and height
			
fillwaitone		btst		#$0e,DMACONR(a6)	; wait for blitter operation finish
			bne		fillwaitone
			rts

;
; calcshadowmask (a0 = imageptr, a1 = shadowptr, d1 = depth, d2 = width; d3 = height)
;
calcshadowmask		move.l		#_custom,a6
			move.l		#$0dfc0000,BLTCON0(a6)	; enable DMA for A, B, D / minterms LF code (D = A+B)
			move.l		#$ffffffff,BLTAFWM(a6)	; first and last word mask
			clr.l		BLTBMOD(a6)		; no modulo for A and B
			clr.w		BLTDMOD(a6)		; no modulo for D
			move.w		d2,d5			; bob width in words
			move.w		d3,d6			; bob height in lines
			move.w		d5,d0
			lsl.w		#1,d0			; bob width in bytes
			mulu		d6,d0			; bob total size in bytes
			and.w		#$03ff,d6		; for special case : height = 1024
			lsl.w		#6,d6			; shift height to bits 15-6
			and.w		#$003f,d5		; for special case: width = 64
			add.w		d6,d5			; ready for BLTSIZE
			sub.w		#1,d1			; decrement depth by 1 for loop
shadowloop		move.l		a0,BLTAPTH(a6)		; source A
			move.l		a1,BLTDPTH(a6)		; destination D
			move.l		a1,BLTBPTH(a6)		; source B
			move.w		d5,BLTSIZE(a6)		; start blitter operation
shadowwait		btst		#$0e,DMACONR(a6)	; wait for blitter finish
			bne		shadowwait
			add.l		d0,a0			; add offset to next bitplane
			dbra		d1,shadowloop		; loop through all bitplanes
			rts
;
; writebob
;
writebob		lea		allbobs,a0
			move.l		a0,a1
			move.w		(a1)+,d0		; number of bobs
			tst.w		d0
			beq		writebobend
			clr.l		d1
			move.w		d0,d1
			lsl.w		#2,d1			; number of bobs * 4
			add.l		d1,a1			; pointer to last bob in list
			subq.w		#1,d0			; number of bobs - 1 for loop
writebobloop		move.l		-(a1),a0
			cmp.l		#0,a0
			beq		writebobnext
			movem.l		d0/a0-a1,-(sp)
			lea		dbuf_table,a1
			move.l		4(a1),a1
			bsr		drawbob			; start bob routine
			movem.l		(sp)+,d0/a0-a1
writebobnext		dbra		d0,writebobloop		; loop through all bobs in list
writebobend		rts
;
; disablebobs (a0 = allbobs list)
;
disablebobs		move.l		a0,a1
			move.w		(a1)+,d0		; number of bobs
			tst.w		d0
			beq		disablebobend
			clr.l		d1
			move.w		d0,d1
			lsl.w		#2,d1			; number of bobs * 4
			add.l		d1,a1			; pointer to last bob in list
			subq.w		#1,d0			; number of bobs - 1 for loop
disablebobloop		move.l		-(a1),a0
			cmp.l		#0,a0
			beq		disablebobnext
			clr.w		bob_State(a0)		; set state to invisible
			movem.l		d0/a0-a1,-(sp)
			lea		dbuf_table,a1
			move.l		4(a1),a1
			bsr		drawbob			; start bob routine
			movem.l		(sp)+,d0/a0-a1
disablebobnext		dbra		d0,disablebobloop	; loop through all bobs in list
disablebobend		rts
;
; animatebobs
;
animatebobs		move.l		animtimer,d0
			addq.l		#1,d0
			move.l		d0,animtimer
			cmp.l		#8,d0
			bne		animend
			move.l		#0,animtimer

			lea		allanims,a3
			move.w		(a3)+,d2		; number of anim structures
			tst.w		d2
			beq		animend
			subq		#1,d2
animstrucloop		clr.l		d0
			move.l		(a3)+,a0		; anim structure
			
			bsr		animnext
			
			dbra		d2,animstrucloop

			lea		allbobs,a3
			move.w		(a3)+,d2		; number of bobs
			tst.w		d2
			beq		animend
			subq		#1,d2
animbobsloop		clr.l		d0
			move.l		(a3)+,a1		; bob structure
			move.l		bob_anim(a1),a0		; anim structure
			
			tst.w		(a0)
			beq		animbobnext
			
			bsr		animsetptr

animbobnext		dbra		d2,animbobsloop		
			bra		animend
			
animnext		move.w		(a0),d0			; max animation image
			beq		animend
			
			subq.w		#1,d0
			clr.l		d1
			move.w		2(a0),d1		; current animation image
			addq.w		#1,d1			; next animation image
			
			cmp.w		d1,d0
			bge		animcount
			clr.l		d1
			
animcount		move.w		d1,2(a0)		; change current animation image counter
			rts
			
animsetptr		move.w		2(a0),d1		; current animation image
			lsl.w		#2,d1			; counter * 4
			addq.w		#4,d1			; 4 + counter * 4
			add.l		d1,a0			; pointer to img structure pointer
			move.l		(a0),a0			; pointer to img structure
			move.l		img_offset(a0),d1	; image offset
			move.l		imageptr,a2
			add.l		d1,a2
			move.l		a2,bob_Image(a1)	; set new bob image
			move.l		img_shadow(a0),bob_ShadowMask(a1)
			move.l		img_shadow(a0),bob_CollMask(a1)

animend			rts
;
; drawbob (a0 = bob structure, a1 = bitmap structure)
;
drawbob			move.l		#_custom,a2		; base address of custom chips
			move.w		bm_BytesPerRow(a1),d6	; bitmap width in bytes
			lsr.w		#1,d6			; bitmap width in words
			move.w		bm_Rows(a1),d7		; bitmap height in lines
			
			move.w		bob_Y(a0),d0
			cmp.w		bm_Rows(a1),d0		; bob_Y >= height of bitmap ?
			bge		db_exit
			
			move.w		bob_Height(a0),d1
			neg.w		d1
			cmp.w		d1,d0			; bob_Y <= -(height of bob) ?
			ble		db_exit
			
			move.w		bob_X(a0),d0
			move.w		bm_BytesPerRow(a1),d1
			lsl.w		#3,d1			; bitmap width in pixel
			cmp.w		d1,d0			; bob_X >= width in pixel ?
			bge		db_exit
			
			move.w 		bob_Width(a0),d1
			sub.w		#1,d1			; ignore last word (always #$0000)
			lsl.w		#4,d1			; bob width in pixel
			neg.w		d1
			cmp.w		d1,d0			; bob_X <= -(bob width in pixel) ?
			ble		db_exit
			
			tst.w		bob_State(a0)		; bob off state ?
			bsr		db_writebob

db_exit			rts
			
;
; db_calc_blitter (d0 = bob y position, d1 = bob x position)
;
db_calc_blitter		tst.w		d0			; bob y position > 0 ?
			bpl		db_calc_y_pl
			move.w		bob_Width(a0),d2	; bob width in words
			lsl.w		#1,d2			; bob width in bytes
			move.w		d0,d3
			neg.w		d3			; absolute value of bob y position
			mulu		d3,d2			; number of bytes out of screen (-> offset)
			add.w		bob_Height(a0),d0	; height of visible part of bob (-> blitter height)
			bra		db_calc_x

db_calc_y_pl		move.w		d7,d2			; bitmap height to d2
			sub.w		d0,d2			; bitmap height - bob y position
			move.w		d2,d0			; ... to d0
			clr.l		d2			; no offset (-> offset)
			cmp.w		bob_Height(a0),d0	; bob out of screen at bottom?
			bmi		db_calc_x
			move.w		bob_Height(a0),d0	; bob height to d0 (-> blitter height)
			
db_calc_x		tst.w		d1			; bob x position > 0 ?
			bpl		db_calc_x_pl
			neg.w		d1			; absolute value of bob x position
			move.w		d1,d4
			lsr.w		#4,d1			; absolute value of bob x in words
			move.w		bob_Width(a0),d5	; bob width to d5
			sub.w		d1,d5			; bob width - bob x position (words)
			clr.l		d3
			move.w		d1,d3
			lsl.w		#1,d3			; absolute value of bob x in bytes
			add.l		d3,d2			; number of bytes out of screen
			move.w		d5,d1
			and.w		#$000f,d4		; out of screen pixel on word boundary? 
			clr.w		d5
			tst.w		d4
			beq		db_calc_mask
			move.w		#16,d5
			sub.w		d4,d5
			subq.w		#1,d4
			move.w		#$ffff,d3		; calculate FWM
db_calc_shift1		lsr.w		#1,d3
			dbra		d4,db_calc_shift1	; loop for each pixel off from word boundary
			move.w		d3,BLTAFWM(a2)
			move.w		d3,BLTALWM(a2)		; FWM = LWM if width = 1 word
			cmp.w		#1,d1
			beq		db_calc_end1
			move.w		#$ffff,BLTALWM(a2)	; if width > 1 word
db_calc_end1		move.l		#2,d4			; bitmap offset: -2 bytes
			rts
			
db_calc_x_pl		move.w		d1,d5
			and.w		#$000f,d5		; blitter shift
			lsr.w		#4,d1			; word x position
			move.w		d6,d4			; bitmap width in words
			sub.w		d1,d4
			move.w		d4,d1
			cmp.w		bob_Width(a0),d1
			bmi		db_calc_neg		; bob out of screen
			move.w		bob_Width(a0),d1	; bob width to d1 (-> blitter width)
			
db_calc_mask		move.l		#$ffffffff,BLTAFWM(a2)	; full blitter mask
			clr.l		d4
			rts
			
db_calc_neg		tst.w		d5
			beq		db_calc_mask
			move.w		d5,d3
			subq.w		#1,d3
			move.w		#$ffff,d4		; calculate FWM
db_calc_shift2		lsl.w		#1,d4
			dbra		d3,db_calc_shift2	; loop for each pixel off from word boundary
			move.w		d4,BLTAFWM(a2)
			move.w		d4,BLTALWM(a2)		; FWM = LWM if width = 1 word
			cmp.w		#1,d1
			beq		db_calc_end2
			move.w		#$ffff,BLTAFWM(a2)	; if width > 1 word
db_calc_end2		clr.l		d4
			rts

; --------
		
db_writebob		move.w		bob_Y(a0),d0
			move.w		bob_X(a0),d1
			bsr		db_calc_blitter		; calculate blitter values
			lsl.w		#8,d5
			lsl.w		#4,d5
			move.w		d5,BLTCON1(a2)		; shift for source B
			add.w		#$0fca,d5		; use A,B,C,D / minterm: B + (!A)C
								; => write bob pixel + background pixel where shadowmask is transparent
			move.l		d5,blitterdata
			move.w		d5,BLTCON0(a2)		; shift for source A
			move.l		bob_ShadowMask(a0),a5
			add.l		d2,a5			; +offset bytes
			move.l		bob_Image(a0),a3
			add.l		d2,a3			; +offset bytes
			lea		bm_Planes0(a1),a4
			move.w		d6,d5			; bitmap width in words
			sub.w		d1,d5			; minus blitter width in words
			lsl.w		#1,d5			; modulus in bytes
			move.w		d5,BLTDMOD(a2)		; set modulus for destination D
			move.w		d5,BLTCMOD(a2)		; set modulus for source C
			move.w		bob_Width(a0),d5
			sub.w		d1,d5			; bob width - blitter width
			lsl.w		#1,d5			; ... in bytes
			move.w		d5,BLTAMOD(a2)		; set modulus for A
			move.w		d5,BLTBMOD(a2)		; set modulus for B
			clr.l		d3
			tst.w		bob_X(a0)
			bmi		db_write_negx
			move.w		bob_X(a0),d3
			lsr.w		#4,d3
			lsl.w		#1,d3			; offset bytes of bob in x direction
db_write_negx		tst.w		bob_Y(a0)
			bmi		db_write_negy
			move.w		bob_Y(a0),d5
			mulu		d6,d5
			lsl.l		#1,d5			; offset bytes of bob in y direction
			add.l		d5,d3			; total offset bytes of bob in bitmap
db_write_negy		sub.l		d4,d3			; -possible bitmap offset
			move.w		bob_Width(a0),d4
			mulu		bob_Height(a0),d4
			lsl.l		#1,d4			; total bob size in bytes (bob bitplane)
			move.w		bob_Depth(a0),d5	; number of bitplanes
			subq.w		#1,d5
			and.w		#$03ff,d0		; for special case : height = 1024
			and.w		#$003f,d1		; for special case : width = 64
			lsl.w		#6,d0			; shift height to bits 15-6
			add.w		d0,d1			; d1 now ready for BLTSIZE
db_write_loop		move.l		a5,BLTAPTH(a2)		; address of shadow mask (source A)
			move.l		a3,BLTBPTH(a2)		; address of bob (source B)
			move.l		(a4)+,a6		; current background bitplane
			add.l		d3,a6			; add offset bytes of bob
			move.l		a6,BLTCPTH(a2)		; address of bitplane (source C)
			move.l		a6,BLTDPTH(a2)		; address of bitplane (destination D)
			move.w		d1,BLTSIZE(a2)		; start blitter operation
db_write_wait		btst		#$0e,DMACONR(a2)
			bne		db_write_wait
			add.l		d4,a3			; next bob bitplane
			dbra		d5,db_write_loop	; loop through all bitplanes

			move.w		bob_Depth(a0),d0
			cmp.b		bm_Depth(a1),d0
			beq		db_write_end		; are bob and bitmap depth the same?
			sub.b		bm_Depth(a1),d0
			neg.b		d0			; remaining bitmap planes
			subq.w		#1,d0
			move.l		blitterdata,d5		; use A,C,D for BLTCON0 / minterm: (!A)C
			sub.w		#$0400,d5
			move.w		d5,BLTCON0(a2)
			clr.w		BLTCON1(a2)		; clear BLTCON1
			clr.w		BLTBDAT(a2)		; no bob data anymore
db_write_loop2		move.l		a5,BLTAPTH(a2)		; address of shadow mask (source A)
			move.l		(a4)+,a6		; current background bitplane
			add.l		d3,a6			; add offset bytes of bob
			move.l		a6,BLTCPTH(a2)		; address of bitplane (source C)
			move.l		a6,BLTDPTH(a2)		; address of bitplane (destination D)
			move.w		d1,BLTSIZE(a2)		; start blitter operation
db_write_wait2		btst		#$0e,DMACONR(a2)
			bne		db_write_wait2
			dbra		d0,db_write_loop2	; loop through remaining bitplanes
db_write_end		rts

;
; initbobs
;
initbobs		lea		allbobs,a1		; all bobs list
			lea		allbobsinit,a2		; bobs init list
			lea		velocity,a3		; bobs velocity list
			lea		allbobsimg,a4		; bobs initial imaga list
			clr.l		d0
			move.w		(a1)+,d0		; number of bobs
			tst.w		d0
			beq		initbobsend
			clr.l		d1
			move.w		d0,d1
			lsl.w		#2,d1			; number of bobs * 4
			add.l		d1,a4			; pointer after last bob in allbobimg list
			add.l		d1,a1			; pointer after last bob in allbobs list
			add.l		d1,a3			; pointer after last bob in velocity list
			add.l		d1,a2			; pointer after last bob in bobinit list
			subq.w		#1,d0			; number of bobs - 1 for loop
			
initbobsloop		move.l		-(a4),a5		; bob image structure
			move.l		-(a1),a0		; bob structure address (from allbobs)
			move.w		img_depth(a5),bob_Depth(a0)	; bob bitplanes (from image structure)
			move.w		img_width(a5),bob_Width(a0)
			move.w		img_height(a5),bob_Height(a0)
			move.w		-(a2),bob_X(a0)		; bob init values (from allbobsinit)
			move.w		-(a2),bob_Y(a0)

			clr.w		bob_OldX(a0)
			clr.w		bob_OldY(a0)
			move.w		#1,bob_State(a0)	; set state to visible
			
			move.l		imageptr,d1
			move.l		img_offset(a5),d2	; img offset
			add.l		d2,d1	
			move.l		d1,bob_Image(a0)
			move.w		-(a3),bob_vx(a0)
			move.w		-(a3),bob_vy(a0)
			
			dbra		d0,initbobsloop
			
initbobsend		rts
;
; allocmasks
;
allocmasks		lea		allimages,a3
			move.w		(a3)+,d2		; number of images
			tst.w		d2
			beq		allocmasksend
			subq		#1,d2
allocmaskloop		clr.l		d0
			move.w		(a3)+,d3		; height
			move.w		(a3)+,d0		; width in words
			move.w		(a3)+,d1		; depth (not needed)
			move.l		(a3)+,d1		; image offset (not needed)

			lsl.w		#1,d0			; width in bytes
			mulu		d3,d0			; width * height
			move.l		_SysBase,a6
			move.l		#MEMF_CHIP!MEMF_CLEAR,d1
			jsr		AllocMem(a6)
			move.l		d0,(a3)+		; shadowmask pointer
			
allocmasknext		dbra		d2,allocmaskloop	; loop through all images in list
						
allocmasksend		rts
;
; initmasks
;
initmasks		bsr		allocmasks

			lea		allbobs,a3
			lea		allbobsimg,a2
			move.w		(a3)+,d2		; number of bobs
			tst.w		d2
			beq		initmasksend
			subq		#1,d2
initmaskloop		clr.l		d0
			move.l		(a3)+,a0		; bob structure
			move.l		(a2)+,a1		; bob image structure
			move.l		img_shadow(a1),d0	; shadowmask pointer
			move.l		d0,bob_ShadowMask(a0)	; set shadowmask pointer in bob structure
			move.l		d0,bob_CollMask(a0)	; ShadowMask = CollMask

			dbra		d2,initmaskloop

			move.l		_GfxBase,a6
			jsr		OwnBlitter(a6)

			bsr		initshadowmasks		; init shadowmasks for all images

			move.l		_GfxBase,a6
			jsr		DisownBlitter(a6)
			
initmasksend		rts
;
; joy
;
joy			moveq		#1,d0
			move.l		_JoyportBase,d1
			tst.l		d1
			beq		joyend			; no joyport library
			move.l		d1,a6
			jsr		getjoyport(a6)
			
			btst		#28,d0			; mouse or unknown?
			beq		joyend
			
			moveq		#0,d2
			btst		#JPB_BUTTON_RED,d0	; FIRE
			beq		joytestUDLR

			moveq		#1,d2
			
joytestUDLR		btst		#JPB_JOY_RIGHT,d0	; RIGHT
			bne		moveright
			
			btst		#JPB_JOY_LEFT,d0	; LEFT
			bne		moveleft
			
joytest2		btst		#JPB_JOY_DOWN,d0	; DOWN
			bne		movedown
			
			btst		#JPB_JOY_UP,d0		; UP
			bne		moveup

joyend			rts

movedown		lea		bob1,a0
			move.w		bob_Y(a0),d0
			add.w		move_vy,d0
			tst.w		d2
			beq		mvdn
			add.w		move_vy,d0
mvdn			cmp.w		#240,d0
			blt		mvbobdn
			move.w		#240,d0
mvbobdn			move.w		d0,bob_Y(a0)
movedownend		rts
			
moveup			lea		bob1,a0
			move.w		bob_Y(a0),d0
			sub.w		move_vy,d0
			tst.w		d2
			beq		mvup
			sub.w		move_vy,d0
mvup			cmp.w		#0,d0
			bgt		mvbobup
			move.w		#0,d0
mvbobup			move.w		d0,bob_Y(a0)
moveupend		rts

moveleft		move.l		d0,-(sp)
			lea		bob1,a0
			move.w		bob_X(a0),d0
			sub.w		move_vx,d0
			tst.w		d2
			beq		mvlt
			sub.w		move_vx,d0
mvlt			cmp.w		#0,d0
			bgt		mvboblt
			move.w		#0,d0
mvboblt			move.w		d0,bob_X(a0)
moveleftend		move.l		(sp)+,d0
			bra		joytest2

moveright		move.l		d0,-(sp)
			lea		bob1,a0
			move.w		bob_X(a0),d0
			add.w		move_vx,d0
			tst.w		d2
			beq		mvrt
			add.w		move_vx,d0
mvrt			cmp.w		#304,d0
			blt		mvbobrt
			move.l		#304,d0
mvbobrt			move.w		d0,bob_X(a0)
moverightend		move.l		(sp)+,d0
			bra		joytest2

;
; freemasks
;
freemasks		lea		allimages,a3
			move.w		(a3)+,d2		; number of images
			tst.w		d2
			beq		freemasksend
			subq		#1,d2
freemaskloop		clr.l		d0
			move.w		(a3)+,d3		; height
			move.w		(a3)+,d0		; width in words
			move.w		(a3)+,d1		; depth (not needed)
			move.l		(a3)+,d1		; img offset (not needed)
			move.l		(a3)+,a2		; shadowmask pointer

			lsl.w		#1,d0			; width in bytes
			mulu		d3,d0			; width * height
			move.l		_SysBase,a6
			move.l		a2,a1
			jsr		FreeMem(a6)		; free allocated memory
			
freemasknext		dbra		d2,freemaskloop		; loop through all images in list
						
freemasksend		rts

;
; initshadowmasks
;
initshadowmasks		move.l		imageptr,a2
			lea		allimages,a0
			move.l		a0,a1
			move.w		(a1)+,d0		; number of images
			tst.w		d0
			beq		initshadowend
			clr.l		d1
			move.w		d0,d1
			lsl.w		#4,d1
			sub.w		d0,d1
			sub.w		d0,d1			; number of images * 14
			add.l		d1,a1			; pointer to last image in list
			subq.w		#1,d0			; number of images - 1 for loop
initshadowloop		move.l		-(a1),a3		; shadowmask pointer
			move.l		-(a1),a4		; image offset
			move.w		-(a1),d1		; depth
			move.w		-(a1),d2		; width
			move.w		-(a1),d3		; height
			
			movem.l		d0-d3/a1-a3,-(sp)
			move.l		a2,a0
			add.l		a4,a0			; imageptr + offset
			move.l		a3,a1			; shadowmask pointer
			bsr		calcshadowmask
			movem.l		(sp)+,d0-d3/a1-a3
			
initshadownext		dbra		d0,initshadowloop	; loop through all images in list
initshadowend		rts
;
; initbitmaps
;
initbitmaps		clr.l		d0
			move.w		bmp_width,d0		; width in pixel
			lsr.w		#3,d0			; width in bytes

			moveq		#2,d5			; bitmap counter (3 bitmap structures)
			lea		dbuf_table,a2		; pointer to bitmap list			
setbmpdataloop		move.l		(a2)+,a0		; select bitmap structure
			move.w		d0,bm_BytesPerRow(a0)	; width 
			move.w 		bmp_height,bm_Rows(a0)	; height
			move.b		bmp_depth,bm_Depth(a0)	; depth
			dbra		d5,setbmpdataloop
			
			move.w		bmp_height,d3
			mulu		d0,d3			; bitplane size
			clr.l		d2
			move.b		bmp_depth,d2
			mulu		d3,d2			; total bitmap size
			
			moveq		#2,d5			; bitmap counter (3 bitmap structures)
			lea		dbuf_table,a2		; pointer to bitmap list
allocbmploop		move.l		_SysBase,a6
			move.l		d2,d0			; bitmap size
			move.l		#MEMF_CHIP!MEMF_CLEAR,d1
			jsr		AllocMem(a6)		; allocate chip ram
			move.l		(a2)+,a0		; select bitmap structure
			add.l		#bm_Planes0,a0
			moveq		#5,d4			; bitplane counter
planeloop		move.l		d0,(a0)+		; address of n-th plane
			add.l		d3,d0			; + bitplane size
			dbra		d4,planeloop		; loop through all planes
			dbra		d5,allocbmploop		; loop through all 3 bitmap structures
			
			move.l		_GfxBase,a6
			jsr		OwnBlitter(a6)
			
			
			moveq		#2,d5			; bitmap counter (3 bitmap structures)
			lea		dbuf_table,a2		; pointer to bitmap list
			
fillbmploop		move.l		(a2)+,a0		; select bitmap

			clr.l		d4
			clr.l		d3
			move.b		bmp_depth,d4
			subq		#1,d4
			lea		fp0,a1			; pointer to fill pattern list
			
fillplaneloop		addq		#1,d3
			move.w		(a1)+,d0		; get fill pattern
			move.l		d3,d1			; select bitplane
			movem.l		d3-d5/a0-a2,-(sp)
			bsr		bitmapfill		; fill bitplane with pattern
			movem.l		(sp)+,d3-d5/a0-a2

nextplaneloop		dbra		d4,fillplaneloop

			dbra		d5,fillbmploop
			
			move.l		_GfxBase,a6
			jsr		DisownBlitter(a6)
			
initbitmapsend		rts
;
; initimages
;
initimages		move.l		_SysBase,a6
			move.l		sizeofimgdata,d0
			move.l		#MEMF_CHIP!MEMF_CLEAR,d1
			jsr		AllocMem(a6)		; allocate chip ram			
			move.l		d0,imageptr

			move.l		sizeofimgdata,d0
			lsr.l		#2,d0			; image memory size in longs
			lea		imagedata,a0
			move.l		imageptr,a1
copyimage		move.l		(a0)+,(a1)+
			dbra		d0,copyimage
			
			rts
;
; buildcopperlist
;
buildcopperlist		move.l		_SysBase,a6
			move.l		sizeofcopper,d0		; bytes in copper list
			move.l		#MEMF_CHIP!MEMF_CLEAR,d1
			jsr		AllocMem(a6)		; allocate chip ram			
			move.l		d0,coplistptr
			
			lea		colortable,a0
			lea		colorbuffer,a1
			bsr		initcolor		; initialize color registers

			move.l		dbuf_table,a0		; pointer to background bitmap structure
			lea		copperbuffer,a1
			bsr		initcopper		; create copperlist for current background bitmap
			
			move.l		sizeofcopper,d0
			lsr.l		#2,d0
			subq.l		#1,d0			; length of copper list in longs - 1
			lea		copperlist,a0
			move.l		coplistptr,a1
copycopper		move.l		(a0)+,(a1)+		; move copper list to allocated ram
			dbra		d0,copycopper
			
			move.l		#_custom,a6
			move.w		#$0080,DMACON(a6)	; disable copper DMA
			move.l		coplistptr,COP1LCH(a6)	; set address of copper list
			clr.w		COPJMP1(a6)		; start copper program
			move.w		#$8080,DMACON(a6)	; enable copper DMA
			
			rts
;
; restorecopper
;
restorecopper		move.l		_GfxBase,a0
			move.l		gb_copinit(a0),COP1LCH(a6)
			clr.w		COPJMP1(a6)
			
			move.w		#$8220,DMACON(a6)	; restore old copper list

			rts
;
; cycle
;
cylce			move.l		cycletimer,d0
			addq		#1,d0
			move.l		d0,cycletimer
			cmp		#4,d0			; cycle effect occurs on every 4th interrupt
			bne		cycleend
			clr.l		cycletimer
			move.l		coplistptr,a1
			add.l		#16,a1			; pointer to colorbuffer in copper list
			add.l		#18,a1			; COLOR04 value
			move.w		(a1),d0			; cycle COLOR04...COLOR11
			move.w		4(a1),(a1)
			move.w		8(a1),4(a1)
			move.w		12(a1),8(a1)
			move.w		16(a1),12(a1)
			move.w		20(a1),16(a1)
			move.w		24(a1),20(a1)
			move.w		28(a1),24(a1)
			move.w		d0,28(a1)
cycleend		rts		
;
; openlibs
;
openlibs		move.l		_SysBase,a6
			lea 		gfxname,a1		; graphics.library
			clr.l 		d0
			jsr		OpenLibrary(a6)
			move.l 		d0,_GfxBase
			
			move.l		_SysBase,a6
			lea		joyportname,a1		; joyport.library
			clr.l		d0
			jsr		OpenLibrary(a6)
			move.l		d0,_JoyportBase
			
			rts
;
; closelibs
;
closelibs		move.l		_JoyportBase,a1
			cmp.l		#0,a1
			beq		closegfx
			move.l		_SysBase,a6
			jsr		CloseLibrary(a6)
			
			move.l		_JoyportBase,a1
			move.l		_SysBase,a6
			jsr		RemLibrary(a6)
			
closegfx		move.l		_GfxBase,a1
			move.l		_SysBase,a6
			jsr		CloseLibrary(a6)
			
			rts
;
; freeall
;
freeall			move.w		bmp_width,d1		; width in pixel
			lsr.w		#3,d1			; width in bytes
			move.w		bmp_height,d3
			mulu		d3,d1			; bitplane size
			clr.l		d2
			move.b		bmp_depth,d2
			mulu		d1,d2			; total bitmap size
			
			moveq		#2,d3			; loop counter
			lea		dbuf_table,a2
freebmploop		move.l		(a2)+,a0
			move.l		bm_Planes0(a0),a1
			move.l		d2,d0
			move.l		_SysBase,a6
			jsr		FreeMem(a6)		; free allocated memory for bitmaps
			dbra		d3,freebmploop

			bsr		freemasks		; #freebob
			
			move.l		coplistptr,a1
			move.l		sizeofcopper,d0
			jsr		FreeMem(a6)		; free allocated memory for copper list
			
			move.l		imageptr,a1
			move.l		sizeofimgdata,d0
			jsr		FreeMem(a6)		; free allocated memory for bob images
			
			rts
;
; waitbutton
;
waitbutton		move.l		#_custom,a6
waitdown		btst.b		#$0a,POTINP(a6)
			bne		waitdown
waitrelease		btst.b		#$0a,POTINP(a6)
			beq		waitrelease
			rts

; --- DATA ---
gfxname			dc.b 		"graphics.library",0
joyportname		dc.b		"joyport.library",0
			cnop		0,2
			
_GfxBase		dc.l 		0
_JoyportBase		dc.l		0

level3int		dc.l		0
intena_old		dc.w		0

bmp_width		dc.w		320
bmp_height		dc.w		256
bmp_depth		dc.b		4
			cnop		0,2

fp0			dc.w		%0101010110101010	; fill pattern bitplane0
fp1			dc.w		%0011001111001100	; fill pattern bitplane1
fp2			dc.w		%1111000000001111	; fill pattern bitplane2
fp3			dc.w		%0000111111110000	; fill pattern bitplane3
fp4			dc.w		0
fp5			dc.w		0

move_vx			dc.w		2
move_vy			dc.w		2

bmp_swap		dc.w		0

bitmap1A		dcb.b		$1e,0
bitmap1B		dcb.b		$1e,0
bitmap1C		dcb.b		$1e,0

fire			dc.w		0
blitterdata		dc.l		0
; ----

allimages		dc.w		4			; total images

img1			dc.w		16,2,2			; image1 - height,width,depth
			dc.l		imagedata-imagedata,0	; image1 - image offset, shadowmask pointer
			
img2			dc.w		16,2,2			; image2 - height,width,depth
			dc.l		imagedata2-imagedata,0	; image2 - image offset, shadowmask pointer
			
img3			dc.w		16,2,2			; image3 - height,width,depth
			dc.l		imagedata3-imagedata,0	; image3 - image offset, shadowmask pointer
			
img4			dc.w		16,2,2			; image4 - height,width,depth
			dc.l		imagedata4-imagedata,0	; image4 - image offset, shadowmask pointer

; ----
			
bob1			dcb.b		$20,0
			dc.l		anim2
bob2			dcb.b		$20,0
			dc.l		anim1
bob3			dcb.b		$20,0
			dc.l		anim1
bob4			dcb.b		$20,0
			dc.l		anim1
bob5			dcb.b		$20,0
			dc.l		anim1
bob6			dcb.b		$20,0
			dc.l		anim1
bob7			dcb.b		$20,0
			dc.l		anim1
bob8			dcb.b		$20,0
			dc.l		anim1
bob9			dcb.b		$20,0
			dc.l		anim1
; ----
			
allbobs			dc.w		9			; number of bobs
			dc.l		bob1
			dc.l		bob2
			dc.l		bob3
			dc.l		bob4
			dc.l		bob5
			dc.l		bob6
			dc.l		bob7
			dc.l		bob8
			dc.l		bob9

allbobsinit		dc.w		0,0			; bob1 y0,x0
			dc.w		0,30			; bob2
			dc.w		0,60			; bob3
			dc.w		0,90			; bob4
			dc.w		0,120			; bob5
			dc.w		0,150			; bob6
			dc.w		0,180			; bob7
			dc.w		0,210			; bob8
			dc.w		0,240			; bob9

allbobsimg		dc.l		img4			; bob1 initial image structure
			dc.l		img1			; bob2
			dc.l		img1			; bob3
			dc.l		img1			; bob4
			dc.l		img1			; bob5
			dc.l		img1			; bob6
			dc.l		img1			; bob7
			dc.l		img1			; bob8
			dc.l		img1			; bob9

; ----
	
velocity		dc.w		0			; y velocity bob1
			dc.w		0			; x velocity bob1
			dc.w		1			; bob2
			dc.w		1
			dc.w		-2			; bob3
			dc.w		2
			dc.w		2			; bob4
			dc.w		-3
			dc.w		3			; bob5
			dc.w		3
			dc.w		-2			; bob6
			dc.w		3
			dc.w		-1			; bob7
			dc.w		3
			dc.w		2			; bob8
			dc.w		2
			dc.w		4			; bob9
			dc.w		2
; ----

allanims		dc.w		2			; number of animation structures
			dc.l		anim1
			dc.l		anim2
			
; ----

anim1			dc.w		4			; number of images
			dc.w		0			; counter
			dc.l		img1
			dc.l		img2
			dc.l		img3
			dc.l		img2
			
anim2			dc.w		0			; no animation
			dc.w		0
			
; ----
			
animtimer		dc.l 		0
cycletimer		dc.l		0
			
dbuf_table		dc.l		bitmap1A		; $00 - dbuf_visible
			dc.l		bitmap1B		; $04 - dbuf_invisible
			dc.l		bitmap1C		; $08 - dbuf_background

colortable		dc.w		$0000,$0555,$0aaa,$0fff
			dc.w		$0300,$0301,$0311,$0312
			dc.w		$0322,$0323,$0333,$0334
			dc.w		$0000,$0000,$0000,$0000
			dcb.w		16,0

sprites			dcb.l		3,0

coplistptr		dc.l		0
imageptr		dc.l		0

sizeofcopper		dc.l		copperend-copperlist
; -----------------------------------------
; copperlist
; -----------------------------------------
copperlist		dc.w		$008e,$2c81		; MOVE $2c81,DIWSTRT
			dc.w		$0090,$2cc1		; MOVE $2cc1,DIWSTOP
			dc.w		$0092,$0038		; MOVE $0038,DDFSTRT
			dc.w		$0094,$00d0		; MOVE $00d0,DDFSTOP

colorbuffer		dcb.l		32,0			; MOVE color value,COLORxx

copperbuffer		dcb.l		15,$fffffffe
			dc.w		$ff01,$fffe		; WAIT for V = 255 / H = 0
			dc.w		$3a01,$fffe		; WAIT for V = 314 / H = 0
			dc.w		$009c,$8010		; MOVE $8010,INTREQ	<-- enable COPER interrupt		
			dc.w		$ffff,$fffe		; end of copper list
			dc.w		$ffff,$fffe		; end of copper list
			dc.w		$ffff,$fffe		; end of copper list
			dc.w		$ffff,$fffe		; end of copper list

copperend		dc.l			0
; -----------------------------------------
; copperlist end
; -----------------------------------------

sizeofimgdata		dc.l		imagedataend-imagedata
; --------------------------------------------------
; bob image data
; --------------------------------------------------
			
imagedata		dc.w 		%0000000110000000,$0
			dc.w 		%0000001001000000,$0
			dc.w 		%0000010000100000,$0
			dc.w 		%0000111111110000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%1111111111111111,$0
			dc.w 		%1111111111111111,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000111111110000,$0
			dc.w 		%0000010000100000,$0
			dc.w 		%0000001001000000,$0
			dc.w 		%0000000110000000,$0
			
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000001111000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000001111000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000000000000,$0
			
imagedata2		dc.w 		%0000000110000000,$0
			dc.w 		%0000001001000000,$0
			dc.w 		%0000001001000000,$0
			dc.w 		%0000011111100000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0001000110001000,$0
			dc.w 		%0111000110001110,$0
			dc.w 		%1001111111111001,$0
			dc.w 		%1001111111111001,$0
			dc.w 		%0111000110001110,$0
			dc.w 		%0001000110001000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000011111100000,$0
			dc.w 		%0000001001000000,$0
			dc.w 		%0000001001000000,$0
			dc.w 		%0000000110000000,$0
			
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0110000110000110,$0
			dc.w 		%0110000110000110,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000000000000,$0

imagedata3		dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0001000110001000,$0
			dc.w 		%0011000110001100,$0
			dc.w 		%0101000110001010,$0
			dc.w 		%1001111111111001,$0
			dc.w 		%1001111111111001,$0
			dc.w 		%0101000110001010,$0
			dc.w 		%0011000110001100,$0
			dc.w 		%0001000110001000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			dc.w 		%0000000110000000,$0
			
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0010000000000100,$0
			dc.w 		%0110000110000110,$0
			dc.w 		%0110000110000110,$0
			dc.w 		%0010000000000100,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			dc.w 		%0000000000000000,$0
			
imagedata4		dc.w 		%0011111111111100,$0
			dc.w 		%0100000000000010,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%0100000000000010,$0
			dc.w 		%0011111111111100,$0
			
			dc.w 		%0011111111111100,$0
			dc.w 		%0100000000000010,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%1000000000000001,$0
			dc.w 		%0100000000000010,$0
			dc.w 		%0011111111111100,$0

imagedataend		dc.l		0

