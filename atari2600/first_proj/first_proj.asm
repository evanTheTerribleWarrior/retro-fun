      	processor 6502

      	include vcs.h
      	include macro.h


;PAL = 1	;uncomment for PAL
	ifnconst PAL
	
SCANLINES = 96
        
        else
        
SCANLINES = 114

	endif
        
        seg.u Variables 	; uninitialised variable space
        org $80			; start of RAM
P0XPos        		byte         	; Player X-Position
P0YPos         		byte         	; Player Y-Position
P0SpritePtr    		word         	; Pointer to sprite lookup table
P0ColorPtr    		word    	; Pointer to color lookup table

MissileXPos		byte
MissileYPos		byte

TargetXPos		byte
TargetYPos		byte
TargetSpritePtr    	word        
TargetColorPtr     	word    

Random			byte

P0_HEIGHT = 8
TARGET_HEIGHT = 9

	seg Code
    	org $F000

Start:
	CLEAN_START

;;;; Initialise positions
    lda #60
    sta P0XPos              
    lda 10
    sta P0YPos      
    
    lda #60
    sta TargetXPos              
    lda #80
    sta TargetYPos      
    
    lda #%11010100
    sta Random               ; Random = $D4
    

;;;;; Initialise pointers
    lda #<P0Sprite
    sta P0SpritePtr        
    lda #>P0Sprite
    sta P0SpritePtr+1       

    lda #<P0Colour
    sta P0ColorPtr         
    lda #>P0Colour
    sta P0ColorPtr+1        
    lda #<TargetSprite
    sta TargetSpritePtr         
    lda #>TargetSprite
    sta TargetSpritePtr+1       

    lda #<TargetColour
    sta TargetColorPtr          
    lda #>TargetColour
    sta TargetColorPtr+1       


;;;;; Start Main Loop
MainLoop:	
	jsr VsyncVblank
        jsr SetPositions
        jsr Kernel
	jsr OverScan
        jmp MainLoop


;;;;; Set VSYNC/VBLANK
VsyncVblank:
	lda #0
        sta VBLANK	; Turn off VBLANK
	lda #2      
        sta VSYNC   	; Turn on VSYNC
        sta WSYNC   
        sta WSYNC   
        sta WSYNC  	; Waited for 3 VSYNC scanlines
        lda #0
        sta VSYNC   	; Turn off VSYNC
        
 	
        ;; Set a timer to operate within 37 VBLANK scanlines and get things done
        ;; just in time.
        ;; #44 --> 44 * 64 / 76 = 37 scanlines
        ldx #44    
        stx TIM64T
        
        rts 


;;;;; Set Positions
SetPositions:

	lda P0XPos
        ldy #0
        jsr SetObjectXPos        

        lda TargetXPos
        ldy #1
        jsr SetObjectXPos        

	lda MissileXPos
        ldy #2
        jsr SetObjectXPos
        
        sta WSYNC
        sta HMOVE  
        
        rts

;;;; Main Kernel
;;;; 1. Check if the timer we set before is still running
;;;; 2. When timer is done, we enter the visible part of the screen
Kernel:

	sta WSYNC     
        lda INTIM      
        bne Kernel     
        lda #0
        sta VBLANK

	;;; Set X = 96 to traverse all visible scanlines (192 / 2)
	ldx #SCANLINES
       
;;;; Start Drawing
KernelLoop:
	
.CheckIfDrawMissile:
	lda #0
	cpx MissileYPos
	bne .NoDraw
 	lda #%000000010
        inc MissileYPos
.NoDraw:
	sta ENAM0
        
.AreWeInsideP0Sprite:
    txa                     
    sec                     
    sbc P0YPos             
    cmp #P0_HEIGHT           
    bcc .DrawSpriteP0       
   
    lda #0                  
.DrawSpriteP0:
    clc                     
    tay                     
    lda (P0SpritePtr),Y     
    sta WSYNC                
    sta GRP0                 
    lda (P0ColorPtr),Y      
    sta COLUP0               

.AreWeInsideTargetSprite:
    txa                     
    sec                      
    sbc TargetYPos              
    cmp #TARGET_HEIGHT          
    bcc .DrawTarget      
    
    lda #0                 
.DrawTarget:
                         
    tay    
    lda #%00000101
    sta NUSIZ1 
    lda (TargetSpritePtr),Y   
    sta WSYNC               
    sta GRP1              
    lda (TargetColorPtr),Y  
    sta COLUP1              

    dex
    bne KernelLoop
    
    rts


;;; Start Overscan
OverScan:
        lda #2
        sta VBLANK      

        ldx #36  
        stx TIM64T


;;;; Check if user moved joystick or pressed buttons
CheckJoystickLeft:
	lda #%01000000
        bit SWCHA
        bne CheckJoystickRight
        dec P0XPos

CheckJoystickRight:
	lda #%10000000
        bit SWCHA
        bne CheckButtonPress
        inc P0XPos

CheckButtonPress:
	lda #%10000000
        bit INPT4
        bne EndCheck
        
        lda P0XPos      
        sta MissileXPos
        lda P0YPos
        sta MissileYPos

EndCheck:


;;;; Check Collisions

CheckMissileCollision:
	lda #%10000000
        bit CXM0P
        bne .CollisionM0
	jmp EndCollisionCheck
.CollisionM0:
	lda #0      
        sta MissileXPos
        sta MissileYPos
        jsr GetRandomTargetPos

EndCollisionCheck:
	sta CXCLR


;;;; Finish overscan
OverScanLoop:
	sta WSYNC      
        lda INTIM      
        bne OverScanLoop   
	rts
        

;;;; Subroutines to set positions of objects and getting random position of target

SetObjectXPos subroutine
    sta WSYNC               
    sec                     
.Div15Loop
    sbc #15                 
    bcs .Div15Loop           
    eor #7                   
    asl
    asl
    asl
    asl                      
    sta HMP0,Y              
    sta RESP0,Y              
    rts


GetRandomTargetPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random               
    lsr
    lsr                     
    sta TargetXPos          
    lda #30
    adc TargetXPos          
    sta TargetXPos          

    lda #80
    sta TargetYPos         

    rts


P0Sprite
        .byte #%00000000;--
        .byte #%01111110;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--
        .byte #%00000000;--

P0Colour
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;

TargetSprite
	.byte #%00000000;--
        .byte #%11111111;$42
        .byte #%10000001;$42
        .byte #%10100101;$42
        .byte #%10000001;$42
        .byte #%10100101;$42
        .byte #%10000001;$42
        .byte #%10000001;$42
        .byte #%11111111;$42

TargetColour
	.byte #$0E;
        .byte #$42;
        .byte #$42;
        .byte #$42;
        .byte #$42;
        .byte #$42;
        .byte #$42;
        .byte #$42;
        .byte #$42;





      	org $FFFC
      	.word Start       
      	.word Start       
