VermilionDock_Script:
	call EnableAutoTextBoxDrawing
	CheckEventHL EVENT_STARTED_WALKING_OUT_OF_DOCK
	jr nz, .asm_1db8d
	CheckEventReuseHL EVENT_GOT_HM01
	ret z
	ld a, [wDestinationWarpID]
	cp $1
	ret nz
	CheckEventReuseHL EVENT_SS_ANNE_LEFT
	jp z, VermilionDock_1db9b
	SetEventReuseHL EVENT_STARTED_WALKING_OUT_OF_DOCK
	call Delay3
	ld hl, wd730
	set 7, [hl]
	ld hl, wSimulatedJoypadStatesEnd
	ld a, D_UP
	ld [hli], a
	ld [hli], a
	ld [hl], a
	ld a, $3
	ld [wSimulatedJoypadStatesIndex], a
	xor a
	ld [wSpriteStateData2 + $06], a
	ld [wOverrideSimulatedJoypadStatesMask], a
	dec a
	ld [wJoyIgnore], a
	ret
.asm_1db8d
    bit 5, [hl]
    jr nz, .afterss
    ld a, [wSimulatedJoypadStatesIndex]
    and a
    ret nz
    ld [wJoyIgnore], a
    set 5, [hl]
    ret
.afterss
    ld bc, $105
    call GetOWCoord
    ld de, AfterSSTable
    ld bc, $204
    ld a, [de]
    cp [hl]
    jr z, .truckCheck
.truckCheck
    bit MEWINTRUCK, a ; is mew in the truck?
    jp nz, ChangeTruckTile
    ld c, HS_MEW_VERMILLION_DOCK
    ld b, $2
    ld hl, wMissableObjectFlags
    predef FlagActionPredef
    ld a, c
    and a
    jr nz, .skiphidingmew
    ld a, HS_MEW_VERMILLION_DOCK
    ld [wMissableObjectIndex], a
    predef HideObject
.skiphidingmew
    ld a, [wd728]
    bit 0, a ; using Strength?
    ret z
    ; the position for moving truck is $00, $15
    ld hl, wYCoord
    ld a, [hli]
    and a
    ret nz
    ld a, [hl]
    cp $16
    ret nz
    ; if the player is trying to walk left
    ld a, [wPlayerMovingDirection]
    cp 2
    ret nz
   
    xor a
    ld [$ff8c], a
    ld a, $8
    ld [$ff8d], a
    call SetSpriteFacingDirection
    ld a, $ff
    ld [wJoyIgnore], a
    ld [wUpdateSpritesEnabled], a
    xor a
    ld bc, $4c48
    ld de, RedLeftOAMTable
    call WriteOAMBlock
    ld bc, (Bank(TruckSpriteGFX) << 8) | 8
    ld hl, vChars1 + $400
    ld de, TruckSpriteGFX
    call CopyVideoData
    ld hl, TruckOAMTable
    ld bc, $20
    ld de, wOAMBuffer + $20
    call CopyData
    ld a, $c
    ld [wNewTileBlockID], a
    ld bc, $a
    predef ReplaceTileBlock
    ; moving the truck
    ld a, SFX_PUSH_BOULDER
    call PlaySound
    ld b, 32
    ld de, 4
.movingtruck
    ld hl, wOAMBuffer + $21
    ld a, 8
.movingtruck2
    dec [hl]
    add hl, de
    dec a
    jr nz, .movingtruck2
    ld c, 2
    call DelayFrames
    dec b
    jr nz, .movingtruck
    ld a, $3
    ld [wNewTileBlockID], a
    ld bc, $9
    predef ReplaceTileBlock
    ; show mew and print its dialogue
    ld a, 1
    ld [wUpdateSpritesEnabled], a
    ld a, HS_MEW_VERMILLION_DOCK
    ld [wMissableObjectIndex], a
    predef ShowObject
    ld c, 60
    call DelayFrames
    xor a
    ld [wJoyIgnore], a
    set MEWINTRUCK, [hl]
    ret
   
ChangeTruckTile:
    ld bc, $9
    call GetOWCoord
    ld a, [hl]
    cp $3
    ret z
    ld a, $3
    ld [hli], a
    ld a, $c
    ld [hl], a
    jpba RedrawMapView
   
GetOWCoord:
    ld hl, wOverworldMap + 2
    ld a, [wCurMapWidth]
    add $6
    ld e, a
    ld d, $0
    add hl, de
    add hl, de
    inc b
    inc c
.bloop
    add hl, de
    dec b
    jr nz, .bloop
.cloop
    inc hl
    dec c
    jr nz, .cloop
    ret

VermilionDock_1db9b:
	SetEventForceReuseHL EVENT_SS_ANNE_LEFT
	ld a, $ff
	ld [wJoyIgnore], a
	ld [wNewSoundID], a
	call PlaySound
	ld c, BANK(Music_Surfing)
	ld a, MUSIC_SURFING
	call PlayMusic
	callba LoadSmokeTileFourTimes
	xor a
	ld [wSpriteStateData1 + 2], a
	ld c, 120
	call DelayFrames
	ld b, $9c
	call CopyScreenTileBufferToVRAM
	coord hl, 0, 10
	ld bc, SCREEN_WIDTH * 6
	ld a, $14 ; water tile
	call FillMemory
	ld a, 1
	ld [H_AUTOBGTRANSFERENABLED], a
	call Delay3
	xor a
	ld [H_AUTOBGTRANSFERENABLED], a
	ld [wSSAnneSmokeDriftAmount], a
	ld [rOBP1], a
	ld a, 88
	ld [wSSAnneSmokeX], a
	ld hl, wMapViewVRAMPointer
	ld c, [hl]
	inc hl
	ld b, [hl]
	push bc
	push hl
	ld a, SFX_SS_ANNE_HORN
	call PlaySoundWaitForCurrent
	ld a, $ff
	ld [wUpdateSpritesEnabled], a
	ld d, $0
	ld e, $8
.asm_1dbfa
	ld hl, $0002
	add hl, bc
	ld a, l
	ld [wMapViewVRAMPointer], a
	ld a, h
	ld [wMapViewVRAMPointer + 1], a
	push hl
	push de
	call ScheduleEastColumnRedraw
	call VermilionDock_EmitSmokePuff
	pop de
	ld b, $10
.asm_1dc11
	call VermilionDock_AnimSmokePuffDriftRight
	ld c, $8
.asm_1dc16
	call VermilionDock_1dc7c
	dec c
	jr nz, .asm_1dc16
	inc d
	dec b
	jr nz, .asm_1dc11
	pop bc
	dec e
	jr nz, .asm_1dbfa
	xor a
	ld [rWY], a
	ld [hWY], a
	call VermilionDock_EraseSSAnne
	ld a, $90
	ld [hWY], a
	ld a, $1
	ld [wUpdateSpritesEnabled], a
	pop hl
	pop bc
	ld [hl], b
	dec hl
	ld [hl], c
	call LoadPlayerSpriteGraphics
	ld hl, wNumberOfWarps
	dec [hl]
	ret

VermilionDock_AnimSmokePuffDriftRight:
	push bc
	push de
	ld hl, wOAMBuffer + $11
	ld a, [wSSAnneSmokeDriftAmount]
	swap a
	ld c, a
	ld de, 4
.loop
	inc [hl]
	inc [hl]
	add hl, de
	dec c
	jr nz, .loop
	pop de
	pop bc
	ret

VermilionDock_EmitSmokePuff:
; new smoke puff above the S.S. Anne's front smokestack
	ld a, [wSSAnneSmokeX]
	sub 16
	ld [wSSAnneSmokeX], a
	ld c, a
	ld b, 100 ; Y
	ld a, [wSSAnneSmokeDriftAmount]
	inc a
	ld [wSSAnneSmokeDriftAmount], a
	ld a, $1
	ld de, VermilionDockOAMBlock
	call WriteOAMBlock
	ret

VermilionDockOAMBlock:
	db $fc, $13
	db $fd, $13
	db $fe, $13
	db $ff, $13
	
AfterSSTable:
    db $1, $d, $15, $1
    db $d, $d,  $d, $d
   
TruckOAMTable:
    db $50, $28, $C0, $16 ; changed the atributte here to colorize the truck in pokered-gbc
    db $50, $30, $C1, $16
    db $50, $38, $C2, $16
    db $50, $40, $C3, $16
    db $58, $28, $C4, $16
    db $58, $30, $C5, $16
    db $58, $38, $C6, $16
    db $58, $40, $C7, $16
 
RedLeftOAMTable:
    db $8,$0,$9,$0
    db $a,$0,$b,$0

VermilionDock_1dc7c:
	ld h, d
	ld l, $50
	call .asm_1dc86
	ld h, $0
	ld l, $80
.asm_1dc86
	ld a, [rLY]
	cp l
	jr nz, .asm_1dc86
	ld a, h
	ld [rSCX], a
.asm_1dc8e
	ld a, [rLY]
	cp h
	jr z, .asm_1dc8e
	ret

VermilionDock_EraseSSAnne:
; Fill the area the S.S. Anne occupies in BG map 0 with water tiles.
; HAX: call another function to do this (also updates palettes).
	CALL_INDIRECT EraseSSAnneWithColor

; Padding to prevent data shifting
rept 17
	nop
endr

; Replace the blocks of the lower half of the ship with water blocks. This
; leaves the upper half alone, but that doesn't matter because replacing any of
; the blocks is unnecessary because the blocks the ship occupies are south of
; the player and won't be redrawn when the player automatically walks north and
; exits the map. This code could be removed without affecting anything.
	overworldMapCoord hl, 5, 2, VERMILION_DOCK_WIDTH
	ld a, $d ; water block
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hl], a

	ld a, SFX_SS_ANNE_HORN
	call PlaySound
	ld c, 120
	call DelayFrames
	ret

VermilionDock_ScriptPointers
    dw CheckFightingMapTrainers
    dw DisplayEnemyTrainerTextAndStartBattle
    dw EndTrainerBattle

VermilionDock_TextPointers:
	dw VermilionDockText1
	dw MewText

VermilionDockText1:
	TX_FAR _VermilionDockText1
	db "@"

MewTrainerHeader:
    dbEventFlagBit EVENT_BEAT_MEW
    db 0 ; view range
    dwEventFlagAddress EVENT_BEAT_MEW
    dw MewBattleText ; TextBeforeBattle
    dw MewBattleText ; TextAfterBattle
    dw MewBattleText ; TextEndBattle
    dw MewBattleText ; TextEndBattle
 
    db $ff
 
MewText:
    TX_ASM
    ld hl, MewTrainerHeader
    call TalkToTrainer
    jp TextScriptEnd
   
MewBattleText:
    TX_FAR _MewtwoBattleText
    TX_ASM
    ld a, MEW
    call PlayCry
    call WaitForSoundToFinish
    jp TextScriptEnd
