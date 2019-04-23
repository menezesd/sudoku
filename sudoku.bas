DECLARE SUB UPDATEBOARD ()
DEFINT A-Z
CONST RANK = 3 ' size of each box
CONST RANK2 = 9 ' size of sudoku grid
CONST EDITOR = "notepad" ' open solution steps in this editor or pager
CONST FILE = "steps.txt" ' write solution steps to this file
CONST NUMBERS = "123456789"
CONST TRUE = -1
CONST FALSE = 0

'indicates possible values for each cell
DIM SHARED BLOCK(1 TO RANK2, 1 TO RANK2, 1 TO RANK2)
' partially filled in sudoku board
DIM SHARED PART(1 TO RANK2, 1 TO RANK2)

OPEN FILE FOR OUTPUT AS #1
CLS

CALL INPUTBOARD
CALL INITBOARD
CALL UPDATEBOARD

DO
    CALL SOLVESTEP
    OLDSUM = SUM
    SUM = 0
    SOLVED = TRUE ' any blanks seen so far
    FOR I = 1 TO RANK2
        FOR J = 1 TO RANK2
            FOR K = 1 TO RANK2
                IF PART(I, J) = 0 THEN SOLVED = FALSE
                SUM = SUM + BLOCK(I, J, K)
            NEXT
        NEXT
    NEXT
LOOP UNTIL SOLVED OR SUM = OLDSUM

IF SOLVED THEN
    PRINT "Solution Found!"
    PRINT #1, "Solution Found!"
    EXIT DO
ELSEIF SUM = OLDSUM THEN ' no progress made
    PRINT "I seem to be stuck. Work so far:"
    EXIT DO
END IF

CALL PRINTSOLUTION


SUB SOLVEROW
    FOR ROW = 1 TO RANK2
        FOR VALL = 1 TO RANK2
            COUNT = 0
            FOR I = 1 TO RANK2
                IF BLOCK(ROW, I, VALL) = VALL THEN COUNT = COUNT + 1
            NEXT
            IF COUNT = 1 THEN
                FOR COL = 1 TO RANK2
                    IF PART(ROW, COL) <> 0 THEN GOTO 2
                    IF BLOCK(ROW, COL, VALL) = VALL THEN
                        PART(ROW, COL) = VALL
                        PRINT #1, "Only one instance of"; VALL; "in row"; ROW; "-- inserting at column"; COL
                        FOR I = 1 TO RANK2
                            BLOCK(ROW, COL, I) = 0
                        NEXT
                        CALL UPDATEBOARD
                        GOTO 4
                    END IF
                2 NEXT
            ELSE
                BLK = -1
                FOR COL = 1 TO RANK2
                    OLDBLK = BLK
                    IF BLOCK(ROW, COL, VALL) = VALL THEN BLK = INT((COL - 1) / RANK)
                    IF OLDBLK >= 0 AND BLK <> OLDBLK THEN GOTO 3
                NEXT
                IF BLK = -1 THEN GOTO 3
                FOR I = INT((ROW - 1) / RANK) * RANK + 1 TO INT((ROW - 1) / RANK) * RANK + RANK
                    FOR J = BLK * RANK + 1 TO BLK * RANK + RANK
                        IF I <> ROW AND BLOCK(I, J, VALL) <> 0 THEN
                            BLOCK(I, J, VALL) = 0
                            PRINT #1, "Eliminating"; VALL; "from"; I; J; "because it must be in row"; ROW; "in same block"
                        END IF
                    NEXT
                NEXT
            END IF
        3 NEXT
    4 NEXT
END SUB

SUB SOLVECOL
    FOR COL = 1 TO RANK2
        FOR VALL = 1 TO RANK2
            COUNT = 0
            FOR I = 1 TO RANK2
                IF BLOCK(I, COL, VALL) = VALL THEN COUNT = COUNT + 1
            NEXT
            IF COUNT = 1 THEN
                FOR ROW = 1 TO RANK2
                    IF PART(ROW, COL) <> 0 THEN GOTO 5
                    IF BLOCK(ROW, COL, VALL) = VALL THEN
                        PART(ROW, COL) = VALL
                        PRINT #1, "Only one instance of"; VALL; "in column"; COL; "-- inserting at row"; ROW
                        FOR I = 1 TO RANK2
                            BLOCK(ROW, COL, I) = 0
                        NEXT
                        CALL UPDATEBOARD
                        GOTO 7
                    END IF
                5 NEXT
            ELSE
                BLK = -1
                FOR ROW = 1 TO RANK2
                    OLDBLK = BLK
                    IF BLOCK(ROW, COL, VALL) = VALL THEN BLK = INT((ROW - 1) / RANK)
                    IF OLDBLK >= 0 AND BLK <> OLDBLK THEN GOTO 6
                NEXT
                IF BLK = -1 THEN GOTO 6
                FOR I = BLK * RANK + 1 TO BLK * RANK + RANK
                    FOR J = INT((COL - 1) / RANK) * RANK + 1 TO INT((COL - 1) / RANK) * RANK + RANK
                        IF J <> COL AND BLOCK(I, J, VALL) <> 0 THEN
                            BLOCK(I, J, VALL) = 0
                            PRINT #1, "Eliminating"; VALL; "from"; I; J; "because is must be in col"; COL; "in same block"
                        END IF
                    NEXT
                NEXT
            END IF
        6 NEXT
    7 NEXT
END SUB

SUB SOLVEBOX
    FOR I = 0 TO RANK - 1
        FOR J = 0 TO RANK - 1
            FOR VALL = 1 TO RANK2
                COUNT = 0
                FOR K = I * RANK + 1 TO I * RANK + RANK
                    FOR L = J * RANK + 1 TO J * RANK + RANK
                        IF BLOCK(K, L, VALL) = VALL THEN COUNT = COUNT + 1
                    NEXT
                NEXT
                IF COUNT <> 1 THEN GOTO 9
                FOR K = I * RANK + 1 TO I * RANK + RANK
                    FOR L = J * RANK + 1 TO J * RANK + RANK
                        IF PART(K, L) <> 0 THEN GOTO 8
                        IF BLOCK(K, L, VALL) = VALL THEN
                            PART(K, L) = VALL
                            PRINT #1, "Only one instance of"; VALL; "in box -- inserting at row"; K; "column"; L
                            FOR M = 1 TO RANK2
                                BLOCK(K, L, M) = 0
                            NEXT
                            CALL UPDATEBOARD
                            GOTO 9
                        END IF
                    8 NEXT
                NEXT
            9 NEXT
        10 NEXT
    NEXT
END SUB

' find cells with only one possible value and fill them in
' find naked pairs
SUB SOLVECOUNT
    FOR ROW = 1 TO RANK2
        FOR COL = 1 TO RANK2
            IF PART(ROW, COL) <> 0 THEN GOTO 11
            COUNT = 0
            FOR VALL = 1 TO RANK2
                IF BLOCK(ROW, COL, VALL) = VALL THEN COUNT = COUNT + 1
            NEXT
            IF COUNT = 1 THEN
                FOR VALL = 1 TO RANK2
                    IF BLOCK(ROW, COL, VALL) = VALL THEN
                        PRINT #1, "Only"; VALL; "may be legally inserted at"; ROW; "column"; COL
                        PART(ROW, COL) = VALL
                        FOR I = 1 TO RANK2
                            BLOCK(ROW, COL, I) = 0
                        NEXT
                        CALL UPDATEBOARD
                        EXIT FOR
                    END IF
                NEXT
            ELSEIF COUNT = 2 THEN
                FOR COL2 = 1 TO RANK2
                    EXACT = TRUE
                    FOR J = 1 TO RANK2
                        IF BLOCK(ROW, COL2, J) <> BLOCK(ROW, COL, J) OR COL2 = COL THEN EXACT = FALSE: EXIT FOR
                    NEXT
                    IF EXACT THEN
                        FOR I = 1 TO RANK2
                            IF (I <> COL) AND (I <> COL2) THEN
                                FOR J = 1 TO RANK2
                                    IF BLOCK(ROW, I, J) = BLOCK(ROW, COL, J) AND BLOCK(ROW, I, J) <> 0 THEN
                                        PRINT #1, "Eliminating "; J; "from"; ROW; I; "b/c naked pair at"; ROW; COL; "and"; ROW; COL2
                                        BLOCK(ROW, I, J) = 0
                                    END IF
                                NEXT
                            END IF
                        NEXT
                        EXIT FOR
                    END IF
                NEXT
                FOR ROW2 = 1 TO RANK2
                    EXACT = TRUE
                    FOR J = 1 TO RANK2
                        IF BLOCK(ROW2, COL, J) <> BLOCK(ROW, COL, J) OR ROW2 = ROW THEN EXACT = FALSE: EXIT FOR
                    NEXT
                    IF EXACT THEN
                        FOR I = 1 TO RANK2
                            IF (I <> ROW) AND (I <> ROW2) THEN
                                FOR J = 1 TO RANK2
                                    IF BLOCK(I, COL, J) = BLOCK(ROW, COL, J) AND BLOCK(I, COL, J) <> 0 THEN
                                        BLOCK(I, COL, J) = 0
                                        PRINT #1, "Eliminating "; J; "from"; I; COL; "b/c naked pair at"; ROW; COL; "and"; ROW2; COL
                                    END IF
                                NEXT
                            END IF
                        NEXT
                        EXIT FOR
                    END IF
                NEXT
                FOR I = INT((ROW - 1) / RANK) * RANK + 1 TO INT((ROW - 1) / RANK) * RANK + RANK
                    FOR J = INT((COL - 1) / RANK) * RANK + 1 TO INT((COL - 1) / RANK) * RANK + RANK
                        EXACT = TRUE
                        FOR K = 1 TO RANK2
                            IF (BLOCK(I, J, K) <> BLOCK(ROW, COL, K)) OR (I = ROW AND J = COL) THEN EXACT = FALSE: EXIT FOR
                        NEXT
                        IF EXACT THEN
                            FOR M = INT((ROW - 1) / RANK) * RANK + 1 TO INT((ROW - 1) / RANK) * RANK + RANK
                                FOR N = INT((COL - 1) / RANK) * RANK + 1 TO INT((COL - 1) / RANK) * RANK + RANK
                                    IF ((M <> ROW OR N <> COL) AND (M <> I OR N <> J)) THEN
                                        FOR K = 1 TO RANK2
                                            IF BLOCK(ROW, COL, K) = BLOCK(M, N, K) AND BLOCK(M, N, K) <> 0 THEN
                                                BLOCK(M, N, K) = 0
                                                PRINT #1, "Eliminating "; K; "from"; M; N; "b/c naked pair at"; ROW; COL; "and"; I; J
                                            END IF
                                        NEXT
                                    END IF
                                NEXT
                            NEXT
                            GOTO 11
                        END IF
                    NEXT
                NEXT
            ELSE
                GOTO 11
            END IF
        11 NEXT
    NEXT
END SUB

' run all solution strategies
SUB SOLVESTEP
    CALL SOLVEROW
    CALL SOLVECOL
    CALL SOLVEBOX
    CALL SOLVECOUNT
END SUB

' output sudoku grid
SUB PRINTSOLUTION
    FOR I = 1 TO RANK2
        FOR J = 1 TO RANK2
            PRINT PART(I, J);
            PRINT #1, PART(I, J);
        NEXT
        PRINT
        PRINT #1, ""
    NEXT
    CLOSE #1
    PRINT
    PRINT "Press ESCAPE to quit or any other key to see the steps"
    PRINT "used to reach this point."
    NULL$ = INPUT$(1)
    IF NULL$ = CHR$(27) THEN
        SYSTEM
    ELSE
        SHELL EDITOR + CHR$(32) + FILE
    END IF
END SUB

' read in sudoku grid
SUB INPUTBOARD
    DO
        PRINT "Which character would you like to use to represent blank (empty squares)? ";
        LINE INPUT BLANK$
        BLANK$ = LEFT$(LTRIM$(RTRIM$(BLANK$)), 1)
        IF BLANK$ = "" THEN BLANK$ = " "
    LOOP UNTIL INSTR(NUMBERS, BLANK$) = 0

    PRINT
    PRINT "Enter your puzzle in plain text form.  Only numbers (1-9) and blanks have"
    PRINT "significance. All other characters are ignored.  This means that the puzzle"
    PRINT "may be input all on one line or with line breaks, spaces, and ASCII art to"
    PRINT "mark block boundaries or beautify your input."
    PRINT

    NUMLEFT = RANK2 * RANK2
    I = 1 ' current row
    J = 0 ' current column

    DO
        LINE INPUT LINE$
        FOR N = 1 TO LEN(LINE$)
            X$ = MID$(LINE$, N, 1)
            IF X$ = BLANK$ OR INSTR(NUMBERS, X$) THEN
                J = J + 1
                IF J > RANK2 THEN J = 1: I = I + 1
                PART(I, J) = INSTR(NUMBERS, X$)
                NUMLEFT = NUMLEFT - 1
            END IF
        NEXT
    LOOP UNTIL NUMLEFT = 0
END SUB

' update BLOCK to reflect entries filled in to PART
SUB UPDATEBOARD
    FOR ROW = 1 TO RANK2
        FOR COL = 1 TO RANK2
            IF PART(ROW, COL) = 0 THEN GOTO 1
            FOR I = 1 TO RANK
				' cannot have a repeated number in same row
                IF BLOCK(ROW, I, PART(ROW, COL)) = PART(ROW, COL) THEN
                    BLOCK(ROW, I, PART(ROW, COL)) = 0
                END IF
				' cannot have a repeated number in same column
                IF BLOCK(I, COL, PART(ROW, COL)) = PART(ROW, COL) THEN
                    BLOCK(I, COL, PART(ROW, COL)) = 0
                END IF
            NEXT
			' cannot have a repeated number in same block
            FOR I = INT((ROW - 1) / RANK) * RANK + 1 TO INT((ROW - 1) / RANK) * RANK + RANK
                FOR J = INT((COL - 1) / RANK) * RANK + 1 TO INT((COL - 1) / RANK) * RANK + RANK
                    IF BLOCK(I, J, PART(ROW, COL)) = PART(ROW, COL) THEN
                        BLOCK(I, J, PART(ROW, COL)) = 0
                    END IF
                NEXT
            NEXT
        1 NEXT
    NEXT
END SUB

' initialize BLOCK to reflect puzzle
SUB INITBOARD
    FOR ROW = 1 TO RANK2
        FOR COL = 1 TO RANK2
            FOR I = 1 TO RANK2
                IF PART(ROW, COL) = 0 THEN
                    BLOCK(ROW, COL, I) = I
                END IF
            NEXT
        NEXT
    NEXT
END SUB
