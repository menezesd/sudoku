DECLARE SUB getmove (z%)
DECLARE SUB disp ()
DECLARE SUB showbd (FLAG%)
DECLARE SUB display2 (text$, fore%, back%)
DECLARE SUB create ()
DECLARE FUNCTION numbits% (x%)
DECLARE SUB swapc (a%, B%, c%)
DECLARE FUNCTION combo% (qftprnt%(), zbits%, mymissing%)
DECLARE FUNCTION footprint% (x%, y%)
DECLARE SUB flash ()
DEFINT A-Z
DECLARE FUNCTION eras (x, y)
RANDOMIZE TIMER
DIM SHARED w(9, 9), aw(9, 9), bw(9, 9)
DIM SHARED xy(81), xysol(81), NSOL
DIM SHARED reason, ez
DIM SHARED p(9)
p(1) = 1
FOR i = 2 TO 9
p(i) = p(i - 1) * 2
NEXT
begin:
CALL create
DO
CALL disp
CALL getmove(0)
FOR j = 0 TO 8
FOR k = 0 TO 8
IF ABS(w(j, k)) <> aw(j, k) THEN GOTO mycont
NEXT
NEXT
CALL flash
GOTO begin
mycont:
LOOP

FUNCTION combo (qftprnt(), zbits, mymissing)
FOR k = 0 TO 7
IF qftprnt(k) = -1 THEN GOTO 60
nq = 1
FOR j = k + 1 TO 8
IF qftprnt(j) = qftprnt(k) THEN nq = nq + 1
NEXT
IF nq > 1 AND numbits(qftprnt(k)) = nq THEN
IF nq = zbits THEN combo = 1: EXIT FUNCTION
IF mymissing = qftprnt(k) AND mymissing THEN combo = 1: EXIT FUNCTION
END IF
60 NEXT
combo = 0
END FUNCTION

SUB create
DIM s(9)
DIM r(9)
s(0) = 1: s(1) = 7: s(2) = 4: s(3) = 9
s(4) = 6: s(5) = 3: s(6) = 8: s(7) = 5: s(8) = 2
FOR i = 0 TO 8
r(i) = i + 1
NEXT
retry:
FOR k = 0 TO 8
j = INT(RND(1) * 9)
SWAP r(j), r(k)
NEXT
FOR j = 0 TO 8
FOR k = 0 TO 8
w(j, k) = r((s(j) - 1 + k) MOD 9)
xy(9 * j + k) = 9 * j + k
NEXT
NEXT
FOR c = 0 TO 1
FOR g = 0 TO 6 STEP 3
FOR k = 0 TO 2
CALL swapc(g + INT(RND(1) * 3), g + k, c)
NEXT
NEXT
NEXT
FOR k = 0 TO 80
j = INT(RND(1) * 81)
SWAP xy(j), xy(k)
NEXT
FOR k = 0 TO 8
FOR j = 0 TO 8
aw(j, k) = w(j, k)
NEXT
NEXT
count = 81
hard = 0
FOR k = 0 TO 80
x = xy(k) \ 9: y = xy(k) MOD 9
IF NOT eras(x, y) THEN GOTO cont
count = count - 1
xysol(80 - count) = 100 * reason + 9 * x + y
IF reason = 2 OR reason = 4 OR reason = 6 THEN hard = hard + 1
cont: NEXT
IF hard < 3 OR count > 31 GOTO retry
FOR k = 0 TO 8
FOR j = 0 TO 8
bw(j, k) = w(j, k)
NEXT
NEXT
END SUB

SUB disp
SCREEN 12
LINE (67, 150)-(106, 390), 14, BF
LINE (531, 150)-(570, 390), 14, BF
FOR k = 1 TO 7
LOCATE 2 * k + 9, 11
CALL display2(MID$("SU DOKU", k, 1), 1, 14)
LOCATE 2 * k + 9, 69
CALL display2(MID$("SU DOKU", k, 1), 1, 14)
NEXT
CALL showbd(0)
END SUB

SUB display2 (text$, fore, back)
DIM pixel(8, 16)
FOR i = 1 TO LEN(text$)
PRINT MID$(text$, i, 1);
xul = 8 * (POS(0) - 2): yul = 16 * (CSRLIN - 1)
FOR y = 0 TO 15
FOR x = 0 TO 7
pixel(x, y) = POINT(xul + x, yul + y)
NEXT
NEXT
LINE (xul, yul)-(xul + 15, yul + 28), back, BF
FOR y = 0 TO 15
FOR x = 0 TO 7
IF pixel(x, y) THEN
PSET (xul + 2 * x, yul + 2 * y), fore
PSET (xul + 2 * x + 1, yul + 2 * y), fore
PSET (xul + 2 * x, yul + 2 * y + 1), fore
PSET (xul + 2 * x + 1, yul + 2 * y + 1), fore
END IF
NEXT
NEXT
LOCATE , POS(0) + 1
NEXT
END SUB

FUNCTION eras (x, y)
DIM qftprnt(9)
myprnt = footprint(x, y)
mymissint = 511 - myprnt - p(w(x, y))
z = w(x, y): w(x, y) = 0
reason = 0: IF numbits(myprnt) = 8 THEN eras = -1: EXIT FUNCTION
FLAG = -1: zbits = 0
FOR k = 0 TO 8
qftprnt(k) = -1
IF w(k, y) OR k = 0 THEN GOTO 40
zbits = zbits + 1
ftprnt = footprint(k, y)
IF (p(z) AND ftprnt) = 0 THEN FLAG = 0
qftprnt(k) = 511 - ftprnt
40 NEXT
reason = 1: IF FLAG THEN eras = -1: EXIT FUNCTION
reason = 2: IF combo(qftprnt(), zbits, mymissing) THEN eras = -1: EXIT FUNCTION
FLAG = 1: zbits = 0
FOR j = 0 TO 8
qftprnt(k) = -1
IF w(x, j) OR j = y THEN GOTO 43
zbits = zbits + 1
ftprnt = footprint(x, j)
IF (p(z) AND ftprnt) = 0 THEN FLAG = 0
qftprnt(k) = 511 - ftprnt
43 NEXT
reason = 3: IF FLAG THEN eras = -1: EXIT FUNCTION
reason = 4: IF combo(qftprnt(), zbits, mymissing) THEN eras = -1: EXIT FUNCTION
FLAG = 1: zbits = 0
xul = 3 * (x \ 3): yul = 3 * (y \ 3)
FOR j = yul TO yul + 2
FOR k = xul TO xul + 2
qftprnt(k) = -1
IF k = x AND j = y THEN GOTO 44
IF w(k, j) THEN 44
zbits = zbits + 1
ftprnt = footprint(k, j)
IF (p(z) AND ftprnt) = 0 THEN FLAG = 0
qftprnt(k) = 511 - ftprnt
44 NEXT
NEXT
reason = 5
IF FLAG THEN eras = -1: EXIT FUNCTION
reason = 6
IF combo(qftprnt(), zbits, mymissing) THEN eras = -1: EXIT FUNCTION
w(x, y) = z: eras = 0: EXIT FUNCTION
END FUNCTION

SUB flash
SCREEN 12
a = 175: B = 127
FOR k = 0 TO 35
c = INT(RND(1) * 15) + 1
LINE (8 * k + a, B)-(8 * k + 7 + a, B + 287), c, BF
LINE (8 * (35 - k) + a, B)-(8 * (35 - k) + 7 + a, B + 287), c, BF
LINE (a, 8 * k + B)-(a + 287, 8 * k + 7 + B), c, BF
LINE (a, 8 * (35 - k) + B)-(a + 287, 8 * (35 - k) + 7 + B), c, BF
t! = TIMER + .05: WHILE t! > 0 AND TIMER < t!: WEND
NEXT

END SUB

FUNCTION footprint (x, y)
ftprnt = 0
FOR k = 0 TO 8
IF k = x THEN 10
IF w(k, y) THEN ftprnt = ftprnt OR p(ABS(w(k, y)))
10 NEXT
FOR j = 0 TO 8
IF j = y THEN 20
IF w(x, j) THEN ftprnt = ftprnt OR p(ABS(w(x, j)))
20 NEXT
xul = 3 * (x \ 3): yul = 3 * (y \ 3)
FOR j = yul TO yul + 2
FOR k = xul TO xul + 2
IF k = x AND j = y THEN 30
IF w(k, j) THEN ftprnt = ftprnt OR p(ABS(w(k, j)))
30 NEXT
NEXT
footprint = ftprnt
END FUNCTION

SUB getmove (z)
SHARED x, y
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 3, B
DO
key$ = INKEY$
SELECT CASE key$
CASE CHR$(0) + CHR$(72)
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 15, B
y = y - 1: IF y < 0 THEN y = 8
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 3, B
CASE CHR$(0) + CHR$(80)
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 15, B
y = (y + 1) MOD 9
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 3, B
CASE CHR$(0) + CHR$(75)
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 15, B
x = x - 1: IF x < 0 THEN x = 8
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 3, B
CASE CHR$(0) + CHR$(77)
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 15, B
x = (x + 1) MOD 9
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 3, B
CASE "1" TO "9"
IF w(x, y) > 0 THEN BEEP ELSE w(x, y) = -VAL(key$)
EXIT SUB
CASE " ", "0"
IF w(x, y) > 0 THEN BEEP ELSE w(x, y) = 0
EXIT SUB
CASE "?"
w(x, y) = -aw(x, y)
EXIT SUB
END SELECT
LOOP
END SUB

FUNCTION numbits (x)
xx = x: n = 0
WHILE xx
n = n + 1
xx = xx AND (xx - 1)
WEND
numbits = n
END FUNCTION

SUB showbd (FLAG)
SHARED x, y
LINE (175, 127)-(463, 414), 15, BF
FOR k = 0 TO 8
FOR j = 0 TO 8
LOCATE 2 * k + 9, 4 * j + 24
IF FLAG THEN
IF w(j, k) > 0 THEN c = 1
IF w(j, k) = 0 THEN c = 7
IF w(j, k) < 0 AND w(j, k) = aw(j, k) THEN c = 3
IF w(j, k) < 0 AND w(j, k) <> aw(j, j) THEN c = 4
CALL display2(CHR$(ASC("0") + aw(j, k)), c, 15)
ELSE
IF w(j, k) > 0 THEN CALL display2(CHR$(ASC("0") + w(j, k)), 1, 15)
IF w(j, k) = 0 THEN CALL display2(" ", 15, 15)
IF w(j, k) < 0 THEN CALL display2(CHR$(ASC("0") - w(j, k)), 3, 15)
END IF
NEXT
NEXT
FOR k = 0 TO 9
IF k MOD 3 THEN c = 7 ELSE c = 0
LINE (32 * k + 175, 127)-(32 * k + 175, 414), c
LINE (175, 126 + 32 * k)-(463, 126 + 32 * k), c
NEXT
IF FLAG THEN EXIT SUB
LINE (176 + 32 * x, 127 + 32 * y)-(206 + 32 * x, 157 + 32 * y), 3, B
END SUB

SUB swapc (a, B, c)
FOR k = 0 TO 8
IF c THEN SWAP w(a, k), w(B, k) ELSE SWAP w(k, a), w(k, B)
NEXT
END SUB
