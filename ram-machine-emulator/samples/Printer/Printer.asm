; at the beginning R_i := 0

Write =0     ; print(0)  
Write =1     ; print(1)
Write =2     ; print(2)
Write =3     ; print(3)

Load   =1    ; R_0 := 1
Store   1    ; R_1 := R_0 = 1
Add    =1    ; R_0 := R_0 + 1 = 2
Store   1    ; R_1 := R_0 = 2
Store  *1    ; R_reg(1) := R_0
             ;   R_2    := R_0
             ;   R_2    := 2 

JGTZ  ALTER_LABEL      ; if (R_0 > 0) ---------------------------|
                       ;                                         |
WRITE_LABEL:  Write 0  ; print(R_0)   <-----------|              |
              Write 1  ; print(R_1)               |              |
              Write 2  ; print(R_2)               |              |
              Write 3  ; print(R_3)               |              |
              Halt  0  ;                          |              |
                       ;                          |              |
ALTER_LABEL:  Sub  1   ; R_0 := R_0 - R_1 = 0     |           <--|
              Jump  WRITE_LABEL ;-----------------|
