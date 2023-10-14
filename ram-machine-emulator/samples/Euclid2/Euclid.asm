      Read 1
      Read 2

LOOP: Load 1
      Sub 2
      JZero END
      JGTZ SUB_A
      MULT =-1
      Store 2
      Jump LOOP

SUB_A: Store 1
       Jump LOOP

END:   Write 1
       Halt 0