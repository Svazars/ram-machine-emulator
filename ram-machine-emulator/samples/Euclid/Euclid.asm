Read 1
Read 2

LOOP: Load 1
      Sub  2 ; r_0 = r_1 - r_2
      JZero FINISH
      JGTZ  G
      Jump  L

G:    Store 1
      Jump LOOP

L:    MULT =-1 ; r_0 = r_2 - r_1
      Store 2
      Jump LOOP

FINISH: Write 1
        Halt 0
