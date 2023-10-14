          Read 1
          
LOOP:     Load 2
          Mult 0
          Sub 1
          JZero FINISHED
          JGTZ FIN_SUB
          Load 2
          Add =1
          Store 2
          JUMP LOOP

FIN_SUB:  Load 2
          Sub =1
          Store 2

FINISHED: Load =2
          Write *0
          Halt 0