player:
  parameters:
    - life:
        value: 10
  inventory:
  leftHand:
    empty
  rightHand:
    empty

initialMessage:
  Witaj w prostej grze!!!

rooms:
  - jeden:
      description:
        Pierwszy pokoj
      items:
        - axe:
            commands:
              - throw:
                  {
                    println WHOOOSH! What a throw!
                  }
      commands:
        - nop:
            {
              print You have 
              printval 2*player.life
              println  points of life
              if player.life <= 0 then 
              {
                println You are dead!!!
              }
              else
              {
                println You are still alive...
                player.life = player.life - 1
              }
              goto jeden
            }

start: jeden
