player:
  parameters:
    - life:
        value: 20
  inventory:
    - water:
        commands:
          - drink:
              {
                println Water is great! You gain 1 point of life!
                player.life = player.life + 1
                discard
              }
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
                    discard
                  }
              - take:
                  {
                    println You took axe into inventory.
                    take
                  }
      commands:
        - nop:
            {
              print You have 
              printval player.life
              println  points of life
              if player.life <= 0 then 
              {
                println You are dead!!!
              }
              else
              {
                println You are still alive...
                player.life -= 2
              }
              goto jeden
            }

start: jeden
