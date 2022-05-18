player:
  parameters:
    - life: 
        value: 100
    - hunger:
        value: 50
  inventory:
  leftHand:
    empty
  rightHand:
    empty

rooms:
  - kitchen:
      description:
        You are in your kitchen, looking at the table. 
      items:
        - food:
            commands:
              - eat: 
                  {
                    player.hunger += 10
                    discard
                  }
        - drink:
            commands:
              - drink:
                  {
                    if player.hunger < 99 then player.hunger += 2 else player.hunger = 100
                    discard
                  }
      commands:
        - nop:
            {
              echo You just wait and do nothing.
              goto kitchen
            }
            
start: kitchen
