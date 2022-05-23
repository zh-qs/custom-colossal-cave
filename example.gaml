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

initialMessage:
  Hej!

rooms:
  - kitchen:
      description:
        You are in your kitchen, looking at the table.
      items:
        - food:
            commands:
              - eat:
                  {
                    println Delicious!
                    player.hunger = player.hunger + 10
                    discard
                  }
        - drink:
            commands:
              - drink:
                  {
                    println So fresh!
                    if player.hunger < 99 then 
                    {
                      player.hunger = player.hunger + 2
                    }
                    else
                    {
                      player.hunger = 100
                    }
                  }
      commands:
        - nop:
            {
              println You just wait and do nothing.
              goto kitchen
            }

start: kitchen
