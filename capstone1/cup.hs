type Name = String
data Robot = Robot { name :: Name, attack :: Int, hp :: Int }

setHp :: Robot -> Int -> Robot
setHp robot hp = Robot {name = name robot, attack = attack robot, hp = hp }

showRobot :: Robot -> String
showRobot robot = "Name: " ++ name robot ++ " attack: " ++ show (attack robot) ++ " hp: " ++ show (hp robot)

damage :: Robot -> Int -> Robot
damage robot aDamage = setHp robot $ hp robot - aDamage

hit :: Robot -> Robot -> Robot
hit robot defRobot = damage defRobot (if hp robot > 0 then attack robot else 0)

killerRobot :: Robot
killerRobot = Robot {name = "Killer", attack = 15, hp = 100}

softyRobot :: Robot
softyRobot = Robot {name = "Softy", attack = 20, hp = 30}

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound then minBound else succ n

battle :: Robot -> Robot -> String
battle robot1 robot2 
    | hp robot1 == 0 && hp robot2 == 0 = "No one wins!"
    | hp robot1 == 0 = name robot2 ++ " wins\n"
    | hp robot2 == 0 = name robot1 ++ " wins\n"
    | otherwise      = battle (hit robot1 robot2) (hit robot2 robot1)