-- greetingCool1.hs

module GreetingIfCool where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool coolness
    then putStrLn "eyyy. What's shaking'?"
    else
      putStrLn "pshhhh."
  where
    cool v = v == "downright frosty yo"
