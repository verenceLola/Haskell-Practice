-- FunctionWithLet.sh

module FunctionWithLet where
    printInc2 n = let plusTwo = n + 2
                    in print plusTwo
    print2 n = 
        (\plusTwo -> print plusTwo) (n + 2)
