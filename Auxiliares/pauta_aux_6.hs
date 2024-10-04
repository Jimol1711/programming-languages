--ghc 8.0.2

-- P1 --
data Natural = Zero |Succ Natural deriving (Show);

-- P1.1 --
int2Nat :: Int -> Natural
int2Nat 0 = Zero
int2Nat n = Succ (int2Nat (n - 1))

nat2Int :: Natural -> Int
nat2Int Zero = 0
nat2Int (Succ nat) = 1 + (nat2Int nat)

-- P1.2 --
sumNatural :: Natural -> Natural -> Natural
sumNatural Zero     m = m
sumNatural (Succ n) m = sumNatural n (Succ m)

-- P1.3 --
multNatural :: Natural -> Natural -> Natural
multNatural Zero m = Zero
multNatural (Succ n) m = sumNatural m (multNatural n m)

factNatural :: Natural -> Natural
factNatural Zero = (Succ Zero)
factNatural (Succ Zero) = (Succ Zero)
factNatural (Succ nat) = multNatural (Succ nat) (factNatural nat)

-- P2 --
ones = 1 : ones -- Racket (cons 1 ones)

-- P2.1 --
myRepeat :: a -> [a]
myRepeat a = a : (myRepeat a)

-- P2.2 --
myCycle :: [a] -> [a]
myCycle l = l ++ (myCycle l)



main = do
    print (nat2Int (sumNatural (int2Nat 10) (int2Nat 17)))
    print (nat2Int (factNatural (int2Nat 10)))
    print (take 10 (myRepeat 1))
    print (take 30 (myCycle [1,2,3]))
