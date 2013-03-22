{-# LANGUAGE ExistentialQuantification,Rank2Types #-}
-- Playing with generalizations of 'nacci sequences



{- functional version
nacci f z m =
  let go n | n <= m = n
           | otherwise = foldr f z
                       . map (\i -> go $ n - i)
                       $ [1..m]
  in  go

fib = nacci (+) 0 2
trib = nacci (+) 0 3
anynacci = nacci (+) 0
multnacci = nacci (*) 1
pownacci = nacci (^) 1
-}



-- stream version

{- some preliminary thoughts on these sequences as streams

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
tribs = 0 : 1 : 1 : zipWith3 sum3 fibs (tail fibs) (tail $ tail fibs)
  where sum3 x y z = x + y + z

mibs = 1 : 2 : zipWith (*) mibs (tail mibs)
mribs = 1 : 2 : 2 : zipWith3 mult3 mribs (tail mribs) (tail $ tail mribs)
  where mult3 x y z = x * y * z
mqibs = 1 : 2 : 2 : 4 : zipWithN product
                          [mqibs,(tail mqibs),(tail $ tail mqibs),(tail $ tail $ tail mqibs)]
-}

-- a useful generalization of zipWith

zipWithN :: ([a] -> b) -> [[a]] -> [b]
zipWithN f = map f . rotateLists
  where rotateLists xss | null $ head xss = []
                        | otherwise = map head xss : rotateLists (map tail xss)

{- and now for a function which takes nearly the same as the earlier definition 
 - of nacci, but this time returns a stream -}
nacci :: Enum a => ([a] -> a) -> a -> Int -> [a]
nacci f z m =
  let base = case m of
               1 -> [z]
               2 -> [z, succ z]
               _ -> drop 2 . take m . nacci f z $ pred m
  in (base ++) . zipWithN f . take m . iterate tail $ nacci f z m

-- and some instances of nacci streams

fibs = nacci sum 0 2
tribs = nacci sum 0 3
sumnacci = nacci sum 0 -- generalize summation-type 'naccis
quatronacci = sumnacci 4 -- and an instance of sumnacci

pronacci = nacci product 1 -- product-type 'naccis
mibs = pronacci 2
mtribs = pronacci 3

pownacci = nacci (foldr (^) 1) 2 -- exponentiation-type 'naccis
pibs = pownacci 2



{- Notice that nacci is only restricted to producing sequences of objects 
 - which instantiate the Enum typeclass. This allows us to build 'nacci streams 
 - of more interesting types, such as the Church type below. -}

data Church = Church (forall x. (x -> x) -> x -> x)
zero = Church (\f x -> x)

instance Enum Church where
  succ (Church n) = Church (\f x -> (f (n f x)))
  toEnum 0 = zero
  toEnum n = succ . toEnum . pred $ n
  fromEnum (Church n) = n (+ 1) 0

churchPlus :: Church -> Church -> Church
churchPlus (Church m) (Church n) = Church (\f x -> m f $ n f x)

churchSum :: [Church] -> Church
churchSum = foldr churchPlus zero

churchFibs = nacci churchSum zero 2

{- And now it holds that
 -      fibs == map toEnum churchFibs
 -}
