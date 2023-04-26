
--apply function to a parametrized type
apply :: Applicative f => (a -> b) -> f a -> f b
apply g f = fmap g f

--apply multiple parameters

applyMultipe :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
applyMultipe g fa fb = g <$> fa <*> fb

--applicative with lists

functorsToSequence :: Applicative f => [f a] -> f [a]
functorsToSequence [] = pure []
functorsToSequence (x:xs) = (:) <$> x <*> (functorsToSequence xs)




--IO applicative

readChars :: Int -> IO String
readChars 0 = pure []
readChars n = (:) <$> getChar <*> readChars (n-1)
  


