{-# OPTIONS_GHC -F -pgmF hspec-discover #-}


testProductData :: [(String, Maybe Product)]
testProductData = [
  ("water", Just Water),
  ("Soda", Just Soda),
  ("coFFEE", Just Coffee),
  ("energy drink", Just Energy),
  ("Green Tea", Just Tea),
  ("Invalid Input", Nothing)
  ]

main :: IO () = hspec $ do
  describe "parseProduct" $ do
    mapM_ (it . fst) testProductData $ snd
  describe 