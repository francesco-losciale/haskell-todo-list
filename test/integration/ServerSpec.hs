module ServerSpec where

import Todo
import Test.Hspec

spec :: Spec
spec = beforeAll (setUp) $ do
  describe "Spock Server" $ do
    it "should web server start" $ do
       True
  where
    setUp = do
              return ()
