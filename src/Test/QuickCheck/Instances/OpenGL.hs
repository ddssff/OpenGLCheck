----------------------------------------------------------------------
-- |
-- Module      :  Test.QuickCheck.Instances.OpenGL
-- Copyright   :  (c) Anygma BVBA & Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  stable
-- 
-- Arbitrary instances for OpenGL data structures
----------------------------------------------------------------------
module Test.QuickCheck.Instances.OpenGL where

import Graphics.Rendering.OpenGL
import Test.QuickCheck
import Test.QuickCheck.Instances
import Control.Applicative

instance Arbitrary a => Arbitrary (Vertex2 a) where
  arbitrary = Vertex2 <$> arbitrary <*> arbitrary
  coarbitrary (Vertex2 x y) = coarbitrary x . coarbitrary y

instance Arbitrary a => Arbitrary (Vertex3 a) where
  arbitrary = Vertex3 <$> arbitrary <*> arbitrary <*> arbitrary
  coarbitrary (Vertex3 x y z) = coarbitrary x . coarbitrary y . coarbitrary z

instance Arbitrary a => Arbitrary (Vertex4 a) where
  arbitrary = Vertex4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  coarbitrary (Vertex4 x y z w) =
    coarbitrary x . coarbitrary y . coarbitrary z . coarbitrary w

instance Arbitrary a => Arbitrary (Normal3 a) where
  arbitrary = Normal3 <$> arbitrary <*> arbitrary <*> arbitrary
  coarbitrary (Normal3 x y z) = coarbitrary x . coarbitrary y . coarbitrary z

instance Arbitrary a => Arbitrary (TexCoord2 a) where
  arbitrary = TexCoord2 <$> arbitrary <*> arbitrary
  coarbitrary (TexCoord2 x y) = coarbitrary x . coarbitrary y

instance Arbitrary a => Arbitrary (TexCoord3 a) where
  arbitrary = TexCoord3 <$> arbitrary <*> arbitrary <*> arbitrary
  coarbitrary (TexCoord3 x y z) = coarbitrary x . coarbitrary y . coarbitrary z

instance Arbitrary a => Arbitrary (Color3 a) where
  arbitrary = Color3 <$> arbitrary <*> arbitrary <*> arbitrary
  coarbitrary (Color3 x y z) = coarbitrary x . coarbitrary y . coarbitrary z

instance Arbitrary a => Arbitrary (Color4 a) where
  arbitrary = Color4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  coarbitrary (Color4 x y z w) =
    coarbitrary x . coarbitrary y . coarbitrary z . coarbitrary w

instance Arbitrary TextureObject where
  arbitrary = TextureObject <$> arbitrary
  coarbitrary (TextureObject x) =
    coarbitrary x

instance Arbitrary DisplayList where
  arbitrary = DisplayList <$> arbitrary
  coarbitrary (DisplayList x) =
    coarbitrary x
