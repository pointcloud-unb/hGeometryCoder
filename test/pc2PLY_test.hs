{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import Test.Hspec ( describe, it, shouldBe, hspec,
                    context)
import Test.Hspec.QuickCheck ()

import Codec.PointCloud.Compression.PLY ( pc2PLY )
import Codec.PointCloud.Types.Voxel
    ( Voxel(Voxel), Voxel(getU, getV, getW) )
import qualified Data.ByteString as B (ByteString)
import Data.Attoparsec.ByteString.Char8
    ( parse, parseOnly, Result, IResult(Partial, Done, Fail) )
import qualified Data.ByteString.Char8 as B8 (pack)
import Codec.PointCloud.Driver.PLY.Parser ( elementData, header )
import Codec.PointCloud.Driver.PLY.Types
    (Property, Element(Element), Header(Header, hElems),
     PLY(PLY), DataBlocks, Format (ASCII), Property(ScalarProperty),
      ScalarType(FloatT), Scalar (FloatS))
import Control.Monad (forM, join)
import Codec.PointCloud.Types.PointCloud(PointCloud(PointCloud), getVoxelsLength)
import Data.Foldable (Foldable(toList))
import qualified Data.Set as S

main :: IO ()
setPc = S.fromList [Voxel 1 2 3]
pc = PointCloud setPc 1
plyResult = PLY header content
  where
    header =
      Header
        ASCII
        [ Element
            (B8.pack "vertex") 1
            [ ScalarProperty FloatT (B8.pack "x")
            , ScalarProperty FloatT (B8.pack "y")
            , ScalarProperty FloatT (B8.pack "z")
            ]
        ]
    content =  [[FloatS 1.0, FloatS 2.0, FloatS 3.0]]

main = hspec $ do
  describe "pc2PLY" $ do
    context "Conversão de uma pc padrão" $ do
      it "match pc2PLY (PointCloud (Set {Voxel 1 2 3}) 4) should be {plyHeader = Header \
            \...plyData = [[FloatS 1.0,FloatS 2.0,FloatS 3.0]]}" $
        pc2PLY pc `shouldBe` plyResult

    