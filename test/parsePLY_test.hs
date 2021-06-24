{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import Test.Hspec ( describe, it, shouldBe, hspec,
                    context)
import Test.Hspec.QuickCheck ()

import Codec.PointCloud.Utils ( Axis(X) )
import Codec.PointCloud.Compression.Dyadic
    ( decodeGeometry, encodeGeometry )
import Codec.PointCloud.Driver.PLY.Parser (parsePLY)
import Codec.PointCloud.Driver.EDX.Bitstream ( buildEDX )
import Codec.PointCloud.Compression.PLY ( pc2PLY )
import qualified Data.ByteString as B (readFile)

createPLYs = do
    plyData1 <- B.readFile "assets/simple1.ply"
    plyData2 <- B.readFile "assets/simple2.ply"
    (Right ply1) <- pure $ parsePLY plyData1
    (Right ply2) <- pure $ parsePLY plyData2
    return (ply1, ply2)

main :: IO ()
main = do
    (ply1, ply2) <- createPLYs
    hspec $ do 
        describe "Checando se a ordem dos voxels não importa para o parser" $ do
            it "match ply1 should be ply2" $ ply1 `shouldBe` ply2