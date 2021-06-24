{-# LANGUAGE OverloadedStrings #-}
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
import Codec.PointCloud.Types.PointCloud ( getPointCloud )
import Codec.PointCloud.Driver.PLY.Utils ( filterFromLabel )

createPCs = do
    plyData1 <- B.readFile "assets/simple1.ply"
    plyData2 <- B.readFile "assets/simple2.ply"
    (Right pc1) <- pure $ getPointCloud X =<< filterFromLabel "vertex" =<< parsePLY plyData1
    (Right pc2) <- pure $ getPointCloud X =<< filterFromLabel "vertex" =<< parsePLY plyData2
    return (pc1, pc2)

main :: IO ()
main = do
    (pc1, pc2) <- createPCs
    hspec $ do 
        describe "Checando se as PCs são iguais em diferentes ordens de voxels" $ do
            it "match pc1 should be pc2" $ pc1 `shouldBe` pc2