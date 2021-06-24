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

createEDXs = do
    plyData1 <- B.readFile "assets/simple1.ply"
    plyData2 <- B.readFile "assets/simple2.ply"
    (Right edx1) <- pure $ buildEDX =<< encodeGeometry X =<< parsePLY plyData1
    (Right edx2) <- pure $ buildEDX =<< encodeGeometry X =<< parsePLY plyData2
    return (edx1, edx2)

main :: IO ()
main = do
    (edx1, edx2) <- createEDXs
    hspec $ do 
        describe "Checando se a codificação de 2 PCs iguais com diferentes ordens de voxels é igual" $ do
            it "match edx1 should be edx2" $ edx1 `shouldBe` edx2