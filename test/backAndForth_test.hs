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
    plyData <- B.readFile "assets/simple.ply"
    (Right plyI) <- pure $ parsePLY plyData
    (Right edx) <- pure $ buildEDX =<< encodeGeometry X plyI
    (Right plyO) <- pure $ Right . pc2PLY =<< decodeGeometry edx
    return (plyI, plyO)

main :: IO ()
main = do
    (plyI, plyO) <- createPLYs
    hspec $ do 
        describe "Checando se codificação + decodificação = original" $ do
            it "match plyI should be plyO" $ plyI `shouldBe` plyO