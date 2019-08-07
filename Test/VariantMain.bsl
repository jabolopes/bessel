me Test.VariantMain

use Test.Variant

let main =
  [f1 mkApple, f1 $ mkBanana 1, f1 $ mkFig (1, 2.0), f1 $ mkOrange mkApple, f1 $ mkLemon (mkOrange mkApple), f1 $ mkMelon (mkBanana 1)]
