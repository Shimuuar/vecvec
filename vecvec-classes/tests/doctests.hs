import Test.DocTest (doctest)

main :: IO ()
main = doctest [ "-XGHC2021"
               , "-XDataKinds"
               , "-XDerivingStrategies"
               , "-XDerivingVia"
               , "-XLambdaCase"
               , "-XLexicalNegation"
               , "-XMultiWayIf"
               , "-XPatternSynonyms"
               , "-XViewPatterns"
               , "Vecvec"
               ]
