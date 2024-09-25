import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ "-XGHC2021"
  , "-XNoPolyKinds"
  , "-XPatternSynonyms"
  , "-XMultiWayIf"
  , "-XDerivingStrategies"
  , "-XDataKinds"
  , "-XViewPatterns"
  , "-XMultiWayIf"
  , "-XLambdaCase"
  , "-XDerivingVia"
  , "-XLexicalNegation"
  , "-XPatternSynonyms"
  , "-XDuplicateRecordFields"
  , "-XOverloadedRecordDot"
    --
  , "Vecvec"
  
  ]
