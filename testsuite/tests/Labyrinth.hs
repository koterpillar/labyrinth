import Labyrinth

import Data.List

import Test.HUnit

main = runTestTT tests

tests = TestList [ test_show
                 ]

empty_labyrinth = Labyrinth { cells = replicate 5 $ replicate 5 $ Land
                            , wallsH = replicate 5 $ replicate 6 $ NoWall
                            , wallsV = replicate 6 $ replicate 5 $ NoWall
                            , players = []
                            }

empty_expected = intercalate "\n" $ [ "+ + + + + +"
                                    , " . . . . . "
                                    , "+ + + + + +"
                                    , " . . . . . "
                                    , "+ + + + + +"
                                    , " . . . . . "
                                    , "+ + + + + +"
                                    , " . . . . . "
                                    , "+ + + + + +"
                                    , " . . . . . "
                                    , "+ + + + + +"
                                    , " . . . . . "
                                    , "+ + + + + +"
                                    ]

test_show = TestCase $ do
    assertEqual "empty labyrinth"
        empty_expected $
        show empty_labyrinth
