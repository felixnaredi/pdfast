import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Test.QuickCheck                ( Testable(property) )
import           ParserSpec.EntrieObject        ( entrieObjParseSpecs )

main :: IO ()
main = hspec entrieObjParseSpecs
