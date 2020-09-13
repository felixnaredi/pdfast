import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Test.QuickCheck                ( Testable(property) )
import           ParserSpec.Object              ( objectParseSpecs )

main :: IO ()
main = hspec objectParseSpecs
