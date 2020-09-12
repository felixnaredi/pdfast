import           Test.Hspec
import           Control.Exception              ( evaluate )
import           Test.QuickCheck                ( Testable(property) )
import           ParserSpec

main :: IO ()
main = hspec objectParseSpecs
