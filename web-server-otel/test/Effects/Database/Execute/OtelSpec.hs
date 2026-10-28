module Effects.Database.Execute.OtelSpec (spec) where

import Data.Text (Text)
import Effects.Database.Execute.Otel (toStatementAttributes)
import Effects.Database.SerializedStatement (SerializedStatement (..))
import OpenTelemetry.Trace.Core qualified as Trace
import Test.Hspec

spec :: Spec
spec = describe "toStatementAttributes" $ do
  it "includes sql attribute" $ do
    let ss = SerializedStatement "SELECT * FROM users" []
        attrs = toStatementAttributes ss
    lookup "sql" attrs `shouldBe` Just (Trace.toAttribute ("SELECT * FROM users" :: Text))

  it "includes params attribute with each value redacted" $ do
    let ss = SerializedStatement "SELECT * FROM users WHERE id = $1" ["123"]
        attrs = toStatementAttributes ss
        expected = "\n  $1 = <redacted>" :: Text
    lookup "params" attrs `shouldBe` Just (Trace.toAttribute expected)

  it "formats multiple params correctly" $ do
    let ss = SerializedStatement "INSERT INTO users (name, email) VALUES ($1, $2)" ["alice", "alice@example.com"]
        attrs = toStatementAttributes ss
        expected = "\n  $1 = <redacted>\n  $2 = <redacted>" :: Text
    lookup "params" attrs `shouldBe` Just (Trace.toAttribute expected)

  it "redacts literals in the sql attribute" $ do
    let ss = SerializedStatement "SELECT * FROM t WHERE token = CAST(E'canary-secret-123' AS \"text\")" []
        attrs = toStatementAttributes ss
    lookup "sql" attrs `shouldBe` Just (Trace.toAttribute ("SELECT * FROM t WHERE token = CAST('<redacted>' AS \"text\")" :: Text))

  it "handles empty params" $ do
    let ss = SerializedStatement "SELECT 1" []
        attrs = toStatementAttributes ss
    lookup "params" attrs `shouldBe` Just (Trace.toAttribute ("" :: Text))
