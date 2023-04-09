import Shan.AST.Diagram (Expr (Number, Var), JudgeOp (Gt, Le, Neq), Judgement (OrJ, SimpleJ), Variable (SimpleVariable))
import Shan.AST.Diagram.Parser (parseJudgement)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec judgementSpec

judgementSpec :: Spec
judgementSpec = do
  describe "parse judgement" $ do
    it "simple judgement" $ do
      parseJudgement "a > 5" `shouldBe` SimpleJ (Var $ SimpleVariable "a") Gt (Number 5.0)

    it "or judgement" $ do
      parseJudgement "a <= 0.8 || (b) !=9" `shouldBe` OrJ (SimpleJ (Var $ SimpleVariable "a") Le (Number 0.8)) (SimpleJ (Var $ SimpleVariable "b") Neq (Number 9))
