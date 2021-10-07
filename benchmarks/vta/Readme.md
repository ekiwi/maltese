VTA Core generated from commit `36a91576edf633479c78649e050f18dd2ddc8103` using:

```.scala
object VerificationCoreConfig extends App {
  implicit val p: Parameters = new DefaultF1Config
  val gen = ChiselGeneratorAnnotation(() => new Core)
  (new chisel3.stage.ChiselStage).execute(Array("-E", "low"), Seq(gen))
}

```
