package coop.rchain.casper.util

import coop.rchain.models.PCost
import cats.implicits._
import cats.{Functor, Monad}
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{DeployData, ProcessedDeploy, ProcessedDeployProto}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.shared.Time

object ConstructDeploy {

  val defaultSec = PrivateKey(
    Base16.unsafeDecode("a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76")
  )
  val defaultPub     = Secp256k1.toPublic(defaultSec)
  val defaultKeyPair = (defaultSec, defaultPub)

  val defaultSec2 = PrivateKey(
    Base16.unsafeDecode("5a0bde2f5857124b1379c78535b07a278e3b9cefbcacc02e62ab3294c02765a1")
  )
  val defaultPub2     = Secp256k1.toPublic(defaultSec2)
  val defaultKeyPair2 = (defaultSec2, defaultPub2)

  def sourceDeploy(
      source: String,
      timestamp: Long,
      phloLimit: Long = 90000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): Signed[DeployData] = {
    val data =
      DeployData(
        term = source,
        timestamp = timestamp,
        phloLimit = phloLimit,
        phloPrice = phloPrice,
        validAfterBlockNumber = 0L
      )

    Signed(data, Secp256k1, sec)
  }

  def sourceDeployNow(
      source: String
  ): Signed[DeployData] =
    sourceDeploy(source = source, timestamp = System.currentTimeMillis())

  def sourceDeployNowF[F[_]: Time: Functor](
      source: String,
      phloLimit: Long = 90000,
      phloPrice: Long = 1L,
      sec: PrivateKey = defaultSec
  ): F[Signed[DeployData]] =
    Time[F].currentMillis
      .map(sourceDeploy(source, _, phloLimit = phloLimit, phloPrice = phloPrice, sec = sec))

  // TODO: replace usages with basicSendDeployData
  def basicDeployData[F[_]: Monad: Time](
      id: Int
  ): F[Signed[DeployData]] =
    sourceDeployNowF(source = s"@$id!($id)")

  def basicSendDeployData[F[_]: Monad: Time](
      id: Int
  ): F[Signed[DeployData]] = basicDeployData[F](id)

  def basicReceiveDeployData[F[_]: Monad: Time](
      id: Int
  ): F[Signed[DeployData]] =
    sourceDeployNowF(source = s"for(_ <- @$id){ Nil }")

  def basicProcessedDeploy[F[_]: Monad: Time](
      id: Int
  ): F[ProcessedDeploy] =
    basicDeployData[F](id).map(
      deploy => ProcessedDeploy(deploy = deploy, cost = PCost(0L), List.empty, false)
    )
}
