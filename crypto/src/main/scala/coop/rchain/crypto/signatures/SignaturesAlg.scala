package coop.rchain.crypto.signatures
import coop.rchain.crypto.{PrivateKey, PublicKey}

trait SignaturesAlg {
  def verify(data: Array[Byte], signature: Array[Byte], pub: Array[Byte]): Boolean
  def sign(data: Array[Byte], sec: Array[Byte]): Array[Byte]
  def toPublic(sec: PrivateKey): PublicKey
  def newKeyPair: (PrivateKey, PublicKey)
  def name: String

  def verify(data: Array[Byte], signature: Array[Byte], pub: PublicKey): Boolean =
    verify(data, signature, pub.bytes)
  def sign(data: Array[Byte], sec: PrivateKey): Array[Byte] = sign(data, sec.bytes)

  val sigLength: Int
}

object SignaturesAlg {
  def apply(algorithm: String): Option[SignaturesAlg] =
    algorithm.toLowerCase match {
      case Ed25519.name   => Some(Ed25519)
      case Secp256k1.name => Some(Secp256k1)
      case _              => None
    }
}
