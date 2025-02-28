package coop.rchain.casper.genesis

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, unsignedBlockProto}
import coop.rchain.casper.util.Sorting.byteArrayOrdering
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.signatures.Signed

final case class Genesis(
    shardId: String,
    timestamp: Long,
    proofOfStake: ProofOfStake,
    vaults: Seq[Vault],
    supply: Long
)

object Genesis {

  def defaultBlessedTerms(
      timestamp: Long,
      posParams: ProofOfStake,
      vaults: Seq[Vault],
      supply: Long
  ): Seq[Signed[DeployData]] =
    Seq(
      StandardDeploys.registry,
      StandardDeploys.listOps,
      StandardDeploys.either,
      StandardDeploys.nonNegativeNumber,
      StandardDeploys.makeMint,
      StandardDeploys.authKey,
      StandardDeploys.revVault,
      StandardDeploys.multiSigRevVault,
      StandardDeploys.revGenerator(vaults, supply),
      StandardDeploys.poSGenerator(posParams),
      StandardDeploys.treeHashMap
    )

  def createGenesisBlock[F[_]: Concurrent](
      runtimeManager: RuntimeManager[F],
      genesis: Genesis
  ): F[BlockMessage] = {
    import genesis._

    val blessedTerms = defaultBlessedTerms(
      timestamp,
      proofOfStake,
      vaults,
      supply = Long.MaxValue
    )

    runtimeManager
      .computeGenesis(blessedTerms, timestamp)
      .map {
        case (startHash, stateHash, processedDeploys) =>
          createProcessedDeploy(genesis, startHash, stateHash, processedDeploys)
      }
  }

  private def createProcessedDeploy(
      genesis: Genesis,
      startHash: StateHash,
      stateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  ): BlockMessage = {
    import genesis._

    val state = RChainState(
      preStateHash = startHash,
      postStateHash = stateHash,
      blockNumber = 0,
      bonds = bondsProto(proofOfStake).toList
    )

    //FIXME any failures here should terminate the genesis ceremony
    val blockDeploys = processedDeploys.filterNot(_.isFailed)
    val sortedDeploys =
      blockDeploys.map(d => d.copy(deployLog = d.deployLog.sortBy(_.toProto.toByteArray)))
    val body    = Body(state = state, deploys = sortedDeploys.toList)
    val version = 1L //FIXME make this part of Genesis, and pass it from upstream
    val header  = blockHeader(body, List.empty[StateHash], version, timestamp)

    unsignedBlockProto(body, header, List.empty[Justification], shardId)
  }

  private def bondsProto(proofOfStake: ProofOfStake): Seq[Bond] = {
    val bonds = proofOfStake.validators.flatMap(Validator.unapply).toMap
    import coop.rchain.crypto.util.Sorting.publicKeyOrdering
    //sort to have deterministic order (to get reproducible hash)
    bonds.toIndexedSeq.sorted.map {
      case (pk, stake) =>
        val validator = ByteString.copyFrom(pk.bytes)
        Bond(validator, stake)
    }
  }
}
