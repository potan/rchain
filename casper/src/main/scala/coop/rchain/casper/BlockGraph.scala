package coop.rchain.casper

//import neotypes.GraphDatabase
import org.neo4j.driver.v1.{AuthTokens, GraphDatabase}
import neotypes.Driver
import neotypes.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash._

object BlockGraph {
  def addBlockQuery(b: BlockMessage): String = {
    val cb =
      s"""MERGE (b: Block {id:"${b.blockHash.base16String}", creater:"${b.sender.base16String}", seqNum:${b.seqNum}}) """
    val withParents = b.header.parentsHashList
      .map(_.base16String)
      .zipWithIndex
      .foldLeft(cb)(
        (q, p) =>
          q ++ s"""MERGE (b${p._2}: Block {id:"${p._1}"}) CREATE (b${p._2}) <-[:PARENT]- (b) """
      )
    b.justifications.zipWithIndex.foldLeft(withParents)(
      (q, j) =>
        q ++ s"""MERGE (v${j._2}: Validator {id:"${j._1.validator.base16String}"}) CREATE (b) <-[:Justificate]- (v${j._2})) """
    )
  }

  val driver = GraphDatabase.driver /*[Future]*/ (
    "bolt://localhost:7687",
    AuthTokens.basic("neo4j", "Ada4Rho")
  ).asScala[Future]

  def addBlock(b: BlockMessage) = {
    val cb = BlockGraph.addBlockQuery(b)
    driver.writeSession(session => cb.query.single(session))
  }
}
