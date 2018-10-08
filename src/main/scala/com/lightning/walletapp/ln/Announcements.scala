package com.lightning.walletapp.ln

import fr.acinq.bitcoin._
import scodec.bits.BitVector


object Announcements { me =>
  // The creating node MUST set node-id-1 and node-id-2 to the public keys of the
  // two nodes who are operating the channel, such that node-id-1 is the numerically-lesser
  // of the two DER encoded keys sorted in ascending numerical order

  def isNode1(localNodeId: BinaryData, remoteNodeId: BinaryData) =
    LexicographicalOrdering.isLessThan(localNodeId, remoteNodeId)

  // BOLT 7: The creating node [...] MUST set the direction bit of flags to 0 if
  // the creating node is node-id-1 in that message, otherwise 1

  def isNode1(flags: BinaryData) = !BitVector(flags.data).reverse.get(0)

  // A node MAY create and send a channel_update with the disable
  // bit set to signal the temporary unavailability of a channel

  def isDisabled(flags: BinaryData) = BitVector(flags.data).reverse.get(1)
}