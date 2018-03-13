package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import freechips.rocketchip.config.Field

case class DataBusParams(
  beatBytes: Int,
  blockBytes: Int,
  masterBuffering: BufferParams = BufferParams.default,
  slaveBuffering: BufferParams = BufferParams.default
) extends TLBusParams

case object DataBusKey extends Field[DataBusParams]

class DataBus(params: DataBusParams)(implicit p: Parameters) extends TLBusWrapper(params, "DataBus") {
  
  
  private val master_splitter = LazyModule(new TLSplitter)  // Allows cycle-free connection to external networks
  master_splitter.suggestName(s"${busName}_master_TLSplitter")
  inwardNode :=* master_splitter.node
  def busView = master_splitter.node.edges.in.head

  protected def inwardSplitNode: TLInwardNode = master_splitter.node
  protected def outwardSplitNode: TLOutwardNode = master_splitter.node


  private val port_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  port_fixer.suggestName(s"${busName}_port_TLFIFOFixer")
  master_splitter.node :=* port_fixer.node

  private val pbus_fixer = LazyModule(new TLFIFOFixer(TLFIFOFixer.all))
  pbus_fixer.suggestName(s"${busName}_pbus_TLFIFOFixer")
  pbus_fixer.node :*= outwardWWNode


  val fromSystemBus: TLInwardNode = {
    
    xbar.node :*= TLBuffer(params.masterBuffering) 
    
  }

  def fromSyncPorts(params: BufferParams =  BufferParams.default, name: Option[String] = None): TLInwardNode = {
    val buffer = LazyModule(new TLBuffer(params))
    name.foreach { n => buffer.suggestName(s"${busName}_${n}_TLBuffer") }
    port_fixer.node :=* buffer.node
    buffer.node
  }



}

trait HasDataBus extends HasSystemBus {
  private val dbusParams = p(DataBusKey)
  val dbusBeatBytes = dbusParams.beatBytes

  val dbus = LazyModule(new DataBus(dbusParams))

  // The Data Bus hangs off of systemBus; here we convert TL-UH -> TL-UL
  dbus.fromSystemBus :*= sbus.toDataBus()
}