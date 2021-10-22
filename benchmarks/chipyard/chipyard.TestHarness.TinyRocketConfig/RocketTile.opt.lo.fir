circuit RocketTile :
  module RocketTile :
    input clock : Clock
    input reset : UInt<1>
    output auto_slave_in_a_ready : UInt<1>
    input auto_slave_in_a_valid : UInt<1>
    input auto_slave_in_a_bits_opcode : UInt<3>
    input auto_slave_in_a_bits_param : UInt<3>
    input auto_slave_in_a_bits_size : UInt<3>
    input auto_slave_in_a_bits_source : UInt<3>
    input auto_slave_in_a_bits_address : UInt<32>
    input auto_slave_in_a_bits_mask : UInt<4>
    input auto_slave_in_a_bits_data : UInt<32>
    input auto_slave_in_a_bits_corrupt : UInt<1>
    input auto_slave_in_d_ready : UInt<1>
    output auto_slave_in_d_valid : UInt<1>
    output auto_slave_in_d_bits_opcode : UInt<3>
    output auto_slave_in_d_bits_param : UInt<2>
    output auto_slave_in_d_bits_size : UInt<3>
    output auto_slave_in_d_bits_source : UInt<3>
    output auto_slave_in_d_bits_sink : UInt<1>
    output auto_slave_in_d_bits_denied : UInt<1>
    output auto_slave_in_d_bits_data : UInt<32>
    output auto_slave_in_d_bits_corrupt : UInt<1>
    output auto_wfi_out_0 : UInt<1>
    output auto_cease_out_0 : UInt<1>
    output auto_halt_out_0 : UInt<1>
    input auto_int_local_in_2_0 : UInt<1>
    input auto_int_local_in_1_0 : UInt<1>
    input auto_int_local_in_1_1 : UInt<1>
    input auto_int_local_in_0_0 : UInt<1>
    output auto_trace_core_source_out_group_0_iretire : UInt<1>
    output auto_trace_core_source_out_group_0_iaddr : UInt<32>
    output auto_trace_core_source_out_group_0_itype : UInt<4>
    output auto_trace_core_source_out_group_0_ilastsize : UInt<1>
    output auto_trace_core_source_out_priv : UInt<4>
    output auto_trace_core_source_out_tval : UInt<32>
    output auto_trace_core_source_out_cause : UInt<32>
    input auto_reset_vector_in : UInt<32>
    input auto_hartid_in : UInt<1>
    input auto_tl_other_masters_out_a_ready : UInt<1>
    output auto_tl_other_masters_out_a_valid : UInt<1>
    output auto_tl_other_masters_out_a_bits_opcode : UInt<3>
    output auto_tl_other_masters_out_a_bits_param : UInt<3>
    output auto_tl_other_masters_out_a_bits_size : UInt<4>
    output auto_tl_other_masters_out_a_bits_source : UInt<1>
    output auto_tl_other_masters_out_a_bits_address : UInt<32>
    output auto_tl_other_masters_out_a_bits_mask : UInt<4>
    output auto_tl_other_masters_out_a_bits_data : UInt<32>
    output auto_tl_other_masters_out_a_bits_corrupt : UInt<1>
    output auto_tl_other_masters_out_d_ready : UInt<1>
    input auto_tl_other_masters_out_d_valid : UInt<1>
    input auto_tl_other_masters_out_d_bits_opcode : UInt<3>
    input auto_tl_other_masters_out_d_bits_param : UInt<2>
    input auto_tl_other_masters_out_d_bits_size : UInt<4>
    input auto_tl_other_masters_out_d_bits_source : UInt<1>
    input auto_tl_other_masters_out_d_bits_sink : UInt<1>
    input auto_tl_other_masters_out_d_bits_denied : UInt<1>
    input auto_tl_other_masters_out_d_bits_data : UInt<32>
    input auto_tl_other_masters_out_d_bits_corrupt : UInt<1>

    inst tlMasterXbar of TLXbar_6 @[BaseTile.scala 195:42]
    inst tlSlaveXbar of TLXbar_7 @[BaseTile.scala 196:41]
    inst intXbar of IntXbar_1 @[BaseTile.scala 197:37]
    inst broadcast of BundleBridgeNexus_4 @[BundleBridge.scala 196:31]
    inst broadcast_1 of BundleBridgeNexus_5 @[BundleBridge.scala 196:31]
    inst dcache of DCache @[HellaCache.scala 265:43]
    inst frontend of Frontend @[Frontend.scala 350:28]
    inst dtim_adapter of ScratchpadSlavePort @[RocketTile.scala 62:15]
    inst fragmenter_1 of TLFragmenter_8 @[Fragmenter.scala 333:34]
    inst dcacheArb of HellaCacheArbiter @[HellaCache.scala 276:25]
    inst ptw of PTW @[PTW.scala 439:19]
    inst core of Rocket @[RocketTile.scala 140:20]
    reg bundleOut_0_0_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), bundleOut_0_0_REG) @[Interrupts.scala 129:36]
    auto_slave_in_a_ready <= tlSlaveXbar.auto_in_a_ready @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_valid <= tlSlaveXbar.auto_in_d_valid @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_bits_opcode <= tlSlaveXbar.auto_in_d_bits_opcode @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_bits_param <= UInt<2>("h0") @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_bits_size <= tlSlaveXbar.auto_in_d_bits_size @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_bits_source <= tlSlaveXbar.auto_in_d_bits_source @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_bits_sink <= UInt<1>("h0") @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_bits_denied <= UInt<1>("h0") @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_bits_data <= tlSlaveXbar.auto_in_d_bits_data @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_slave_in_d_bits_corrupt <= UInt<1>("h0") @[Nodes.scala 1207:84 LazyModule.scala 298:16]
    auto_wfi_out_0 <= bundleOut_0_0_REG @[Nodes.scala 1207:84 Interrupts.scala 129:12]
    auto_cease_out_0 <= UInt<1>("h0") @[Interrupts.scala 115:14 Nodes.scala 1207:84]
    auto_halt_out_0 <= UInt<1>("h0") @[Nodes.scala 1207:84 Interrupts.scala 95:28]
    auto_trace_core_source_out_group_0_iretire <= UInt<1>("h0") @[LazyModule.scala 311:12]
    auto_trace_core_source_out_group_0_iaddr <= UInt<32>("h0") @[LazyModule.scala 311:12]
    auto_trace_core_source_out_group_0_itype <= UInt<4>("h0") @[LazyModule.scala 311:12]
    auto_trace_core_source_out_group_0_ilastsize <= UInt<1>("h0") @[LazyModule.scala 311:12]
    auto_trace_core_source_out_priv <= UInt<4>("h0") @[LazyModule.scala 311:12]
    auto_trace_core_source_out_tval <= UInt<32>("h0") @[LazyModule.scala 311:12]
    auto_trace_core_source_out_cause <= UInt<32>("h0") @[LazyModule.scala 311:12]
    auto_tl_other_masters_out_a_valid <= tlMasterXbar.auto_out_a_valid @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_a_bits_opcode <= tlMasterXbar.auto_out_a_bits_opcode @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_a_bits_param <= tlMasterXbar.auto_out_a_bits_param @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_a_bits_size <= tlMasterXbar.auto_out_a_bits_size @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_a_bits_source <= tlMasterXbar.auto_out_a_bits_source @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_a_bits_address <= tlMasterXbar.auto_out_a_bits_address @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_a_bits_mask <= tlMasterXbar.auto_out_a_bits_mask @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_a_bits_data <= tlMasterXbar.auto_out_a_bits_data @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_a_bits_corrupt <= UInt<1>("h0") @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    auto_tl_other_masters_out_d_ready <= tlMasterXbar.auto_out_d_ready @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    tlMasterXbar.clock <= clock
    tlMasterXbar.reset <= reset
    tlMasterXbar.auto_in_1_a_valid <= frontend.auto_icache_master_out_a_valid @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_1_a_bits_address <= frontend.auto_icache_master_out_a_bits_address @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_0_a_valid <= dcache.auto_out_a_valid @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_0_a_bits_opcode <= dcache.auto_out_a_bits_opcode @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_0_a_bits_param <= dcache.auto_out_a_bits_param @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_0_a_bits_size <= dcache.auto_out_a_bits_size @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_0_a_bits_address <= dcache.auto_out_a_bits_address @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_0_a_bits_mask <= dcache.auto_out_a_bits_mask @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_0_a_bits_data <= dcache.auto_out_a_bits_data @[LazyModule.scala 296:16]
    tlMasterXbar.auto_in_0_d_ready <= dcache.auto_out_d_ready @[LazyModule.scala 296:16]
    tlMasterXbar.auto_out_a_ready <= auto_tl_other_masters_out_a_ready @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    tlMasterXbar.auto_out_d_valid <= auto_tl_other_masters_out_d_valid @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    tlMasterXbar.auto_out_d_bits_opcode <= auto_tl_other_masters_out_d_bits_opcode @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    tlMasterXbar.auto_out_d_bits_size <= auto_tl_other_masters_out_d_bits_size @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    tlMasterXbar.auto_out_d_bits_source <= auto_tl_other_masters_out_d_bits_source @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    tlMasterXbar.auto_out_d_bits_denied <= auto_tl_other_masters_out_d_bits_denied @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    tlMasterXbar.auto_out_d_bits_data <= auto_tl_other_masters_out_d_bits_data @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    tlMasterXbar.auto_out_d_bits_corrupt <= auto_tl_other_masters_out_d_bits_corrupt @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    tlSlaveXbar.auto_in_a_valid <= auto_slave_in_a_valid @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_in_a_bits_opcode <= auto_slave_in_a_bits_opcode @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_in_a_bits_param <= auto_slave_in_a_bits_param @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_in_a_bits_size <= auto_slave_in_a_bits_size @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_in_a_bits_source <= auto_slave_in_a_bits_source @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_in_a_bits_address <= auto_slave_in_a_bits_address @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_in_a_bits_mask <= auto_slave_in_a_bits_mask @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_in_a_bits_data <= auto_slave_in_a_bits_data @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_in_d_ready <= auto_slave_in_d_ready @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    tlSlaveXbar.auto_out_a_ready <= fragmenter_1.auto_in_a_ready @[LazyModule.scala 298:16]
    tlSlaveXbar.auto_out_d_valid <= fragmenter_1.auto_in_d_valid @[LazyModule.scala 298:16]
    tlSlaveXbar.auto_out_d_bits_opcode <= fragmenter_1.auto_in_d_bits_opcode @[LazyModule.scala 298:16]
    tlSlaveXbar.auto_out_d_bits_size <= fragmenter_1.auto_in_d_bits_size @[LazyModule.scala 298:16]
    tlSlaveXbar.auto_out_d_bits_source <= fragmenter_1.auto_in_d_bits_source @[LazyModule.scala 298:16]
    tlSlaveXbar.auto_out_d_bits_data <= fragmenter_1.auto_in_d_bits_data @[LazyModule.scala 298:16]
    intXbar.auto_int_in_2_0 <= auto_int_local_in_2_0 @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    intXbar.auto_int_in_1_0 <= auto_int_local_in_1_0 @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    intXbar.auto_int_in_1_1 <= auto_int_local_in_1_1 @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    intXbar.auto_int_in_0_0 <= auto_int_local_in_0_0 @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    broadcast.auto_in <= auto_hartid_in @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    broadcast_1.auto_in <= auto_reset_vector_in @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    dcache.gated_clock <= clock
    dcache.reset <= reset
    dcache.auto_out_a_ready <= tlMasterXbar.auto_in_0_a_ready @[LazyModule.scala 296:16]
    dcache.auto_out_d_valid <= tlMasterXbar.auto_in_0_d_valid @[LazyModule.scala 296:16]
    dcache.auto_out_d_bits_opcode <= tlMasterXbar.auto_in_0_d_bits_opcode @[LazyModule.scala 296:16]
    dcache.auto_out_d_bits_size <= tlMasterXbar.auto_in_0_d_bits_size @[LazyModule.scala 296:16]
    dcache.auto_out_d_bits_denied <= tlMasterXbar.auto_in_0_d_bits_denied @[LazyModule.scala 296:16]
    dcache.auto_out_d_bits_data <= tlMasterXbar.auto_in_0_d_bits_data @[LazyModule.scala 296:16]
    dcache.io_cpu_req_valid <= dcacheArb.io_mem_req_valid @[HellaCache.scala 277:30]
    dcache.io_cpu_req_bits_addr <= dcacheArb.io_mem_req_bits_addr @[HellaCache.scala 277:30]
    dcache.io_cpu_req_bits_tag <= dcacheArb.io_mem_req_bits_tag @[HellaCache.scala 277:30]
    dcache.io_cpu_req_bits_cmd <= dcacheArb.io_mem_req_bits_cmd @[HellaCache.scala 277:30]
    dcache.io_cpu_req_bits_size <= dcacheArb.io_mem_req_bits_size @[HellaCache.scala 277:30]
    dcache.io_cpu_req_bits_signed <= dcacheArb.io_mem_req_bits_signed @[HellaCache.scala 277:30]
    dcache.io_cpu_req_bits_no_xcpt <= dcacheArb.io_mem_req_bits_no_xcpt @[HellaCache.scala 277:30]
    dcache.io_cpu_s1_kill <= dcacheArb.io_mem_s1_kill @[HellaCache.scala 277:30]
    dcache.io_cpu_s1_data_data <= dcacheArb.io_mem_s1_data_data @[HellaCache.scala 277:30]
    dcache.io_cpu_s1_data_mask <= dcacheArb.io_mem_s1_data_mask @[HellaCache.scala 277:30]
    dcache.io_ptw_status_debug <= ptw.io_requestor_0_status_debug @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_0_cfg_l <= ptw.io_requestor_0_pmp_0_cfg_l @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_0_cfg_a <= ptw.io_requestor_0_pmp_0_cfg_a @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_0_cfg_x <= ptw.io_requestor_0_pmp_0_cfg_x @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_0_cfg_w <= ptw.io_requestor_0_pmp_0_cfg_w @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_0_cfg_r <= ptw.io_requestor_0_pmp_0_cfg_r @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_0_addr <= ptw.io_requestor_0_pmp_0_addr @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_0_mask <= ptw.io_requestor_0_pmp_0_mask @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_1_cfg_l <= ptw.io_requestor_0_pmp_1_cfg_l @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_1_cfg_a <= ptw.io_requestor_0_pmp_1_cfg_a @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_1_cfg_x <= ptw.io_requestor_0_pmp_1_cfg_x @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_1_cfg_w <= ptw.io_requestor_0_pmp_1_cfg_w @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_1_cfg_r <= ptw.io_requestor_0_pmp_1_cfg_r @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_1_addr <= ptw.io_requestor_0_pmp_1_addr @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_1_mask <= ptw.io_requestor_0_pmp_1_mask @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_2_cfg_l <= ptw.io_requestor_0_pmp_2_cfg_l @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_2_cfg_a <= ptw.io_requestor_0_pmp_2_cfg_a @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_2_cfg_x <= ptw.io_requestor_0_pmp_2_cfg_x @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_2_cfg_w <= ptw.io_requestor_0_pmp_2_cfg_w @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_2_cfg_r <= ptw.io_requestor_0_pmp_2_cfg_r @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_2_addr <= ptw.io_requestor_0_pmp_2_addr @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_2_mask <= ptw.io_requestor_0_pmp_2_mask @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_3_cfg_l <= ptw.io_requestor_0_pmp_3_cfg_l @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_3_cfg_a <= ptw.io_requestor_0_pmp_3_cfg_a @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_3_cfg_x <= ptw.io_requestor_0_pmp_3_cfg_x @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_3_cfg_w <= ptw.io_requestor_0_pmp_3_cfg_w @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_3_cfg_r <= ptw.io_requestor_0_pmp_3_cfg_r @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_3_addr <= ptw.io_requestor_0_pmp_3_addr @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_3_mask <= ptw.io_requestor_0_pmp_3_mask @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_4_cfg_l <= ptw.io_requestor_0_pmp_4_cfg_l @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_4_cfg_a <= ptw.io_requestor_0_pmp_4_cfg_a @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_4_cfg_x <= ptw.io_requestor_0_pmp_4_cfg_x @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_4_cfg_w <= ptw.io_requestor_0_pmp_4_cfg_w @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_4_cfg_r <= ptw.io_requestor_0_pmp_4_cfg_r @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_4_addr <= ptw.io_requestor_0_pmp_4_addr @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_4_mask <= ptw.io_requestor_0_pmp_4_mask @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_5_cfg_l <= ptw.io_requestor_0_pmp_5_cfg_l @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_5_cfg_a <= ptw.io_requestor_0_pmp_5_cfg_a @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_5_cfg_x <= ptw.io_requestor_0_pmp_5_cfg_x @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_5_cfg_w <= ptw.io_requestor_0_pmp_5_cfg_w @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_5_cfg_r <= ptw.io_requestor_0_pmp_5_cfg_r @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_5_addr <= ptw.io_requestor_0_pmp_5_addr @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_5_mask <= ptw.io_requestor_0_pmp_5_mask @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_6_cfg_l <= ptw.io_requestor_0_pmp_6_cfg_l @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_6_cfg_a <= ptw.io_requestor_0_pmp_6_cfg_a @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_6_cfg_x <= ptw.io_requestor_0_pmp_6_cfg_x @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_6_cfg_w <= ptw.io_requestor_0_pmp_6_cfg_w @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_6_cfg_r <= ptw.io_requestor_0_pmp_6_cfg_r @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_6_addr <= ptw.io_requestor_0_pmp_6_addr @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_6_mask <= ptw.io_requestor_0_pmp_6_mask @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_7_cfg_l <= ptw.io_requestor_0_pmp_7_cfg_l @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_7_cfg_a <= ptw.io_requestor_0_pmp_7_cfg_a @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_7_cfg_x <= ptw.io_requestor_0_pmp_7_cfg_x @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_7_cfg_w <= ptw.io_requestor_0_pmp_7_cfg_w @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_7_cfg_r <= ptw.io_requestor_0_pmp_7_cfg_r @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_7_addr <= ptw.io_requestor_0_pmp_7_addr @[RocketTile.scala 198:20]
    dcache.io_ptw_pmp_7_mask <= ptw.io_requestor_0_pmp_7_mask @[RocketTile.scala 198:20]
    frontend.gated_clock <= clock
    frontend.reset <= reset
    frontend.auto_icache_master_out_a_ready <= tlMasterXbar.auto_in_1_a_ready @[LazyModule.scala 296:16]
    frontend.auto_icache_master_out_d_valid <= tlMasterXbar.auto_in_1_d_valid @[LazyModule.scala 296:16]
    frontend.auto_icache_master_out_d_bits_opcode <= tlMasterXbar.auto_in_1_d_bits_opcode @[LazyModule.scala 296:16]
    frontend.auto_icache_master_out_d_bits_size <= tlMasterXbar.auto_in_1_d_bits_size @[LazyModule.scala 296:16]
    frontend.auto_icache_master_out_d_bits_data <= tlMasterXbar.auto_in_1_d_bits_data @[LazyModule.scala 296:16]
    frontend.auto_icache_master_out_d_bits_corrupt <= tlMasterXbar.auto_in_1_d_bits_corrupt @[LazyModule.scala 296:16]
    frontend.auto_reset_vector_sink_in <= broadcast_1.auto_out_1 @[LazyModule.scala 298:16]
    frontend.io_cpu_might_request <= core.io_imem_might_request @[RocketTile.scala 173:32]
    frontend.io_cpu_req_valid <= core.io_imem_req_valid @[RocketTile.scala 173:32]
    frontend.io_cpu_req_bits_pc <= core.io_imem_req_bits_pc @[RocketTile.scala 173:32]
    frontend.io_cpu_req_bits_speculative <= core.io_imem_req_bits_speculative @[RocketTile.scala 173:32]
    frontend.io_cpu_resp_ready <= core.io_imem_resp_ready @[RocketTile.scala 173:32]
    frontend.io_cpu_btb_update_valid <= core.io_imem_btb_update_valid @[RocketTile.scala 173:32]
    frontend.io_cpu_bht_update_valid <= core.io_imem_bht_update_valid @[RocketTile.scala 173:32]
    frontend.io_cpu_flush_icache <= core.io_imem_flush_icache @[RocketTile.scala 173:32]
    frontend.io_ptw_status_debug <= ptw.io_requestor_1_status_debug @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_0_cfg_l <= ptw.io_requestor_1_pmp_0_cfg_l @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_0_cfg_a <= ptw.io_requestor_1_pmp_0_cfg_a @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_0_cfg_x <= ptw.io_requestor_1_pmp_0_cfg_x @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_0_cfg_w <= ptw.io_requestor_1_pmp_0_cfg_w @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_0_cfg_r <= ptw.io_requestor_1_pmp_0_cfg_r @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_0_addr <= ptw.io_requestor_1_pmp_0_addr @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_0_mask <= ptw.io_requestor_1_pmp_0_mask @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_1_cfg_l <= ptw.io_requestor_1_pmp_1_cfg_l @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_1_cfg_a <= ptw.io_requestor_1_pmp_1_cfg_a @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_1_cfg_x <= ptw.io_requestor_1_pmp_1_cfg_x @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_1_cfg_w <= ptw.io_requestor_1_pmp_1_cfg_w @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_1_cfg_r <= ptw.io_requestor_1_pmp_1_cfg_r @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_1_addr <= ptw.io_requestor_1_pmp_1_addr @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_1_mask <= ptw.io_requestor_1_pmp_1_mask @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_2_cfg_l <= ptw.io_requestor_1_pmp_2_cfg_l @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_2_cfg_a <= ptw.io_requestor_1_pmp_2_cfg_a @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_2_cfg_x <= ptw.io_requestor_1_pmp_2_cfg_x @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_2_cfg_w <= ptw.io_requestor_1_pmp_2_cfg_w @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_2_cfg_r <= ptw.io_requestor_1_pmp_2_cfg_r @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_2_addr <= ptw.io_requestor_1_pmp_2_addr @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_2_mask <= ptw.io_requestor_1_pmp_2_mask @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_3_cfg_l <= ptw.io_requestor_1_pmp_3_cfg_l @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_3_cfg_a <= ptw.io_requestor_1_pmp_3_cfg_a @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_3_cfg_x <= ptw.io_requestor_1_pmp_3_cfg_x @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_3_cfg_w <= ptw.io_requestor_1_pmp_3_cfg_w @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_3_cfg_r <= ptw.io_requestor_1_pmp_3_cfg_r @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_3_addr <= ptw.io_requestor_1_pmp_3_addr @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_3_mask <= ptw.io_requestor_1_pmp_3_mask @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_4_cfg_l <= ptw.io_requestor_1_pmp_4_cfg_l @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_4_cfg_a <= ptw.io_requestor_1_pmp_4_cfg_a @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_4_cfg_x <= ptw.io_requestor_1_pmp_4_cfg_x @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_4_cfg_w <= ptw.io_requestor_1_pmp_4_cfg_w @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_4_cfg_r <= ptw.io_requestor_1_pmp_4_cfg_r @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_4_addr <= ptw.io_requestor_1_pmp_4_addr @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_4_mask <= ptw.io_requestor_1_pmp_4_mask @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_5_cfg_l <= ptw.io_requestor_1_pmp_5_cfg_l @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_5_cfg_a <= ptw.io_requestor_1_pmp_5_cfg_a @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_5_cfg_x <= ptw.io_requestor_1_pmp_5_cfg_x @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_5_cfg_w <= ptw.io_requestor_1_pmp_5_cfg_w @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_5_cfg_r <= ptw.io_requestor_1_pmp_5_cfg_r @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_5_addr <= ptw.io_requestor_1_pmp_5_addr @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_5_mask <= ptw.io_requestor_1_pmp_5_mask @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_6_cfg_l <= ptw.io_requestor_1_pmp_6_cfg_l @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_6_cfg_a <= ptw.io_requestor_1_pmp_6_cfg_a @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_6_cfg_x <= ptw.io_requestor_1_pmp_6_cfg_x @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_6_cfg_w <= ptw.io_requestor_1_pmp_6_cfg_w @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_6_cfg_r <= ptw.io_requestor_1_pmp_6_cfg_r @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_6_addr <= ptw.io_requestor_1_pmp_6_addr @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_6_mask <= ptw.io_requestor_1_pmp_6_mask @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_7_cfg_l <= ptw.io_requestor_1_pmp_7_cfg_l @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_7_cfg_a <= ptw.io_requestor_1_pmp_7_cfg_a @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_7_cfg_x <= ptw.io_requestor_1_pmp_7_cfg_x @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_7_cfg_w <= ptw.io_requestor_1_pmp_7_cfg_w @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_7_cfg_r <= ptw.io_requestor_1_pmp_7_cfg_r @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_7_addr <= ptw.io_requestor_1_pmp_7_addr @[RocketTile.scala 198:20]
    frontend.io_ptw_pmp_7_mask <= ptw.io_requestor_1_pmp_7_mask @[RocketTile.scala 198:20]
    frontend.io_ptw_customCSRs_csrs_0_value <= ptw.io_requestor_1_customCSRs_csrs_0_value @[RocketTile.scala 198:20]
    dtim_adapter.clock <= clock
    dtim_adapter.reset <= reset
    dtim_adapter.auto_in_a_valid <= fragmenter_1.auto_out_a_valid @[LazyModule.scala 296:16]
    dtim_adapter.auto_in_a_bits_opcode <= fragmenter_1.auto_out_a_bits_opcode @[LazyModule.scala 296:16]
    dtim_adapter.auto_in_a_bits_param <= fragmenter_1.auto_out_a_bits_param @[LazyModule.scala 296:16]
    dtim_adapter.auto_in_a_bits_size <= fragmenter_1.auto_out_a_bits_size @[LazyModule.scala 296:16]
    dtim_adapter.auto_in_a_bits_source <= fragmenter_1.auto_out_a_bits_source @[LazyModule.scala 296:16]
    dtim_adapter.auto_in_a_bits_address <= fragmenter_1.auto_out_a_bits_address @[LazyModule.scala 296:16]
    dtim_adapter.auto_in_a_bits_mask <= fragmenter_1.auto_out_a_bits_mask @[LazyModule.scala 296:16]
    dtim_adapter.auto_in_a_bits_data <= fragmenter_1.auto_out_a_bits_data @[LazyModule.scala 296:16]
    dtim_adapter.auto_in_d_ready <= fragmenter_1.auto_out_d_ready @[LazyModule.scala 296:16]
    dtim_adapter.io_dmem_req_ready <= dcacheArb.io_requestor_1_req_ready @[RocketTile.scala 197:26]
    dtim_adapter.io_dmem_s2_nack <= dcacheArb.io_requestor_1_s2_nack @[RocketTile.scala 197:26]
    dtim_adapter.io_dmem_resp_valid <= dcacheArb.io_requestor_1_resp_valid @[RocketTile.scala 197:26]
    dtim_adapter.io_dmem_resp_bits_data_raw <= dcacheArb.io_requestor_1_resp_bits_data_raw @[RocketTile.scala 197:26]
    fragmenter_1.clock <= clock
    fragmenter_1.reset <= reset
    fragmenter_1.auto_in_a_valid <= tlSlaveXbar.auto_out_a_valid @[LazyModule.scala 298:16]
    fragmenter_1.auto_in_a_bits_opcode <= tlSlaveXbar.auto_out_a_bits_opcode @[LazyModule.scala 298:16]
    fragmenter_1.auto_in_a_bits_param <= tlSlaveXbar.auto_out_a_bits_param @[LazyModule.scala 298:16]
    fragmenter_1.auto_in_a_bits_size <= tlSlaveXbar.auto_out_a_bits_size @[LazyModule.scala 298:16]
    fragmenter_1.auto_in_a_bits_source <= tlSlaveXbar.auto_out_a_bits_source @[LazyModule.scala 298:16]
    fragmenter_1.auto_in_a_bits_address <= tlSlaveXbar.auto_out_a_bits_address @[LazyModule.scala 298:16]
    fragmenter_1.auto_in_a_bits_mask <= tlSlaveXbar.auto_out_a_bits_mask @[LazyModule.scala 298:16]
    fragmenter_1.auto_in_a_bits_data <= tlSlaveXbar.auto_out_a_bits_data @[LazyModule.scala 298:16]
    fragmenter_1.auto_in_d_ready <= tlSlaveXbar.auto_out_d_ready @[LazyModule.scala 298:16]
    fragmenter_1.auto_out_a_ready <= dtim_adapter.auto_in_a_ready @[LazyModule.scala 296:16]
    fragmenter_1.auto_out_d_valid <= dtim_adapter.auto_in_d_valid @[LazyModule.scala 296:16]
    fragmenter_1.auto_out_d_bits_opcode <= dtim_adapter.auto_in_d_bits_opcode @[LazyModule.scala 296:16]
    fragmenter_1.auto_out_d_bits_size <= dtim_adapter.auto_in_d_bits_size @[LazyModule.scala 296:16]
    fragmenter_1.auto_out_d_bits_source <= dtim_adapter.auto_in_d_bits_source @[LazyModule.scala 296:16]
    fragmenter_1.auto_out_d_bits_data <= dtim_adapter.auto_in_d_bits_data @[LazyModule.scala 296:16]
    dcacheArb.clock <= clock
    dcacheArb.io_requestor_0_req_valid <= core.io_dmem_req_valid @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_0_req_bits_addr <= core.io_dmem_req_bits_addr @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_0_req_bits_tag <= core.io_dmem_req_bits_tag @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_0_req_bits_cmd <= core.io_dmem_req_bits_cmd @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_0_req_bits_size <= core.io_dmem_req_bits_size @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_0_req_bits_signed <= core.io_dmem_req_bits_signed @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_0_s1_kill <= core.io_dmem_s1_kill @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_0_s1_data_data <= core.io_dmem_s1_data_data @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_1_req_valid <= dtim_adapter.io_dmem_req_valid @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_1_req_bits_addr <= dtim_adapter.io_dmem_req_bits_addr @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_1_req_bits_cmd <= dtim_adapter.io_dmem_req_bits_cmd @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_1_req_bits_size <= dtim_adapter.io_dmem_req_bits_size @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_1_s1_kill <= dtim_adapter.io_dmem_s1_kill @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_1_s1_data_data <= dtim_adapter.io_dmem_s1_data_data @[RocketTile.scala 197:26]
    dcacheArb.io_requestor_1_s1_data_mask <= dtim_adapter.io_dmem_s1_data_mask @[RocketTile.scala 197:26]
    dcacheArb.io_mem_req_ready <= dcache.io_cpu_req_ready @[HellaCache.scala 277:30]
    dcacheArb.io_mem_s2_nack <= dcache.io_cpu_s2_nack @[HellaCache.scala 277:30]
    dcacheArb.io_mem_resp_valid <= dcache.io_cpu_resp_valid @[HellaCache.scala 277:30]
    dcacheArb.io_mem_resp_bits_tag <= dcache.io_cpu_resp_bits_tag @[HellaCache.scala 277:30]
    dcacheArb.io_mem_resp_bits_data <= dcache.io_cpu_resp_bits_data @[HellaCache.scala 277:30]
    dcacheArb.io_mem_resp_bits_replay <= dcache.io_cpu_resp_bits_replay @[HellaCache.scala 277:30]
    dcacheArb.io_mem_resp_bits_has_data <= dcache.io_cpu_resp_bits_has_data @[HellaCache.scala 277:30]
    dcacheArb.io_mem_resp_bits_data_word_bypass <= dcache.io_cpu_resp_bits_data_word_bypass @[HellaCache.scala 277:30]
    dcacheArb.io_mem_resp_bits_data_raw <= dcache.io_cpu_resp_bits_data_raw @[HellaCache.scala 277:30]
    dcacheArb.io_mem_replay_next <= dcache.io_cpu_replay_next @[HellaCache.scala 277:30]
    dcacheArb.io_mem_s2_xcpt_ma_ld <= dcache.io_cpu_s2_xcpt_ma_ld @[HellaCache.scala 277:30]
    dcacheArb.io_mem_s2_xcpt_ma_st <= dcache.io_cpu_s2_xcpt_ma_st @[HellaCache.scala 277:30]
    dcacheArb.io_mem_s2_xcpt_pf_ld <= dcache.io_cpu_s2_xcpt_pf_ld @[HellaCache.scala 277:30]
    dcacheArb.io_mem_s2_xcpt_pf_st <= dcache.io_cpu_s2_xcpt_pf_st @[HellaCache.scala 277:30]
    dcacheArb.io_mem_s2_xcpt_ae_ld <= dcache.io_cpu_s2_xcpt_ae_ld @[HellaCache.scala 277:30]
    dcacheArb.io_mem_s2_xcpt_ae_st <= dcache.io_cpu_s2_xcpt_ae_st @[HellaCache.scala 277:30]
    dcacheArb.io_mem_ordered <= dcache.io_cpu_ordered @[HellaCache.scala 277:30]
    dcacheArb.io_mem_perf_grant <= dcache.io_cpu_perf_grant @[HellaCache.scala 277:30]
    ptw.clock <= clock
    ptw.reset <= reset
    ptw.io_dpath_status_debug <= core.io_ptw_status_debug @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_0_cfg_l <= core.io_ptw_pmp_0_cfg_l @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_0_cfg_a <= core.io_ptw_pmp_0_cfg_a @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_0_cfg_x <= core.io_ptw_pmp_0_cfg_x @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_0_cfg_w <= core.io_ptw_pmp_0_cfg_w @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_0_cfg_r <= core.io_ptw_pmp_0_cfg_r @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_0_addr <= core.io_ptw_pmp_0_addr @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_0_mask <= core.io_ptw_pmp_0_mask @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_1_cfg_l <= core.io_ptw_pmp_1_cfg_l @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_1_cfg_a <= core.io_ptw_pmp_1_cfg_a @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_1_cfg_x <= core.io_ptw_pmp_1_cfg_x @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_1_cfg_w <= core.io_ptw_pmp_1_cfg_w @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_1_cfg_r <= core.io_ptw_pmp_1_cfg_r @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_1_addr <= core.io_ptw_pmp_1_addr @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_1_mask <= core.io_ptw_pmp_1_mask @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_2_cfg_l <= core.io_ptw_pmp_2_cfg_l @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_2_cfg_a <= core.io_ptw_pmp_2_cfg_a @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_2_cfg_x <= core.io_ptw_pmp_2_cfg_x @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_2_cfg_w <= core.io_ptw_pmp_2_cfg_w @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_2_cfg_r <= core.io_ptw_pmp_2_cfg_r @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_2_addr <= core.io_ptw_pmp_2_addr @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_2_mask <= core.io_ptw_pmp_2_mask @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_3_cfg_l <= core.io_ptw_pmp_3_cfg_l @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_3_cfg_a <= core.io_ptw_pmp_3_cfg_a @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_3_cfg_x <= core.io_ptw_pmp_3_cfg_x @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_3_cfg_w <= core.io_ptw_pmp_3_cfg_w @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_3_cfg_r <= core.io_ptw_pmp_3_cfg_r @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_3_addr <= core.io_ptw_pmp_3_addr @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_3_mask <= core.io_ptw_pmp_3_mask @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_4_cfg_l <= core.io_ptw_pmp_4_cfg_l @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_4_cfg_a <= core.io_ptw_pmp_4_cfg_a @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_4_cfg_x <= core.io_ptw_pmp_4_cfg_x @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_4_cfg_w <= core.io_ptw_pmp_4_cfg_w @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_4_cfg_r <= core.io_ptw_pmp_4_cfg_r @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_4_addr <= core.io_ptw_pmp_4_addr @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_4_mask <= core.io_ptw_pmp_4_mask @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_5_cfg_l <= core.io_ptw_pmp_5_cfg_l @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_5_cfg_a <= core.io_ptw_pmp_5_cfg_a @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_5_cfg_x <= core.io_ptw_pmp_5_cfg_x @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_5_cfg_w <= core.io_ptw_pmp_5_cfg_w @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_5_cfg_r <= core.io_ptw_pmp_5_cfg_r @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_5_addr <= core.io_ptw_pmp_5_addr @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_5_mask <= core.io_ptw_pmp_5_mask @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_6_cfg_l <= core.io_ptw_pmp_6_cfg_l @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_6_cfg_a <= core.io_ptw_pmp_6_cfg_a @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_6_cfg_x <= core.io_ptw_pmp_6_cfg_x @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_6_cfg_w <= core.io_ptw_pmp_6_cfg_w @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_6_cfg_r <= core.io_ptw_pmp_6_cfg_r @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_6_addr <= core.io_ptw_pmp_6_addr @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_6_mask <= core.io_ptw_pmp_6_mask @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_7_cfg_l <= core.io_ptw_pmp_7_cfg_l @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_7_cfg_a <= core.io_ptw_pmp_7_cfg_a @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_7_cfg_x <= core.io_ptw_pmp_7_cfg_x @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_7_cfg_w <= core.io_ptw_pmp_7_cfg_w @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_7_cfg_r <= core.io_ptw_pmp_7_cfg_r @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_7_addr <= core.io_ptw_pmp_7_addr @[RocketTile.scala 176:15]
    ptw.io_dpath_pmp_7_mask <= core.io_ptw_pmp_7_mask @[RocketTile.scala 176:15]
    ptw.io_dpath_customCSRs_csrs_0_value <= core.io_ptw_customCSRs_csrs_0_value @[RocketTile.scala 176:15]
    core.clock <= clock
    core.reset <= reset
    core.io_hartid <= broadcast.auto_out_0 @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    core.io_interrupts_debug <= intXbar.auto_int_out_0 @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    core.io_interrupts_mtip <= intXbar.auto_int_out_2 @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    core.io_interrupts_msip <= intXbar.auto_int_out_1 @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    core.io_interrupts_meip <= intXbar.auto_int_out_3 @[Nodes.scala 1210:84 LazyModule.scala 296:16]
    core.io_imem_resp_valid <= frontend.io_cpu_resp_valid @[RocketTile.scala 173:32]
    core.io_imem_resp_bits_pc <= frontend.io_cpu_resp_bits_pc @[RocketTile.scala 173:32]
    core.io_imem_resp_bits_data <= frontend.io_cpu_resp_bits_data @[RocketTile.scala 173:32]
    core.io_imem_resp_bits_xcpt_ae_inst <= frontend.io_cpu_resp_bits_xcpt_ae_inst @[RocketTile.scala 173:32]
    core.io_imem_resp_bits_replay <= frontend.io_cpu_resp_bits_replay @[RocketTile.scala 173:32]
    core.io_dmem_req_ready <= dcacheArb.io_requestor_0_req_ready @[RocketTile.scala 197:26]
    core.io_dmem_s2_nack <= dcacheArb.io_requestor_0_s2_nack @[RocketTile.scala 197:26]
    core.io_dmem_resp_valid <= dcacheArb.io_requestor_0_resp_valid @[RocketTile.scala 197:26]
    core.io_dmem_resp_bits_tag <= dcacheArb.io_requestor_0_resp_bits_tag @[RocketTile.scala 197:26]
    core.io_dmem_resp_bits_data <= dcacheArb.io_requestor_0_resp_bits_data @[RocketTile.scala 197:26]
    core.io_dmem_resp_bits_replay <= dcacheArb.io_requestor_0_resp_bits_replay @[RocketTile.scala 197:26]
    core.io_dmem_resp_bits_has_data <= dcacheArb.io_requestor_0_resp_bits_has_data @[RocketTile.scala 197:26]
    core.io_dmem_resp_bits_data_word_bypass <= dcacheArb.io_requestor_0_resp_bits_data_word_bypass @[RocketTile.scala 197:26]
    core.io_dmem_replay_next <= dcacheArb.io_requestor_0_replay_next @[RocketTile.scala 197:26]
    core.io_dmem_s2_xcpt_ma_ld <= dcacheArb.io_requestor_0_s2_xcpt_ma_ld @[RocketTile.scala 197:26]
    core.io_dmem_s2_xcpt_ma_st <= dcacheArb.io_requestor_0_s2_xcpt_ma_st @[RocketTile.scala 197:26]
    core.io_dmem_s2_xcpt_pf_ld <= dcacheArb.io_requestor_0_s2_xcpt_pf_ld @[RocketTile.scala 197:26]
    core.io_dmem_s2_xcpt_pf_st <= dcacheArb.io_requestor_0_s2_xcpt_pf_st @[RocketTile.scala 197:26]
    core.io_dmem_s2_xcpt_ae_ld <= dcacheArb.io_requestor_0_s2_xcpt_ae_ld @[RocketTile.scala 197:26]
    core.io_dmem_s2_xcpt_ae_st <= dcacheArb.io_requestor_0_s2_xcpt_ae_st @[RocketTile.scala 197:26]
    core.io_dmem_ordered <= dcacheArb.io_requestor_0_ordered @[RocketTile.scala 197:26]
    core.io_dmem_perf_grant <= dcacheArb.io_requestor_0_perf_grant @[RocketTile.scala 197:26]
    bundleOut_0_0_REG <= mux(reset, UInt<1>("h0"), core.io_wfi) @[Interrupts.scala 129:{36,36,36}]

  module TLXbar_6 :
    input clock : Clock
    input reset : UInt<1>
    output auto_in_1_a_ready : UInt<1>
    input auto_in_1_a_valid : UInt<1>
    input auto_in_1_a_bits_address : UInt<32>
    output auto_in_1_d_valid : UInt<1>
    output auto_in_1_d_bits_opcode : UInt<3>
    output auto_in_1_d_bits_size : UInt<4>
    output auto_in_1_d_bits_data : UInt<32>
    output auto_in_1_d_bits_corrupt : UInt<1>
    output auto_in_0_a_ready : UInt<1>
    input auto_in_0_a_valid : UInt<1>
    input auto_in_0_a_bits_opcode : UInt<3>
    input auto_in_0_a_bits_param : UInt<3>
    input auto_in_0_a_bits_size : UInt<4>
    input auto_in_0_a_bits_address : UInt<32>
    input auto_in_0_a_bits_mask : UInt<4>
    input auto_in_0_a_bits_data : UInt<32>
    input auto_in_0_d_ready : UInt<1>
    output auto_in_0_d_valid : UInt<1>
    output auto_in_0_d_bits_opcode : UInt<3>
    output auto_in_0_d_bits_size : UInt<4>
    output auto_in_0_d_bits_denied : UInt<1>
    output auto_in_0_d_bits_data : UInt<32>
    input auto_out_a_ready : UInt<1>
    output auto_out_a_valid : UInt<1>
    output auto_out_a_bits_opcode : UInt<3>
    output auto_out_a_bits_param : UInt<3>
    output auto_out_a_bits_size : UInt<4>
    output auto_out_a_bits_source : UInt<1>
    output auto_out_a_bits_address : UInt<32>
    output auto_out_a_bits_mask : UInt<4>
    output auto_out_a_bits_data : UInt<32>
    output auto_out_d_ready : UInt<1>
    input auto_out_d_valid : UInt<1>
    input auto_out_d_bits_opcode : UInt<3>
    input auto_out_d_bits_size : UInt<4>
    input auto_out_d_bits_source : UInt<1>
    input auto_out_d_bits_denied : UInt<1>
    input auto_out_d_bits_data : UInt<32>
    input auto_out_d_bits_corrupt : UInt<1>

    node requestDOI_0_1 = not(auto_out_d_bits_source) @[Parameters.scala 46:9]
    node _beatsAI_decode_T_1 = dshl(UInt<12>("hfff"), auto_in_0_a_bits_size) @[package.scala 234:77]
    node _beatsAI_decode_T_2 = bits(_beatsAI_decode_T_1, 11, 0) @[package.scala 234:82]
    node _beatsAI_decode_T_3 = not(_beatsAI_decode_T_2) @[package.scala 234:46]
    node beatsAI_decode = shr(_beatsAI_decode_T_3, 2) @[Edges.scala 219:59]
    node _beatsAI_opdata_T = bits(auto_in_0_a_bits_opcode, 2, 2) @[Edges.scala 91:37]
    node beatsAI_opdata = not(_beatsAI_opdata_T) @[Edges.scala 91:28]
    node beatsAI_0 = mux(beatsAI_opdata, beatsAI_decode, UInt<10>("h0")) @[Edges.scala 220:14]
    node _portsDIO_out_0_d_ready_T = and(auto_out_d_bits_source, auto_in_0_d_ready) @[Mux.scala 27:72]
    reg beatsLeft : UInt<10>, clock with :
      reset => (UInt<1>("h0"), beatsLeft) @[Arbiter.scala 87:30]
    node idle = eq(beatsLeft, UInt<10>("h0")) @[Arbiter.scala 88:28]
    node latch = and(idle, auto_out_a_ready) @[Arbiter.scala 89:24]
    node readys_filter_lo = cat(auto_in_1_a_valid, auto_in_0_a_valid) @[Cat.scala 30:58]
    reg readys_mask : UInt<2>, clock with :
      reset => (UInt<1>("h0"), readys_mask) @[Arbiter.scala 23:23]
    node _readys_filter_T = not(readys_mask) @[Arbiter.scala 24:30]
    node readys_filter_hi = and(readys_filter_lo, _readys_filter_T) @[Arbiter.scala 24:28]
    node readys_filter = cat(readys_filter_hi, readys_filter_lo) @[Cat.scala 30:58]
    node _readys_unready_T = shr(readys_filter, 1) @[package.scala 253:48]
    node _GEN_1 = pad(_readys_unready_T, 4) @[package.scala 253:43]
    node _readys_unready_T_1 = or(readys_filter, _GEN_1) @[package.scala 253:43]
    node _readys_unready_T_3 = shr(_readys_unready_T_1, 1) @[Arbiter.scala 25:52]
    node _readys_unready_T_4 = shl(readys_mask, 2) @[Arbiter.scala 25:66]
    node _GEN_2 = pad(_readys_unready_T_3, 4) @[Arbiter.scala 25:58]
    node readys_unready = or(_GEN_2, _readys_unready_T_4) @[Arbiter.scala 25:58]
    node _readys_readys_T = shr(readys_unready, 2) @[Arbiter.scala 26:29]
    node _readys_readys_T_1 = bits(readys_unready, 1, 0) @[Arbiter.scala 26:48]
    node _readys_readys_T_2 = and(_readys_readys_T, _readys_readys_T_1) @[Arbiter.scala 26:39]
    node readys_readys = not(_readys_readys_T_2) @[Arbiter.scala 26:18]
    node _readys_T_5 = orr(readys_filter_lo) @[Arbiter.scala 27:27]
    node _readys_T_6 = and(latch, _readys_T_5) @[Arbiter.scala 27:18]
    node _readys_mask_T = and(readys_readys, readys_filter_lo) @[Arbiter.scala 28:29]
    node _readys_mask_T_1 = shl(_readys_mask_T, 1) @[package.scala 244:48]
    node _readys_mask_T_2 = bits(_readys_mask_T_1, 1, 0) @[package.scala 244:53]
    node _readys_mask_T_3 = or(_readys_mask_T, _readys_mask_T_2) @[package.scala 244:43]
    node _GEN_0 = mux(_readys_T_6, _readys_mask_T_3, readys_mask) @[Arbiter.scala 27:32 28:12 23:23]
    node readys_0 = bits(readys_readys, 0, 0) @[Arbiter.scala 95:86]
    node readys_1 = bits(readys_readys, 1, 1) @[Arbiter.scala 95:86]
    node earlyWinner_0 = and(readys_0, auto_in_0_a_valid) @[Arbiter.scala 97:79]
    node earlyWinner_1 = and(readys_1, auto_in_1_a_valid) @[Arbiter.scala 97:79]
    node _prefixOR_T = or(earlyWinner_0, earlyWinner_1) @[Arbiter.scala 104:53]
    node _T_1 = not(earlyWinner_0) @[Arbiter.scala 105:67]
    node _T_4 = not(earlyWinner_1) @[Arbiter.scala 105:67]
    node _T_5 = or(_T_1, _T_4) @[Arbiter.scala 105:64]
    node _T_8 = or(_T_5, reset) @[Arbiter.scala 105:13]
    node _T_9 = not(_T_8) @[Arbiter.scala 105:13]
    node _T_10 = or(auto_in_0_a_valid, auto_in_1_a_valid) @[Arbiter.scala 107:36]
    node _T_11 = not(_T_10) @[Arbiter.scala 107:15]
    node _T_13 = or(_T_11, _prefixOR_T) @[Arbiter.scala 107:41]
    node _T_15 = or(_T_13, reset) @[Arbiter.scala 107:14]
    node _T_16 = not(_T_15) @[Arbiter.scala 107:14]
    node _T_20 = or(_T_11, _T_10) @[Arbiter.scala 108:41]
    node _T_22 = or(_T_20, reset) @[Arbiter.scala 108:14]
    node _T_23 = not(_T_22) @[Arbiter.scala 108:14]
    node maskedBeats_0 = mux(earlyWinner_0, beatsAI_0, UInt<10>("h0")) @[Arbiter.scala 111:73]
    reg state_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), state_0) @[Arbiter.scala 116:26]
    node muxStateEarly_0 = mux(idle, earlyWinner_0, state_0) @[Arbiter.scala 117:30]
    reg state_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), state_1) @[Arbiter.scala 116:26]
    node muxStateEarly_1 = mux(idle, earlyWinner_1, state_1) @[Arbiter.scala 117:30]
    node _out_0_a_earlyValid_T_1 = and(state_0, auto_in_0_a_valid) @[Mux.scala 27:72]
    node _out_0_a_earlyValid_T_2 = and(state_1, auto_in_1_a_valid) @[Mux.scala 27:72]
    node _out_0_a_earlyValid_T_3 = or(_out_0_a_earlyValid_T_1, _out_0_a_earlyValid_T_2) @[Mux.scala 27:72]
    node out_2_0_a_earlyValid = mux(idle, _T_10, _out_0_a_earlyValid_T_3) @[Arbiter.scala 125:29]
    node _beatsLeft_T_2 = and(auto_out_a_ready, out_2_0_a_earlyValid) @[ReadyValidCancel.scala 50:33]
    node _GEN_3 = pad(_beatsLeft_T_2, 10) @[Arbiter.scala 113:52]
    node _beatsLeft_T_3 = sub(beatsLeft, _GEN_3) @[Arbiter.scala 113:52]
    node _beatsLeft_T_4 = tail(_beatsLeft_T_3, 1) @[Arbiter.scala 113:52]
    node _beatsLeft_T_5 = mux(latch, maskedBeats_0, _beatsLeft_T_4) @[Arbiter.scala 113:23]
    node allowed_0 = mux(idle, readys_0, state_0) @[Arbiter.scala 121:24]
    node allowed_1 = mux(idle, readys_1, state_1) @[Arbiter.scala 121:24]
    node _T_30 = mux(muxStateEarly_0, auto_in_0_a_bits_mask, UInt<4>("h0")) @[Mux.scala 27:72]
    node _T_31 = mux(muxStateEarly_1, UInt<4>("hf"), UInt<4>("h0")) @[Mux.scala 27:72]
    node _T_33 = mux(muxStateEarly_0, auto_in_0_a_bits_address, UInt<32>("h0")) @[Mux.scala 27:72]
    node _T_34 = mux(muxStateEarly_1, auto_in_1_a_bits_address, UInt<32>("h0")) @[Mux.scala 27:72]
    node _T_39 = mux(muxStateEarly_0, auto_in_0_a_bits_size, UInt<4>("h0")) @[Mux.scala 27:72]
    node _T_40 = mux(muxStateEarly_1, UInt<4>("h6"), UInt<4>("h0")) @[Mux.scala 27:72]
    node _T_45 = mux(muxStateEarly_0, auto_in_0_a_bits_opcode, UInt<3>("h0")) @[Mux.scala 27:72]
    node _T_46 = mux(muxStateEarly_1, UInt<3>("h4"), UInt<3>("h0")) @[Mux.scala 27:72]
    auto_in_1_a_ready <= and(auto_out_a_ready, allowed_1) @[Arbiter.scala 123:31]
    auto_in_1_d_valid <= and(auto_out_d_valid, requestDOI_0_1) @[Xbar.scala 179:40]
    auto_in_1_d_bits_opcode <= auto_out_d_bits_opcode @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_1_d_bits_size <= auto_out_d_bits_size @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_1_d_bits_data <= auto_out_d_bits_data @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_1_d_bits_corrupt <= auto_out_d_bits_corrupt @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_0_a_ready <= and(auto_out_a_ready, allowed_0) @[Arbiter.scala 123:31]
    auto_in_0_d_valid <= and(auto_out_d_valid, auto_out_d_bits_source) @[Xbar.scala 179:40]
    auto_in_0_d_bits_opcode <= auto_out_d_bits_opcode @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_0_d_bits_size <= auto_out_d_bits_size @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_0_d_bits_denied <= auto_out_d_bits_denied @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_0_d_bits_data <= auto_out_d_bits_data @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_out_a_valid <= mux(idle, _T_10, _out_0_a_earlyValid_T_3) @[Arbiter.scala 125:29]
    auto_out_a_bits_opcode <= or(_T_45, _T_46) @[Mux.scala 27:72]
    auto_out_a_bits_param <= mux(muxStateEarly_0, auto_in_0_a_bits_param, UInt<3>("h0")) @[Mux.scala 27:72]
    auto_out_a_bits_size <= or(_T_39, _T_40) @[Mux.scala 27:72]
    auto_out_a_bits_source <= mux(idle, earlyWinner_0, state_0) @[Arbiter.scala 117:30]
    auto_out_a_bits_address <= or(_T_33, _T_34) @[Mux.scala 27:72]
    auto_out_a_bits_mask <= or(_T_30, _T_31) @[Mux.scala 27:72]
    auto_out_a_bits_data <= mux(muxStateEarly_0, auto_in_0_a_bits_data, UInt<32>("h0")) @[Mux.scala 27:72]
    auto_out_d_ready <= or(_portsDIO_out_0_d_ready_T, requestDOI_0_1) @[Mux.scala 27:72]
    beatsLeft <= mux(reset, UInt<10>("h0"), _beatsLeft_T_5) @[Arbiter.scala 113:17 87:{30,30}]
    readys_mask <= mux(reset, UInt<2>("h3"), _GEN_0) @[Arbiter.scala 23:{23,23}]
    state_0 <= mux(reset, UInt<1>("h0"), muxStateEarly_0) @[Arbiter.scala 116:{26,26} 119:13]
    state_1 <= mux(reset, UInt<1>("h0"), muxStateEarly_1) @[Arbiter.scala 116:{26,26} 119:13]
    printf(clock, _T_9, "Assertion failed\n    at Arbiter.scala:105 assert((prefixOR zip earlyWinner) map { case (p,w) => !p || !w } reduce {_ && _})\n") @[Arbiter.scala 105:13]
    stop(clock, _T_9, 1) @[Arbiter.scala 105:13]
    printf(clock, _T_16, "Assertion failed\n    at Arbiter.scala:107 assert (!earlyValids.reduce(_||_) || earlyWinner.reduce(_||_))\n") @[Arbiter.scala 107:14]
    stop(clock, _T_16, 1) @[Arbiter.scala 107:14]
    printf(clock, _T_23, "Assertion failed\n    at Arbiter.scala:108 assert (!validQuals .reduce(_||_) || validQuals .reduce(_||_))\n") @[Arbiter.scala 108:14]
    stop(clock, _T_23, 1) @[Arbiter.scala 108:14]

  module TLXbar_7 :
    output auto_in_a_ready : UInt<1>
    input auto_in_a_valid : UInt<1>
    input auto_in_a_bits_opcode : UInt<3>
    input auto_in_a_bits_param : UInt<3>
    input auto_in_a_bits_size : UInt<3>
    input auto_in_a_bits_source : UInt<3>
    input auto_in_a_bits_address : UInt<32>
    input auto_in_a_bits_mask : UInt<4>
    input auto_in_a_bits_data : UInt<32>
    input auto_in_d_ready : UInt<1>
    output auto_in_d_valid : UInt<1>
    output auto_in_d_bits_opcode : UInt<3>
    output auto_in_d_bits_size : UInt<3>
    output auto_in_d_bits_source : UInt<3>
    output auto_in_d_bits_data : UInt<32>
    input auto_out_a_ready : UInt<1>
    output auto_out_a_valid : UInt<1>
    output auto_out_a_bits_opcode : UInt<3>
    output auto_out_a_bits_param : UInt<3>
    output auto_out_a_bits_size : UInt<3>
    output auto_out_a_bits_source : UInt<3>
    output auto_out_a_bits_address : UInt<32>
    output auto_out_a_bits_mask : UInt<4>
    output auto_out_a_bits_data : UInt<32>
    output auto_out_d_ready : UInt<1>
    input auto_out_d_valid : UInt<1>
    input auto_out_d_bits_opcode : UInt<3>
    input auto_out_d_bits_size : UInt<3>
    input auto_out_d_bits_source : UInt<3>
    input auto_out_d_bits_data : UInt<32>

    auto_in_a_ready <= auto_out_a_ready @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_d_valid <= auto_out_d_valid @[ReadyValidCancel.scala 21:38]
    auto_in_d_bits_opcode <= auto_out_d_bits_opcode @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_d_bits_size <= auto_out_d_bits_size @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_d_bits_source <= auto_out_d_bits_source @[Xbar.scala 228:69]
    auto_in_d_bits_data <= auto_out_d_bits_data @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_out_a_valid <= auto_in_a_valid @[ReadyValidCancel.scala 21:38]
    auto_out_a_bits_opcode <= auto_in_a_bits_opcode @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_out_a_bits_param <= auto_in_a_bits_param @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_out_a_bits_size <= auto_in_a_bits_size @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_out_a_bits_source <= auto_in_a_bits_source @[Xbar.scala 237:55]
    auto_out_a_bits_address <= auto_in_a_bits_address @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_out_a_bits_mask <= auto_in_a_bits_mask @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_out_a_bits_data <= auto_in_a_bits_data @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_out_d_ready <= auto_in_d_ready @[Nodes.scala 1210:84 LazyModule.scala 309:16]

  module IntXbar_1 :
    input auto_int_in_2_0 : UInt<1>
    input auto_int_in_1_0 : UInt<1>
    input auto_int_in_1_1 : UInt<1>
    input auto_int_in_0_0 : UInt<1>
    output auto_int_out_0 : UInt<1>
    output auto_int_out_1 : UInt<1>
    output auto_int_out_2 : UInt<1>
    output auto_int_out_3 : UInt<1>

    auto_int_out_0 <= auto_int_in_0_0 @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_int_out_1 <= auto_int_in_1_0 @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_int_out_2 <= auto_int_in_1_1 @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_int_out_3 <= auto_int_in_2_0 @[Nodes.scala 1210:84 LazyModule.scala 309:16]

  module BundleBridgeNexus_4 :
    input auto_in : UInt<1>
    output auto_out_0 : UInt<1>

    auto_out_0 <= auto_in @[Nodes.scala 1210:84 LazyModule.scala 309:16]

  module BundleBridgeNexus_5 :
    input auto_in : UInt<32>
    output auto_out_1 : UInt<32>

    auto_out_1 <= auto_in @[Nodes.scala 1210:84 LazyModule.scala 309:16]

  module DCache :
    input gated_clock : Clock
    input reset : UInt<1>
    input auto_out_a_ready : UInt<1>
    output auto_out_a_valid : UInt<1>
    output auto_out_a_bits_opcode : UInt<3>
    output auto_out_a_bits_param : UInt<3>
    output auto_out_a_bits_size : UInt<4>
    output auto_out_a_bits_address : UInt<32>
    output auto_out_a_bits_mask : UInt<4>
    output auto_out_a_bits_data : UInt<32>
    output auto_out_d_ready : UInt<1>
    input auto_out_d_valid : UInt<1>
    input auto_out_d_bits_opcode : UInt<3>
    input auto_out_d_bits_size : UInt<4>
    input auto_out_d_bits_denied : UInt<1>
    input auto_out_d_bits_data : UInt<32>
    output io_cpu_req_ready : UInt<1>
    input io_cpu_req_valid : UInt<1>
    input io_cpu_req_bits_addr : UInt<32>
    input io_cpu_req_bits_tag : UInt<7>
    input io_cpu_req_bits_cmd : UInt<5>
    input io_cpu_req_bits_size : UInt<2>
    input io_cpu_req_bits_signed : UInt<1>
    input io_cpu_req_bits_no_xcpt : UInt<1>
    input io_cpu_s1_kill : UInt<1>
    input io_cpu_s1_data_data : UInt<32>
    input io_cpu_s1_data_mask : UInt<4>
    output io_cpu_s2_nack : UInt<1>
    output io_cpu_resp_valid : UInt<1>
    output io_cpu_resp_bits_tag : UInt<7>
    output io_cpu_resp_bits_data : UInt<32>
    output io_cpu_resp_bits_replay : UInt<1>
    output io_cpu_resp_bits_has_data : UInt<1>
    output io_cpu_resp_bits_data_word_bypass : UInt<32>
    output io_cpu_resp_bits_data_raw : UInt<32>
    output io_cpu_replay_next : UInt<1>
    output io_cpu_s2_xcpt_ma_ld : UInt<1>
    output io_cpu_s2_xcpt_ma_st : UInt<1>
    output io_cpu_s2_xcpt_pf_ld : UInt<1>
    output io_cpu_s2_xcpt_pf_st : UInt<1>
    output io_cpu_s2_xcpt_ae_ld : UInt<1>
    output io_cpu_s2_xcpt_ae_st : UInt<1>
    output io_cpu_ordered : UInt<1>
    output io_cpu_perf_grant : UInt<1>
    input io_ptw_status_debug : UInt<1>
    input io_ptw_pmp_0_cfg_l : UInt<1>
    input io_ptw_pmp_0_cfg_a : UInt<2>
    input io_ptw_pmp_0_cfg_x : UInt<1>
    input io_ptw_pmp_0_cfg_w : UInt<1>
    input io_ptw_pmp_0_cfg_r : UInt<1>
    input io_ptw_pmp_0_addr : UInt<30>
    input io_ptw_pmp_0_mask : UInt<32>
    input io_ptw_pmp_1_cfg_l : UInt<1>
    input io_ptw_pmp_1_cfg_a : UInt<2>
    input io_ptw_pmp_1_cfg_x : UInt<1>
    input io_ptw_pmp_1_cfg_w : UInt<1>
    input io_ptw_pmp_1_cfg_r : UInt<1>
    input io_ptw_pmp_1_addr : UInt<30>
    input io_ptw_pmp_1_mask : UInt<32>
    input io_ptw_pmp_2_cfg_l : UInt<1>
    input io_ptw_pmp_2_cfg_a : UInt<2>
    input io_ptw_pmp_2_cfg_x : UInt<1>
    input io_ptw_pmp_2_cfg_w : UInt<1>
    input io_ptw_pmp_2_cfg_r : UInt<1>
    input io_ptw_pmp_2_addr : UInt<30>
    input io_ptw_pmp_2_mask : UInt<32>
    input io_ptw_pmp_3_cfg_l : UInt<1>
    input io_ptw_pmp_3_cfg_a : UInt<2>
    input io_ptw_pmp_3_cfg_x : UInt<1>
    input io_ptw_pmp_3_cfg_w : UInt<1>
    input io_ptw_pmp_3_cfg_r : UInt<1>
    input io_ptw_pmp_3_addr : UInt<30>
    input io_ptw_pmp_3_mask : UInt<32>
    input io_ptw_pmp_4_cfg_l : UInt<1>
    input io_ptw_pmp_4_cfg_a : UInt<2>
    input io_ptw_pmp_4_cfg_x : UInt<1>
    input io_ptw_pmp_4_cfg_w : UInt<1>
    input io_ptw_pmp_4_cfg_r : UInt<1>
    input io_ptw_pmp_4_addr : UInt<30>
    input io_ptw_pmp_4_mask : UInt<32>
    input io_ptw_pmp_5_cfg_l : UInt<1>
    input io_ptw_pmp_5_cfg_a : UInt<2>
    input io_ptw_pmp_5_cfg_x : UInt<1>
    input io_ptw_pmp_5_cfg_w : UInt<1>
    input io_ptw_pmp_5_cfg_r : UInt<1>
    input io_ptw_pmp_5_addr : UInt<30>
    input io_ptw_pmp_5_mask : UInt<32>
    input io_ptw_pmp_6_cfg_l : UInt<1>
    input io_ptw_pmp_6_cfg_a : UInt<2>
    input io_ptw_pmp_6_cfg_x : UInt<1>
    input io_ptw_pmp_6_cfg_w : UInt<1>
    input io_ptw_pmp_6_cfg_r : UInt<1>
    input io_ptw_pmp_6_addr : UInt<30>
    input io_ptw_pmp_6_mask : UInt<32>
    input io_ptw_pmp_7_cfg_l : UInt<1>
    input io_ptw_pmp_7_cfg_a : UInt<2>
    input io_ptw_pmp_7_cfg_x : UInt<1>
    input io_ptw_pmp_7_cfg_w : UInt<1>
    input io_ptw_pmp_7_cfg_r : UInt<1>
    input io_ptw_pmp_7_addr : UInt<30>
    input io_ptw_pmp_7_mask : UInt<32>

    inst tlb of TLB @[DCache.scala 117:19]
    inst pma_checker of TLB @[DCache.scala 118:27]
    inst metaArb of DCacheModuleImpl_Anon_1 @[DCache.scala 122:23]
    inst data of DCacheDataArray @[DCache.scala 132:20]
    inst dataArb of DCacheModuleImpl_Anon_2 @[DCache.scala 133:23]
    inst amoalu of AMOALU @[DCache.scala 952:26]
    node s1_valid_x9 = and(io_cpu_req_ready, io_cpu_req_valid) @[Decoupled.scala 40:37]
    reg s1_valid : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s1_valid) @[DCache.scala 162:21]
    node _s1_valid_masked_T = not(io_cpu_s1_kill) @[DCache.scala 166:37]
    node s1_valid_masked = and(s1_valid, _s1_valid_masked_T) @[DCache.scala 166:34]
    reg s1_req_cmd : UInt<5>, gated_clock with :
      reset => (UInt<1>("h0"), s1_req_cmd) @[Reg.scala 15:16]
    node _s1_read_T = eq(s1_req_cmd, UInt<5>("h0")) @[Consts.scala 81:31]
    node _s1_read_T_1 = eq(s1_req_cmd, UInt<5>("h6")) @[Consts.scala 81:48]
    node _s1_read_T_2 = or(_s1_read_T, _s1_read_T_1) @[Consts.scala 81:41]
    node _s1_read_T_3 = eq(s1_req_cmd, UInt<5>("h7")) @[Consts.scala 81:65]
    node _s1_read_T_4 = or(_s1_read_T_2, _s1_read_T_3) @[Consts.scala 81:58]
    node _s1_read_T_5 = eq(s1_req_cmd, UInt<5>("h4")) @[package.scala 15:47]
    node _s1_read_T_6 = eq(s1_req_cmd, UInt<5>("h9")) @[package.scala 15:47]
    node _s1_read_T_9 = or(_s1_read_T_5, _s1_read_T_6) @[package.scala 72:59]
    node _s1_read_T_7 = eq(s1_req_cmd, UInt<5>("ha")) @[package.scala 15:47]
    node _s1_read_T_10 = or(_s1_read_T_9, _s1_read_T_7) @[package.scala 72:59]
    node _s1_read_T_8 = eq(s1_req_cmd, UInt<5>("hb")) @[package.scala 15:47]
    node _s1_read_T_11 = or(_s1_read_T_10, _s1_read_T_8) @[package.scala 72:59]
    node _s1_read_T_12 = eq(s1_req_cmd, UInt<5>("h8")) @[package.scala 15:47]
    node _s1_read_T_13 = eq(s1_req_cmd, UInt<5>("hc")) @[package.scala 15:47]
    node _s1_read_T_17 = or(_s1_read_T_12, _s1_read_T_13) @[package.scala 72:59]
    node _s1_read_T_14 = eq(s1_req_cmd, UInt<5>("hd")) @[package.scala 15:47]
    node _s1_read_T_18 = or(_s1_read_T_17, _s1_read_T_14) @[package.scala 72:59]
    node _s1_read_T_15 = eq(s1_req_cmd, UInt<5>("he")) @[package.scala 15:47]
    node _s1_read_T_19 = or(_s1_read_T_18, _s1_read_T_15) @[package.scala 72:59]
    node _s1_read_T_16 = eq(s1_req_cmd, UInt<5>("hf")) @[package.scala 15:47]
    node _s1_read_T_20 = or(_s1_read_T_19, _s1_read_T_16) @[package.scala 72:59]
    node _s1_read_T_21 = or(_s1_read_T_11, _s1_read_T_20) @[Consts.scala 79:44]
    node s1_read = or(_s1_read_T_4, _s1_read_T_21) @[Consts.scala 81:75]
    reg s2_valid : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_valid) @[DCache.scala 303:21]
    reg s2_req_cmd : UInt<5>, gated_clock with :
      reset => (UInt<1>("h0"), s2_req_cmd) @[DCache.scala 311:19]
    node _s2_write_T = eq(s2_req_cmd, UInt<5>("h1")) @[Consts.scala 82:32]
    node _s2_write_T_1 = eq(s2_req_cmd, UInt<5>("h11")) @[Consts.scala 82:49]
    node _s2_write_T_2 = or(_s2_write_T, _s2_write_T_1) @[Consts.scala 82:42]
    node _s2_write_T_3 = eq(s2_req_cmd, UInt<5>("h7")) @[Consts.scala 82:66]
    node _s2_write_T_4 = or(_s2_write_T_2, _s2_write_T_3) @[Consts.scala 82:59]
    node _s2_write_T_5 = eq(s2_req_cmd, UInt<5>("h4")) @[package.scala 15:47]
    node _s2_write_T_6 = eq(s2_req_cmd, UInt<5>("h9")) @[package.scala 15:47]
    node _s2_write_T_9 = or(_s2_write_T_5, _s2_write_T_6) @[package.scala 72:59]
    node _s2_write_T_7 = eq(s2_req_cmd, UInt<5>("ha")) @[package.scala 15:47]
    node _s2_write_T_10 = or(_s2_write_T_9, _s2_write_T_7) @[package.scala 72:59]
    node _s2_write_T_8 = eq(s2_req_cmd, UInt<5>("hb")) @[package.scala 15:47]
    node _s2_write_T_11 = or(_s2_write_T_10, _s2_write_T_8) @[package.scala 72:59]
    node _s2_write_T_12 = eq(s2_req_cmd, UInt<5>("h8")) @[package.scala 15:47]
    node _s2_write_T_13 = eq(s2_req_cmd, UInt<5>("hc")) @[package.scala 15:47]
    node _s2_write_T_17 = or(_s2_write_T_12, _s2_write_T_13) @[package.scala 72:59]
    node _s2_write_T_14 = eq(s2_req_cmd, UInt<5>("hd")) @[package.scala 15:47]
    node _s2_write_T_18 = or(_s2_write_T_17, _s2_write_T_14) @[package.scala 72:59]
    node _s2_write_T_15 = eq(s2_req_cmd, UInt<5>("he")) @[package.scala 15:47]
    node _s2_write_T_19 = or(_s2_write_T_18, _s2_write_T_15) @[package.scala 72:59]
    node _s2_write_T_16 = eq(s2_req_cmd, UInt<5>("hf")) @[package.scala 15:47]
    node _s2_write_T_20 = or(_s2_write_T_19, _s2_write_T_16) @[package.scala 72:59]
    node _s2_write_T_21 = or(_s2_write_T_11, _s2_write_T_20) @[Consts.scala 79:44]
    node s2_write = or(_s2_write_T_4, _s2_write_T_21) @[Consts.scala 82:76]
    node _pstore1_valid_likely_T = and(s2_valid, s2_write) @[DCache.scala 477:39]
    reg pstore1_held : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), pstore1_held) @[DCache.scala 476:29]
    node pstore1_valid_likely = or(_pstore1_valid_likely_T, pstore1_held) @[DCache.scala 477:51]
    reg pstore1_addr : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), pstore1_addr) @[Reg.scala 15:16]
    node _s1_hazard_T = bits(pstore1_addr, 13, 2) @[DCache.scala 533:9]
    reg s1_req_addr : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), s1_req_addr) @[Reg.scala 15:16]
    node s1_vaddr_hi = shr(s1_req_addr, 14) @[DCache.scala 177:56]
    node s1_vaddr_lo = bits(s1_req_addr, 13, 0) @[DCache.scala 177:78]
    node s1_vaddr = cat(s1_vaddr_hi, s1_vaddr_lo) @[Cat.scala 30:58]
    node _s1_hazard_T_1 = bits(s1_vaddr, 13, 2) @[DCache.scala 533:43]
    node _s1_hazard_T_2 = eq(_s1_hazard_T, _s1_hazard_T_1) @[DCache.scala 533:31]
    node _s1_write_T = eq(s1_req_cmd, UInt<5>("h1")) @[Consts.scala 82:32]
    node _s1_write_T_1 = eq(s1_req_cmd, UInt<5>("h11")) @[Consts.scala 82:49]
    node _s1_write_T_2 = or(_s1_write_T, _s1_write_T_1) @[Consts.scala 82:42]
    node _s1_write_T_4 = or(_s1_write_T_2, _s1_read_T_3) @[Consts.scala 82:59]
    node s1_write = or(_s1_write_T_4, _s1_read_T_21) @[Consts.scala 82:76]
    reg pstore1_mask : UInt<4>, gated_clock with :
      reset => (UInt<1>("h0"), pstore1_mask) @[Reg.scala 15:16]
    node _s1_hazard_T_6 = bits(pstore1_mask, 3, 3) @[package.scala 202:50]
    node s1_hazard_hi_hi = orr(_s1_hazard_T_6) @[DCache.scala 1151:66]
    node _s1_hazard_T_5 = bits(pstore1_mask, 2, 2) @[package.scala 202:50]
    node s1_hazard_hi_lo = orr(_s1_hazard_T_5) @[DCache.scala 1151:66]
    node s1_hazard_hi = cat(s1_hazard_hi_hi, s1_hazard_hi_lo) @[Cat.scala 30:58]
    node _s1_hazard_T_4 = bits(pstore1_mask, 1, 1) @[package.scala 202:50]
    node s1_hazard_lo_hi = orr(_s1_hazard_T_4) @[DCache.scala 1151:66]
    node _s1_hazard_T_3 = bits(pstore1_mask, 0, 0) @[package.scala 202:50]
    node s1_hazard_lo_lo = orr(_s1_hazard_T_3) @[DCache.scala 1151:66]
    node s1_hazard_lo = cat(s1_hazard_lo_hi, s1_hazard_lo_lo) @[Cat.scala 30:58]
    node _s1_hazard_T_7 = cat(s1_hazard_hi, s1_hazard_lo) @[Cat.scala 30:58]
    node s1_hazard_hi_hi_1 = bits(_s1_hazard_T_7, 3, 3) @[Bitwise.scala 26:51]
    node s1_hazard_hi_lo_1 = bits(_s1_hazard_T_7, 2, 2) @[Bitwise.scala 26:51]
    node s1_hazard_hi_1 = cat(s1_hazard_hi_hi_1, s1_hazard_hi_lo_1) @[Cat.scala 30:58]
    node s1_hazard_lo_hi_1 = bits(_s1_hazard_T_7, 1, 1) @[Bitwise.scala 26:51]
    node s1_hazard_lo_lo_1 = bits(_s1_hazard_T_7, 0, 0) @[Bitwise.scala 26:51]
    node s1_hazard_lo_1 = cat(s1_hazard_lo_hi_1, s1_hazard_lo_lo_1) @[Cat.scala 30:58]
    node _s1_hazard_T_8 = cat(s1_hazard_hi_1, s1_hazard_lo_1) @[Cat.scala 30:58]
    node _s1_mask_xwr_upper_T_4 = bits(s1_req_addr, 1, 1) @[AMOALU.scala 17:27]
    node _s1_mask_xwr_upper_T = bits(s1_req_addr, 0, 0) @[AMOALU.scala 17:27]
    reg s1_req_size : UInt<2>, gated_clock with :
      reset => (UInt<1>("h0"), s1_req_size) @[Reg.scala 15:16]
    node _s1_mask_xwr_upper_T_2 = geq(s1_req_size, UInt<2>("h1")) @[AMOALU.scala 17:57]
    node s1_mask_xwr_hi = or(_s1_mask_xwr_upper_T, _s1_mask_xwr_upper_T_2) @[AMOALU.scala 17:46]
    node s1_mask_xwr_lo = mux(_s1_mask_xwr_upper_T, UInt<1>("h0"), UInt<1>("h1")) @[AMOALU.scala 18:22]
    node _s1_mask_xwr_T = cat(s1_mask_xwr_hi, s1_mask_xwr_lo) @[Cat.scala 30:58]
    node _s1_mask_xwr_upper_T_5 = mux(_s1_mask_xwr_upper_T_4, _s1_mask_xwr_T, UInt<2>("h0")) @[AMOALU.scala 17:22]
    node _s1_mask_xwr_upper_T_6 = geq(s1_req_size, UInt<2>("h2")) @[AMOALU.scala 17:57]
    node _s1_mask_xwr_upper_T_7 = mux(_s1_mask_xwr_upper_T_6, UInt<2>("h3"), UInt<2>("h0")) @[AMOALU.scala 17:51]
    node s1_mask_xwr_hi_1 = or(_s1_mask_xwr_upper_T_5, _s1_mask_xwr_upper_T_7) @[AMOALU.scala 17:46]
    node s1_mask_xwr_lo_1 = mux(_s1_mask_xwr_upper_T_4, UInt<2>("h0"), _s1_mask_xwr_T) @[AMOALU.scala 18:22]
    node s1_mask_xwr = cat(s1_mask_xwr_hi_1, s1_mask_xwr_lo_1) @[Cat.scala 30:58]
    node _s1_hazard_T_12 = bits(s1_mask_xwr, 3, 3) @[package.scala 202:50]
    node s1_hazard_hi_hi_2 = orr(_s1_hazard_T_12) @[DCache.scala 1151:66]
    node _s1_hazard_T_11 = bits(s1_mask_xwr, 2, 2) @[package.scala 202:50]
    node s1_hazard_hi_lo_2 = orr(_s1_hazard_T_11) @[DCache.scala 1151:66]
    node s1_hazard_hi_2 = cat(s1_hazard_hi_hi_2, s1_hazard_hi_lo_2) @[Cat.scala 30:58]
    node _s1_hazard_T_10 = bits(s1_mask_xwr, 1, 1) @[package.scala 202:50]
    node s1_hazard_lo_hi_2 = orr(_s1_hazard_T_10) @[DCache.scala 1151:66]
    node _s1_hazard_T_9 = bits(s1_mask_xwr, 0, 0) @[package.scala 202:50]
    node s1_hazard_lo_lo_2 = orr(_s1_hazard_T_9) @[DCache.scala 1151:66]
    node s1_hazard_lo_2 = cat(s1_hazard_lo_hi_2, s1_hazard_lo_lo_2) @[Cat.scala 30:58]
    node _s1_hazard_T_13 = cat(s1_hazard_hi_2, s1_hazard_lo_2) @[Cat.scala 30:58]
    node s1_hazard_hi_hi_3 = bits(_s1_hazard_T_13, 3, 3) @[Bitwise.scala 26:51]
    node s1_hazard_hi_lo_3 = bits(_s1_hazard_T_13, 2, 2) @[Bitwise.scala 26:51]
    node s1_hazard_hi_3 = cat(s1_hazard_hi_hi_3, s1_hazard_hi_lo_3) @[Cat.scala 30:58]
    node s1_hazard_lo_hi_3 = bits(_s1_hazard_T_13, 1, 1) @[Bitwise.scala 26:51]
    node s1_hazard_lo_lo_3 = bits(_s1_hazard_T_13, 0, 0) @[Bitwise.scala 26:51]
    node s1_hazard_lo_3 = cat(s1_hazard_lo_hi_3, s1_hazard_lo_lo_3) @[Cat.scala 30:58]
    node _s1_hazard_T_14 = cat(s1_hazard_hi_3, s1_hazard_lo_3) @[Cat.scala 30:58]
    node _s1_hazard_T_15 = and(_s1_hazard_T_8, _s1_hazard_T_14) @[DCache.scala 534:38]
    node _s1_hazard_T_16 = orr(_s1_hazard_T_15) @[DCache.scala 534:66]
    node _s1_hazard_T_17 = and(pstore1_mask, s1_mask_xwr) @[DCache.scala 534:77]
    node _s1_hazard_T_18 = orr(_s1_hazard_T_17) @[DCache.scala 534:92]
    node _s1_hazard_T_19 = mux(s1_write, _s1_hazard_T_16, _s1_hazard_T_18) @[DCache.scala 534:8]
    node _s1_hazard_T_20 = and(_s1_hazard_T_2, _s1_hazard_T_19) @[DCache.scala 533:65]
    node _s1_hazard_T_21 = and(pstore1_valid_likely, _s1_hazard_T_20) @[DCache.scala 536:27]
    reg pstore2_valid : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), pstore2_valid) @[DCache.scala 473:30]
    reg pstore2_addr : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), pstore2_addr) @[Reg.scala 15:16]
    node _s1_hazard_T_22 = bits(pstore2_addr, 13, 2) @[DCache.scala 533:9]
    node _s1_hazard_T_24 = eq(_s1_hazard_T_22, _s1_hazard_T_1) @[DCache.scala 533:31]
    reg mask : UInt<4>, gated_clock with :
      reset => (UInt<1>("h0"), mask) @[DCache.scala 503:19]
    node _s1_hazard_T_28 = bits(mask, 3, 3) @[package.scala 202:50]
    node s1_hazard_hi_hi_4 = orr(_s1_hazard_T_28) @[DCache.scala 1151:66]
    node _s1_hazard_T_27 = bits(mask, 2, 2) @[package.scala 202:50]
    node s1_hazard_hi_lo_4 = orr(_s1_hazard_T_27) @[DCache.scala 1151:66]
    node s1_hazard_hi_4 = cat(s1_hazard_hi_hi_4, s1_hazard_hi_lo_4) @[Cat.scala 30:58]
    node _s1_hazard_T_26 = bits(mask, 1, 1) @[package.scala 202:50]
    node s1_hazard_lo_hi_4 = orr(_s1_hazard_T_26) @[DCache.scala 1151:66]
    node _s1_hazard_T_25 = bits(mask, 0, 0) @[package.scala 202:50]
    node s1_hazard_lo_lo_4 = orr(_s1_hazard_T_25) @[DCache.scala 1151:66]
    node s1_hazard_lo_4 = cat(s1_hazard_lo_hi_4, s1_hazard_lo_lo_4) @[Cat.scala 30:58]
    node _s1_hazard_T_29 = cat(s1_hazard_hi_4, s1_hazard_lo_4) @[Cat.scala 30:58]
    node s1_hazard_hi_hi_5 = bits(_s1_hazard_T_29, 3, 3) @[Bitwise.scala 26:51]
    node s1_hazard_hi_lo_5 = bits(_s1_hazard_T_29, 2, 2) @[Bitwise.scala 26:51]
    node s1_hazard_hi_5 = cat(s1_hazard_hi_hi_5, s1_hazard_hi_lo_5) @[Cat.scala 30:58]
    node s1_hazard_lo_hi_5 = bits(_s1_hazard_T_29, 1, 1) @[Bitwise.scala 26:51]
    node s1_hazard_lo_lo_5 = bits(_s1_hazard_T_29, 0, 0) @[Bitwise.scala 26:51]
    node s1_hazard_lo_5 = cat(s1_hazard_lo_hi_5, s1_hazard_lo_lo_5) @[Cat.scala 30:58]
    node _s1_hazard_T_30 = cat(s1_hazard_hi_5, s1_hazard_lo_5) @[Cat.scala 30:58]
    node _s1_hazard_T_37 = and(_s1_hazard_T_30, _s1_hazard_T_14) @[DCache.scala 534:38]
    node _s1_hazard_T_38 = orr(_s1_hazard_T_37) @[DCache.scala 534:66]
    node _s1_hazard_T_39 = and(mask, s1_mask_xwr) @[DCache.scala 534:77]
    node _s1_hazard_T_40 = orr(_s1_hazard_T_39) @[DCache.scala 534:92]
    node _s1_hazard_T_41 = mux(s1_write, _s1_hazard_T_38, _s1_hazard_T_40) @[DCache.scala 534:8]
    node _s1_hazard_T_42 = and(_s1_hazard_T_24, _s1_hazard_T_41) @[DCache.scala 533:65]
    node _s1_hazard_T_43 = and(pstore2_valid, _s1_hazard_T_42) @[DCache.scala 537:21]
    node s1_hazard = or(_s1_hazard_T_21, _s1_hazard_T_43) @[DCache.scala 536:69]
    node s1_raw_hazard = and(s1_read, s1_hazard) @[DCache.scala 538:31]
    node _T_226 = and(s1_valid, s1_raw_hazard) @[DCache.scala 543:18]
    node s2_valid_no_xcpt_hi_hi = cat(io_cpu_s2_xcpt_ma_ld, io_cpu_s2_xcpt_ma_st) @[DCache.scala 304:54]
    node s2_valid_no_xcpt_hi = cat(s2_valid_no_xcpt_hi_hi, io_cpu_s2_xcpt_pf_ld) @[DCache.scala 304:54]
    node s2_valid_no_xcpt_lo_hi = cat(io_cpu_s2_xcpt_pf_st, io_cpu_s2_xcpt_ae_ld) @[DCache.scala 304:54]
    node s2_valid_no_xcpt_lo = cat(s2_valid_no_xcpt_lo_hi, io_cpu_s2_xcpt_ae_st) @[DCache.scala 304:54]
    node _s2_valid_no_xcpt_T = cat(s2_valid_no_xcpt_hi, s2_valid_no_xcpt_lo) @[DCache.scala 304:54]
    node _s2_valid_no_xcpt_T_1 = orr(_s2_valid_no_xcpt_T) @[DCache.scala 304:61]
    node _s2_valid_no_xcpt_T_2 = not(_s2_valid_no_xcpt_T_1) @[DCache.scala 304:38]
    node s2_valid_no_xcpt = and(s2_valid, _s2_valid_no_xcpt_T_2) @[DCache.scala 304:35]
    reg s2_not_nacked_in_s1 : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_not_nacked_in_s1) @[DCache.scala 307:36]
    node s2_valid_masked = and(s2_valid_no_xcpt, s2_not_nacked_in_s1) @[DCache.scala 309:42]
    node _c_cat_T_45 = eq(s2_req_cmd, UInt<5>("h3")) @[Consts.scala 83:54]
    node _c_cat_T_46 = or(s2_write, _c_cat_T_45) @[Consts.scala 83:47]
    node _c_cat_T_47 = eq(s2_req_cmd, UInt<5>("h6")) @[Consts.scala 83:71]
    node c_cat_lo = or(_c_cat_T_46, _c_cat_T_47) @[Consts.scala 83:64]
    node hi = cat(s2_write, c_cat_lo) @[Cat.scala 30:58]
    reg s2_hit_state_state : UInt<2>, gated_clock with :
      reset => (UInt<1>("h0"), s2_hit_state_state) @[Reg.scala 15:16]
    node _T_35 = cat(hi, s2_hit_state_state) @[Cat.scala 30:58]
    node _T_81 = eq(UInt<4>("h3"), _T_35) @[Misc.scala 48:20]
    node _T_78 = eq(UInt<4>("h2"), _T_35) @[Misc.scala 48:20]
    node _T_75 = eq(UInt<4>("h1"), _T_35) @[Misc.scala 48:20]
    node _T_72 = eq(UInt<4>("h7"), _T_35) @[Misc.scala 48:20]
    node _T_69 = eq(UInt<4>("h6"), _T_35) @[Misc.scala 48:20]
    node _T_66 = eq(UInt<4>("hf"), _T_35) @[Misc.scala 48:20]
    node _T_63 = eq(UInt<4>("he"), _T_35) @[Misc.scala 48:20]
    node _T_60 = eq(UInt<4>("h0"), _T_35) @[Misc.scala 48:20]
    node _T_57 = eq(UInt<4>("h5"), _T_35) @[Misc.scala 48:20]
    node _T_54 = eq(UInt<4>("h4"), _T_35) @[Misc.scala 48:20]
    node _T_51 = eq(UInt<4>("hd"), _T_35) @[Misc.scala 48:20]
    node _T_48 = eq(UInt<4>("hc"), _T_35) @[Misc.scala 48:20]
    node _T_67 = or(_T_66, _T_63) @[Misc.scala 34:9]
    node _T_70 = or(_T_69, _T_67) @[Misc.scala 34:9]
    node _T_73 = or(_T_72, _T_70) @[Misc.scala 34:9]
    node _T_76 = or(_T_75, _T_73) @[Misc.scala 34:9]
    node _T_79 = or(_T_78, _T_76) @[Misc.scala 34:9]
    node s2_hit = or(_T_81, _T_79) @[Misc.scala 34:9]
    node s2_valid_hit_maybe_flush_pre_data_ecc_and_waw = and(s2_valid_masked, s2_hit) @[DCache.scala 369:89]
    node _s2_read_T = eq(s2_req_cmd, UInt<5>("h0")) @[Consts.scala 81:31]
    node _s2_read_T_2 = or(_s2_read_T, _c_cat_T_47) @[Consts.scala 81:41]
    node _s2_read_T_4 = or(_s2_read_T_2, _s2_write_T_3) @[Consts.scala 81:58]
    node s2_read = or(_s2_read_T_4, _s2_write_T_21) @[Consts.scala 81:75]
    node s2_readwrite = or(s2_read, s2_write) @[DCache.scala 326:30]
    node s2_valid_hit_pre_data_ecc_and_waw = and(s2_valid_hit_maybe_flush_pre_data_ecc_and_waw, s2_readwrite) @[DCache.scala 390:89]
    node _T_50 = mux(_T_48, UInt<2>("h1"), UInt<2>("h0")) @[Misc.scala 34:36]
    node _T_53 = mux(_T_51, UInt<2>("h2"), _T_50) @[Misc.scala 34:36]
    node _T_56 = mux(_T_54, UInt<2>("h1"), _T_53) @[Misc.scala 34:36]
    node _T_59 = mux(_T_57, UInt<2>("h2"), _T_56) @[Misc.scala 34:36]
    node _T_62 = mux(_T_60, UInt<2>("h0"), _T_59) @[Misc.scala 34:36]
    node _T_65 = mux(_T_63, UInt<2>("h3"), _T_62) @[Misc.scala 34:36]
    node _T_68 = mux(_T_66, UInt<2>("h3"), _T_65) @[Misc.scala 34:36]
    node _T_71 = mux(_T_69, UInt<2>("h2"), _T_68) @[Misc.scala 34:36]
    node _T_74 = mux(_T_72, UInt<2>("h3"), _T_71) @[Misc.scala 34:36]
    node _T_77 = mux(_T_75, UInt<2>("h1"), _T_74) @[Misc.scala 34:36]
    node _T_80 = mux(_T_78, UInt<2>("h2"), _T_77) @[Misc.scala 34:36]
    node s2_grow_param = mux(_T_81, UInt<2>("h3"), _T_80) @[Misc.scala 34:36]
    node _s2_update_meta_T = eq(s2_hit_state_state, s2_grow_param) @[Metadata.scala 45:46]
    node s2_update_meta = not(_s2_update_meta_T) @[Metadata.scala 46:40]
    node _T_205 = and(s2_valid_hit_pre_data_ecc_and_waw, s2_update_meta) @[DCache.scala 418:62]
    node _T_206 = or(io_cpu_s2_nack, _T_205) @[DCache.scala 418:24]
    reg s1_req_no_xcpt : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s1_req_no_xcpt) @[Reg.scala 15:16]
    node s1_readwrite = or(s1_read, s1_write) @[DCache.scala 190:30]
    node _s1_flush_line_T = eq(s1_req_cmd, UInt<5>("h5")) @[DCache.scala 192:34]
    node _s1_flush_line_T_1 = bits(s1_req_size, 0, 0) @[DCache.scala 192:64]
    node s1_flush_line = and(_s1_flush_line_T, _s1_flush_line_T_1) @[DCache.scala 192:50]
    node _s1_cmd_uses_tlb_T = or(s1_readwrite, s1_flush_line) @[DCache.scala 249:38]
    node _s1_cmd_uses_tlb_T_1 = eq(s1_req_cmd, UInt<5>("h17")) @[DCache.scala 249:69]
    node s1_cmd_uses_tlb = or(_s1_cmd_uses_tlb_T, _s1_cmd_uses_tlb_T_1) @[DCache.scala 249:55]
    node s1_nack = or(_T_226, _T_206) @[DCache.scala 543:{36,46}]
    node _s1_valid_not_nacked_T = not(s1_nack) @[DCache.scala 167:41]
    node s1_valid_not_nacked = and(s1_valid, _s1_valid_not_nacked_T) @[DCache.scala 167:38]
    node _s0_clk_en_T = not(metaArb.io_out_bits_write) @[DCache.scala 170:43]
    node s0_clk_en = and(metaArb.io_out_valid, _s0_clk_en_T) @[DCache.scala 170:40]
    node s0_req_addr_hi = shr(metaArb.io_out_bits_addr, 6) @[DCache.scala 173:47]
    node s0_req_addr_lo = bits(io_cpu_req_bits_addr, 5, 0) @[DCache.scala 173:84]
    node s0_req_addr = cat(s0_req_addr_hi, s0_req_addr_lo) @[Cat.scala 30:58]
    node _T = not(metaArb.io_in_7_ready) @[DCache.scala 175:9]
    reg s1_req_tag : UInt<7>, gated_clock with :
      reset => (UInt<1>("h0"), s1_req_tag) @[Reg.scala 15:16]
    reg s1_req_signed : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s1_req_signed) @[Reg.scala 15:16]
    reg s1_tlb_req_vaddr : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), s1_tlb_req_vaddr) @[Reg.scala 15:16]
    reg s1_tlb_req_size : UInt<2>, gated_clock with :
      reset => (UInt<1>("h0"), s1_tlb_req_size) @[Reg.scala 15:16]
    reg s1_tlb_req_cmd : UInt<5>, gated_clock with :
      reset => (UInt<1>("h0"), s1_tlb_req_cmd) @[Reg.scala 15:16]
    node s1_sfence = eq(s1_req_cmd, UInt<5>("h14")) @[DCache.scala 191:30]
    reg s1_flush_valid : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s1_flush_valid) @[DCache.scala 193:27]
    reg uncachedInFlight_0 : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), uncachedInFlight_0) @[DCache.scala 215:33]
    reg uncachedReqs_0_addr : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), uncachedReqs_0_addr) @[DCache.scala 216:25]
    reg uncachedReqs_0_tag : UInt<7>, gated_clock with :
      reset => (UInt<1>("h0"), uncachedReqs_0_tag) @[DCache.scala 216:25]
    reg uncachedReqs_0_size : UInt<2>, gated_clock with :
      reset => (UInt<1>("h0"), uncachedReqs_0_size) @[DCache.scala 216:25]
    reg uncachedReqs_0_signed : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), uncachedReqs_0_signed) @[DCache.scala 216:25]
    node _s0_read_T = eq(io_cpu_req_bits_cmd, UInt<5>("h0")) @[Consts.scala 81:31]
    node _s0_read_T_1 = eq(io_cpu_req_bits_cmd, UInt<5>("h6")) @[Consts.scala 81:48]
    node _s0_read_T_2 = or(_s0_read_T, _s0_read_T_1) @[Consts.scala 81:41]
    node _s0_read_T_3 = eq(io_cpu_req_bits_cmd, UInt<5>("h7")) @[Consts.scala 81:65]
    node _s0_read_T_4 = or(_s0_read_T_2, _s0_read_T_3) @[Consts.scala 81:58]
    node _s0_read_T_5 = eq(io_cpu_req_bits_cmd, UInt<5>("h4")) @[package.scala 15:47]
    node _s0_read_T_6 = eq(io_cpu_req_bits_cmd, UInt<5>("h9")) @[package.scala 15:47]
    node _s0_read_T_7 = eq(io_cpu_req_bits_cmd, UInt<5>("ha")) @[package.scala 15:47]
    node _s0_read_T_8 = eq(io_cpu_req_bits_cmd, UInt<5>("hb")) @[package.scala 15:47]
    node _s0_read_T_9 = or(_s0_read_T_5, _s0_read_T_6) @[package.scala 72:59]
    node _s0_read_T_10 = or(_s0_read_T_9, _s0_read_T_7) @[package.scala 72:59]
    node _s0_read_T_11 = or(_s0_read_T_10, _s0_read_T_8) @[package.scala 72:59]
    node _s0_read_T_12 = eq(io_cpu_req_bits_cmd, UInt<5>("h8")) @[package.scala 15:47]
    node _s0_read_T_13 = eq(io_cpu_req_bits_cmd, UInt<5>("hc")) @[package.scala 15:47]
    node _s0_read_T_14 = eq(io_cpu_req_bits_cmd, UInt<5>("hd")) @[package.scala 15:47]
    node _s0_read_T_15 = eq(io_cpu_req_bits_cmd, UInt<5>("he")) @[package.scala 15:47]
    node _s0_read_T_16 = eq(io_cpu_req_bits_cmd, UInt<5>("hf")) @[package.scala 15:47]
    node _s0_read_T_17 = or(_s0_read_T_12, _s0_read_T_13) @[package.scala 72:59]
    node _s0_read_T_18 = or(_s0_read_T_17, _s0_read_T_14) @[package.scala 72:59]
    node _s0_read_T_19 = or(_s0_read_T_18, _s0_read_T_15) @[package.scala 72:59]
    node _s0_read_T_20 = or(_s0_read_T_19, _s0_read_T_16) @[package.scala 72:59]
    node _s0_read_T_21 = or(_s0_read_T_11, _s0_read_T_20) @[Consts.scala 79:44]
    node s0_read = or(_s0_read_T_4, _s0_read_T_21) @[Consts.scala 81:75]
    node _dataArb_io_in_3_valid_res_T = eq(io_cpu_req_bits_cmd, UInt<5>("h1")) @[package.scala 15:47]
    node _dataArb_io_in_3_valid_res_T_1 = eq(io_cpu_req_bits_cmd, UInt<5>("h3")) @[package.scala 15:47]
    node _dataArb_io_in_3_valid_res_T_2 = or(_dataArb_io_in_3_valid_res_T, _dataArb_io_in_3_valid_res_T_1) @[package.scala 72:59]
    node res = not(_dataArb_io_in_3_valid_res_T_2) @[DCache.scala 1155:15]
    node _dataArb_io_in_3_valid_T_24 = eq(io_cpu_req_bits_cmd, UInt<5>("h11")) @[Consts.scala 82:49]
    node _dataArb_io_in_3_valid_T_25 = or(_dataArb_io_in_3_valid_res_T, _dataArb_io_in_3_valid_T_24) @[Consts.scala 82:42]
    node _dataArb_io_in_3_valid_T_27 = or(_dataArb_io_in_3_valid_T_25, _s0_read_T_3) @[Consts.scala 82:59]
    node _dataArb_io_in_3_valid_T_45 = or(_dataArb_io_in_3_valid_T_27, _s0_read_T_21) @[Consts.scala 82:76]
    node _dataArb_io_in_3_valid_T_49 = and(_dataArb_io_in_3_valid_T_45, _dataArb_io_in_3_valid_T_24) @[DCache.scala 1161:23]
    node _dataArb_io_in_3_valid_T_50 = or(s0_read, _dataArb_io_in_3_valid_T_49) @[DCache.scala 1160:21]
    node _dataArb_io_in_3_valid_T_51 = not(_dataArb_io_in_3_valid_T_50) @[DCache.scala 1156:12]
    node _dataArb_io_in_3_valid_T_52 = or(_dataArb_io_in_3_valid_T_51, res) @[DCache.scala 1156:28]
    node _dataArb_io_in_3_valid_T_54 = or(_dataArb_io_in_3_valid_T_52, reset) @[DCache.scala 1156:11]
    node _dataArb_io_in_3_valid_T_55 = not(_dataArb_io_in_3_valid_T_54) @[DCache.scala 1156:11]
    node _dataArb_io_in_3_valid_T_56 = and(io_cpu_req_valid, res) @[DCache.scala 221:46]
    node dataArb_io_in_3_bits_addr_hi = shr(io_cpu_req_bits_addr, 14) @[DCache.scala 224:89]
    node dataArb_io_in_3_bits_addr_lo = bits(io_cpu_req_bits_addr, 13, 0) @[DCache.scala 224:120]
    node _dataArb_io_in_3_bits_addr_T = cat(dataArb_io_in_3_bits_addr_hi, dataArb_io_in_3_bits_addr_lo) @[Cat.scala 30:58]
    node _T_3 = not(dataArb.io_in_3_ready) @[DCache.scala 237:9]
    node _T_4 = and(_T_3, s0_read) @[DCache.scala 237:33]
    node _GEN_28 = mux(_T_4, UInt<1>("h0"), _s1_valid_not_nacked_T) @[DCache.scala 212:20 237:{45,64}]
    node _s1_did_read_T_51 = and(io_cpu_req_valid, _dataArb_io_in_3_valid_T_50) @[DCache.scala 238:75]
    node _s1_did_read_T_52 = and(dataArb.io_in_3_ready, _s1_did_read_T_51) @[DCache.scala 238:54]
    reg s1_did_read : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s1_did_read) @[Reg.scala 15:16]
    reg s1_read_mask : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s1_read_mask) @[Reg.scala 15:16]
    node _GEN_31 = mux(_T, UInt<1>("h0"), _GEN_28) @[DCache.scala 246:{34,53}]
    node s1_paddr_hi = shr(tlb.io_resp_paddr, 12) @[DCache.scala 270:99]
    node s1_paddr_lo = bits(s1_req_addr, 11, 0) @[DCache.scala 270:125]
    node s1_paddr = cat(s1_paddr_hi, s1_paddr_lo) @[Cat.scala 30:58]
    node _T_20 = geq(s1_paddr, UInt<32>("h80000000")) @[DCache.scala 275:35]
    node _T_23 = lt(s1_paddr, UInt<32>("h80004000")) @[DCache.scala 275:59]
    node inScratchpad = and(_T_20, _T_23) @[DCache.scala 275:47]
    node hitState_state = mux(inScratchpad, UInt<2>("h3"), UInt<2>("h0")) @[DCache.scala 276:25]
    node tl_d_data_encoded_lo_lo = bits(auto_out_d_bits_data, 7, 0) @[package.scala 202:50]
    node tl_d_data_encoded_lo_hi = bits(auto_out_d_bits_data, 15, 8) @[package.scala 202:50]
    node tl_d_data_encoded_hi_lo = bits(auto_out_d_bits_data, 23, 16) @[package.scala 202:50]
    node tl_d_data_encoded_hi_hi = bits(auto_out_d_bits_data, 31, 24) @[package.scala 202:50]
    node tl_d_data_encoded_lo = cat(tl_d_data_encoded_lo_hi, tl_d_data_encoded_lo_lo) @[Cat.scala 30:58]
    node tl_d_data_encoded_hi = cat(tl_d_data_encoded_hi_hi, tl_d_data_encoded_hi_lo) @[Cat.scala 30:58]
    node _tl_d_data_encoded_T = cat(tl_d_data_encoded_hi, tl_d_data_encoded_lo) @[Cat.scala 30:58]
    node s1_mask = mux(_s1_write_T_1, io_cpu_s1_data_mask, s1_mask_xwr) @[DCache.scala 299:20]
    node _T_25 = and(s1_valid_masked, _s1_write_T_1) @[DCache.scala 301:28]
    node _T_26 = not(_T_25) @[DCache.scala 301:10]
    node _T_27 = not(io_cpu_s1_data_mask) @[DCache.scala 301:71]
    node _T_28 = or(s1_mask_xwr, _T_27) @[DCache.scala 301:69]
    node _T_29 = andr(_T_28) @[DCache.scala 301:93]
    node _T_30 = or(_T_26, _T_29) @[DCache.scala 301:53]
    node _T_32 = or(_T_30, reset) @[DCache.scala 301:9]
    node _T_33 = not(_T_32) @[DCache.scala 301:9]
    node _s2_valid_x37_T = not(s1_sfence) @[DCache.scala 303:46]
    node s2_valid_x37 = and(s1_valid_masked, _s2_valid_x37_T) @[DCache.scala 303:43]
    reg s2_req_addr : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), s2_req_addr) @[DCache.scala 311:19]
    reg s2_req_tag : UInt<7>, gated_clock with :
      reset => (UInt<1>("h0"), s2_req_tag) @[DCache.scala 311:19]
    reg s2_req_size : UInt<2>, gated_clock with :
      reset => (UInt<1>("h0"), s2_req_size) @[DCache.scala 311:19]
    reg s2_req_signed : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_req_signed) @[DCache.scala 311:19]
    reg s2_req_no_xcpt : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_req_no_xcpt) @[DCache.scala 311:19]
    node _s2_cmd_flush_all_T_1 = bits(s2_req_size, 0, 0) @[DCache.scala 312:68]
    reg s2_tlb_xcpt_pf_ld : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_tlb_xcpt_pf_ld) @[DCache.scala 314:24]
    reg s2_tlb_xcpt_pf_st : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_tlb_xcpt_pf_st) @[DCache.scala 314:24]
    reg s2_tlb_xcpt_ae_ld : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_tlb_xcpt_ae_ld) @[DCache.scala 314:24]
    reg s2_tlb_xcpt_ae_st : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_tlb_xcpt_ae_st) @[DCache.scala 314:24]
    reg s2_tlb_xcpt_ma_ld : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_tlb_xcpt_ma_ld) @[DCache.scala 314:24]
    reg s2_tlb_xcpt_ma_st : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_tlb_xcpt_ma_st) @[DCache.scala 314:24]
    node _T_34 = or(s1_valid_not_nacked, s1_flush_valid) @[DCache.scala 317:29]
    node _GEN_35 = mux(_T_34, s1_paddr, s2_req_addr) @[DCache.scala 317:48 319:17 311:19]
    node _GEN_36 = mux(_T_34, s1_req_tag, s2_req_tag) @[DCache.scala 317:48 318:12 311:19]
    node _GEN_37 = mux(_T_34, s1_req_cmd, s2_req_cmd) @[DCache.scala 317:48 318:12 311:19]
    node _GEN_38 = mux(_T_34, s1_req_size, s2_req_size) @[DCache.scala 317:48 318:12 311:19]
    node _GEN_39 = mux(_T_34, s1_req_signed, s2_req_signed) @[DCache.scala 317:48 318:12 311:19]
    reg s2_vaddr_r : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), s2_vaddr_r) @[Reg.scala 15:16]
    node s2_vaddr_hi = shr(s2_vaddr_r, 14) @[DCache.scala 323:81]
    node s2_vaddr_lo = bits(s2_req_addr, 13, 0) @[DCache.scala 323:103]
    node s2_vaddr = cat(s2_vaddr_hi, s2_vaddr_lo) @[Cat.scala 30:58]
    reg s2_flush_valid_pre_tag_ecc : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_flush_valid_pre_tag_ecc) @[DCache.scala 327:43]
    node en = or(s1_valid, io_cpu_replay_next) @[DCache.scala 338:38]
    node word_en = and(s1_did_read, s1_read_mask) @[DCache.scala 339:63]
    node s1_all_data_ways_0 = data.io_resp_0 @[DCache.scala 297:{29,29}]
    node _s2_data_s1_word_en_T = not(io_cpu_replay_next) @[DCache.scala 349:28]
    node s1_word_en = mux(_s2_data_s1_word_en_T, word_en, UInt<1>("h1")) @[DCache.scala 349:27]
    node opc = bits(auto_out_d_bits_opcode, 1, 0) @[DCache.scala 628:26]
    node _T_240 = and(opc, UInt<2>("h1")) @[Decode.scala 14:65]
    node data_1 = eq(_T_240, UInt<2>("h1")) @[Decode.scala 14:121]
    reg blockUncachedGrant : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), blockUncachedGrant) @[DCache.scala 722:33]
    node _T_269 = or(blockUncachedGrant, s1_valid) @[DCache.scala 724:54]
    node _T_270 = and(data_1, _T_269) @[DCache.scala 724:31]
    node _GEN_217 = pad(opc, 3) @[DCache.scala 638:29]
    node grantIsRefill = eq(_GEN_217, UInt<3>("h5")) @[DCache.scala 638:29]
    node _T_267 = not(dataArb.io_in_1_ready) @[DCache.scala 694:26]
    node _T_268 = and(grantIsRefill, _T_267) @[DCache.scala 694:23]
    node _grantIsCached_T = eq(_GEN_217, UInt<3>("h4")) @[package.scala 15:47]
    node grantIsCached = or(_grantIsCached_T, grantIsRefill) @[package.scala 72:59]
    reg counter : UInt<10>, gated_clock with :
      reset => (UInt<1>("h0"), counter) @[Edges.scala 228:27]
    node d_first = eq(counter, UInt<10>("h0")) @[Edges.scala 230:25]
    node _bundleOut_0_d_ready_T = not(d_first) @[DCache.scala 643:41]
    node _bundleOut_0_d_ready_T_3 = mux(grantIsCached, _bundleOut_0_d_ready_T, UInt<1>("h1")) @[DCache.scala 643:24]
    node _GEN_196 = mux(_T_268, UInt<1>("h0"), _bundleOut_0_d_ready_T_3) @[DCache.scala 643:18 694:51 696:20]
    node tl_out__d_ready = mux(_T_270, UInt<1>("h0"), _GEN_196) @[DCache.scala 724:68 725:22]
    node _T_244 = and(tl_out__d_ready, auto_out_d_valid) @[Decoupled.scala 40:37]
    node _GEN_153 = mux(data_1, UInt<2>("h2"), UInt<2>("h1")) @[DCache.scala 663:34 666:25]
    node _GEN_175 = mux(grantIsCached, UInt<2>("h1"), _GEN_153) @[DCache.scala 647:26]
    node s1_data_way = mux(_T_244, _GEN_175, UInt<2>("h1")) @[DCache.scala 646:26]
    node _s2_data_T_1 = mux(s1_word_en, s1_data_way, UInt<2>("h0")) @[DCache.scala 351:28]
    node _s2_data_T_2 = bits(_s2_data_T_1, 0, 0) @[Mux.scala 29:36]
    node _s2_data_T_3 = bits(_s2_data_T_1, 1, 1) @[Mux.scala 29:36]
    node _s2_data_T_4 = mux(_s2_data_T_2, s1_all_data_ways_0, UInt<32>("h0")) @[Mux.scala 27:72]
    node _s2_data_T_5 = mux(_s2_data_T_3, _tl_d_data_encoded_T, UInt<32>("h0")) @[Mux.scala 27:72]
    node _s2_data_T_6 = or(_s2_data_T_4, _s2_data_T_5) @[Mux.scala 27:72]
    reg s2_data : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), s2_data) @[Reg.scala 15:16]
    node s2_data_uncorrected_lo_lo = bits(s2_data, 7, 0) @[package.scala 202:50]
    node s2_data_uncorrected_lo_hi = bits(s2_data, 15, 8) @[package.scala 202:50]
    node s2_data_uncorrected_hi_lo = bits(s2_data, 23, 16) @[package.scala 202:50]
    node s2_data_uncorrected_hi_hi = bits(s2_data, 31, 24) @[package.scala 202:50]
    node s2_data_corrected_lo = cat(s2_data_uncorrected_lo_hi, s2_data_uncorrected_lo_lo) @[Cat.scala 30:58]
    node s2_data_corrected_hi = cat(s2_data_uncorrected_hi_hi, s2_data_uncorrected_hi_lo) @[Cat.scala 30:58]
    node s2_data_corrected = cat(s2_data_corrected_hi, s2_data_corrected_lo) @[Cat.scala 30:58]
    node _s2_valid_miss_T = and(s2_valid_masked, s2_readwrite) @[DCache.scala 395:39]
    node _s2_valid_miss_T_3 = not(s2_hit) @[DCache.scala 395:76]
    node s2_valid_miss = and(_s2_valid_miss_T, _s2_valid_miss_T_3) @[DCache.scala 395:73]
    node _s2_valid_cached_miss_T_2 = orr(uncachedInFlight_0) @[DCache.scala 397:88]
    node _s2_cannot_victimize_T = not(s2_flush_valid_pre_tag_ecc) @[DCache.scala 400:29]
    node _s2_valid_uncached_pending_T_1 = andr(uncachedInFlight_0) @[DCache.scala 402:92]
    node _s2_valid_uncached_pending_T_2 = not(_s2_valid_uncached_pending_T_1) @[DCache.scala 402:67]
    node s2_valid_uncached_pending = and(s2_valid_miss, _s2_valid_uncached_pending_T_2) @[DCache.scala 402:64]
    node s2_dont_nack_uncached = and(s2_valid_uncached_pending, auto_out_a_ready) @[DCache.scala 412:57]
    node _s2_dont_nack_misc_T_10 = eq(s2_req_cmd, UInt<5>("h17")) @[DCache.scala 416:17]
    node s2_dont_nack_misc = and(s2_valid_masked, _s2_dont_nack_misc_T_10) @[DCache.scala 413:61]
    node _io_cpu_s2_nack_T = not(s2_dont_nack_uncached) @[DCache.scala 417:41]
    node _io_cpu_s2_nack_T_1 = and(s2_valid_no_xcpt, _io_cpu_s2_nack_T) @[DCache.scala 417:38]
    node _io_cpu_s2_nack_T_2 = not(s2_dont_nack_misc) @[DCache.scala 417:67]
    node _io_cpu_s2_nack_T_3 = and(_io_cpu_s2_nack_T_1, _io_cpu_s2_nack_T_2) @[DCache.scala 417:64]
    node _io_cpu_s2_nack_T_4 = not(s2_valid_hit_pre_data_ecc_and_waw) @[DCache.scala 417:89]
    node metaArb_io_in_2_bits_addr_lo = bits(s2_vaddr, 13, 0) @[DCache.scala 438:80]
    node _pstore1_cmd_T = and(s1_valid_not_nacked, s1_write) @[DCache.scala 464:63]
    reg pstore1_cmd : UInt<5>, gated_clock with :
      reset => (UInt<1>("h0"), pstore1_cmd) @[Reg.scala 15:16]
    reg pstore1_data : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), pstore1_data) @[Reg.scala 15:16]
    node _pstore1_rmw_T_49 = and(s1_write, _s1_write_T_1) @[DCache.scala 1161:23]
    node _pstore1_rmw_T_50 = or(s1_read, _pstore1_rmw_T_49) @[DCache.scala 1160:21]
    reg pstore1_rmw_r : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), pstore1_rmw_r) @[Reg.scala 15:16]
    node _pstore1_merge_T = and(s2_valid_hit_pre_data_ecc_and_waw, s2_write) @[DCache.scala 462:46]
    node pstore_drain_opportunistic = not(_dataArb_io_in_3_valid_T_56) @[DCache.scala 474:36]
    reg pstore_drain_on_miss_REG : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), pstore_drain_on_miss_REG) @[DCache.scala 475:56]
    node pstore1_valid = or(_pstore1_merge_T, pstore1_held) @[DCache.scala 479:38]
    node _pstore_drain_structural_T = and(pstore1_valid_likely, pstore2_valid) @[DCache.scala 481:54]
    node _pstore_drain_structural_T_1 = and(s1_valid, s1_write) @[DCache.scala 481:85]
    node _pstore_drain_structural_T_2 = or(_pstore_drain_structural_T_1, pstore1_rmw_r) @[DCache.scala 481:98]
    node pstore_drain_structural = and(_pstore_drain_structural_T, _pstore_drain_structural_T_2) @[DCache.scala 481:71]
    node _pstore_drain_T_6 = not(pstore1_rmw_r) @[DCache.scala 490:44]
    node _pstore_drain_T_7 = and(pstore1_valid, _pstore_drain_T_6) @[DCache.scala 490:41]
    node _pstore_drain_T_8 = or(_pstore_drain_T_7, pstore2_valid) @[DCache.scala 490:58]
    node _pstore_drain_T_9 = or(pstore_drain_opportunistic, pstore_drain_on_miss_REG) @[DCache.scala 490:107]
    node _pstore_drain_T_10 = and(_pstore_drain_T_8, _pstore_drain_T_9) @[DCache.scala 490:76]
    node pstore_drain = or(pstore_drain_structural, _pstore_drain_T_10) @[DCache.scala 489:48]
    node _pstore1_held_T_8 = and(pstore1_valid, pstore2_valid) @[DCache.scala 493:71]
    node _pstore1_held_T_9 = not(pstore_drain) @[DCache.scala 493:91]
    node _pstore1_held_T_10 = and(_pstore1_held_T_8, _pstore1_held_T_9) @[DCache.scala 493:88]
    node _advance_pstore1_T_1 = eq(pstore2_valid, pstore_drain) @[DCache.scala 494:79]
    node advance_pstore1 = and(pstore1_valid, _advance_pstore1_T_1) @[DCache.scala 494:61]
    node _pstore2_valid_T_1 = and(pstore2_valid, _pstore1_held_T_9) @[DCache.scala 495:34]
    node _pstore2_valid_T_2 = or(_pstore2_valid_T_1, advance_pstore1) @[DCache.scala 495:51]
    node pstore1_storegen_data_mask_hi_hi = mux(_s1_hazard_T_6, UInt<8>("hff"), UInt<8>("h0")) @[Bitwise.scala 72:12]
    node pstore1_storegen_data_mask_hi_lo = mux(_s1_hazard_T_5, UInt<8>("hff"), UInt<8>("h0")) @[Bitwise.scala 72:12]
    node pstore1_storegen_data_mask_hi = cat(pstore1_storegen_data_mask_hi_hi, pstore1_storegen_data_mask_hi_lo) @[Cat.scala 30:58]
    node pstore1_storegen_data_mask_lo_hi = mux(_s1_hazard_T_4, UInt<8>("hff"), UInt<8>("h0")) @[Bitwise.scala 72:12]
    node pstore1_storegen_data_mask_lo_lo = mux(_s1_hazard_T_3, UInt<8>("hff"), UInt<8>("h0")) @[Bitwise.scala 72:12]
    node pstore1_storegen_data_mask_lo = cat(pstore1_storegen_data_mask_lo_hi, pstore1_storegen_data_mask_lo_lo) @[Cat.scala 30:58]
    node mask_1 = cat(pstore1_storegen_data_mask_hi, pstore1_storegen_data_mask_lo) @[Cat.scala 30:58]
    node _pstore1_storegen_data_T = and(amoalu.io_out_unmasked, mask_1) @[DCache.scala 961:45]
    node _pstore1_storegen_data_T_1 = not(mask_1) @[DCache.scala 961:79]
    node _pstore1_storegen_data_T_2 = and(s2_data_corrected, _pstore1_storegen_data_T_1) @[DCache.scala 961:77]
    node pstore1_storegen_data = or(_pstore1_storegen_data_T, _pstore1_storegen_data_T_2) @[DCache.scala 961:52]
    node _pstore2_storegen_data_T = bits(pstore1_storegen_data, 7, 0) @[DCache.scala 500:44]
    reg pstore2_storegen_data_lo_lo : UInt<8>, gated_clock with :
      reset => (UInt<1>("h0"), pstore2_storegen_data_lo_lo) @[Reg.scala 15:16]
    node _pstore2_storegen_data_T_4 = bits(pstore1_storegen_data, 15, 8) @[DCache.scala 500:44]
    reg pstore2_storegen_data_lo_hi : UInt<8>, gated_clock with :
      reset => (UInt<1>("h0"), pstore2_storegen_data_lo_hi) @[Reg.scala 15:16]
    node _pstore2_storegen_data_T_8 = bits(pstore1_storegen_data, 23, 16) @[DCache.scala 500:44]
    reg pstore2_storegen_data_hi_lo : UInt<8>, gated_clock with :
      reset => (UInt<1>("h0"), pstore2_storegen_data_hi_lo) @[Reg.scala 15:16]
    node _pstore2_storegen_data_T_12 = bits(pstore1_storegen_data, 31, 24) @[DCache.scala 500:44]
    reg pstore2_storegen_data_hi_hi : UInt<8>, gated_clock with :
      reset => (UInt<1>("h0"), pstore2_storegen_data_hi_hi) @[Reg.scala 15:16]
    node pstore2_storegen_data_lo = cat(pstore2_storegen_data_lo_hi, pstore2_storegen_data_lo_lo) @[Cat.scala 30:58]
    node pstore2_storegen_data_hi = cat(pstore2_storegen_data_hi_hi, pstore2_storegen_data_hi_lo) @[Cat.scala 30:58]
    node pstore2_storegen_data = cat(pstore2_storegen_data_hi, pstore2_storegen_data_lo) @[Cat.scala 30:58]
    node _pstore2_storegen_mask_mask_T = not(pstore1_mask) @[DCache.scala 506:37]
    node _pstore2_storegen_mask_mask_T_2 = not(_pstore2_storegen_mask_mask_T) @[DCache.scala 506:15]
    node _dataArb_io_in_0_bits_addr_T = mux(pstore2_valid, pstore2_addr, pstore1_addr) @[DCache.scala 521:36]
    node _dataArb_io_in_0_bits_wdata_T = mux(pstore2_valid, pstore2_storegen_data, pstore1_data) @[DCache.scala 523:63]
    node dataArb_io_in_0_bits_wdata_lo_lo = bits(_dataArb_io_in_0_bits_wdata_T, 7, 0) @[package.scala 202:50]
    node dataArb_io_in_0_bits_wdata_lo_hi = bits(_dataArb_io_in_0_bits_wdata_T, 15, 8) @[package.scala 202:50]
    node dataArb_io_in_0_bits_wdata_hi_lo = bits(_dataArb_io_in_0_bits_wdata_T, 23, 16) @[package.scala 202:50]
    node dataArb_io_in_0_bits_wdata_hi_hi = bits(_dataArb_io_in_0_bits_wdata_T, 31, 24) @[package.scala 202:50]
    node dataArb_io_in_0_bits_wdata_lo = cat(dataArb_io_in_0_bits_wdata_lo_hi, dataArb_io_in_0_bits_wdata_lo_lo) @[Cat.scala 30:58]
    node dataArb_io_in_0_bits_wdata_hi = cat(dataArb_io_in_0_bits_wdata_hi_hi, dataArb_io_in_0_bits_wdata_hi_lo) @[Cat.scala 30:58]
    node _dataArb_io_in_0_bits_eccMask_T = mux(pstore2_valid, mask, pstore1_mask) @[DCache.scala 529:47]
    node _dataArb_io_in_0_bits_eccMask_T_1 = bits(_dataArb_io_in_0_bits_eccMask_T, 0, 0) @[package.scala 202:50]
    node _dataArb_io_in_0_bits_eccMask_T_2 = bits(_dataArb_io_in_0_bits_eccMask_T, 1, 1) @[package.scala 202:50]
    node _dataArb_io_in_0_bits_eccMask_T_3 = bits(_dataArb_io_in_0_bits_eccMask_T, 2, 2) @[package.scala 202:50]
    node _dataArb_io_in_0_bits_eccMask_T_4 = bits(_dataArb_io_in_0_bits_eccMask_T, 3, 3) @[package.scala 202:50]
    node dataArb_io_in_0_bits_eccMask_lo_lo = orr(_dataArb_io_in_0_bits_eccMask_T_1) @[DCache.scala 1151:66]
    node dataArb_io_in_0_bits_eccMask_lo_hi = orr(_dataArb_io_in_0_bits_eccMask_T_2) @[DCache.scala 1151:66]
    node dataArb_io_in_0_bits_eccMask_hi_lo = orr(_dataArb_io_in_0_bits_eccMask_T_3) @[DCache.scala 1151:66]
    node dataArb_io_in_0_bits_eccMask_hi_hi = orr(_dataArb_io_in_0_bits_eccMask_T_4) @[DCache.scala 1151:66]
    node dataArb_io_in_0_bits_eccMask_lo = cat(dataArb_io_in_0_bits_eccMask_lo_hi, dataArb_io_in_0_bits_eccMask_lo_lo) @[Cat.scala 30:58]
    node dataArb_io_in_0_bits_eccMask_hi = cat(dataArb_io_in_0_bits_eccMask_hi_hi, dataArb_io_in_0_bits_eccMask_hi_lo) @[Cat.scala 30:58]
    node a_mask = pad(pstore1_mask, 19) @[DCache.scala 554:29]
    node _get_a_mask_sizeOH_T_1 = dshl(UInt<1>("h1"), _s2_cmd_flush_all_T_1) @[OneHot.scala 65:12]
    node get_a_mask_sizeOH = or(_get_a_mask_sizeOH_T_1, UInt<2>("h1")) @[Misc.scala 201:81]
    node _get_a_mask_T = geq(s2_req_size, UInt<2>("h2")) @[Misc.scala 205:21]
    node get_a_mask_size = bits(get_a_mask_sizeOH, 1, 1) @[Misc.scala 208:26]
    node get_a_mask_bit = bits(s2_req_addr, 1, 1) @[Misc.scala 209:26]
    node get_a_mask_nbit = not(get_a_mask_bit) @[Misc.scala 210:20]
    node _get_a_mask_acc_T = and(get_a_mask_size, get_a_mask_nbit) @[Misc.scala 214:38]
    node get_a_mask_acc = or(_get_a_mask_T, _get_a_mask_acc_T) @[Misc.scala 214:29]
    node _get_a_mask_acc_T_1 = and(get_a_mask_size, get_a_mask_bit) @[Misc.scala 214:38]
    node get_a_mask_acc_1 = or(_get_a_mask_T, _get_a_mask_acc_T_1) @[Misc.scala 214:29]
    node get_a_mask_size_1 = bits(get_a_mask_sizeOH, 0, 0) @[Misc.scala 208:26]
    node get_a_mask_bit_1 = bits(s2_req_addr, 0, 0) @[Misc.scala 209:26]
    node get_a_mask_nbit_1 = not(get_a_mask_bit_1) @[Misc.scala 210:20]
    node get_a_mask_eq_2 = and(get_a_mask_nbit, get_a_mask_nbit_1) @[Misc.scala 213:27]
    node _get_a_mask_acc_T_2 = and(get_a_mask_size_1, get_a_mask_eq_2) @[Misc.scala 214:38]
    node get_a_mask_lo_lo = or(get_a_mask_acc, _get_a_mask_acc_T_2) @[Misc.scala 214:29]
    node get_a_mask_eq_3 = and(get_a_mask_nbit, get_a_mask_bit_1) @[Misc.scala 213:27]
    node _get_a_mask_acc_T_3 = and(get_a_mask_size_1, get_a_mask_eq_3) @[Misc.scala 214:38]
    node get_a_mask_lo_hi = or(get_a_mask_acc, _get_a_mask_acc_T_3) @[Misc.scala 214:29]
    node get_a_mask_eq_4 = and(get_a_mask_bit, get_a_mask_nbit_1) @[Misc.scala 213:27]
    node _get_a_mask_acc_T_4 = and(get_a_mask_size_1, get_a_mask_eq_4) @[Misc.scala 214:38]
    node get_a_mask_hi_lo = or(get_a_mask_acc_1, _get_a_mask_acc_T_4) @[Misc.scala 214:29]
    node get_a_mask_eq_5 = and(get_a_mask_bit, get_a_mask_bit_1) @[Misc.scala 213:27]
    node _get_a_mask_acc_T_5 = and(get_a_mask_size_1, get_a_mask_eq_5) @[Misc.scala 214:38]
    node get_a_mask_hi_hi = or(get_a_mask_acc_1, _get_a_mask_acc_T_5) @[Misc.scala 214:29]
    node get_a_mask_lo = cat(get_a_mask_lo_hi, get_a_mask_lo_lo) @[Cat.scala 30:58]
    node get_a_mask_hi = cat(get_a_mask_hi_hi, get_a_mask_hi_lo) @[Cat.scala 30:58]
    node get_mask = cat(get_a_mask_hi, get_a_mask_lo) @[Cat.scala 30:58]
    node _atomics_T = eq(UInt<5>("h4"), s2_req_cmd) @[Mux.scala 80:60]
    node _atomics_T_1_opcode = mux(_atomics_T, UInt<3>("h3"), UInt<3>("h0")) @[Mux.scala 80:57]
    node atomics_a_size = pad(s2_req_size, 4) @[Edges.scala 513:17 516:15]
    node _atomics_T_1_size = mux(_atomics_T, atomics_a_size, UInt<4>("h0")) @[Mux.scala 80:57]
    node _atomics_T_1_address = mux(_atomics_T, s2_req_addr, UInt<32>("h0")) @[Mux.scala 80:57]
    node _atomics_T_1_mask = mux(_atomics_T, get_mask, UInt<4>("h0")) @[Mux.scala 80:57]
    node _atomics_T_1_data = mux(_atomics_T, pstore1_data, UInt<32>("h0")) @[Mux.scala 80:57]
    node _atomics_T_2 = eq(UInt<5>("h9"), s2_req_cmd) @[Mux.scala 80:60]
    node _atomics_T_3_opcode = mux(_atomics_T_2, UInt<3>("h3"), _atomics_T_1_opcode) @[Mux.scala 80:57]
    node _atomics_T_3_param = mux(_atomics_T_2, UInt<3>("h0"), _atomics_T_1_opcode) @[Mux.scala 80:57]
    node _atomics_T_3_size = mux(_atomics_T_2, atomics_a_size, _atomics_T_1_size) @[Mux.scala 80:57]
    node _atomics_T_3_address = mux(_atomics_T_2, s2_req_addr, _atomics_T_1_address) @[Mux.scala 80:57]
    node _atomics_T_3_mask = mux(_atomics_T_2, get_mask, _atomics_T_1_mask) @[Mux.scala 80:57]
    node _atomics_T_3_data = mux(_atomics_T_2, pstore1_data, _atomics_T_1_data) @[Mux.scala 80:57]
    node _atomics_T_4 = eq(UInt<5>("ha"), s2_req_cmd) @[Mux.scala 80:60]
    node _atomics_T_5_opcode = mux(_atomics_T_4, UInt<3>("h3"), _atomics_T_3_opcode) @[Mux.scala 80:57]
    node _atomics_T_5_param = mux(_atomics_T_4, UInt<3>("h1"), _atomics_T_3_param) @[Mux.scala 80:57]
    node _atomics_T_5_size = mux(_atomics_T_4, atomics_a_size, _atomics_T_3_size) @[Mux.scala 80:57]
    node _atomics_T_5_address = mux(_atomics_T_4, s2_req_addr, _atomics_T_3_address) @[Mux.scala 80:57]
    node _atomics_T_5_mask = mux(_atomics_T_4, get_mask, _atomics_T_3_mask) @[Mux.scala 80:57]
    node _atomics_T_5_data = mux(_atomics_T_4, pstore1_data, _atomics_T_3_data) @[Mux.scala 80:57]
    node _atomics_T_6 = eq(UInt<5>("hb"), s2_req_cmd) @[Mux.scala 80:60]
    node _atomics_T_7_opcode = mux(_atomics_T_6, UInt<3>("h3"), _atomics_T_5_opcode) @[Mux.scala 80:57]
    node _atomics_T_7_param = mux(_atomics_T_6, UInt<3>("h2"), _atomics_T_5_param) @[Mux.scala 80:57]
    node _atomics_T_7_size = mux(_atomics_T_6, atomics_a_size, _atomics_T_5_size) @[Mux.scala 80:57]
    node _atomics_T_7_address = mux(_atomics_T_6, s2_req_addr, _atomics_T_5_address) @[Mux.scala 80:57]
    node _atomics_T_7_mask = mux(_atomics_T_6, get_mask, _atomics_T_5_mask) @[Mux.scala 80:57]
    node _atomics_T_7_data = mux(_atomics_T_6, pstore1_data, _atomics_T_5_data) @[Mux.scala 80:57]
    node _atomics_T_8 = eq(UInt<5>("h8"), s2_req_cmd) @[Mux.scala 80:60]
    node _atomics_T_9_opcode = mux(_atomics_T_8, UInt<3>("h2"), _atomics_T_7_opcode) @[Mux.scala 80:57]
    node _atomics_T_9_param = mux(_atomics_T_8, UInt<3>("h4"), _atomics_T_7_param) @[Mux.scala 80:57]
    node _atomics_T_9_size = mux(_atomics_T_8, atomics_a_size, _atomics_T_7_size) @[Mux.scala 80:57]
    node _atomics_T_9_address = mux(_atomics_T_8, s2_req_addr, _atomics_T_7_address) @[Mux.scala 80:57]
    node _atomics_T_9_mask = mux(_atomics_T_8, get_mask, _atomics_T_7_mask) @[Mux.scala 80:57]
    node _atomics_T_9_data = mux(_atomics_T_8, pstore1_data, _atomics_T_7_data) @[Mux.scala 80:57]
    node _atomics_T_10 = eq(UInt<5>("hc"), s2_req_cmd) @[Mux.scala 80:60]
    node _atomics_T_11_opcode = mux(_atomics_T_10, UInt<3>("h2"), _atomics_T_9_opcode) @[Mux.scala 80:57]
    node _atomics_T_11_param = mux(_atomics_T_10, UInt<3>("h0"), _atomics_T_9_param) @[Mux.scala 80:57]
    node _atomics_T_11_size = mux(_atomics_T_10, atomics_a_size, _atomics_T_9_size) @[Mux.scala 80:57]
    node _atomics_T_11_address = mux(_atomics_T_10, s2_req_addr, _atomics_T_9_address) @[Mux.scala 80:57]
    node _atomics_T_11_mask = mux(_atomics_T_10, get_mask, _atomics_T_9_mask) @[Mux.scala 80:57]
    node _atomics_T_11_data = mux(_atomics_T_10, pstore1_data, _atomics_T_9_data) @[Mux.scala 80:57]
    node _atomics_T_12 = eq(UInt<5>("hd"), s2_req_cmd) @[Mux.scala 80:60]
    node _atomics_T_13_opcode = mux(_atomics_T_12, UInt<3>("h2"), _atomics_T_11_opcode) @[Mux.scala 80:57]
    node _atomics_T_13_param = mux(_atomics_T_12, UInt<3>("h1"), _atomics_T_11_param) @[Mux.scala 80:57]
    node _atomics_T_13_size = mux(_atomics_T_12, atomics_a_size, _atomics_T_11_size) @[Mux.scala 80:57]
    node _atomics_T_13_address = mux(_atomics_T_12, s2_req_addr, _atomics_T_11_address) @[Mux.scala 80:57]
    node _atomics_T_13_mask = mux(_atomics_T_12, get_mask, _atomics_T_11_mask) @[Mux.scala 80:57]
    node _atomics_T_13_data = mux(_atomics_T_12, pstore1_data, _atomics_T_11_data) @[Mux.scala 80:57]
    node _atomics_T_14 = eq(UInt<5>("he"), s2_req_cmd) @[Mux.scala 80:60]
    node _atomics_T_15_opcode = mux(_atomics_T_14, UInt<3>("h2"), _atomics_T_13_opcode) @[Mux.scala 80:57]
    node _atomics_T_15_param = mux(_atomics_T_14, UInt<3>("h2"), _atomics_T_13_param) @[Mux.scala 80:57]
    node _atomics_T_15_size = mux(_atomics_T_14, atomics_a_size, _atomics_T_13_size) @[Mux.scala 80:57]
    node _atomics_T_15_address = mux(_atomics_T_14, s2_req_addr, _atomics_T_13_address) @[Mux.scala 80:57]
    node _atomics_T_15_mask = mux(_atomics_T_14, get_mask, _atomics_T_13_mask) @[Mux.scala 80:57]
    node _atomics_T_15_data = mux(_atomics_T_14, pstore1_data, _atomics_T_13_data) @[Mux.scala 80:57]
    node _atomics_T_16 = eq(UInt<5>("hf"), s2_req_cmd) @[Mux.scala 80:60]
    node atomics_opcode = mux(_atomics_T_16, UInt<3>("h2"), _atomics_T_15_opcode) @[Mux.scala 80:57]
    node atomics_param = mux(_atomics_T_16, UInt<3>("h3"), _atomics_T_15_param) @[Mux.scala 80:57]
    node atomics_size = mux(_atomics_T_16, atomics_a_size, _atomics_T_15_size) @[Mux.scala 80:57]
    node atomics_address = mux(_atomics_T_16, s2_req_addr, _atomics_T_15_address) @[Mux.scala 80:57]
    node atomics_mask = mux(_atomics_T_16, get_mask, _atomics_T_15_mask) @[Mux.scala 80:57]
    node atomics_data = mux(_atomics_T_16, pstore1_data, _atomics_T_15_data) @[Mux.scala 80:57]
    node _tl_out_a_bits_T_1 = not(s2_write) @[DCache.scala 581:9]
    node _tl_out_a_bits_T_3 = not(s2_read) @[DCache.scala 583:9]
    node _tl_out_a_bits_T_4_opcode = mux(_tl_out_a_bits_T_3, UInt<3>("h0"), atomics_opcode) @[DCache.scala 583:8]
    node _tl_out_a_bits_T_4_param = mux(_tl_out_a_bits_T_3, UInt<3>("h0"), atomics_param) @[DCache.scala 583:8]
    node _tl_out_a_bits_T_4_size = mux(_tl_out_a_bits_T_3, atomics_a_size, atomics_size) @[DCache.scala 583:8]
    node _tl_out_a_bits_T_4_address = mux(_tl_out_a_bits_T_3, s2_req_addr, atomics_address) @[DCache.scala 583:8]
    node _tl_out_a_bits_T_4_mask = mux(_tl_out_a_bits_T_3, get_mask, atomics_mask) @[DCache.scala 583:8]
    node _tl_out_a_bits_T_4_data = mux(_tl_out_a_bits_T_3, pstore1_data, atomics_data) @[DCache.scala 583:8]
    node _tl_out_a_bits_T_5_opcode = mux(_s2_write_T_1, UInt<3>("h1"), _tl_out_a_bits_T_4_opcode) @[DCache.scala 582:8]
    node _tl_out_a_bits_T_5_param = mux(_s2_write_T_1, UInt<3>("h0"), _tl_out_a_bits_T_4_param) @[DCache.scala 582:8]
    node _tl_out_a_bits_T_5_size = mux(_s2_write_T_1, atomics_a_size, _tl_out_a_bits_T_4_size) @[DCache.scala 582:8]
    node _tl_out_a_bits_T_5_address = mux(_s2_write_T_1, s2_req_addr, _tl_out_a_bits_T_4_address) @[DCache.scala 582:8]
    node putpartial_mask = bits(a_mask, 3, 0) @[Edges.scala 483:17 489:15]
    node _tl_out_a_bits_T_5_mask = mux(_s2_write_T_1, putpartial_mask, _tl_out_a_bits_T_4_mask) @[DCache.scala 582:8]
    node _tl_out_a_bits_T_5_data = mux(_s2_write_T_1, pstore1_data, _tl_out_a_bits_T_4_data) @[DCache.scala 582:8]
    node _T_227 = and(auto_out_a_ready, s2_valid_uncached_pending) @[Decoupled.scala 40:37]
    node _GEN_132 = or(_T_227, uncachedInFlight_0) @[DCache.scala 603:26 215:33]
    node _beats1_decode_T_1 = dshl(UInt<12>("hfff"), auto_out_d_bits_size) @[package.scala 234:77]
    node _beats1_decode_T_2 = bits(_beats1_decode_T_1, 11, 0) @[package.scala 234:82]
    node _beats1_decode_T_3 = not(_beats1_decode_T_2) @[package.scala 234:46]
    node beats1_decode = shr(_beats1_decode_T_3, 2) @[Edges.scala 219:59]
    node beats1_opdata = bits(auto_out_d_bits_opcode, 0, 0) @[Edges.scala 105:36]
    node beats1 = mux(beats1_opdata, beats1_decode, UInt<10>("h0")) @[Edges.scala 220:14]
    node _counter1_T = sub(counter, UInt<10>("h1")) @[Edges.scala 229:28]
    node counter1 = tail(_counter1_T, 1) @[Edges.scala 229:28]
    node _last_T = eq(counter, UInt<10>("h1")) @[Edges.scala 231:25]
    node _last_T_1 = eq(beats1, UInt<10>("h0")) @[Edges.scala 231:47]
    node d_last = or(_last_T, _last_T_1) @[Edges.scala 231:37]
    node d_done = and(d_last, _T_244) @[Edges.scala 232:22]
    node _counter_T = mux(d_first, beats1, counter1) @[Edges.scala 235:21]
    node _GEN_146 = mux(_T_244, _counter_T, counter) @[Edges.scala 234:17 235:15 228:27]
    node _T_230 = not(auto_out_d_valid) @[DCache.scala 626:14]
    node _T_231 = eq(auto_out_d_bits_opcode, UInt<3>("h1")) @[package.scala 15:47]
    node _T_232 = eq(auto_out_d_bits_opcode, UInt<3>("h0")) @[package.scala 15:47]
    node _T_233 = eq(auto_out_d_bits_opcode, UInt<3>("h2")) @[package.scala 15:47]
    node _T_234 = or(_T_231, _T_232) @[package.scala 72:59]
    node _T_235 = or(_T_234, _T_233) @[package.scala 72:59]
    node _T_236 = or(_T_230, _T_235) @[DCache.scala 626:30]
    node _T_238 = or(_T_236, reset) @[DCache.scala 626:13]
    node _T_239 = not(_T_238) @[DCache.scala 626:13]
    node _T_247 = not(reset) @[DCache.scala 649:13]
    node _T_251 = or(uncachedInFlight_0, reset) @[DCache.scala 659:17]
    node _T_252 = not(_T_251) @[DCache.scala 659:17]
    node _GEN_152 = mux(d_last, UInt<1>("h0"), _GEN_132) @[DCache.scala 658:28 660:13]
    node _s2_req_addr_dontCareBits_T = shr(s1_paddr, 2) @[DCache.scala 673:41]
    node dontCareBits = shl(_s2_req_addr_dontCareBits_T, 2) @[DCache.scala 673:55]
    node _s2_req_addr_T = bits(uncachedReqs_0_addr, 1, 0) @[DCache.scala 674:45]
    node _GEN_222 = pad(_s2_req_addr_T, 32) @[DCache.scala 674:26]
    node _s2_req_addr_T_1 = or(dontCareBits, _GEN_222) @[DCache.scala 674:26]
    node _GEN_154 = mux(data_1, UInt<5>("h0"), _GEN_37) @[DCache.scala 663:34 667:22]
    node _GEN_155 = mux(data_1, uncachedReqs_0_size, _GEN_38) @[DCache.scala 663:34 668:23]
    node _GEN_156 = mux(data_1, uncachedReqs_0_signed, _GEN_39) @[DCache.scala 663:34 669:25]
    node _GEN_157 = mux(data_1, uncachedReqs_0_tag, _GEN_36) @[DCache.scala 663:34 670:22]
    node _GEN_158 = mux(data_1, _s2_req_addr_T_1, _GEN_35) @[DCache.scala 663:34 671:23]
    node _GEN_174 = mux(grantIsCached, _GEN_132, _GEN_152) @[DCache.scala 647:26]
    node _GEN_176 = mux(grantIsCached, _GEN_37, _GEN_154) @[DCache.scala 647:26]
    node _GEN_177 = mux(grantIsCached, _GEN_38, _GEN_155) @[DCache.scala 647:26]
    node _GEN_178 = mux(grantIsCached, _GEN_39, _GEN_156) @[DCache.scala 647:26]
    node _GEN_179 = mux(grantIsCached, _GEN_36, _GEN_157) @[DCache.scala 647:26]
    node _GEN_180 = mux(grantIsCached, _GEN_35, _GEN_158) @[DCache.scala 647:26]
    node _GEN_187 = mux(_T_244, _GEN_174, _GEN_132) @[DCache.scala 646:26]
    node _T_261 = and(_T_244, d_first) @[DCache.scala 688:47]
    node _T_262 = and(_T_261, grantIsCached) @[DCache.scala 688:58]
    node _T_263 = not(_T_262) @[DCache.scala 688:26]
    node _T_265 = or(_T_263, reset) @[DCache.scala 688:9]
    node _T_266 = not(_T_265) @[DCache.scala 688:9]
    node _dataArb_io_in_1_valid_T = and(auto_out_d_valid, grantIsRefill) @[DCache.scala 693:44]
    node _metaArb_io_in_3_valid_T = and(grantIsCached, d_done) @[DCache.scala 713:43]
    node _metaArb_io_in_3_valid_T_1 = not(auto_out_d_bits_denied) @[DCache.scala 713:56]
    node _GEN_197 = mux(auto_out_d_valid, UInt<1>("h0"), _GEN_31) @[DCache.scala 727:29 728:26]
    node _GEN_198 = or(auto_out_d_valid, _dataArb_io_in_1_valid_T) @[DCache.scala 693:26 727:29 729:32]
    node _GEN_199 = mux(auto_out_d_valid, UInt<1>("h0"), dataArb.io_in_0_bits_write) @[DCache.scala 706:27 727:29 730:37]
    node _GEN_200 = mux(auto_out_d_valid, _T_267, dataArb.io_out_valid) @[DCache.scala 723:24 727:29 731:28]
    node _io_cpu_ordered_T = not(s1_req_no_xcpt) @[DCache.scala 900:35]
    node _io_cpu_ordered_T_1 = and(s1_valid, _io_cpu_ordered_T) @[DCache.scala 900:32]
    node _io_cpu_ordered_T_2 = not(s2_req_no_xcpt) @[DCache.scala 900:72]
    node _io_cpu_ordered_T_3 = and(s2_valid, _io_cpu_ordered_T_2) @[DCache.scala 900:69]
    node _io_cpu_ordered_T_4 = or(_io_cpu_ordered_T_1, _io_cpu_ordered_T_3) @[DCache.scala 900:57]
    node _io_cpu_ordered_T_7 = or(_io_cpu_ordered_T_4, _s2_valid_cached_miss_T_2) @[DCache.scala 900:115]
    node _s1_xcpt_valid_T_1 = and(tlb.io_req_valid, _io_cpu_ordered_T) @[DCache.scala 902:40]
    reg io_cpu_s2_xcpt_REG : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), io_cpu_s2_xcpt_REG) @[DCache.scala 903:32]
    node _T_282 = or(_c_cat_T_47, _s2_write_T_3) @[package.scala 72:59]
    node _T_283 = and(s2_valid_masked, _T_282) @[DCache.scala 906:30]
    node _T_284 = not(_T_283) @[DCache.scala 906:12]
    node _T_286 = or(_T_284, reset) @[DCache.scala 906:11]
    node _T_287 = not(_T_286) @[DCache.scala 906:11]
    reg doUncachedResp : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), doUncachedResp) @[DCache.scala 918:27]
    node _T_290 = or(_io_cpu_s2_nack_T_4, reset) @[DCache.scala 922:11]
    node _T_291 = not(_T_290) @[DCache.scala 922:11]
    node _io_cpu_resp_bits_data_shifted_T_1 = bits(s2_data_corrected, 31, 16) @[AMOALU.scala 39:37]
    node _io_cpu_resp_bits_data_shifted_T_2 = bits(s2_data_corrected, 15, 0) @[AMOALU.scala 39:55]
    node io_cpu_resp_bits_data_shifted = mux(get_a_mask_bit, _io_cpu_resp_bits_data_shifted_T_1, _io_cpu_resp_bits_data_shifted_T_2) @[AMOALU.scala 39:24]
    node _io_cpu_resp_bits_data_T = eq(s2_req_size, UInt<2>("h1")) @[AMOALU.scala 42:26]
    node _io_cpu_resp_bits_data_T_2 = bits(io_cpu_resp_bits_data_shifted, 15, 15) @[AMOALU.scala 42:85]
    node _io_cpu_resp_bits_data_T_3 = and(s2_req_signed, _io_cpu_resp_bits_data_T_2) @[AMOALU.scala 42:76]
    node _io_cpu_resp_bits_data_T_5 = mux(_io_cpu_resp_bits_data_T_3, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 72:12]
    node io_cpu_resp_bits_data_hi = mux(_io_cpu_resp_bits_data_T, _io_cpu_resp_bits_data_T_5, _io_cpu_resp_bits_data_shifted_T_1) @[AMOALU.scala 42:20]
    node _io_cpu_resp_bits_data_T_7 = cat(io_cpu_resp_bits_data_hi, io_cpu_resp_bits_data_shifted) @[Cat.scala 30:58]
    node _io_cpu_resp_bits_data_shifted_T_4 = bits(_io_cpu_resp_bits_data_T_7, 15, 8) @[AMOALU.scala 39:37]
    node _io_cpu_resp_bits_data_shifted_T_5 = bits(_io_cpu_resp_bits_data_T_7, 7, 0) @[AMOALU.scala 39:55]
    node io_cpu_resp_bits_data_shifted_1 = mux(get_a_mask_bit_1, _io_cpu_resp_bits_data_shifted_T_4, _io_cpu_resp_bits_data_shifted_T_5) @[AMOALU.scala 39:24]
    node _io_cpu_resp_bits_data_T_8 = eq(s2_req_size, UInt<2>("h0")) @[AMOALU.scala 42:26]
    node _io_cpu_resp_bits_data_T_10 = bits(io_cpu_resp_bits_data_shifted_1, 7, 7) @[AMOALU.scala 42:85]
    node _io_cpu_resp_bits_data_T_11 = and(s2_req_signed, _io_cpu_resp_bits_data_T_10) @[AMOALU.scala 42:76]
    node _io_cpu_resp_bits_data_T_13 = mux(_io_cpu_resp_bits_data_T_11, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 72:12]
    node _io_cpu_resp_bits_data_T_14 = bits(_io_cpu_resp_bits_data_T_7, 31, 8) @[AMOALU.scala 42:98]
    node io_cpu_resp_bits_data_hi_1 = mux(_io_cpu_resp_bits_data_T_8, _io_cpu_resp_bits_data_T_13, _io_cpu_resp_bits_data_T_14) @[AMOALU.scala 42:20]
    node _s1_flush_valid_T = and(metaArb.io_in_5_ready, metaArb.io_in_5_valid) @[Decoupled.scala 40:37]
    node _s1_flush_valid_T_1 = not(s1_flush_valid) @[DCache.scala 984:48]
    node _s1_flush_valid_T_2 = and(_s1_flush_valid_T, _s1_flush_valid_T_1) @[DCache.scala 984:45]
    node _T_297 = not(grantIsCached) @[DCache.scala 1103:35]
    node _GEN_226 = and(_T_244, grantIsCached) @[DCache.scala 649:13]
    node _GEN_229 = and(_T_244, _T_297) @[DCache.scala 659:17]
    node _GEN_230 = and(_GEN_229, d_last) @[DCache.scala 659:17]
    auto_out_a_valid <= and(s2_valid_miss, _s2_valid_uncached_pending_T_2) @[DCache.scala 402:64]
    auto_out_a_bits_opcode <= mux(_tl_out_a_bits_T_1, UInt<3>("h4"), _tl_out_a_bits_T_5_opcode) @[DCache.scala 581:8]
    auto_out_a_bits_param <= mux(_tl_out_a_bits_T_1, UInt<3>("h0"), _tl_out_a_bits_T_5_param) @[DCache.scala 581:8]
    auto_out_a_bits_size <= mux(_tl_out_a_bits_T_1, atomics_a_size, _tl_out_a_bits_T_5_size) @[DCache.scala 581:8]
    auto_out_a_bits_address <= mux(_tl_out_a_bits_T_1, s2_req_addr, _tl_out_a_bits_T_5_address) @[DCache.scala 581:8]
    auto_out_a_bits_mask <= mux(_tl_out_a_bits_T_1, get_mask, _tl_out_a_bits_T_5_mask) @[DCache.scala 581:8]
    auto_out_a_bits_data <= mux(_tl_out_a_bits_T_1, UInt<32>("h0"), _tl_out_a_bits_T_5_data) @[DCache.scala 581:8]
    auto_out_d_ready <= mux(_T_270, UInt<1>("h0"), _GEN_196) @[DCache.scala 724:68 725:22]
    io_cpu_req_ready <= mux(_T_270, _GEN_197, _GEN_31) @[DCache.scala 724:68]
    io_cpu_s2_nack <= and(_io_cpu_s2_nack_T_3, _io_cpu_s2_nack_T_4) @[DCache.scala 417:86]
    io_cpu_resp_valid <= or(s2_valid_hit_pre_data_ecc_and_waw, doUncachedResp) @[DCache.scala 919:51]
    io_cpu_resp_bits_tag <= s2_req_tag @[DCache.scala 890:20]
    io_cpu_resp_bits_data <= cat(io_cpu_resp_bits_data_hi_1, io_cpu_resp_bits_data_shifted_1) @[Cat.scala 30:58]
    io_cpu_resp_bits_replay <= doUncachedResp @[DCache.scala 921:25 892:27 923:29]
    io_cpu_resp_bits_has_data <= or(_s2_read_T_4, _s2_write_T_21) @[Consts.scala 81:75]
    io_cpu_resp_bits_data_word_bypass <= cat(s2_data_corrected_hi, s2_data_corrected_lo) @[Cat.scala 30:58]
    io_cpu_resp_bits_data_raw <= cat(s2_data_corrected_hi, s2_data_corrected_lo) @[Cat.scala 30:58]
    io_cpu_replay_next <= and(_T_244, data_1) @[DCache.scala 920:41]
    io_cpu_s2_xcpt_ma_ld <= and(io_cpu_s2_xcpt_REG, s2_tlb_xcpt_ma_ld) @[DCache.scala 903:24]
    io_cpu_s2_xcpt_ma_st <= and(io_cpu_s2_xcpt_REG, s2_tlb_xcpt_ma_st) @[DCache.scala 903:24]
    io_cpu_s2_xcpt_pf_ld <= and(io_cpu_s2_xcpt_REG, s2_tlb_xcpt_pf_ld) @[DCache.scala 903:24]
    io_cpu_s2_xcpt_pf_st <= and(io_cpu_s2_xcpt_REG, s2_tlb_xcpt_pf_st) @[DCache.scala 903:24]
    io_cpu_s2_xcpt_ae_ld <= and(io_cpu_s2_xcpt_REG, s2_tlb_xcpt_ae_ld) @[DCache.scala 903:24]
    io_cpu_s2_xcpt_ae_st <= and(io_cpu_s2_xcpt_REG, s2_tlb_xcpt_ae_st) @[DCache.scala 903:24]
    io_cpu_ordered <= not(_io_cpu_ordered_T_7) @[DCache.scala 900:21]
    io_cpu_perf_grant <= and(auto_out_d_valid, d_last) @[DCache.scala 1048:39]
    tlb.io_req_valid <= and(s1_valid_masked, s1_cmd_uses_tlb) @[DCache.scala 252:71]
    tlb.io_req_bits_vaddr <= s1_tlb_req_vaddr @[DCache.scala 253:19]
    tlb.io_req_bits_size <= s1_tlb_req_size @[DCache.scala 253:19]
    tlb.io_req_bits_cmd <= s1_tlb_req_cmd @[DCache.scala 253:19]
    tlb.io_ptw_status_debug <= io_ptw_status_debug @[DCache.scala 250:10]
    tlb.io_ptw_status_dprv <= UInt<2>("h3") @[DCache.scala 250:10]
    tlb.io_ptw_pmp_0_cfg_l <= io_ptw_pmp_0_cfg_l @[DCache.scala 250:10]
    tlb.io_ptw_pmp_0_cfg_a <= io_ptw_pmp_0_cfg_a @[DCache.scala 250:10]
    tlb.io_ptw_pmp_0_cfg_x <= io_ptw_pmp_0_cfg_x @[DCache.scala 250:10]
    tlb.io_ptw_pmp_0_cfg_w <= io_ptw_pmp_0_cfg_w @[DCache.scala 250:10]
    tlb.io_ptw_pmp_0_cfg_r <= io_ptw_pmp_0_cfg_r @[DCache.scala 250:10]
    tlb.io_ptw_pmp_0_addr <= io_ptw_pmp_0_addr @[DCache.scala 250:10]
    tlb.io_ptw_pmp_0_mask <= io_ptw_pmp_0_mask @[DCache.scala 250:10]
    tlb.io_ptw_pmp_1_cfg_l <= io_ptw_pmp_1_cfg_l @[DCache.scala 250:10]
    tlb.io_ptw_pmp_1_cfg_a <= io_ptw_pmp_1_cfg_a @[DCache.scala 250:10]
    tlb.io_ptw_pmp_1_cfg_x <= io_ptw_pmp_1_cfg_x @[DCache.scala 250:10]
    tlb.io_ptw_pmp_1_cfg_w <= io_ptw_pmp_1_cfg_w @[DCache.scala 250:10]
    tlb.io_ptw_pmp_1_cfg_r <= io_ptw_pmp_1_cfg_r @[DCache.scala 250:10]
    tlb.io_ptw_pmp_1_addr <= io_ptw_pmp_1_addr @[DCache.scala 250:10]
    tlb.io_ptw_pmp_1_mask <= io_ptw_pmp_1_mask @[DCache.scala 250:10]
    tlb.io_ptw_pmp_2_cfg_l <= io_ptw_pmp_2_cfg_l @[DCache.scala 250:10]
    tlb.io_ptw_pmp_2_cfg_a <= io_ptw_pmp_2_cfg_a @[DCache.scala 250:10]
    tlb.io_ptw_pmp_2_cfg_x <= io_ptw_pmp_2_cfg_x @[DCache.scala 250:10]
    tlb.io_ptw_pmp_2_cfg_w <= io_ptw_pmp_2_cfg_w @[DCache.scala 250:10]
    tlb.io_ptw_pmp_2_cfg_r <= io_ptw_pmp_2_cfg_r @[DCache.scala 250:10]
    tlb.io_ptw_pmp_2_addr <= io_ptw_pmp_2_addr @[DCache.scala 250:10]
    tlb.io_ptw_pmp_2_mask <= io_ptw_pmp_2_mask @[DCache.scala 250:10]
    tlb.io_ptw_pmp_3_cfg_l <= io_ptw_pmp_3_cfg_l @[DCache.scala 250:10]
    tlb.io_ptw_pmp_3_cfg_a <= io_ptw_pmp_3_cfg_a @[DCache.scala 250:10]
    tlb.io_ptw_pmp_3_cfg_x <= io_ptw_pmp_3_cfg_x @[DCache.scala 250:10]
    tlb.io_ptw_pmp_3_cfg_w <= io_ptw_pmp_3_cfg_w @[DCache.scala 250:10]
    tlb.io_ptw_pmp_3_cfg_r <= io_ptw_pmp_3_cfg_r @[DCache.scala 250:10]
    tlb.io_ptw_pmp_3_addr <= io_ptw_pmp_3_addr @[DCache.scala 250:10]
    tlb.io_ptw_pmp_3_mask <= io_ptw_pmp_3_mask @[DCache.scala 250:10]
    tlb.io_ptw_pmp_4_cfg_l <= io_ptw_pmp_4_cfg_l @[DCache.scala 250:10]
    tlb.io_ptw_pmp_4_cfg_a <= io_ptw_pmp_4_cfg_a @[DCache.scala 250:10]
    tlb.io_ptw_pmp_4_cfg_x <= io_ptw_pmp_4_cfg_x @[DCache.scala 250:10]
    tlb.io_ptw_pmp_4_cfg_w <= io_ptw_pmp_4_cfg_w @[DCache.scala 250:10]
    tlb.io_ptw_pmp_4_cfg_r <= io_ptw_pmp_4_cfg_r @[DCache.scala 250:10]
    tlb.io_ptw_pmp_4_addr <= io_ptw_pmp_4_addr @[DCache.scala 250:10]
    tlb.io_ptw_pmp_4_mask <= io_ptw_pmp_4_mask @[DCache.scala 250:10]
    tlb.io_ptw_pmp_5_cfg_l <= io_ptw_pmp_5_cfg_l @[DCache.scala 250:10]
    tlb.io_ptw_pmp_5_cfg_a <= io_ptw_pmp_5_cfg_a @[DCache.scala 250:10]
    tlb.io_ptw_pmp_5_cfg_x <= io_ptw_pmp_5_cfg_x @[DCache.scala 250:10]
    tlb.io_ptw_pmp_5_cfg_w <= io_ptw_pmp_5_cfg_w @[DCache.scala 250:10]
    tlb.io_ptw_pmp_5_cfg_r <= io_ptw_pmp_5_cfg_r @[DCache.scala 250:10]
    tlb.io_ptw_pmp_5_addr <= io_ptw_pmp_5_addr @[DCache.scala 250:10]
    tlb.io_ptw_pmp_5_mask <= io_ptw_pmp_5_mask @[DCache.scala 250:10]
    tlb.io_ptw_pmp_6_cfg_l <= io_ptw_pmp_6_cfg_l @[DCache.scala 250:10]
    tlb.io_ptw_pmp_6_cfg_a <= io_ptw_pmp_6_cfg_a @[DCache.scala 250:10]
    tlb.io_ptw_pmp_6_cfg_x <= io_ptw_pmp_6_cfg_x @[DCache.scala 250:10]
    tlb.io_ptw_pmp_6_cfg_w <= io_ptw_pmp_6_cfg_w @[DCache.scala 250:10]
    tlb.io_ptw_pmp_6_cfg_r <= io_ptw_pmp_6_cfg_r @[DCache.scala 250:10]
    tlb.io_ptw_pmp_6_addr <= io_ptw_pmp_6_addr @[DCache.scala 250:10]
    tlb.io_ptw_pmp_6_mask <= io_ptw_pmp_6_mask @[DCache.scala 250:10]
    tlb.io_ptw_pmp_7_cfg_l <= io_ptw_pmp_7_cfg_l @[DCache.scala 250:10]
    tlb.io_ptw_pmp_7_cfg_a <= io_ptw_pmp_7_cfg_a @[DCache.scala 250:10]
    tlb.io_ptw_pmp_7_cfg_x <= io_ptw_pmp_7_cfg_x @[DCache.scala 250:10]
    tlb.io_ptw_pmp_7_cfg_w <= io_ptw_pmp_7_cfg_w @[DCache.scala 250:10]
    tlb.io_ptw_pmp_7_cfg_r <= io_ptw_pmp_7_cfg_r @[DCache.scala 250:10]
    tlb.io_ptw_pmp_7_addr <= io_ptw_pmp_7_addr @[DCache.scala 250:10]
    tlb.io_ptw_pmp_7_mask <= io_ptw_pmp_7_mask @[DCache.scala 250:10]
    pma_checker.io_req_valid <= UInt<1>("h0")
    pma_checker.io_req_bits_vaddr <= UInt<32>("h0")
    pma_checker.io_req_bits_size <= s1_req_size @[DCache.scala 268:27]
    pma_checker.io_req_bits_cmd <= s1_req_cmd @[DCache.scala 268:27]
    pma_checker.io_ptw_status_debug <= UInt<1>("h0")
    pma_checker.io_ptw_status_dprv <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_0_cfg_l <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_0_cfg_a <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_0_cfg_x <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_0_cfg_w <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_0_cfg_r <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_0_addr <= UInt<30>("h0")
    pma_checker.io_ptw_pmp_0_mask <= UInt<32>("h0")
    pma_checker.io_ptw_pmp_1_cfg_l <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_1_cfg_a <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_1_cfg_x <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_1_cfg_w <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_1_cfg_r <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_1_addr <= UInt<30>("h0")
    pma_checker.io_ptw_pmp_1_mask <= UInt<32>("h0")
    pma_checker.io_ptw_pmp_2_cfg_l <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_2_cfg_a <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_2_cfg_x <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_2_cfg_w <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_2_cfg_r <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_2_addr <= UInt<30>("h0")
    pma_checker.io_ptw_pmp_2_mask <= UInt<32>("h0")
    pma_checker.io_ptw_pmp_3_cfg_l <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_3_cfg_a <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_3_cfg_x <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_3_cfg_w <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_3_cfg_r <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_3_addr <= UInt<30>("h0")
    pma_checker.io_ptw_pmp_3_mask <= UInt<32>("h0")
    pma_checker.io_ptw_pmp_4_cfg_l <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_4_cfg_a <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_4_cfg_x <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_4_cfg_w <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_4_cfg_r <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_4_addr <= UInt<30>("h0")
    pma_checker.io_ptw_pmp_4_mask <= UInt<32>("h0")
    pma_checker.io_ptw_pmp_5_cfg_l <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_5_cfg_a <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_5_cfg_x <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_5_cfg_w <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_5_cfg_r <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_5_addr <= UInt<30>("h0")
    pma_checker.io_ptw_pmp_5_mask <= UInt<32>("h0")
    pma_checker.io_ptw_pmp_6_cfg_l <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_6_cfg_a <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_6_cfg_x <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_6_cfg_w <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_6_cfg_r <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_6_addr <= UInt<30>("h0")
    pma_checker.io_ptw_pmp_6_mask <= UInt<32>("h0")
    pma_checker.io_ptw_pmp_7_cfg_l <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_7_cfg_a <= UInt<2>("h0")
    pma_checker.io_ptw_pmp_7_cfg_x <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_7_cfg_w <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_7_cfg_r <= UInt<1>("h0")
    pma_checker.io_ptw_pmp_7_addr <= UInt<30>("h0")
    pma_checker.io_ptw_pmp_7_mask <= UInt<32>("h0")
    metaArb.io_in_2_valid <= and(s2_valid_hit_pre_data_ecc_and_waw, s2_update_meta) @[DCache.scala 434:63]
    metaArb.io_in_2_bits_addr <= cat(dataArb_io_in_3_bits_addr_hi, metaArb_io_in_2_bits_addr_lo) @[Cat.scala 30:58]
    metaArb.io_in_3_valid <= and(_metaArb_io_in_3_valid_T, _metaArb_io_in_3_valid_T_1) @[DCache.scala 713:53]
    metaArb.io_in_3_bits_addr <= cat(dataArb_io_in_3_bits_addr_hi, metaArb_io_in_2_bits_addr_lo) @[Cat.scala 30:58]
    metaArb.io_in_5_valid <= UInt<1>("h0") @[DCache.scala 985:38]
    metaArb.io_in_7_valid <= io_cpu_req_valid @[DCache.scala 240:26]
    metaArb.io_in_7_bits_addr <= io_cpu_req_bits_addr @[DCache.scala 243:30]
    data.clock <= gated_clock
    data.io_req_valid <= dataArb.io_out_valid @[DCache.scala 135:15]
    data.io_req_bits_addr <= dataArb.io_out_bits_addr @[DCache.scala 135:15]
    data.io_req_bits_write <= dataArb.io_out_bits_write @[DCache.scala 135:15]
    data.io_req_bits_wdata <= dataArb.io_out_bits_wdata @[DCache.scala 135:15]
    data.io_req_bits_eccMask <= dataArb.io_out_bits_eccMask @[DCache.scala 135:15]
    dataArb.io_in_0_valid <= or(pstore_drain_structural, _pstore_drain_T_10) @[DCache.scala 489:48]
    dataArb.io_in_0_bits_addr <= bits(_dataArb_io_in_0_bits_addr_T, 13, 0) @[DCache.scala 521:30]
    dataArb.io_in_0_bits_write <= or(pstore_drain_structural, _pstore_drain_T_10) @[DCache.scala 489:48]
    dataArb.io_in_0_bits_wdata <= cat(dataArb_io_in_0_bits_wdata_hi, dataArb_io_in_0_bits_wdata_lo) @[Cat.scala 30:58]
    dataArb.io_in_0_bits_eccMask <= cat(dataArb_io_in_0_bits_eccMask_hi, dataArb_io_in_0_bits_eccMask_lo) @[Cat.scala 30:58]
    dataArb.io_in_1_valid <= mux(_T_270, _GEN_198, _dataArb_io_in_1_valid_T) @[DCache.scala 693:26 724:68]
    dataArb.io_in_1_bits_addr <= dataArb.io_in_0_bits_addr @[DCache.scala 706:27]
    dataArb.io_in_1_bits_write <= mux(_T_270, _GEN_199, dataArb.io_in_0_bits_write) @[DCache.scala 706:27 724:68]
    dataArb.io_in_1_bits_wdata <= dataArb.io_in_0_bits_wdata @[DCache.scala 706:27]
    dataArb.io_in_1_bits_eccMask <= dataArb.io_in_0_bits_eccMask @[DCache.scala 706:27]
    dataArb.io_in_3_valid <= and(io_cpu_req_valid, res) @[DCache.scala 221:46]
    dataArb.io_in_3_bits_addr <= bits(_dataArb_io_in_3_bits_addr_T, 13, 0) @[DCache.scala 224:30]
    dataArb.io_in_3_bits_wdata <= dataArb.io_in_1_bits_wdata @[DCache.scala 222:25]
    dataArb.io_in_3_bits_wordMask <= UInt<1>("h1") @[DCache.scala 225:34]
    amoalu.io_cmd <= pstore1_cmd @[DCache.scala 954:21]
    amoalu.io_lhs <= cat(s2_data_corrected_hi, s2_data_corrected_lo) @[Cat.scala 30:58]
    amoalu.io_rhs <= pstore1_data @[DCache.scala 956:37]
    s1_valid <= mux(reset, UInt<1>("h0"), s1_valid_x9) @[DCache.scala 162:{21,21,21}]
    s1_req_cmd <= mux(s0_clk_en, io_cpu_req_bits_cmd, s1_req_cmd) @[Reg.scala 15:16 16:{19,23}]
    s2_valid <= mux(reset, UInt<1>("h0"), s2_valid_x37) @[DCache.scala 303:{21,21,21}]
    s2_req_cmd <= mux(_T_244, _GEN_176, _GEN_37) @[DCache.scala 646:26]
    pstore1_held <= mux(reset, UInt<1>("h0"), _pstore1_held_T_10) @[DCache.scala 476:{29,29} 493:16]
    pstore1_addr <= mux(_pstore1_cmd_T, s1_vaddr, pstore1_addr) @[Reg.scala 15:16 16:{19,23}]
    s1_req_addr <= mux(s0_clk_en, s0_req_addr, s1_req_addr) @[Reg.scala 15:16 16:{19,23}]
    pstore1_mask <= mux(_pstore1_cmd_T, s1_mask, pstore1_mask) @[Reg.scala 15:16 16:{19,23}]
    s1_req_size <= mux(s0_clk_en, io_cpu_req_bits_size, s1_req_size) @[Reg.scala 15:16 16:{19,23}]
    pstore2_valid <= mux(reset, UInt<1>("h0"), _pstore2_valid_T_2) @[DCache.scala 473:{30,30} 495:17]
    pstore2_addr <= mux(advance_pstore1, pstore1_addr, pstore2_addr) @[Reg.scala 15:16 16:{19,23}]
    mask <= mux(advance_pstore1, _pstore2_storegen_mask_mask_T_2, mask) @[DCache.scala 504:45 506:12 503:19]
    s2_not_nacked_in_s1 <= not(s1_nack) @[DCache.scala 307:37]
    s2_hit_state_state <= mux(_T_34, hitState_state, s2_hit_state_state) @[Reg.scala 15:16 16:{19,23}]
    s1_req_no_xcpt <= mux(s0_clk_en, io_cpu_req_bits_no_xcpt, s1_req_no_xcpt) @[Reg.scala 15:16 16:{19,23}]
    s1_req_tag <= mux(s0_clk_en, io_cpu_req_bits_tag, s1_req_tag) @[Reg.scala 15:16 16:{19,23}]
    s1_req_signed <= mux(s0_clk_en, io_cpu_req_bits_signed, s1_req_signed) @[Reg.scala 15:16 16:{19,23}]
    s1_tlb_req_vaddr <= mux(s0_clk_en, s0_req_addr, s1_tlb_req_vaddr) @[Reg.scala 15:16 16:{19,23}]
    s1_tlb_req_size <= mux(s0_clk_en, io_cpu_req_bits_size, s1_tlb_req_size) @[Reg.scala 15:16 16:{19,23}]
    s1_tlb_req_cmd <= mux(s0_clk_en, io_cpu_req_bits_cmd, s1_tlb_req_cmd) @[Reg.scala 15:16 16:{19,23}]
    s1_flush_valid <= and(_s1_flush_valid_T_2, _s2_cannot_victimize_T) @[DCache.scala 984:64]
    uncachedInFlight_0 <= mux(reset, UInt<1>("h0"), _GEN_187) @[DCache.scala 215:{33,33}]
    uncachedReqs_0_addr <= mux(_T_227, s2_req_addr, uncachedReqs_0_addr) @[DCache.scala 216:25 603:26]
    uncachedReqs_0_tag <= mux(_T_227, s2_req_tag, uncachedReqs_0_tag) @[DCache.scala 216:25 603:26]
    uncachedReqs_0_size <= mux(_T_227, s2_req_size, uncachedReqs_0_size) @[DCache.scala 216:25 603:26]
    uncachedReqs_0_signed <= mux(_T_227, s2_req_signed, uncachedReqs_0_signed) @[DCache.scala 216:25 603:26]
    s1_did_read <= mux(s0_clk_en, _s1_did_read_T_52, s1_did_read) @[Reg.scala 15:16 16:{19,23}]
    s1_read_mask <= mux(s0_clk_en, dataArb.io_in_3_bits_wordMask, s1_read_mask) @[Reg.scala 15:16 16:{19,23}]
    s2_req_addr <= mux(_T_244, _GEN_180, _GEN_35) @[DCache.scala 646:26]
    s2_req_tag <= mux(_T_244, _GEN_179, _GEN_36) @[DCache.scala 646:26]
    s2_req_size <= mux(_T_244, _GEN_177, _GEN_38) @[DCache.scala 646:26]
    s2_req_signed <= mux(_T_244, _GEN_178, _GEN_39) @[DCache.scala 646:26]
    s2_req_no_xcpt <= mux(_T_34, s1_req_no_xcpt, s2_req_no_xcpt) @[DCache.scala 317:48 318:12 311:19]
    s2_tlb_xcpt_pf_ld <= mux(_T_34, tlb.io_resp_pf_ld, s2_tlb_xcpt_pf_ld) @[DCache.scala 317:48 320:17 314:24]
    s2_tlb_xcpt_pf_st <= mux(_T_34, tlb.io_resp_pf_st, s2_tlb_xcpt_pf_st) @[DCache.scala 317:48 320:17 314:24]
    s2_tlb_xcpt_ae_ld <= mux(_T_34, tlb.io_resp_ae_ld, s2_tlb_xcpt_ae_ld) @[DCache.scala 317:48 320:17 314:24]
    s2_tlb_xcpt_ae_st <= mux(_T_34, tlb.io_resp_ae_st, s2_tlb_xcpt_ae_st) @[DCache.scala 317:48 320:17 314:24]
    s2_tlb_xcpt_ma_ld <= mux(_T_34, tlb.io_resp_ma_ld, s2_tlb_xcpt_ma_ld) @[DCache.scala 317:48 320:17 314:24]
    s2_tlb_xcpt_ma_st <= mux(_T_34, tlb.io_resp_ma_st, s2_tlb_xcpt_ma_st) @[DCache.scala 317:48 320:17 314:24]
    s2_vaddr_r <= mux(_T_34, s1_vaddr, s2_vaddr_r) @[Reg.scala 15:16 16:{19,23}]
    s2_flush_valid_pre_tag_ecc <= s1_flush_valid @[DCache.scala 327:43]
    blockUncachedGrant <= mux(_T_270, _GEN_200, dataArb.io_out_valid) @[DCache.scala 723:24 724:68]
    counter <= mux(reset, UInt<10>("h0"), _GEN_146) @[Edges.scala 228:{27,27}]
    s2_data <= mux(en, _s2_data_T_6, s2_data) @[Reg.scala 15:16 16:{19,23}]
    pstore1_cmd <= mux(_pstore1_cmd_T, s1_req_cmd, pstore1_cmd) @[Reg.scala 15:16 16:{19,23}]
    pstore1_data <= mux(_pstore1_cmd_T, io_cpu_s1_data_data, pstore1_data) @[Reg.scala 15:16 16:{19,23}]
    pstore1_rmw_r <= mux(_pstore1_cmd_T, _pstore1_rmw_T_50, pstore1_rmw_r) @[Reg.scala 15:16 16:{19,23}]
    pstore_drain_on_miss_REG <= io_cpu_s2_nack @[DCache.scala 475:56]
    pstore2_storegen_data_lo_lo <= mux(advance_pstore1, _pstore2_storegen_data_T, pstore2_storegen_data_lo_lo) @[Reg.scala 15:16 16:{19,23}]
    pstore2_storegen_data_lo_hi <= mux(advance_pstore1, _pstore2_storegen_data_T_4, pstore2_storegen_data_lo_hi) @[Reg.scala 15:16 16:{19,23}]
    pstore2_storegen_data_hi_lo <= mux(advance_pstore1, _pstore2_storegen_data_T_8, pstore2_storegen_data_hi_lo) @[Reg.scala 15:16 16:{19,23}]
    pstore2_storegen_data_hi_hi <= mux(advance_pstore1, _pstore2_storegen_data_T_12, pstore2_storegen_data_hi_hi) @[Reg.scala 15:16 16:{19,23}]
    io_cpu_s2_xcpt_REG <= and(_s1_xcpt_valid_T_1, _s1_valid_not_nacked_T) @[DCache.scala 902:65]
    doUncachedResp <= io_cpu_replay_next @[DCache.scala 918:27]
    printf(gated_clock, _dataArb_io_in_3_valid_T_55, "Assertion failed\n    at DCache.scala:1156 assert(!needsRead(req) || res)\n") @[DCache.scala 1156:11]
    stop(gated_clock, _dataArb_io_in_3_valid_T_55, 1) @[DCache.scala 1156:11]
    printf(gated_clock, _T_33, "Assertion failed\n    at DCache.scala:301 assert(!(s1_valid_masked && s1_req.cmd === M_PWR) || (s1_mask_xwr | ~io.cpu.s1_data.mask).andR)\n") @[DCache.scala 301:9]
    stop(gated_clock, _T_33, 1) @[DCache.scala 301:9]
    printf(gated_clock, _dataArb_io_in_3_valid_T_55, "Assertion failed\n    at DCache.scala:1156 assert(!needsRead(req) || res)\n") @[DCache.scala 1156:11]
    stop(gated_clock, _dataArb_io_in_3_valid_T_55, 1) @[DCache.scala 1156:11]
    printf(gated_clock, _T_239, "Assertion failed\n    at DCache.scala:626 assert(!tl_out.d.valid || whole_opc.isOneOf(uncachedGrantOpcodes))\n") @[DCache.scala 626:13]
    stop(gated_clock, _T_239, 1) @[DCache.scala 626:13]
    printf(gated_clock, and(_GEN_226, _T_247), "Assertion failed: A GrantData was unexpected by the dcache.\n    at DCache.scala:649 assert(cached_grant_wait, \"A GrantData was unexpected by the dcache.\")\n") @[DCache.scala 649:13]
    stop(gated_clock, and(_GEN_226, _T_247), 1) @[DCache.scala 649:13]
    printf(gated_clock, and(_GEN_230, _T_252), "Assertion failed: An AccessAck was unexpected by the dcache.\n    at DCache.scala:659 assert(f, \"An AccessAck was unexpected by the dcache.\") // TODO must handle Ack coming back on same cycle!\n") @[DCache.scala 659:17]
    stop(gated_clock, and(_GEN_230, _T_252), 1) @[DCache.scala 659:17]
    printf(gated_clock, _T_266, "Assertion failed\n    at DCache.scala:688 assert(tl_out.e.fire() === (tl_out.d.fire() && d_first && grantIsCached))\n") @[DCache.scala 688:9]
    stop(gated_clock, _T_266, 1) @[DCache.scala 688:9]
    printf(gated_clock, _T_287, "Assertion failed\n    at DCache.scala:906 assert(!(s2_valid_masked && s2_req.cmd.isOneOf(M_XLR, M_XSC)))\n") @[DCache.scala 906:11]
    stop(gated_clock, _T_287, 1) @[DCache.scala 906:11]
    printf(gated_clock, and(doUncachedResp, _T_291), "Assertion failed\n    at DCache.scala:922 assert(!s2_valid_hit)\n") @[DCache.scala 922:11]
    stop(gated_clock, and(doUncachedResp, _T_291), 1) @[DCache.scala 922:11]

  module Frontend :
    input gated_clock : Clock
    input reset : UInt<1>
    input auto_icache_master_out_a_ready : UInt<1>
    output auto_icache_master_out_a_valid : UInt<1>
    output auto_icache_master_out_a_bits_address : UInt<32>
    input auto_icache_master_out_d_valid : UInt<1>
    input auto_icache_master_out_d_bits_opcode : UInt<3>
    input auto_icache_master_out_d_bits_size : UInt<4>
    input auto_icache_master_out_d_bits_data : UInt<32>
    input auto_icache_master_out_d_bits_corrupt : UInt<1>
    input auto_reset_vector_sink_in : UInt<32>
    input io_cpu_might_request : UInt<1>
    input io_cpu_req_valid : UInt<1>
    input io_cpu_req_bits_pc : UInt<32>
    input io_cpu_req_bits_speculative : UInt<1>
    input io_cpu_resp_ready : UInt<1>
    output io_cpu_resp_valid : UInt<1>
    output io_cpu_resp_bits_pc : UInt<32>
    output io_cpu_resp_bits_data : UInt<32>
    output io_cpu_resp_bits_xcpt_ae_inst : UInt<1>
    output io_cpu_resp_bits_replay : UInt<1>
    input io_cpu_btb_update_valid : UInt<1>
    input io_cpu_bht_update_valid : UInt<1>
    input io_cpu_flush_icache : UInt<1>
    output io_cpu_npc : UInt<32>
    input io_ptw_status_debug : UInt<1>
    input io_ptw_pmp_0_cfg_l : UInt<1>
    input io_ptw_pmp_0_cfg_a : UInt<2>
    input io_ptw_pmp_0_cfg_x : UInt<1>
    input io_ptw_pmp_0_cfg_w : UInt<1>
    input io_ptw_pmp_0_cfg_r : UInt<1>
    input io_ptw_pmp_0_addr : UInt<30>
    input io_ptw_pmp_0_mask : UInt<32>
    input io_ptw_pmp_1_cfg_l : UInt<1>
    input io_ptw_pmp_1_cfg_a : UInt<2>
    input io_ptw_pmp_1_cfg_x : UInt<1>
    input io_ptw_pmp_1_cfg_w : UInt<1>
    input io_ptw_pmp_1_cfg_r : UInt<1>
    input io_ptw_pmp_1_addr : UInt<30>
    input io_ptw_pmp_1_mask : UInt<32>
    input io_ptw_pmp_2_cfg_l : UInt<1>
    input io_ptw_pmp_2_cfg_a : UInt<2>
    input io_ptw_pmp_2_cfg_x : UInt<1>
    input io_ptw_pmp_2_cfg_w : UInt<1>
    input io_ptw_pmp_2_cfg_r : UInt<1>
    input io_ptw_pmp_2_addr : UInt<30>
    input io_ptw_pmp_2_mask : UInt<32>
    input io_ptw_pmp_3_cfg_l : UInt<1>
    input io_ptw_pmp_3_cfg_a : UInt<2>
    input io_ptw_pmp_3_cfg_x : UInt<1>
    input io_ptw_pmp_3_cfg_w : UInt<1>
    input io_ptw_pmp_3_cfg_r : UInt<1>
    input io_ptw_pmp_3_addr : UInt<30>
    input io_ptw_pmp_3_mask : UInt<32>
    input io_ptw_pmp_4_cfg_l : UInt<1>
    input io_ptw_pmp_4_cfg_a : UInt<2>
    input io_ptw_pmp_4_cfg_x : UInt<1>
    input io_ptw_pmp_4_cfg_w : UInt<1>
    input io_ptw_pmp_4_cfg_r : UInt<1>
    input io_ptw_pmp_4_addr : UInt<30>
    input io_ptw_pmp_4_mask : UInt<32>
    input io_ptw_pmp_5_cfg_l : UInt<1>
    input io_ptw_pmp_5_cfg_a : UInt<2>
    input io_ptw_pmp_5_cfg_x : UInt<1>
    input io_ptw_pmp_5_cfg_w : UInt<1>
    input io_ptw_pmp_5_cfg_r : UInt<1>
    input io_ptw_pmp_5_addr : UInt<30>
    input io_ptw_pmp_5_mask : UInt<32>
    input io_ptw_pmp_6_cfg_l : UInt<1>
    input io_ptw_pmp_6_cfg_a : UInt<2>
    input io_ptw_pmp_6_cfg_x : UInt<1>
    input io_ptw_pmp_6_cfg_w : UInt<1>
    input io_ptw_pmp_6_cfg_r : UInt<1>
    input io_ptw_pmp_6_addr : UInt<30>
    input io_ptw_pmp_6_mask : UInt<32>
    input io_ptw_pmp_7_cfg_l : UInt<1>
    input io_ptw_pmp_7_cfg_a : UInt<2>
    input io_ptw_pmp_7_cfg_x : UInt<1>
    input io_ptw_pmp_7_cfg_w : UInt<1>
    input io_ptw_pmp_7_cfg_r : UInt<1>
    input io_ptw_pmp_7_addr : UInt<30>
    input io_ptw_pmp_7_mask : UInt<32>
    input io_ptw_customCSRs_csrs_0_value : UInt<32>

    inst icache of ICache @[Frontend.scala 62:26]
    inst fq of ShiftQueue @[Frontend.scala 84:57]
    inst tlb of TLB_1 @[Frontend.scala 98:19]
    node _T_3 = or(io_cpu_req_valid, io_cpu_flush_icache) @[Frontend.scala 89:52]
    node _T_4 = or(_T_3, io_cpu_bht_update_valid) @[Frontend.scala 89:75]
    node _T_5 = or(_T_4, io_cpu_btb_update_valid) @[Frontend.scala 89:102]
    node _T_6 = not(_T_5) @[Frontend.scala 89:10]
    node _T_7 = or(_T_6, io_cpu_might_request) @[Frontend.scala 89:130]
    node _T_9 = or(_T_7, reset) @[Frontend.scala 89:9]
    node _T_10 = not(_T_9) @[Frontend.scala 89:9]
    reg s1_valid : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s1_valid) @[Frontend.scala 100:21]
    reg s2_valid : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_valid) @[Frontend.scala 101:25]
    node _s0_fq_has_space_T = bits(fq.io_mask, 2, 2) @[Frontend.scala 103:16]
    node _s0_fq_has_space_T_1 = not(_s0_fq_has_space_T) @[Frontend.scala 103:5]
    node _s0_fq_has_space_T_2 = bits(fq.io_mask, 3, 3) @[Frontend.scala 104:17]
    node _s0_fq_has_space_T_3 = not(_s0_fq_has_space_T_2) @[Frontend.scala 104:6]
    node _s0_fq_has_space_T_4 = not(s1_valid) @[Frontend.scala 104:45]
    node _s0_fq_has_space_T_5 = not(s2_valid) @[Frontend.scala 104:58]
    node _s0_fq_has_space_T_6 = or(_s0_fq_has_space_T_4, _s0_fq_has_space_T_5) @[Frontend.scala 104:55]
    node _s0_fq_has_space_T_7 = and(_s0_fq_has_space_T_3, _s0_fq_has_space_T_6) @[Frontend.scala 104:41]
    node _s0_fq_has_space_T_8 = or(_s0_fq_has_space_T_1, _s0_fq_has_space_T_7) @[Frontend.scala 103:40]
    node _s0_fq_has_space_T_9 = bits(fq.io_mask, 4, 4) @[Frontend.scala 105:17]
    node _s0_fq_has_space_T_10 = not(_s0_fq_has_space_T_9) @[Frontend.scala 105:6]
    node _s0_fq_has_space_T_13 = and(_s0_fq_has_space_T_4, _s0_fq_has_space_T_5) @[Frontend.scala 105:55]
    node _s0_fq_has_space_T_14 = and(_s0_fq_has_space_T_10, _s0_fq_has_space_T_13) @[Frontend.scala 105:41]
    node s0_fq_has_space = or(_s0_fq_has_space_T_8, _s0_fq_has_space_T_14) @[Frontend.scala 104:70]
    node s0_valid = or(io_cpu_req_valid, s0_fq_has_space) @[Frontend.scala 106:35]
    reg s1_pc : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), s1_pc) @[Frontend.scala 108:18]
    reg s1_speculative : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s1_speculative) @[Frontend.scala 109:27]
    node _s2_pc_T = not(auto_reset_vector_sink_in) @[Frontend.scala 341:29]
    node _s2_pc_T_1 = or(_s2_pc_T, UInt<32>("h1")) @[Frontend.scala 341:33]
    node _s2_pc_T_2 = not(_s2_pc_T_1) @[Frontend.scala 341:27]
    reg s2_pc : UInt<32>, gated_clock with :
      reset => (UInt<1>("h0"), s2_pc) @[Frontend.scala 110:22]
    reg s2_tlb_resp_ae_inst : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_tlb_resp_ae_inst) @[Frontend.scala 114:24]
    reg s2_speculative : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_speculative) @[Frontend.scala 116:27]
    node _s1_base_pc_T = not(s1_pc) @[Frontend.scala 121:22]
    node _s1_base_pc_T_1 = or(_s1_base_pc_T, UInt<32>("h3")) @[Frontend.scala 121:29]
    node s1_base_pc = not(_s1_base_pc_T_1) @[Frontend.scala 121:20]
    node _ntpc_T = add(s1_base_pc, UInt<32>("h4")) @[Frontend.scala 122:25]
    node ntpc = tail(_ntpc_T, 1) @[Frontend.scala 122:25]
    node _s2_replay_T = and(fq.io_enq_ready, fq.io_enq_valid) @[Decoupled.scala 40:37]
    node _s2_replay_T_1 = not(_s2_replay_T) @[Frontend.scala 127:29]
    node _s2_replay_T_2 = and(s2_valid, _s2_replay_T_1) @[Frontend.scala 127:26]
    node _s2_replay_T_3 = not(s0_valid) @[Frontend.scala 127:72]
    reg s2_replay_REG : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), s2_replay_REG) @[Frontend.scala 127:58]
    node s2_replay = or(_s2_replay_T_2, s2_replay_REG) @[Frontend.scala 127:48]
    node _s2_replay_T_4 = and(s2_replay, _s2_replay_T_3) @[Frontend.scala 127:69]
    node npc = mux(s2_replay, s2_pc, ntpc) @[Frontend.scala 128:16]
    node _s0_speculative_T = not(s2_speculative) @[Frontend.scala 134:56]
    node _s0_speculative_T_1 = and(s2_valid, _s0_speculative_T) @[Frontend.scala 134:53]
    node s0_speculative = or(s1_speculative, _s0_speculative_T_1) @[Frontend.scala 134:41]
    node _s1_speculative_T = mux(s2_replay, s2_speculative, s0_speculative) @[Frontend.scala 136:75]
    node _T_11 = not(s2_replay) @[Frontend.scala 140:9]
    node _s2_valid_T = not(io_cpu_req_valid) @[Frontend.scala 141:17]
    node _GEN_0 = and(_T_11, _s2_valid_T) @[Frontend.scala 139:12 140:21 141:14]
    node _GEN_1 = mux(_T_11, s1_pc, s2_pc) @[Frontend.scala 140:21 142:11 110:22]
    node _GEN_2 = mux(_T_11, s1_speculative, s2_speculative) @[Frontend.scala 140:21 143:20 116:27]
    node _s2_can_speculatively_refill_T = bits(io_ptw_customCSRs_csrs_0_value, 3, 3) @[CustomCSRs.scala 40:69]
    reg fq_io_enq_valid_REG : UInt<1>, gated_clock with :
      reset => (UInt<1>("h0"), fq_io_enq_valid_REG) @[Frontend.scala 165:29]
    node _fq_io_enq_valid_T = and(fq_io_enq_valid_REG, s2_valid) @[Frontend.scala 165:40]
    node _fq_io_enq_valid_T_2 = icache.io_s2_kill @[Frontend.scala 165:98]
    node _fq_io_enq_valid_T_3 = or(icache.io_resp_valid, _fq_io_enq_valid_T_2) @[Frontend.scala 165:77]
    node _io_cpu_npc_T = mux(io_cpu_req_valid, io_cpu_req_bits_pc, npc) @[Frontend.scala 167:28]
    node _io_cpu_npc_T_1 = not(_io_cpu_npc_T) @[Frontend.scala 341:29]
    node _io_cpu_npc_T_2 = or(_io_cpu_npc_T_1, UInt<32>("h1")) @[Frontend.scala 341:33]
    node _fq_io_enq_bits_replay_T = not(icache.io_resp_valid) @[Frontend.scala 171:79]
    node _fq_io_enq_bits_replay_T_1 = and(icache.io_s2_kill, _fq_io_enq_bits_replay_T) @[Frontend.scala 171:76]
    node _fq_io_enq_bits_replay_T_2 = not(s2_tlb_resp_ae_inst) @[Frontend.scala 171:104]
    node _T_13 = and(s2_speculative, _s2_can_speculatively_refill_T) @[Frontend.scala 175:27]
    node _T_14 = not(icache.io_s2_kill) @[Frontend.scala 175:113]
    node _T_15 = and(_T_13, _T_14) @[Frontend.scala 175:110]
    node _T_16 = not(_T_15) @[Frontend.scala 175:10]
    node _T_18 = or(_T_16, reset) @[Frontend.scala 175:9]
    node _T_19 = not(_T_18) @[Frontend.scala 175:9]
    node _T_20 = and(icache.io_resp_valid, icache.io_resp_bits_ae) @[Frontend.scala 176:30]
    auto_icache_master_out_a_valid <= icache.auto_master_out_a_valid @[LazyModule.scala 311:12]
    auto_icache_master_out_a_bits_address <= icache.auto_master_out_a_bits_address @[LazyModule.scala 311:12]
    io_cpu_resp_valid <= fq.io_deq_valid @[Frontend.scala 325:15]
    io_cpu_resp_bits_pc <= fq.io_deq_bits_pc @[Frontend.scala 325:15]
    io_cpu_resp_bits_data <= fq.io_deq_bits_data @[Frontend.scala 325:15]
    io_cpu_resp_bits_xcpt_ae_inst <= fq.io_deq_bits_xcpt_ae_inst @[Frontend.scala 325:15]
    io_cpu_resp_bits_replay <= fq.io_deq_bits_replay @[Frontend.scala 325:15]
    io_cpu_npc <= not(_io_cpu_npc_T_2) @[Frontend.scala 341:27]
    icache.clock <= gated_clock @[Frontend.scala 94:16]
    icache.reset <= reset
    icache.auto_master_out_a_ready <= auto_icache_master_out_a_ready @[LazyModule.scala 311:12]
    icache.auto_master_out_d_valid <= auto_icache_master_out_d_valid @[LazyModule.scala 311:12]
    icache.auto_master_out_d_bits_opcode <= auto_icache_master_out_d_bits_opcode @[LazyModule.scala 311:12]
    icache.auto_master_out_d_bits_size <= auto_icache_master_out_d_bits_size @[LazyModule.scala 311:12]
    icache.auto_master_out_d_bits_data <= auto_icache_master_out_d_bits_data @[LazyModule.scala 311:12]
    icache.auto_master_out_d_bits_corrupt <= auto_icache_master_out_d_bits_corrupt @[LazyModule.scala 311:12]
    icache.io_req_valid <= or(io_cpu_req_valid, s0_fq_has_space) @[Frontend.scala 106:35]
    icache.io_req_bits_addr <= io_cpu_npc @[Frontend.scala 156:27]
    icache.io_s1_paddr <= tlb.io_resp_paddr @[Frontend.scala 158:22]
    icache.io_s1_kill <= or(io_cpu_req_valid, s2_replay) @[Frontend.scala 160:56]
    icache.io_s2_kill <= or(s2_speculative, s2_tlb_resp_ae_inst) @[Frontend.scala 162:71]
    icache.io_invalidate <= io_cpu_flush_icache @[Frontend.scala 157:24]
    fq.clock <= gated_clock
    fq.reset <= or(reset, io_cpu_req_valid) @[Frontend.scala 84:28]
    fq.io_enq_valid <= and(_fq_io_enq_valid_T, _fq_io_enq_valid_T_3) @[Frontend.scala 165:52]
    fq.io_enq_bits_pc <= s2_pc @[Frontend.scala 166:21]
    fq.io_enq_bits_data <= icache.io_resp_bits_data @[Frontend.scala 169:23]
    fq.io_enq_bits_xcpt_ae_inst <= or(_T_20, s2_tlb_resp_ae_inst) @[Frontend.scala 174:23 176:{57,87}]
    fq.io_enq_bits_replay <= and(_fq_io_enq_bits_replay_T_1, _fq_io_enq_bits_replay_T_2) @[Frontend.scala 171:101]
    fq.io_deq_ready <= io_cpu_resp_ready @[Frontend.scala 325:15]
    tlb.io_req_bits_vaddr <= s1_pc @[Frontend.scala 149:25]
    tlb.io_ptw_status_debug <= io_ptw_status_debug @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_0_cfg_l <= io_ptw_pmp_0_cfg_l @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_0_cfg_a <= io_ptw_pmp_0_cfg_a @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_0_cfg_x <= io_ptw_pmp_0_cfg_x @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_0_cfg_w <= io_ptw_pmp_0_cfg_w @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_0_cfg_r <= io_ptw_pmp_0_cfg_r @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_0_addr <= io_ptw_pmp_0_addr @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_0_mask <= io_ptw_pmp_0_mask @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_1_cfg_l <= io_ptw_pmp_1_cfg_l @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_1_cfg_a <= io_ptw_pmp_1_cfg_a @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_1_cfg_x <= io_ptw_pmp_1_cfg_x @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_1_cfg_w <= io_ptw_pmp_1_cfg_w @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_1_cfg_r <= io_ptw_pmp_1_cfg_r @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_1_addr <= io_ptw_pmp_1_addr @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_1_mask <= io_ptw_pmp_1_mask @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_2_cfg_l <= io_ptw_pmp_2_cfg_l @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_2_cfg_a <= io_ptw_pmp_2_cfg_a @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_2_cfg_x <= io_ptw_pmp_2_cfg_x @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_2_cfg_w <= io_ptw_pmp_2_cfg_w @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_2_cfg_r <= io_ptw_pmp_2_cfg_r @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_2_addr <= io_ptw_pmp_2_addr @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_2_mask <= io_ptw_pmp_2_mask @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_3_cfg_l <= io_ptw_pmp_3_cfg_l @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_3_cfg_a <= io_ptw_pmp_3_cfg_a @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_3_cfg_x <= io_ptw_pmp_3_cfg_x @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_3_cfg_w <= io_ptw_pmp_3_cfg_w @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_3_cfg_r <= io_ptw_pmp_3_cfg_r @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_3_addr <= io_ptw_pmp_3_addr @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_3_mask <= io_ptw_pmp_3_mask @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_4_cfg_l <= io_ptw_pmp_4_cfg_l @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_4_cfg_a <= io_ptw_pmp_4_cfg_a @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_4_cfg_x <= io_ptw_pmp_4_cfg_x @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_4_cfg_w <= io_ptw_pmp_4_cfg_w @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_4_cfg_r <= io_ptw_pmp_4_cfg_r @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_4_addr <= io_ptw_pmp_4_addr @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_4_mask <= io_ptw_pmp_4_mask @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_5_cfg_l <= io_ptw_pmp_5_cfg_l @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_5_cfg_a <= io_ptw_pmp_5_cfg_a @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_5_cfg_x <= io_ptw_pmp_5_cfg_x @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_5_cfg_w <= io_ptw_pmp_5_cfg_w @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_5_cfg_r <= io_ptw_pmp_5_cfg_r @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_5_addr <= io_ptw_pmp_5_addr @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_5_mask <= io_ptw_pmp_5_mask @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_6_cfg_l <= io_ptw_pmp_6_cfg_l @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_6_cfg_a <= io_ptw_pmp_6_cfg_a @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_6_cfg_x <= io_ptw_pmp_6_cfg_x @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_6_cfg_w <= io_ptw_pmp_6_cfg_w @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_6_cfg_r <= io_ptw_pmp_6_cfg_r @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_6_addr <= io_ptw_pmp_6_addr @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_6_mask <= io_ptw_pmp_6_mask @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_7_cfg_l <= io_ptw_pmp_7_cfg_l @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_7_cfg_a <= io_ptw_pmp_7_cfg_a @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_7_cfg_x <= io_ptw_pmp_7_cfg_x @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_7_cfg_w <= io_ptw_pmp_7_cfg_w @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_7_cfg_r <= io_ptw_pmp_7_cfg_r @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_7_addr <= io_ptw_pmp_7_addr @[Frontend.scala 147:10]
    tlb.io_ptw_pmp_7_mask <= io_ptw_pmp_7_mask @[Frontend.scala 147:10]
    s1_valid <= or(io_cpu_req_valid, s0_fq_has_space) @[Frontend.scala 106:35]
    s2_valid <= mux(reset, UInt<1>("h0"), _GEN_0) @[Frontend.scala 101:{25,25}]
    s1_pc <= io_cpu_npc @[Frontend.scala 130:9]
    s1_speculative <= mux(io_cpu_req_valid, io_cpu_req_bits_speculative, _s1_speculative_T) @[Frontend.scala 136:24]
    s2_pc <= mux(reset, _s2_pc_T_2, _GEN_1) @[Frontend.scala 110:{22,22}]
    s2_tlb_resp_ae_inst <= mux(_T_11, tlb.io_resp_ae_inst, s2_tlb_resp_ae_inst) @[Frontend.scala 140:21 144:17 114:24]
    s2_speculative <= mux(reset, UInt<1>("h0"), _GEN_2) @[Frontend.scala 116:{27,27}]
    s2_replay_REG <= or(reset, _s2_replay_T_4) @[Frontend.scala 127:{58,58,58}]
    fq_io_enq_valid_REG <= s1_valid @[Frontend.scala 165:29]
    printf(gated_clock, _T_10, "Assertion failed\n    at Frontend.scala:89 assert(!(io.cpu.req.valid || io.cpu.sfence.valid || io.cpu.flush_icache || io.cpu.bht_update.valid || io.cpu.btb_update.valid) || io.cpu.might_request)\n") @[Frontend.scala 89:9]
    stop(gated_clock, _T_10, 1) @[Frontend.scala 89:9]
    printf(gated_clock, _T_19, "Assertion failed\n    at Frontend.scala:175 assert(!(s2_speculative && io.ptw.customCSRs.asInstanceOf[RocketCustomCSRs].disableSpeculativeICacheRefill && !icache.io.s2_kill))\n") @[Frontend.scala 175:9]
    stop(gated_clock, _T_19, 1) @[Frontend.scala 175:9]

  module ScratchpadSlavePort :
    input clock : Clock
    input reset : UInt<1>
    output auto_in_a_ready : UInt<1>
    input auto_in_a_valid : UInt<1>
    input auto_in_a_bits_opcode : UInt<3>
    input auto_in_a_bits_param : UInt<3>
    input auto_in_a_bits_size : UInt<2>
    input auto_in_a_bits_source : UInt<9>
    input auto_in_a_bits_address : UInt<32>
    input auto_in_a_bits_mask : UInt<4>
    input auto_in_a_bits_data : UInt<32>
    input auto_in_d_ready : UInt<1>
    output auto_in_d_valid : UInt<1>
    output auto_in_d_bits_opcode : UInt<3>
    output auto_in_d_bits_size : UInt<2>
    output auto_in_d_bits_source : UInt<9>
    output auto_in_d_bits_data : UInt<32>
    input io_dmem_req_ready : UInt<1>
    output io_dmem_req_valid : UInt<1>
    output io_dmem_req_bits_addr : UInt<32>
    output io_dmem_req_bits_cmd : UInt<5>
    output io_dmem_req_bits_size : UInt<2>
    output io_dmem_s1_kill : UInt<1>
    output io_dmem_s1_data_data : UInt<32>
    output io_dmem_s1_data_mask : UInt<4>
    input io_dmem_s2_nack : UInt<1>
    input io_dmem_resp_valid : UInt<1>
    input io_dmem_resp_bits_data_raw : UInt<32>

    reg state : UInt<3>, clock with :
      reset => (UInt<1>("h0"), state) @[ScratchpadSlavePort.scala 45:20]
    node _T = eq(state, UInt<3>("h1")) @[ScratchpadSlavePort.scala 47:17]
    node _GEN_0 = mux(_T, UInt<3>("h2"), state) @[ScratchpadSlavePort.scala 45:20 47:{30,38}]
    node _GEN_1 = mux(io_dmem_resp_valid, UInt<3>("h4"), _GEN_0) @[ScratchpadSlavePort.scala 48:{31,39}]
    node _bundleIn_0_d_valid_T = eq(state, UInt<3>("h4")) @[ScratchpadSlavePort.scala 98:50]
    node tl_in_d_valid = or(io_dmem_resp_valid, _bundleIn_0_d_valid_T) @[ScratchpadSlavePort.scala 98:41]
    node _T_1 = and(auto_in_d_ready, tl_in_d_valid) @[Decoupled.scala 40:37]
    node _GEN_2 = mux(_T_1, UInt<3>("h0"), _GEN_1) @[ScratchpadSlavePort.scala 49:{27,35}]
    node _GEN_3 = mux(io_dmem_s2_nack, UInt<3>("h3"), _GEN_2) @[ScratchpadSlavePort.scala 50:{28,36}]
    node _ready_T = eq(state, UInt<3>("h0")) @[ScratchpadSlavePort.scala 86:23]
    node _ready_T_1 = eq(state, UInt<3>("h2")) @[ScratchpadSlavePort.scala 86:44]
    node _ready_T_2 = and(_ready_T_1, io_dmem_resp_valid) @[ScratchpadSlavePort.scala 86:56]
    node _ready_T_3 = and(_ready_T_2, auto_in_d_ready) @[ScratchpadSlavePort.scala 86:78]
    node ready = or(_ready_T, _ready_T_3) @[ScratchpadSlavePort.scala 86:35]
    node _dmem_req_valid_T = and(auto_in_a_valid, ready) @[ScratchpadSlavePort.scala 87:38]
    node _dmem_req_valid_T_1 = eq(state, UInt<3>("h3")) @[ScratchpadSlavePort.scala 87:57]
    node dmem_req_valid = or(_dmem_req_valid_T, _dmem_req_valid_T_1) @[ScratchpadSlavePort.scala 87:48]
    node _T_2 = and(dmem_req_valid, io_dmem_req_ready) @[ScratchpadSlavePort.scala 51:26]
    node _GEN_4 = mux(_T_2, UInt<3>("h1"), _GEN_3) @[ScratchpadSlavePort.scala 51:{48,56}]
    reg acq_opcode : UInt<3>, clock with :
      reset => (UInt<1>("h0"), acq_opcode) @[ScratchpadSlavePort.scala 53:18]
    reg acq_param : UInt<3>, clock with :
      reset => (UInt<1>("h0"), acq_param) @[ScratchpadSlavePort.scala 53:18]
    reg acq_size : UInt<2>, clock with :
      reset => (UInt<1>("h0"), acq_size) @[ScratchpadSlavePort.scala 53:18]
    reg acq_source : UInt<9>, clock with :
      reset => (UInt<1>("h0"), acq_source) @[ScratchpadSlavePort.scala 53:18]
    reg acq_address : UInt<32>, clock with :
      reset => (UInt<1>("h0"), acq_address) @[ScratchpadSlavePort.scala 53:18]
    reg acq_mask : UInt<4>, clock with :
      reset => (UInt<1>("h0"), acq_mask) @[ScratchpadSlavePort.scala 53:18]
    reg acq_data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), acq_data) @[ScratchpadSlavePort.scala 53:18]
    node tl_in_a_ready = and(io_dmem_req_ready, ready) @[ScratchpadSlavePort.scala 91:40]
    node _T_3 = and(tl_in_a_ready, auto_in_a_valid) @[Decoupled.scala 40:37]
    node ready_likely = or(_ready_T, _ready_T_1) @[ScratchpadSlavePort.scala 85:42]
    node _dmem_req_valid_likely_T = and(auto_in_a_valid, ready_likely) @[ScratchpadSlavePort.scala 88:48]
    node _io_dmem_req_bits_T_1_opcode = mux(_dmem_req_valid_T_1, acq_opcode, auto_in_a_bits_opcode) @[ScratchpadSlavePort.scala 92:41]
    node _io_dmem_req_bits_T_1_param = mux(_dmem_req_valid_T_1, acq_param, auto_in_a_bits_param) @[ScratchpadSlavePort.scala 92:41]
    node _io_dmem_req_bits_req_cmd_T = eq(UInt<3>("h0"), _io_dmem_req_bits_T_1_param) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_1 = mux(_io_dmem_req_bits_req_cmd_T, UInt<4>("hc"), UInt<4>("h0")) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_2 = eq(UInt<3>("h1"), _io_dmem_req_bits_T_1_param) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_3 = mux(_io_dmem_req_bits_req_cmd_T_2, UInt<4>("hd"), _io_dmem_req_bits_req_cmd_T_1) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_4 = eq(UInt<3>("h2"), _io_dmem_req_bits_T_1_param) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_5 = mux(_io_dmem_req_bits_req_cmd_T_4, UInt<4>("he"), _io_dmem_req_bits_req_cmd_T_3) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_6 = eq(UInt<3>("h3"), _io_dmem_req_bits_T_1_param) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_7 = mux(_io_dmem_req_bits_req_cmd_T_6, UInt<4>("hf"), _io_dmem_req_bits_req_cmd_T_5) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_8 = eq(UInt<3>("h4"), _io_dmem_req_bits_T_1_param) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_9 = mux(_io_dmem_req_bits_req_cmd_T_8, UInt<4>("h8"), _io_dmem_req_bits_req_cmd_T_7) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_11 = mux(_io_dmem_req_bits_req_cmd_T, UInt<4>("h9"), UInt<4>("h0")) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_13 = mux(_io_dmem_req_bits_req_cmd_T_2, UInt<4>("ha"), _io_dmem_req_bits_req_cmd_T_11) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_15 = mux(_io_dmem_req_bits_req_cmd_T_4, UInt<4>("hb"), _io_dmem_req_bits_req_cmd_T_13) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_17 = mux(_io_dmem_req_bits_req_cmd_T_6, UInt<4>("h4"), _io_dmem_req_bits_req_cmd_T_15) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_18 = eq(UInt<3>("h0"), _io_dmem_req_bits_T_1_opcode) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_20 = eq(UInt<3>("h1"), _io_dmem_req_bits_T_1_opcode) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_21 = mux(_io_dmem_req_bits_req_cmd_T_20, UInt<5>("h11"), pad(_io_dmem_req_bits_req_cmd_T_18, 5)) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_22 = eq(UInt<3>("h2"), _io_dmem_req_bits_T_1_opcode) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_23 = mux(_io_dmem_req_bits_req_cmd_T_22, pad(_io_dmem_req_bits_req_cmd_T_9, 5), _io_dmem_req_bits_req_cmd_T_21) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_24 = eq(UInt<3>("h3"), _io_dmem_req_bits_T_1_opcode) @[Mux.scala 80:60]
    node _io_dmem_req_bits_req_cmd_T_25 = mux(_io_dmem_req_bits_req_cmd_T_24, pad(_io_dmem_req_bits_req_cmd_T_17, 5), _io_dmem_req_bits_req_cmd_T_23) @[Mux.scala 80:57]
    node _io_dmem_req_bits_req_cmd_T_26 = eq(UInt<3>("h4"), _io_dmem_req_bits_T_1_opcode) @[Mux.scala 80:60]
    node _bundleIn_0_d_bits_T = eq(acq_opcode, UInt<3>("h0")) @[package.scala 15:47]
    node _bundleIn_0_d_bits_T_1 = eq(acq_opcode, UInt<3>("h1")) @[package.scala 15:47]
    node _bundleIn_0_d_bits_T_2 = or(_bundleIn_0_d_bits_T, _bundleIn_0_d_bits_T_1) @[package.scala 72:59]
    reg bundleIn_0_d_bits_data_r : UInt<32>, clock with :
      reset => (UInt<1>("h0"), bundleIn_0_d_bits_data_r) @[Reg.scala 15:16]
    auto_in_a_ready <= and(io_dmem_req_ready, ready) @[ScratchpadSlavePort.scala 91:40]
    auto_in_d_valid <= or(io_dmem_resp_valid, _bundleIn_0_d_valid_T) @[ScratchpadSlavePort.scala 98:41]
    auto_in_d_bits_opcode <= mux(_bundleIn_0_d_bits_T_2, UInt<3>("h0"), UInt<3>("h1")) @[ScratchpadSlavePort.scala 99:24]
    auto_in_d_bits_size <= acq_size @[ScratchpadSlavePort.scala 99:24]
    auto_in_d_bits_source <= acq_source @[ScratchpadSlavePort.scala 99:24]
    auto_in_d_bits_data <= mux(_ready_T_1, io_dmem_resp_bits_data_raw, bundleIn_0_d_bits_data_r) @[package.scala 79:42]
    io_dmem_req_valid <= or(_dmem_req_valid_likely_T, _dmem_req_valid_T_1) @[ScratchpadSlavePort.scala 88:65]
    io_dmem_req_bits_addr <= mux(_dmem_req_valid_T_1, acq_address, auto_in_a_bits_address) @[ScratchpadSlavePort.scala 92:41]
    io_dmem_req_bits_cmd <= mux(_io_dmem_req_bits_req_cmd_T_26, UInt<5>("h0"), _io_dmem_req_bits_req_cmd_T_25) @[Mux.scala 80:57]
    io_dmem_req_bits_size <= mux(_dmem_req_valid_T_1, acq_size, auto_in_a_bits_size) @[ScratchpadSlavePort.scala 92:41]
    io_dmem_s1_kill <= neq(state, UInt<3>("h1")) @[ScratchpadSlavePort.scala 95:30]
    io_dmem_s1_data_data <= acq_data @[ScratchpadSlavePort.scala 93:26]
    io_dmem_s1_data_mask <= acq_mask @[ScratchpadSlavePort.scala 94:26]
    state <= mux(reset, UInt<3>("h0"), _GEN_4) @[ScratchpadSlavePort.scala 45:{20,20}]
    acq_opcode <= mux(_T_3, auto_in_a_bits_opcode, acq_opcode) @[ScratchpadSlavePort.scala 53:18 54:{27,33}]
    acq_param <= mux(_T_3, auto_in_a_bits_param, acq_param) @[ScratchpadSlavePort.scala 53:18 54:{27,33}]
    acq_size <= mux(_T_3, auto_in_a_bits_size, acq_size) @[ScratchpadSlavePort.scala 53:18 54:{27,33}]
    acq_source <= mux(_T_3, auto_in_a_bits_source, acq_source) @[ScratchpadSlavePort.scala 53:18 54:{27,33}]
    acq_address <= mux(_T_3, auto_in_a_bits_address, acq_address) @[ScratchpadSlavePort.scala 53:18 54:{27,33}]
    acq_mask <= mux(_T_3, auto_in_a_bits_mask, acq_mask) @[ScratchpadSlavePort.scala 53:18 54:{27,33}]
    acq_data <= mux(_T_3, auto_in_a_bits_data, acq_data) @[ScratchpadSlavePort.scala 53:18 54:{27,33}]
    bundleIn_0_d_bits_data_r <= mux(_ready_T_1, io_dmem_resp_bits_data_raw, bundleIn_0_d_bits_data_r) @[Reg.scala 15:16 16:{19,23}]

  module TLFragmenter_8 :
    input clock : Clock
    input reset : UInt<1>
    output auto_in_a_ready : UInt<1>
    input auto_in_a_valid : UInt<1>
    input auto_in_a_bits_opcode : UInt<3>
    input auto_in_a_bits_param : UInt<3>
    input auto_in_a_bits_size : UInt<3>
    input auto_in_a_bits_source : UInt<3>
    input auto_in_a_bits_address : UInt<32>
    input auto_in_a_bits_mask : UInt<4>
    input auto_in_a_bits_data : UInt<32>
    input auto_in_d_ready : UInt<1>
    output auto_in_d_valid : UInt<1>
    output auto_in_d_bits_opcode : UInt<3>
    output auto_in_d_bits_size : UInt<3>
    output auto_in_d_bits_source : UInt<3>
    output auto_in_d_bits_data : UInt<32>
    input auto_out_a_ready : UInt<1>
    output auto_out_a_valid : UInt<1>
    output auto_out_a_bits_opcode : UInt<3>
    output auto_out_a_bits_param : UInt<3>
    output auto_out_a_bits_size : UInt<2>
    output auto_out_a_bits_source : UInt<9>
    output auto_out_a_bits_address : UInt<32>
    output auto_out_a_bits_mask : UInt<4>
    output auto_out_a_bits_data : UInt<32>
    output auto_out_d_ready : UInt<1>
    input auto_out_d_valid : UInt<1>
    input auto_out_d_bits_opcode : UInt<3>
    input auto_out_d_bits_size : UInt<2>
    input auto_out_d_bits_source : UInt<9>
    input auto_out_d_bits_data : UInt<32>

    inst repeater of Repeater_7 @[Fragmenter.scala 262:30]
    reg acknum : UInt<4>, clock with :
      reset => (UInt<1>("h0"), acknum) @[Fragmenter.scala 189:29]
    reg dOrig : UInt<3>, clock with :
      reset => (UInt<1>("h0"), dOrig) @[Fragmenter.scala 190:24]
    reg dToggle : UInt<1>, clock with :
      reset => (UInt<1>("h0"), dToggle) @[Fragmenter.scala 191:30]
    node dFragnum = bits(auto_out_d_bits_source, 3, 0) @[Fragmenter.scala 192:41]
    node dFirst = eq(acknum, UInt<4>("h0")) @[Fragmenter.scala 193:29]
    node dLast = eq(dFragnum, UInt<4>("h0")) @[Fragmenter.scala 194:30]
    node _dsizeOH_T = dshl(UInt<1>("h1"), auto_out_d_bits_size) @[OneHot.scala 65:12]
    node dsizeOH = bits(_dsizeOH_T, 2, 0) @[OneHot.scala 65:27]
    node _dsizeOH1_T_1 = dshl(UInt<2>("h3"), auto_out_d_bits_size) @[package.scala 234:77]
    node _dsizeOH1_T_2 = bits(_dsizeOH1_T_1, 1, 0) @[package.scala 234:82]
    node dsizeOH1 = not(_dsizeOH1_T_2) @[package.scala 234:46]
    node dHasData = bits(auto_out_d_bits_opcode, 0, 0) @[Edges.scala 105:36]
    node _ack_decrement_T = shr(dsizeOH, 2) @[Fragmenter.scala 204:60]
    node ack_decrement = or(dHasData, _ack_decrement_T) @[Fragmenter.scala 204:32]
    node _dFirst_size_T = shl(dFragnum, 2) @[Fragmenter.scala 206:47]
    node _GEN_7 = pad(dsizeOH1, 6) @[Fragmenter.scala 206:69]
    node dFirst_size_lo = or(_dFirst_size_T, _GEN_7) @[Fragmenter.scala 206:69]
    node _dFirst_size_T_1 = shl(dFirst_size_lo, 1) @[package.scala 232:35]
    node _dFirst_size_T_2 = or(_dFirst_size_T_1, UInt<7>("h1")) @[package.scala 232:40]
    node _dFirst_size_T_3 = cat(UInt<1>("h0"), dFirst_size_lo) @[Cat.scala 30:58]
    node _dFirst_size_T_4 = not(_dFirst_size_T_3) @[package.scala 232:53]
    node _dFirst_size_T_5 = and(_dFirst_size_T_2, _dFirst_size_T_4) @[package.scala 232:51]
    node dFirst_size_hi = bits(_dFirst_size_T_5, 6, 4) @[OneHot.scala 30:18]
    node dFirst_size_lo_1 = bits(_dFirst_size_T_5, 3, 0) @[OneHot.scala 31:18]
    node dFirst_size_hi_1 = orr(dFirst_size_hi) @[OneHot.scala 32:14]
    node _GEN_8 = pad(dFirst_size_hi, 4) @[OneHot.scala 32:28]
    node _dFirst_size_T_6 = or(_GEN_8, dFirst_size_lo_1) @[OneHot.scala 32:28]
    node dFirst_size_hi_2 = bits(_dFirst_size_T_6, 3, 2) @[OneHot.scala 30:18]
    node dFirst_size_lo_2 = bits(_dFirst_size_T_6, 1, 0) @[OneHot.scala 31:18]
    node dFirst_size_hi_3 = orr(dFirst_size_hi_2) @[OneHot.scala 32:14]
    node _dFirst_size_T_7 = or(dFirst_size_hi_2, dFirst_size_lo_2) @[OneHot.scala 32:28]
    node dFirst_size_lo_3 = bits(_dFirst_size_T_7, 1, 1) @[CircuitMath.scala 30:8]
    node dFirst_size_lo_4 = cat(dFirst_size_hi_3, dFirst_size_lo_3) @[Cat.scala 30:58]
    node dFirst_size = cat(dFirst_size_hi_1, dFirst_size_lo_4) @[Cat.scala 30:58]
    node _drop_T = not(dHasData) @[Fragmenter.scala 222:20]
    node doEarlyAck = bits(auto_out_d_bits_source, 5, 5) @[Fragmenter.scala 219:54]
    node _drop_T_1 = mux(doEarlyAck, dFirst, dLast) @[Fragmenter.scala 222:37]
    node _drop_T_2 = not(_drop_T_1) @[Fragmenter.scala 222:33]
    node drop = and(_drop_T, _drop_T_2) @[Fragmenter.scala 222:30]
    node bundleOut_0_d_ready = or(auto_in_d_ready, drop) @[Fragmenter.scala 223:35]
    node _T_7 = and(bundleOut_0_d_ready, auto_out_d_valid) @[Decoupled.scala 40:37]
    node _GEN_9 = pad(ack_decrement, 4) @[Fragmenter.scala 209:55]
    node _acknum_T = sub(acknum, _GEN_9) @[Fragmenter.scala 209:55]
    node _acknum_T_1 = tail(_acknum_T, 1) @[Fragmenter.scala 209:55]
    node _acknum_T_2 = mux(dFirst, dFragnum, _acknum_T_1) @[Fragmenter.scala 209:24]
    node _dToggle_T = bits(auto_out_d_bits_source, 4, 4) @[Fragmenter.scala 212:41]
    node _GEN_0 = mux(dFirst, dFirst_size, dOrig) @[Fragmenter.scala 210:25 211:19 190:24]
    node _GEN_1 = mux(dFirst, _dToggle_T, dToggle) @[Fragmenter.scala 210:25 212:21 191:30]
    node _GEN_2 = mux(_T_7, _acknum_T_2, acknum) @[Fragmenter.scala 208:29 209:18 189:29]
    node _GEN_4 = mux(_T_7, _GEN_1, dToggle) @[Fragmenter.scala 208:29 191:30]
    node _bundleIn_0_d_valid_T = not(drop) @[Fragmenter.scala 224:39]
    node _aFrag_T = gt(repeater.io_deq_bits_size, UInt<3>("h2")) @[Fragmenter.scala 285:31]
    node aFrag = mux(_aFrag_T, UInt<3>("h2"), repeater.io_deq_bits_size) @[Fragmenter.scala 285:24]
    node _aOrigOH1_T_1 = dshl(UInt<6>("h3f"), repeater.io_deq_bits_size) @[package.scala 234:77]
    node _aOrigOH1_T_2 = bits(_aOrigOH1_T_1, 5, 0) @[package.scala 234:82]
    node aOrigOH1 = not(_aOrigOH1_T_2) @[package.scala 234:46]
    node _aFragOH1_T_1 = dshl(UInt<2>("h3"), aFrag) @[package.scala 234:77]
    node _aFragOH1_T_2 = bits(_aFragOH1_T_1, 1, 0) @[package.scala 234:82]
    node aFragOH1 = not(_aFragOH1_T_2) @[package.scala 234:46]
    node _aHasData_opdata_T = bits(repeater.io_deq_bits_opcode, 2, 2) @[Edges.scala 91:37]
    node aHasData = not(_aHasData_opdata_T) @[Edges.scala 91:28]
    reg gennum : UInt<4>, clock with :
      reset => (UInt<1>("h0"), gennum) @[Fragmenter.scala 291:29]
    node aFirst = eq(gennum, UInt<4>("h0")) @[Fragmenter.scala 292:29]
    node _old_gennum1_T = shr(aOrigOH1, 2) @[Fragmenter.scala 293:48]
    node _old_gennum1_T_1 = sub(gennum, UInt<4>("h1")) @[Fragmenter.scala 293:79]
    node _old_gennum1_T_2 = tail(_old_gennum1_T_1, 1) @[Fragmenter.scala 293:79]
    node old_gennum1 = mux(aFirst, _old_gennum1_T, _old_gennum1_T_2) @[Fragmenter.scala 293:30]
    node _new_gennum_T = not(old_gennum1) @[Fragmenter.scala 294:28]
    node new_gennum = not(_new_gennum_T) @[Fragmenter.scala 294:26]
    reg aToggle_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), aToggle_r) @[Reg.scala 15:16]
    node _GEN_5 = mux(aFirst, dToggle, aToggle_r) @[Reg.scala 15:16 16:{19,23}]
    node bundleOut_0_a_bits_source_lo_hi = not(_GEN_5) @[Fragmenter.scala 297:23]
    node bundleOut_0_a_bits_source_hi_lo = eq(repeater.io_deq_bits_opcode, UInt<3>("h0")) @[Fragmenter.scala 298:78]
    node bundleOut_0_a_valid = repeater.io_deq_valid @[Nodes.scala 1207:84 Fragmenter.scala 303:15]
    node _T_8 = and(auto_out_a_ready, bundleOut_0_a_valid) @[Decoupled.scala 40:37]
    node _GEN_6 = mux(_T_8, new_gennum, gennum) @[Fragmenter.scala 291:29 300:{29,38}]
    node _repeater_io_repeat_T = not(aHasData) @[Fragmenter.scala 302:31]
    node _repeater_io_repeat_T_1 = neq(new_gennum, UInt<4>("h0")) @[Fragmenter.scala 302:53]
    node _bundleOut_0_a_bits_address_T = shl(old_gennum1, 2) @[Fragmenter.scala 304:65]
    node _bundleOut_0_a_bits_address_T_1 = not(aOrigOH1) @[Fragmenter.scala 304:90]
    node _bundleOut_0_a_bits_address_T_2 = or(_bundleOut_0_a_bits_address_T, _bundleOut_0_a_bits_address_T_1) @[Fragmenter.scala 304:88]
    node _GEN_10 = pad(aFragOH1, 6) @[Fragmenter.scala 304:100]
    node _bundleOut_0_a_bits_address_T_3 = or(_bundleOut_0_a_bits_address_T_2, _GEN_10) @[Fragmenter.scala 304:100]
    node _bundleOut_0_a_bits_address_T_4 = or(_bundleOut_0_a_bits_address_T_3, UInt<6>("h3")) @[Fragmenter.scala 304:111]
    node _bundleOut_0_a_bits_address_T_5 = not(_bundleOut_0_a_bits_address_T_4) @[Fragmenter.scala 304:51]
    node _GEN_11 = pad(_bundleOut_0_a_bits_address_T_5, 32) @[Fragmenter.scala 304:49]
    node bundleOut_0_a_bits_source_lo = cat(bundleOut_0_a_bits_source_lo_hi, new_gennum) @[Cat.scala 30:58]
    node bundleOut_0_a_bits_source_hi = cat(repeater.io_deq_bits_source, bundleOut_0_a_bits_source_hi_lo) @[Cat.scala 30:58]
    node _T_9 = not(repeater.io_full) @[Fragmenter.scala 309:17]
    node _T_11 = or(_T_9, _repeater_io_repeat_T) @[Fragmenter.scala 309:35]
    node _T_13 = or(_T_11, reset) @[Fragmenter.scala 309:16]
    node _T_14 = not(_T_13) @[Fragmenter.scala 309:16]
    node _T_16 = eq(repeater.io_deq_bits_mask, UInt<4>("hf")) @[Fragmenter.scala 312:53]
    node _T_17 = or(_T_9, _T_16) @[Fragmenter.scala 312:35]
    node _T_19 = or(_T_17, reset) @[Fragmenter.scala 312:16]
    node _T_20 = not(_T_19) @[Fragmenter.scala 312:16]
    auto_in_a_ready <= repeater.io_enq_ready @[Nodes.scala 1210:84 Fragmenter.scala 263:25]
    auto_in_d_valid <= and(auto_out_d_valid, _bundleIn_0_d_valid_T) @[Fragmenter.scala 224:36]
    auto_in_d_bits_opcode <= auto_out_d_bits_opcode @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_in_d_bits_size <= mux(dFirst, dFirst_size, dOrig) @[Fragmenter.scala 227:32]
    auto_in_d_bits_source <= shr(auto_out_d_bits_source, 6) @[Fragmenter.scala 226:47]
    auto_in_d_bits_data <= auto_out_d_bits_data @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    auto_out_a_valid <= repeater.io_deq_valid @[Nodes.scala 1207:84 Fragmenter.scala 303:15]
    auto_out_a_bits_opcode <= repeater.io_deq_bits_opcode @[Nodes.scala 1207:84 Fragmenter.scala 303:15]
    auto_out_a_bits_param <= repeater.io_deq_bits_param @[Nodes.scala 1207:84 Fragmenter.scala 303:15]
    auto_out_a_bits_size <= bits(aFrag, 1, 0) @[Nodes.scala 1207:84 Fragmenter.scala 306:25]
    auto_out_a_bits_source <= cat(bundleOut_0_a_bits_source_hi, bundleOut_0_a_bits_source_lo) @[Cat.scala 30:58]
    auto_out_a_bits_address <= or(repeater.io_deq_bits_address, _GEN_11) @[Fragmenter.scala 304:49]
    auto_out_a_bits_mask <= mux(repeater.io_full, UInt<4>("hf"), auto_in_a_bits_mask) @[Fragmenter.scala 313:31]
    auto_out_a_bits_data <= auto_in_a_bits_data @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    auto_out_d_ready <= or(auto_in_d_ready, drop) @[Fragmenter.scala 223:35]
    repeater.clock <= clock
    repeater.reset <= reset
    repeater.io_repeat <= and(_repeater_io_repeat_T, _repeater_io_repeat_T_1) @[Fragmenter.scala 302:41]
    repeater.io_enq_valid <= auto_in_a_valid @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    repeater.io_enq_bits_opcode <= auto_in_a_bits_opcode @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    repeater.io_enq_bits_param <= auto_in_a_bits_param @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    repeater.io_enq_bits_size <= auto_in_a_bits_size @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    repeater.io_enq_bits_source <= auto_in_a_bits_source @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    repeater.io_enq_bits_address <= auto_in_a_bits_address @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    repeater.io_enq_bits_mask <= auto_in_a_bits_mask @[Nodes.scala 1210:84 LazyModule.scala 309:16]
    repeater.io_deq_ready <= auto_out_a_ready @[Nodes.scala 1207:84 LazyModule.scala 311:12]
    acknum <= mux(reset, UInt<4>("h0"), _GEN_2) @[Fragmenter.scala 189:{29,29}]
    dOrig <= mux(_T_7, _GEN_0, dOrig) @[Fragmenter.scala 190:24 208:29]
    dToggle <= mux(reset, UInt<1>("h0"), _GEN_4) @[Fragmenter.scala 191:{30,30}]
    gennum <= mux(reset, UInt<4>("h0"), _GEN_6) @[Fragmenter.scala 291:{29,29}]
    aToggle_r <= mux(aFirst, dToggle, aToggle_r) @[Reg.scala 15:16 16:{19,23}]
    printf(clock, _T_14, "Assertion failed\n    at Fragmenter.scala:309 assert (!repeater.io.full || !aHasData)\n") @[Fragmenter.scala 309:16]
    stop(clock, _T_14, 1) @[Fragmenter.scala 309:16]
    printf(clock, _T_20, "Assertion failed\n    at Fragmenter.scala:312 assert (!repeater.io.full || in_a.bits.mask === fullMask)\n") @[Fragmenter.scala 312:16]
    stop(clock, _T_20, 1) @[Fragmenter.scala 312:16]

  module HellaCacheArbiter :
    input clock : Clock
    output io_requestor_0_req_ready : UInt<1>
    input io_requestor_0_req_valid : UInt<1>
    input io_requestor_0_req_bits_addr : UInt<32>
    input io_requestor_0_req_bits_tag : UInt<7>
    input io_requestor_0_req_bits_cmd : UInt<5>
    input io_requestor_0_req_bits_size : UInt<2>
    input io_requestor_0_req_bits_signed : UInt<1>
    input io_requestor_0_s1_kill : UInt<1>
    input io_requestor_0_s1_data_data : UInt<32>
    output io_requestor_0_s2_nack : UInt<1>
    output io_requestor_0_resp_valid : UInt<1>
    output io_requestor_0_resp_bits_tag : UInt<7>
    output io_requestor_0_resp_bits_data : UInt<32>
    output io_requestor_0_resp_bits_replay : UInt<1>
    output io_requestor_0_resp_bits_has_data : UInt<1>
    output io_requestor_0_resp_bits_data_word_bypass : UInt<32>
    output io_requestor_0_replay_next : UInt<1>
    output io_requestor_0_s2_xcpt_ma_ld : UInt<1>
    output io_requestor_0_s2_xcpt_ma_st : UInt<1>
    output io_requestor_0_s2_xcpt_pf_ld : UInt<1>
    output io_requestor_0_s2_xcpt_pf_st : UInt<1>
    output io_requestor_0_s2_xcpt_ae_ld : UInt<1>
    output io_requestor_0_s2_xcpt_ae_st : UInt<1>
    output io_requestor_0_ordered : UInt<1>
    output io_requestor_0_perf_grant : UInt<1>
    output io_requestor_1_req_ready : UInt<1>
    input io_requestor_1_req_valid : UInt<1>
    input io_requestor_1_req_bits_addr : UInt<32>
    input io_requestor_1_req_bits_cmd : UInt<5>
    input io_requestor_1_req_bits_size : UInt<2>
    input io_requestor_1_s1_kill : UInt<1>
    input io_requestor_1_s1_data_data : UInt<32>
    input io_requestor_1_s1_data_mask : UInt<4>
    output io_requestor_1_s2_nack : UInt<1>
    output io_requestor_1_resp_valid : UInt<1>
    output io_requestor_1_resp_bits_data_raw : UInt<32>
    input io_mem_req_ready : UInt<1>
    output io_mem_req_valid : UInt<1>
    output io_mem_req_bits_addr : UInt<32>
    output io_mem_req_bits_tag : UInt<7>
    output io_mem_req_bits_cmd : UInt<5>
    output io_mem_req_bits_size : UInt<2>
    output io_mem_req_bits_signed : UInt<1>
    output io_mem_req_bits_no_xcpt : UInt<1>
    output io_mem_s1_kill : UInt<1>
    output io_mem_s1_data_data : UInt<32>
    output io_mem_s1_data_mask : UInt<4>
    input io_mem_s2_nack : UInt<1>
    input io_mem_resp_valid : UInt<1>
    input io_mem_resp_bits_tag : UInt<7>
    input io_mem_resp_bits_data : UInt<32>
    input io_mem_resp_bits_replay : UInt<1>
    input io_mem_resp_bits_has_data : UInt<1>
    input io_mem_resp_bits_data_word_bypass : UInt<32>
    input io_mem_resp_bits_data_raw : UInt<32>
    input io_mem_replay_next : UInt<1>
    input io_mem_s2_xcpt_ma_ld : UInt<1>
    input io_mem_s2_xcpt_ma_st : UInt<1>
    input io_mem_s2_xcpt_pf_ld : UInt<1>
    input io_mem_s2_xcpt_pf_st : UInt<1>
    input io_mem_s2_xcpt_ae_ld : UInt<1>
    input io_mem_s2_xcpt_ae_st : UInt<1>
    input io_mem_ordered : UInt<1>
    input io_mem_perf_grant : UInt<1>

    reg s1_id : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s1_id) @[HellaCacheArbiter.scala 19:20]
    reg s2_id : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s2_id) @[HellaCacheArbiter.scala 20:20]
    node _io_requestor_1_req_ready_T = not(io_requestor_0_req_valid) @[HellaCacheArbiter.scala 27:67]
    node _io_mem_req_bits_tag_T_1 = cat(io_requestor_0_req_bits_tag, UInt<1>("h0")) @[Cat.scala 30:58]
    node _GEN_1 = mux(io_requestor_0_req_valid, _io_mem_req_bits_tag_T_1, UInt<8>("h1")) @[HellaCacheArbiter.scala 49:26 33:{29,29}]
    node _T = not(s1_id) @[HellaCacheArbiter.scala 50:21]
    node _T_1 = not(s2_id) @[HellaCacheArbiter.scala 51:21]
    node _tag_hit_T = bits(io_mem_resp_bits_tag, 0, 0) @[HellaCacheArbiter.scala 59:41]
    node tag_hit = not(_tag_hit_T) @[HellaCacheArbiter.scala 59:57]
    node _io_requestor_0_resp_bits_tag_T = shr(io_mem_resp_bits_tag, 1) @[HellaCacheArbiter.scala 70:45]
    io_requestor_0_req_ready <= io_mem_req_ready @[HellaCacheArbiter.scala 25:31]
    io_requestor_0_s2_nack <= and(io_mem_s2_nack, _T_1) @[HellaCacheArbiter.scala 64:49]
    io_requestor_0_resp_valid <= and(io_mem_resp_valid, tag_hit) @[HellaCacheArbiter.scala 60:39]
    io_requestor_0_resp_bits_tag <= pad(_io_requestor_0_resp_bits_tag_T, 7) @[HellaCacheArbiter.scala 70:21]
    io_requestor_0_resp_bits_data <= io_mem_resp_bits_data @[HellaCacheArbiter.scala 69:17]
    io_requestor_0_resp_bits_replay <= io_mem_resp_bits_replay @[HellaCacheArbiter.scala 69:17]
    io_requestor_0_resp_bits_has_data <= io_mem_resp_bits_has_data @[HellaCacheArbiter.scala 69:17]
    io_requestor_0_resp_bits_data_word_bypass <= io_mem_resp_bits_data_word_bypass @[HellaCacheArbiter.scala 69:17]
    io_requestor_0_replay_next <= io_mem_replay_next @[HellaCacheArbiter.scala 72:35]
    io_requestor_0_s2_xcpt_ma_ld <= io_mem_s2_xcpt_ma_ld @[HellaCacheArbiter.scala 61:31]
    io_requestor_0_s2_xcpt_ma_st <= io_mem_s2_xcpt_ma_st @[HellaCacheArbiter.scala 61:31]
    io_requestor_0_s2_xcpt_pf_ld <= io_mem_s2_xcpt_pf_ld @[HellaCacheArbiter.scala 61:31]
    io_requestor_0_s2_xcpt_pf_st <= io_mem_s2_xcpt_pf_st @[HellaCacheArbiter.scala 61:31]
    io_requestor_0_s2_xcpt_ae_ld <= io_mem_s2_xcpt_ae_ld @[HellaCacheArbiter.scala 61:31]
    io_requestor_0_s2_xcpt_ae_st <= io_mem_s2_xcpt_ae_st @[HellaCacheArbiter.scala 61:31]
    io_requestor_0_ordered <= io_mem_ordered @[HellaCacheArbiter.scala 62:31]
    io_requestor_0_perf_grant <= io_mem_perf_grant @[HellaCacheArbiter.scala 63:28]
    io_requestor_1_req_ready <= and(io_requestor_0_req_ready, _io_requestor_1_req_ready_T) @[HellaCacheArbiter.scala 27:64]
    io_requestor_1_s2_nack <= and(io_mem_s2_nack, s2_id) @[HellaCacheArbiter.scala 64:49]
    io_requestor_1_resp_valid <= and(io_mem_resp_valid, _tag_hit_T) @[HellaCacheArbiter.scala 60:39]
    io_requestor_1_resp_bits_data_raw <= io_mem_resp_bits_data_raw @[HellaCacheArbiter.scala 69:17]
    io_mem_req_valid <= or(io_requestor_0_req_valid, io_requestor_1_req_valid) @[HellaCacheArbiter.scala 24:63]
    io_mem_req_bits_addr <= mux(io_requestor_0_req_valid, io_requestor_0_req_bits_addr, io_requestor_1_req_bits_addr) @[HellaCacheArbiter.scala 32:{25,25} 49:26]
    io_mem_req_bits_tag <= bits(_GEN_1, 6, 0)
    io_mem_req_bits_cmd <= mux(io_requestor_0_req_valid, io_requestor_0_req_bits_cmd, io_requestor_1_req_bits_cmd) @[HellaCacheArbiter.scala 32:{25,25} 49:26]
    io_mem_req_bits_size <= mux(io_requestor_0_req_valid, io_requestor_0_req_bits_size, io_requestor_1_req_bits_size) @[HellaCacheArbiter.scala 32:{25,25} 49:26]
    io_mem_req_bits_signed <= and(io_requestor_0_req_valid, io_requestor_0_req_bits_signed) @[HellaCacheArbiter.scala 32:{25,25} 49:26]
    io_mem_req_bits_no_xcpt <= mux(io_requestor_0_req_valid, UInt<1>("h0"), UInt<1>("h1")) @[HellaCacheArbiter.scala 32:{25,25} 49:26]
    io_mem_s1_kill <= mux(_T, io_requestor_0_s1_kill, io_requestor_1_s1_kill) @[HellaCacheArbiter.scala 37:{24,24} 50:34]
    io_mem_s1_data_data <= mux(_T, io_requestor_0_s1_data_data, io_requestor_1_s1_data_data) @[HellaCacheArbiter.scala 38:{24,24} 50:34]
    io_mem_s1_data_mask <= mux(_T, UInt<4>("h0"), io_requestor_1_s1_data_mask) @[HellaCacheArbiter.scala 38:{24,24} 50:34]
    s1_id <= mux(io_requestor_0_req_valid, UInt<1>("h0"), UInt<1>("h1")) @[HellaCacheArbiter.scala 34:{15,15} 49:26]
    s2_id <= s1_id @[HellaCacheArbiter.scala 20:20]

  module PTW :
    input clock : Clock
    input reset : UInt<1>
    output io_requestor_0_status_debug : UInt<1>
    output io_requestor_0_pmp_0_cfg_l : UInt<1>
    output io_requestor_0_pmp_0_cfg_a : UInt<2>
    output io_requestor_0_pmp_0_cfg_x : UInt<1>
    output io_requestor_0_pmp_0_cfg_w : UInt<1>
    output io_requestor_0_pmp_0_cfg_r : UInt<1>
    output io_requestor_0_pmp_0_addr : UInt<30>
    output io_requestor_0_pmp_0_mask : UInt<32>
    output io_requestor_0_pmp_1_cfg_l : UInt<1>
    output io_requestor_0_pmp_1_cfg_a : UInt<2>
    output io_requestor_0_pmp_1_cfg_x : UInt<1>
    output io_requestor_0_pmp_1_cfg_w : UInt<1>
    output io_requestor_0_pmp_1_cfg_r : UInt<1>
    output io_requestor_0_pmp_1_addr : UInt<30>
    output io_requestor_0_pmp_1_mask : UInt<32>
    output io_requestor_0_pmp_2_cfg_l : UInt<1>
    output io_requestor_0_pmp_2_cfg_a : UInt<2>
    output io_requestor_0_pmp_2_cfg_x : UInt<1>
    output io_requestor_0_pmp_2_cfg_w : UInt<1>
    output io_requestor_0_pmp_2_cfg_r : UInt<1>
    output io_requestor_0_pmp_2_addr : UInt<30>
    output io_requestor_0_pmp_2_mask : UInt<32>
    output io_requestor_0_pmp_3_cfg_l : UInt<1>
    output io_requestor_0_pmp_3_cfg_a : UInt<2>
    output io_requestor_0_pmp_3_cfg_x : UInt<1>
    output io_requestor_0_pmp_3_cfg_w : UInt<1>
    output io_requestor_0_pmp_3_cfg_r : UInt<1>
    output io_requestor_0_pmp_3_addr : UInt<30>
    output io_requestor_0_pmp_3_mask : UInt<32>
    output io_requestor_0_pmp_4_cfg_l : UInt<1>
    output io_requestor_0_pmp_4_cfg_a : UInt<2>
    output io_requestor_0_pmp_4_cfg_x : UInt<1>
    output io_requestor_0_pmp_4_cfg_w : UInt<1>
    output io_requestor_0_pmp_4_cfg_r : UInt<1>
    output io_requestor_0_pmp_4_addr : UInt<30>
    output io_requestor_0_pmp_4_mask : UInt<32>
    output io_requestor_0_pmp_5_cfg_l : UInt<1>
    output io_requestor_0_pmp_5_cfg_a : UInt<2>
    output io_requestor_0_pmp_5_cfg_x : UInt<1>
    output io_requestor_0_pmp_5_cfg_w : UInt<1>
    output io_requestor_0_pmp_5_cfg_r : UInt<1>
    output io_requestor_0_pmp_5_addr : UInt<30>
    output io_requestor_0_pmp_5_mask : UInt<32>
    output io_requestor_0_pmp_6_cfg_l : UInt<1>
    output io_requestor_0_pmp_6_cfg_a : UInt<2>
    output io_requestor_0_pmp_6_cfg_x : UInt<1>
    output io_requestor_0_pmp_6_cfg_w : UInt<1>
    output io_requestor_0_pmp_6_cfg_r : UInt<1>
    output io_requestor_0_pmp_6_addr : UInt<30>
    output io_requestor_0_pmp_6_mask : UInt<32>
    output io_requestor_0_pmp_7_cfg_l : UInt<1>
    output io_requestor_0_pmp_7_cfg_a : UInt<2>
    output io_requestor_0_pmp_7_cfg_x : UInt<1>
    output io_requestor_0_pmp_7_cfg_w : UInt<1>
    output io_requestor_0_pmp_7_cfg_r : UInt<1>
    output io_requestor_0_pmp_7_addr : UInt<30>
    output io_requestor_0_pmp_7_mask : UInt<32>
    output io_requestor_1_status_debug : UInt<1>
    output io_requestor_1_pmp_0_cfg_l : UInt<1>
    output io_requestor_1_pmp_0_cfg_a : UInt<2>
    output io_requestor_1_pmp_0_cfg_x : UInt<1>
    output io_requestor_1_pmp_0_cfg_w : UInt<1>
    output io_requestor_1_pmp_0_cfg_r : UInt<1>
    output io_requestor_1_pmp_0_addr : UInt<30>
    output io_requestor_1_pmp_0_mask : UInt<32>
    output io_requestor_1_pmp_1_cfg_l : UInt<1>
    output io_requestor_1_pmp_1_cfg_a : UInt<2>
    output io_requestor_1_pmp_1_cfg_x : UInt<1>
    output io_requestor_1_pmp_1_cfg_w : UInt<1>
    output io_requestor_1_pmp_1_cfg_r : UInt<1>
    output io_requestor_1_pmp_1_addr : UInt<30>
    output io_requestor_1_pmp_1_mask : UInt<32>
    output io_requestor_1_pmp_2_cfg_l : UInt<1>
    output io_requestor_1_pmp_2_cfg_a : UInt<2>
    output io_requestor_1_pmp_2_cfg_x : UInt<1>
    output io_requestor_1_pmp_2_cfg_w : UInt<1>
    output io_requestor_1_pmp_2_cfg_r : UInt<1>
    output io_requestor_1_pmp_2_addr : UInt<30>
    output io_requestor_1_pmp_2_mask : UInt<32>
    output io_requestor_1_pmp_3_cfg_l : UInt<1>
    output io_requestor_1_pmp_3_cfg_a : UInt<2>
    output io_requestor_1_pmp_3_cfg_x : UInt<1>
    output io_requestor_1_pmp_3_cfg_w : UInt<1>
    output io_requestor_1_pmp_3_cfg_r : UInt<1>
    output io_requestor_1_pmp_3_addr : UInt<30>
    output io_requestor_1_pmp_3_mask : UInt<32>
    output io_requestor_1_pmp_4_cfg_l : UInt<1>
    output io_requestor_1_pmp_4_cfg_a : UInt<2>
    output io_requestor_1_pmp_4_cfg_x : UInt<1>
    output io_requestor_1_pmp_4_cfg_w : UInt<1>
    output io_requestor_1_pmp_4_cfg_r : UInt<1>
    output io_requestor_1_pmp_4_addr : UInt<30>
    output io_requestor_1_pmp_4_mask : UInt<32>
    output io_requestor_1_pmp_5_cfg_l : UInt<1>
    output io_requestor_1_pmp_5_cfg_a : UInt<2>
    output io_requestor_1_pmp_5_cfg_x : UInt<1>
    output io_requestor_1_pmp_5_cfg_w : UInt<1>
    output io_requestor_1_pmp_5_cfg_r : UInt<1>
    output io_requestor_1_pmp_5_addr : UInt<30>
    output io_requestor_1_pmp_5_mask : UInt<32>
    output io_requestor_1_pmp_6_cfg_l : UInt<1>
    output io_requestor_1_pmp_6_cfg_a : UInt<2>
    output io_requestor_1_pmp_6_cfg_x : UInt<1>
    output io_requestor_1_pmp_6_cfg_w : UInt<1>
    output io_requestor_1_pmp_6_cfg_r : UInt<1>
    output io_requestor_1_pmp_6_addr : UInt<30>
    output io_requestor_1_pmp_6_mask : UInt<32>
    output io_requestor_1_pmp_7_cfg_l : UInt<1>
    output io_requestor_1_pmp_7_cfg_a : UInt<2>
    output io_requestor_1_pmp_7_cfg_x : UInt<1>
    output io_requestor_1_pmp_7_cfg_w : UInt<1>
    output io_requestor_1_pmp_7_cfg_r : UInt<1>
    output io_requestor_1_pmp_7_addr : UInt<30>
    output io_requestor_1_pmp_7_mask : UInt<32>
    output io_requestor_1_customCSRs_csrs_0_value : UInt<32>
    input io_dpath_status_debug : UInt<1>
    input io_dpath_pmp_0_cfg_l : UInt<1>
    input io_dpath_pmp_0_cfg_a : UInt<2>
    input io_dpath_pmp_0_cfg_x : UInt<1>
    input io_dpath_pmp_0_cfg_w : UInt<1>
    input io_dpath_pmp_0_cfg_r : UInt<1>
    input io_dpath_pmp_0_addr : UInt<30>
    input io_dpath_pmp_0_mask : UInt<32>
    input io_dpath_pmp_1_cfg_l : UInt<1>
    input io_dpath_pmp_1_cfg_a : UInt<2>
    input io_dpath_pmp_1_cfg_x : UInt<1>
    input io_dpath_pmp_1_cfg_w : UInt<1>
    input io_dpath_pmp_1_cfg_r : UInt<1>
    input io_dpath_pmp_1_addr : UInt<30>
    input io_dpath_pmp_1_mask : UInt<32>
    input io_dpath_pmp_2_cfg_l : UInt<1>
    input io_dpath_pmp_2_cfg_a : UInt<2>
    input io_dpath_pmp_2_cfg_x : UInt<1>
    input io_dpath_pmp_2_cfg_w : UInt<1>
    input io_dpath_pmp_2_cfg_r : UInt<1>
    input io_dpath_pmp_2_addr : UInt<30>
    input io_dpath_pmp_2_mask : UInt<32>
    input io_dpath_pmp_3_cfg_l : UInt<1>
    input io_dpath_pmp_3_cfg_a : UInt<2>
    input io_dpath_pmp_3_cfg_x : UInt<1>
    input io_dpath_pmp_3_cfg_w : UInt<1>
    input io_dpath_pmp_3_cfg_r : UInt<1>
    input io_dpath_pmp_3_addr : UInt<30>
    input io_dpath_pmp_3_mask : UInt<32>
    input io_dpath_pmp_4_cfg_l : UInt<1>
    input io_dpath_pmp_4_cfg_a : UInt<2>
    input io_dpath_pmp_4_cfg_x : UInt<1>
    input io_dpath_pmp_4_cfg_w : UInt<1>
    input io_dpath_pmp_4_cfg_r : UInt<1>
    input io_dpath_pmp_4_addr : UInt<30>
    input io_dpath_pmp_4_mask : UInt<32>
    input io_dpath_pmp_5_cfg_l : UInt<1>
    input io_dpath_pmp_5_cfg_a : UInt<2>
    input io_dpath_pmp_5_cfg_x : UInt<1>
    input io_dpath_pmp_5_cfg_w : UInt<1>
    input io_dpath_pmp_5_cfg_r : UInt<1>
    input io_dpath_pmp_5_addr : UInt<30>
    input io_dpath_pmp_5_mask : UInt<32>
    input io_dpath_pmp_6_cfg_l : UInt<1>
    input io_dpath_pmp_6_cfg_a : UInt<2>
    input io_dpath_pmp_6_cfg_x : UInt<1>
    input io_dpath_pmp_6_cfg_w : UInt<1>
    input io_dpath_pmp_6_cfg_r : UInt<1>
    input io_dpath_pmp_6_addr : UInt<30>
    input io_dpath_pmp_6_mask : UInt<32>
    input io_dpath_pmp_7_cfg_l : UInt<1>
    input io_dpath_pmp_7_cfg_a : UInt<2>
    input io_dpath_pmp_7_cfg_x : UInt<1>
    input io_dpath_pmp_7_cfg_w : UInt<1>
    input io_dpath_pmp_7_cfg_r : UInt<1>
    input io_dpath_pmp_7_addr : UInt<30>
    input io_dpath_pmp_7_mask : UInt<32>
    output io_dpath_perf_l2hit : UInt<1>
    output io_dpath_perf_pte_miss : UInt<1>
    output io_dpath_perf_pte_hit : UInt<1>
    input io_dpath_customCSRs_csrs_0_value : UInt<32>

    inst state_barrier of OptimizationBarrier_54 @[package.scala 258:25]
    reg state : UInt<3>, clock with :
      reset => (UInt<1>("h0"), state) @[PTW.scala 109:18]
    reg count : UInt<1>, clock with :
      reset => (UInt<1>("h0"), count) @[PTW.scala 126:18]
    node _traverse_T_8 = lt(count, UInt<1>("h1")) @[PTW.scala 156:57]
    node _T_61 = or(io_dpath_perf_pte_miss, io_dpath_perf_pte_hit) @[PTW.scala 198:59]
    node _T_62 = and(io_dpath_perf_l2hit, _T_61) @[PTW.scala 198:32]
    node _T_63 = not(_T_62) @[PTW.scala 198:10]
    node _T_65 = or(_T_63, reset) @[PTW.scala 198:9]
    node _T_66 = not(_T_65) @[PTW.scala 198:9]
    node _pmaPgLevelHomogeneous_T_1 = cvt(UInt<28>("hc000000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_2 = and(_pmaPgLevelHomogeneous_T_1, SInt<29>("h-4000000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_3 = asSInt(_pmaPgLevelHomogeneous_T_2) @[Parameters.scala 137:52]
    node pmaPgLevelHomogeneous_0 = eq(_pmaPgLevelHomogeneous_T_3, SInt<29>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_10 = cvt(UInt<1>("h0")) @[Parameters.scala 137:49]
    node _GEN_43 = pad(_pmaPgLevelHomogeneous_T_10, 16) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_11 = and(_GEN_43, SInt<16>("h-5000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_12 = asSInt(_pmaPgLevelHomogeneous_T_11) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_13 = eq(_pmaPgLevelHomogeneous_T_12, SInt<16>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_15 = cvt(UInt<14>("h3000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_16 = and(_pmaPgLevelHomogeneous_T_15, SInt<15>("h-1000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_17 = asSInt(_pmaPgLevelHomogeneous_T_16) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_18 = eq(_pmaPgLevelHomogeneous_T_17, SInt<15>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_20 = cvt(UInt<17>("h10000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_21 = and(_pmaPgLevelHomogeneous_T_20, SInt<18>("h-10000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_22 = asSInt(_pmaPgLevelHomogeneous_T_21) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_23 = eq(_pmaPgLevelHomogeneous_T_22, SInt<18>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_25 = cvt(UInt<18>("h20000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_26 = and(_pmaPgLevelHomogeneous_T_25, SInt<19>("h-10000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_27 = asSInt(_pmaPgLevelHomogeneous_T_26) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_28 = eq(_pmaPgLevelHomogeneous_T_27, SInt<19>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_30 = cvt(UInt<21>("h100000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_31 = and(_pmaPgLevelHomogeneous_T_30, SInt<22>("h-1000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_32 = asSInt(_pmaPgLevelHomogeneous_T_31) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_33 = eq(_pmaPgLevelHomogeneous_T_32, SInt<22>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_35 = cvt(UInt<26>("h2000000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_36 = and(_pmaPgLevelHomogeneous_T_35, SInt<27>("h-10000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_37 = asSInt(_pmaPgLevelHomogeneous_T_36) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_38 = eq(_pmaPgLevelHomogeneous_T_37, SInt<27>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_45 = cvt(UInt<29>("h10000000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_46 = and(_pmaPgLevelHomogeneous_T_45, SInt<30>("h-1000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_47 = asSInt(_pmaPgLevelHomogeneous_T_46) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_48 = eq(_pmaPgLevelHomogeneous_T_47, SInt<30>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_50 = cvt(UInt<31>("h54000000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_51 = and(_pmaPgLevelHomogeneous_T_50, SInt<32>("h-1000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_52 = asSInt(_pmaPgLevelHomogeneous_T_51) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_53 = eq(_pmaPgLevelHomogeneous_T_52, SInt<32>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_55 = cvt(UInt<32>("h80000000")) @[Parameters.scala 137:49]
    node _pmaPgLevelHomogeneous_T_56 = and(_pmaPgLevelHomogeneous_T_55, SInt<33>("h-4000")) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_57 = asSInt(_pmaPgLevelHomogeneous_T_56) @[Parameters.scala 137:52]
    node _pmaPgLevelHomogeneous_T_58 = eq(_pmaPgLevelHomogeneous_T_57, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _pmaPgLevelHomogeneous_T_60 = or(_pmaPgLevelHomogeneous_T_13, _pmaPgLevelHomogeneous_T_18) @[TLBPermissions.scala 98:65]
    node _pmaPgLevelHomogeneous_T_61 = or(_pmaPgLevelHomogeneous_T_60, _pmaPgLevelHomogeneous_T_23) @[TLBPermissions.scala 98:65]
    node _pmaPgLevelHomogeneous_T_62 = or(_pmaPgLevelHomogeneous_T_61, _pmaPgLevelHomogeneous_T_28) @[TLBPermissions.scala 98:65]
    node _pmaPgLevelHomogeneous_T_63 = or(_pmaPgLevelHomogeneous_T_62, _pmaPgLevelHomogeneous_T_33) @[TLBPermissions.scala 98:65]
    node _pmaPgLevelHomogeneous_T_64 = or(_pmaPgLevelHomogeneous_T_63, _pmaPgLevelHomogeneous_T_38) @[TLBPermissions.scala 98:65]
    node _pmaPgLevelHomogeneous_T_65 = or(_pmaPgLevelHomogeneous_T_64, pmaPgLevelHomogeneous_0) @[TLBPermissions.scala 98:65]
    node _pmaPgLevelHomogeneous_T_66 = or(_pmaPgLevelHomogeneous_T_65, _pmaPgLevelHomogeneous_T_48) @[TLBPermissions.scala 98:65]
    node _pmaPgLevelHomogeneous_T_67 = or(_pmaPgLevelHomogeneous_T_66, _pmaPgLevelHomogeneous_T_53) @[TLBPermissions.scala 98:65]
    node pmaPgLevelHomogeneous_1 = or(_pmaPgLevelHomogeneous_T_67, _pmaPgLevelHomogeneous_T_58) @[TLBPermissions.scala 98:65]
    node pmaHomogeneous = mux(count, pmaPgLevelHomogeneous_1, pmaPgLevelHomogeneous_0) @[package.scala 32:76]
    node _pmpHomogeneous_T_2 = bits(io_dpath_pmp_0_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _pmpHomogeneous_maskHomogeneous_T = bits(io_dpath_pmp_0_mask, 21, 21) @[PMP.scala 99:93]
    node _pmpHomogeneous_maskHomogeneous_T_1 = bits(io_dpath_pmp_0_mask, 11, 11) @[PMP.scala 99:93]
    node pmpHomogeneous_maskHomogeneous = mux(count, _pmpHomogeneous_maskHomogeneous_T_1, _pmpHomogeneous_maskHomogeneous_T) @[package.scala 32:76]
    node _pmpHomogeneous_T_3 = shl(io_dpath_pmp_0_addr, 2) @[PMP.scala 62:36]
    node _pmpHomogeneous_T_4 = not(_pmpHomogeneous_T_3) @[PMP.scala 62:29]
    node _pmpHomogeneous_T_5 = or(_pmpHomogeneous_T_4, UInt<32>("h3")) @[PMP.scala 62:48]
    node _pmpHomogeneous_T_6 = not(_pmpHomogeneous_T_5) @[PMP.scala 62:27]
    node _pmpHomogeneous_T_8 = shr(_pmpHomogeneous_T_6, 22) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_9 = neq(_pmpHomogeneous_T_8, UInt<10>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_15 = shr(_pmpHomogeneous_T_6, 12) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_16 = neq(_pmpHomogeneous_T_15, UInt<20>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_18 = mux(count, _pmpHomogeneous_T_16, _pmpHomogeneous_T_9) @[package.scala 32:76]
    node _pmpHomogeneous_T_19 = or(pmpHomogeneous_maskHomogeneous, _pmpHomogeneous_T_18) @[PMP.scala 100:21]
    node _pmpHomogeneous_T_20 = bits(io_dpath_pmp_0_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _pmpHomogeneous_T_21 = not(_pmpHomogeneous_T_20) @[PMP.scala 120:45]
    node _pmpHomogeneous_beginsAfterUpper_T_4 = lt(UInt<32>("h0"), _pmpHomogeneous_T_6) @[PMP.scala 109:32]
    node pmpHomogeneous_beginsAfterUpper = not(_pmpHomogeneous_beginsAfterUpper_T_4) @[PMP.scala 109:28]
    node pmpHomogeneous_pgMask = mux(count, UInt<32>("hfffff000"), UInt<32>("hffc00000")) @[package.scala 32:76]
    node _pmpHomogeneous_endsBeforeUpper_T_5 = and(_pmpHomogeneous_T_6, pmpHomogeneous_pgMask) @[PMP.scala 113:53]
    node pmpHomogeneous_endsBeforeUpper = lt(UInt<32>("h0"), _pmpHomogeneous_endsBeforeUpper_T_5) @[PMP.scala 113:40]
    node _pmpHomogeneous_T_24 = or(pmpHomogeneous_beginsAfterUpper, pmpHomogeneous_endsBeforeUpper) @[PMP.scala 115:41]
    node _pmpHomogeneous_T_25 = or(_pmpHomogeneous_T_21, _pmpHomogeneous_T_24) @[PMP.scala 120:58]
    node _pmpHomogeneous_T_26 = mux(_pmpHomogeneous_T_2, _pmpHomogeneous_T_19, _pmpHomogeneous_T_25) @[PMP.scala 120:8]
    node _pmpHomogeneous_T_28 = bits(io_dpath_pmp_1_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _pmpHomogeneous_maskHomogeneous_T_3 = bits(io_dpath_pmp_1_mask, 21, 21) @[PMP.scala 99:93]
    node _pmpHomogeneous_maskHomogeneous_T_4 = bits(io_dpath_pmp_1_mask, 11, 11) @[PMP.scala 99:93]
    node pmpHomogeneous_maskHomogeneous_1 = mux(count, _pmpHomogeneous_maskHomogeneous_T_4, _pmpHomogeneous_maskHomogeneous_T_3) @[package.scala 32:76]
    node _pmpHomogeneous_T_29 = shl(io_dpath_pmp_1_addr, 2) @[PMP.scala 62:36]
    node _pmpHomogeneous_T_30 = not(_pmpHomogeneous_T_29) @[PMP.scala 62:29]
    node _pmpHomogeneous_T_31 = or(_pmpHomogeneous_T_30, UInt<32>("h3")) @[PMP.scala 62:48]
    node _pmpHomogeneous_T_32 = not(_pmpHomogeneous_T_31) @[PMP.scala 62:27]
    node _pmpHomogeneous_T_34 = shr(_pmpHomogeneous_T_32, 22) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_35 = neq(_pmpHomogeneous_T_34, UInt<10>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_41 = shr(_pmpHomogeneous_T_32, 12) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_42 = neq(_pmpHomogeneous_T_41, UInt<20>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_44 = mux(count, _pmpHomogeneous_T_42, _pmpHomogeneous_T_35) @[package.scala 32:76]
    node _pmpHomogeneous_T_45 = or(pmpHomogeneous_maskHomogeneous_1, _pmpHomogeneous_T_44) @[PMP.scala 100:21]
    node _pmpHomogeneous_T_46 = bits(io_dpath_pmp_1_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _pmpHomogeneous_T_47 = not(_pmpHomogeneous_T_46) @[PMP.scala 120:45]
    node _pmpHomogeneous_beginsAfterUpper_T_9 = lt(UInt<32>("h0"), _pmpHomogeneous_T_32) @[PMP.scala 109:32]
    node pmpHomogeneous_beginsAfterUpper_1 = not(_pmpHomogeneous_beginsAfterUpper_T_9) @[PMP.scala 109:28]
    node _pmpHomogeneous_endsBeforeUpper_T_11 = and(_pmpHomogeneous_T_32, pmpHomogeneous_pgMask) @[PMP.scala 113:53]
    node pmpHomogeneous_endsBeforeUpper_1 = lt(UInt<32>("h0"), _pmpHomogeneous_endsBeforeUpper_T_11) @[PMP.scala 113:40]
    node _pmpHomogeneous_T_48 = or(pmpHomogeneous_endsBeforeUpper, pmpHomogeneous_beginsAfterUpper_1) @[PMP.scala 115:21]
    node _pmpHomogeneous_T_49 = and(pmpHomogeneous_beginsAfterUpper, pmpHomogeneous_endsBeforeUpper_1) @[PMP.scala 115:62]
    node _pmpHomogeneous_T_50 = or(_pmpHomogeneous_T_48, _pmpHomogeneous_T_49) @[PMP.scala 115:41]
    node _pmpHomogeneous_T_51 = or(_pmpHomogeneous_T_47, _pmpHomogeneous_T_50) @[PMP.scala 120:58]
    node _pmpHomogeneous_T_52 = mux(_pmpHomogeneous_T_28, _pmpHomogeneous_T_45, _pmpHomogeneous_T_51) @[PMP.scala 120:8]
    node _pmpHomogeneous_T_53 = and(_pmpHomogeneous_T_26, _pmpHomogeneous_T_52) @[PMP.scala 140:10]
    node _pmpHomogeneous_T_54 = bits(io_dpath_pmp_2_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _pmpHomogeneous_maskHomogeneous_T_6 = bits(io_dpath_pmp_2_mask, 21, 21) @[PMP.scala 99:93]
    node _pmpHomogeneous_maskHomogeneous_T_7 = bits(io_dpath_pmp_2_mask, 11, 11) @[PMP.scala 99:93]
    node pmpHomogeneous_maskHomogeneous_2 = mux(count, _pmpHomogeneous_maskHomogeneous_T_7, _pmpHomogeneous_maskHomogeneous_T_6) @[package.scala 32:76]
    node _pmpHomogeneous_T_55 = shl(io_dpath_pmp_2_addr, 2) @[PMP.scala 62:36]
    node _pmpHomogeneous_T_56 = not(_pmpHomogeneous_T_55) @[PMP.scala 62:29]
    node _pmpHomogeneous_T_57 = or(_pmpHomogeneous_T_56, UInt<32>("h3")) @[PMP.scala 62:48]
    node _pmpHomogeneous_T_58 = not(_pmpHomogeneous_T_57) @[PMP.scala 62:27]
    node _pmpHomogeneous_T_60 = shr(_pmpHomogeneous_T_58, 22) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_61 = neq(_pmpHomogeneous_T_60, UInt<10>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_67 = shr(_pmpHomogeneous_T_58, 12) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_68 = neq(_pmpHomogeneous_T_67, UInt<20>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_70 = mux(count, _pmpHomogeneous_T_68, _pmpHomogeneous_T_61) @[package.scala 32:76]
    node _pmpHomogeneous_T_71 = or(pmpHomogeneous_maskHomogeneous_2, _pmpHomogeneous_T_70) @[PMP.scala 100:21]
    node _pmpHomogeneous_T_72 = bits(io_dpath_pmp_2_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _pmpHomogeneous_T_73 = not(_pmpHomogeneous_T_72) @[PMP.scala 120:45]
    node _pmpHomogeneous_beginsAfterUpper_T_14 = lt(UInt<32>("h0"), _pmpHomogeneous_T_58) @[PMP.scala 109:32]
    node pmpHomogeneous_beginsAfterUpper_2 = not(_pmpHomogeneous_beginsAfterUpper_T_14) @[PMP.scala 109:28]
    node _pmpHomogeneous_endsBeforeUpper_T_17 = and(_pmpHomogeneous_T_58, pmpHomogeneous_pgMask) @[PMP.scala 113:53]
    node pmpHomogeneous_endsBeforeUpper_2 = lt(UInt<32>("h0"), _pmpHomogeneous_endsBeforeUpper_T_17) @[PMP.scala 113:40]
    node _pmpHomogeneous_T_74 = or(pmpHomogeneous_endsBeforeUpper_1, pmpHomogeneous_beginsAfterUpper_2) @[PMP.scala 115:21]
    node _pmpHomogeneous_T_75 = and(pmpHomogeneous_beginsAfterUpper_1, pmpHomogeneous_endsBeforeUpper_2) @[PMP.scala 115:62]
    node _pmpHomogeneous_T_76 = or(_pmpHomogeneous_T_74, _pmpHomogeneous_T_75) @[PMP.scala 115:41]
    node _pmpHomogeneous_T_77 = or(_pmpHomogeneous_T_73, _pmpHomogeneous_T_76) @[PMP.scala 120:58]
    node _pmpHomogeneous_T_78 = mux(_pmpHomogeneous_T_54, _pmpHomogeneous_T_71, _pmpHomogeneous_T_77) @[PMP.scala 120:8]
    node _pmpHomogeneous_T_79 = and(_pmpHomogeneous_T_53, _pmpHomogeneous_T_78) @[PMP.scala 140:10]
    node _pmpHomogeneous_T_80 = bits(io_dpath_pmp_3_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _pmpHomogeneous_maskHomogeneous_T_9 = bits(io_dpath_pmp_3_mask, 21, 21) @[PMP.scala 99:93]
    node _pmpHomogeneous_maskHomogeneous_T_10 = bits(io_dpath_pmp_3_mask, 11, 11) @[PMP.scala 99:93]
    node pmpHomogeneous_maskHomogeneous_3 = mux(count, _pmpHomogeneous_maskHomogeneous_T_10, _pmpHomogeneous_maskHomogeneous_T_9) @[package.scala 32:76]
    node _pmpHomogeneous_T_81 = shl(io_dpath_pmp_3_addr, 2) @[PMP.scala 62:36]
    node _pmpHomogeneous_T_82 = not(_pmpHomogeneous_T_81) @[PMP.scala 62:29]
    node _pmpHomogeneous_T_83 = or(_pmpHomogeneous_T_82, UInt<32>("h3")) @[PMP.scala 62:48]
    node _pmpHomogeneous_T_84 = not(_pmpHomogeneous_T_83) @[PMP.scala 62:27]
    node _pmpHomogeneous_T_86 = shr(_pmpHomogeneous_T_84, 22) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_87 = neq(_pmpHomogeneous_T_86, UInt<10>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_93 = shr(_pmpHomogeneous_T_84, 12) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_94 = neq(_pmpHomogeneous_T_93, UInt<20>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_96 = mux(count, _pmpHomogeneous_T_94, _pmpHomogeneous_T_87) @[package.scala 32:76]
    node _pmpHomogeneous_T_97 = or(pmpHomogeneous_maskHomogeneous_3, _pmpHomogeneous_T_96) @[PMP.scala 100:21]
    node _pmpHomogeneous_T_98 = bits(io_dpath_pmp_3_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _pmpHomogeneous_T_99 = not(_pmpHomogeneous_T_98) @[PMP.scala 120:45]
    node _pmpHomogeneous_beginsAfterUpper_T_19 = lt(UInt<32>("h0"), _pmpHomogeneous_T_84) @[PMP.scala 109:32]
    node pmpHomogeneous_beginsAfterUpper_3 = not(_pmpHomogeneous_beginsAfterUpper_T_19) @[PMP.scala 109:28]
    node _pmpHomogeneous_endsBeforeUpper_T_23 = and(_pmpHomogeneous_T_84, pmpHomogeneous_pgMask) @[PMP.scala 113:53]
    node pmpHomogeneous_endsBeforeUpper_3 = lt(UInt<32>("h0"), _pmpHomogeneous_endsBeforeUpper_T_23) @[PMP.scala 113:40]
    node _pmpHomogeneous_T_100 = or(pmpHomogeneous_endsBeforeUpper_2, pmpHomogeneous_beginsAfterUpper_3) @[PMP.scala 115:21]
    node _pmpHomogeneous_T_101 = and(pmpHomogeneous_beginsAfterUpper_2, pmpHomogeneous_endsBeforeUpper_3) @[PMP.scala 115:62]
    node _pmpHomogeneous_T_102 = or(_pmpHomogeneous_T_100, _pmpHomogeneous_T_101) @[PMP.scala 115:41]
    node _pmpHomogeneous_T_103 = or(_pmpHomogeneous_T_99, _pmpHomogeneous_T_102) @[PMP.scala 120:58]
    node _pmpHomogeneous_T_104 = mux(_pmpHomogeneous_T_80, _pmpHomogeneous_T_97, _pmpHomogeneous_T_103) @[PMP.scala 120:8]
    node _pmpHomogeneous_T_105 = and(_pmpHomogeneous_T_79, _pmpHomogeneous_T_104) @[PMP.scala 140:10]
    node _pmpHomogeneous_T_106 = bits(io_dpath_pmp_4_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _pmpHomogeneous_maskHomogeneous_T_12 = bits(io_dpath_pmp_4_mask, 21, 21) @[PMP.scala 99:93]
    node _pmpHomogeneous_maskHomogeneous_T_13 = bits(io_dpath_pmp_4_mask, 11, 11) @[PMP.scala 99:93]
    node pmpHomogeneous_maskHomogeneous_4 = mux(count, _pmpHomogeneous_maskHomogeneous_T_13, _pmpHomogeneous_maskHomogeneous_T_12) @[package.scala 32:76]
    node _pmpHomogeneous_T_107 = shl(io_dpath_pmp_4_addr, 2) @[PMP.scala 62:36]
    node _pmpHomogeneous_T_108 = not(_pmpHomogeneous_T_107) @[PMP.scala 62:29]
    node _pmpHomogeneous_T_109 = or(_pmpHomogeneous_T_108, UInt<32>("h3")) @[PMP.scala 62:48]
    node _pmpHomogeneous_T_110 = not(_pmpHomogeneous_T_109) @[PMP.scala 62:27]
    node _pmpHomogeneous_T_112 = shr(_pmpHomogeneous_T_110, 22) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_113 = neq(_pmpHomogeneous_T_112, UInt<10>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_119 = shr(_pmpHomogeneous_T_110, 12) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_120 = neq(_pmpHomogeneous_T_119, UInt<20>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_122 = mux(count, _pmpHomogeneous_T_120, _pmpHomogeneous_T_113) @[package.scala 32:76]
    node _pmpHomogeneous_T_123 = or(pmpHomogeneous_maskHomogeneous_4, _pmpHomogeneous_T_122) @[PMP.scala 100:21]
    node _pmpHomogeneous_T_124 = bits(io_dpath_pmp_4_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _pmpHomogeneous_T_125 = not(_pmpHomogeneous_T_124) @[PMP.scala 120:45]
    node _pmpHomogeneous_beginsAfterUpper_T_24 = lt(UInt<32>("h0"), _pmpHomogeneous_T_110) @[PMP.scala 109:32]
    node pmpHomogeneous_beginsAfterUpper_4 = not(_pmpHomogeneous_beginsAfterUpper_T_24) @[PMP.scala 109:28]
    node _pmpHomogeneous_endsBeforeUpper_T_29 = and(_pmpHomogeneous_T_110, pmpHomogeneous_pgMask) @[PMP.scala 113:53]
    node pmpHomogeneous_endsBeforeUpper_4 = lt(UInt<32>("h0"), _pmpHomogeneous_endsBeforeUpper_T_29) @[PMP.scala 113:40]
    node _pmpHomogeneous_T_126 = or(pmpHomogeneous_endsBeforeUpper_3, pmpHomogeneous_beginsAfterUpper_4) @[PMP.scala 115:21]
    node _pmpHomogeneous_T_127 = and(pmpHomogeneous_beginsAfterUpper_3, pmpHomogeneous_endsBeforeUpper_4) @[PMP.scala 115:62]
    node _pmpHomogeneous_T_128 = or(_pmpHomogeneous_T_126, _pmpHomogeneous_T_127) @[PMP.scala 115:41]
    node _pmpHomogeneous_T_129 = or(_pmpHomogeneous_T_125, _pmpHomogeneous_T_128) @[PMP.scala 120:58]
    node _pmpHomogeneous_T_130 = mux(_pmpHomogeneous_T_106, _pmpHomogeneous_T_123, _pmpHomogeneous_T_129) @[PMP.scala 120:8]
    node _pmpHomogeneous_T_131 = and(_pmpHomogeneous_T_105, _pmpHomogeneous_T_130) @[PMP.scala 140:10]
    node _pmpHomogeneous_T_132 = bits(io_dpath_pmp_5_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _pmpHomogeneous_maskHomogeneous_T_15 = bits(io_dpath_pmp_5_mask, 21, 21) @[PMP.scala 99:93]
    node _pmpHomogeneous_maskHomogeneous_T_16 = bits(io_dpath_pmp_5_mask, 11, 11) @[PMP.scala 99:93]
    node pmpHomogeneous_maskHomogeneous_5 = mux(count, _pmpHomogeneous_maskHomogeneous_T_16, _pmpHomogeneous_maskHomogeneous_T_15) @[package.scala 32:76]
    node _pmpHomogeneous_T_133 = shl(io_dpath_pmp_5_addr, 2) @[PMP.scala 62:36]
    node _pmpHomogeneous_T_134 = not(_pmpHomogeneous_T_133) @[PMP.scala 62:29]
    node _pmpHomogeneous_T_135 = or(_pmpHomogeneous_T_134, UInt<32>("h3")) @[PMP.scala 62:48]
    node _pmpHomogeneous_T_136 = not(_pmpHomogeneous_T_135) @[PMP.scala 62:27]
    node _pmpHomogeneous_T_138 = shr(_pmpHomogeneous_T_136, 22) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_139 = neq(_pmpHomogeneous_T_138, UInt<10>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_145 = shr(_pmpHomogeneous_T_136, 12) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_146 = neq(_pmpHomogeneous_T_145, UInt<20>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_148 = mux(count, _pmpHomogeneous_T_146, _pmpHomogeneous_T_139) @[package.scala 32:76]
    node _pmpHomogeneous_T_149 = or(pmpHomogeneous_maskHomogeneous_5, _pmpHomogeneous_T_148) @[PMP.scala 100:21]
    node _pmpHomogeneous_T_150 = bits(io_dpath_pmp_5_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _pmpHomogeneous_T_151 = not(_pmpHomogeneous_T_150) @[PMP.scala 120:45]
    node _pmpHomogeneous_beginsAfterUpper_T_29 = lt(UInt<32>("h0"), _pmpHomogeneous_T_136) @[PMP.scala 109:32]
    node pmpHomogeneous_beginsAfterUpper_5 = not(_pmpHomogeneous_beginsAfterUpper_T_29) @[PMP.scala 109:28]
    node _pmpHomogeneous_endsBeforeUpper_T_35 = and(_pmpHomogeneous_T_136, pmpHomogeneous_pgMask) @[PMP.scala 113:53]
    node pmpHomogeneous_endsBeforeUpper_5 = lt(UInt<32>("h0"), _pmpHomogeneous_endsBeforeUpper_T_35) @[PMP.scala 113:40]
    node _pmpHomogeneous_T_152 = or(pmpHomogeneous_endsBeforeUpper_4, pmpHomogeneous_beginsAfterUpper_5) @[PMP.scala 115:21]
    node _pmpHomogeneous_T_153 = and(pmpHomogeneous_beginsAfterUpper_4, pmpHomogeneous_endsBeforeUpper_5) @[PMP.scala 115:62]
    node _pmpHomogeneous_T_154 = or(_pmpHomogeneous_T_152, _pmpHomogeneous_T_153) @[PMP.scala 115:41]
    node _pmpHomogeneous_T_155 = or(_pmpHomogeneous_T_151, _pmpHomogeneous_T_154) @[PMP.scala 120:58]
    node _pmpHomogeneous_T_156 = mux(_pmpHomogeneous_T_132, _pmpHomogeneous_T_149, _pmpHomogeneous_T_155) @[PMP.scala 120:8]
    node _pmpHomogeneous_T_157 = and(_pmpHomogeneous_T_131, _pmpHomogeneous_T_156) @[PMP.scala 140:10]
    node _pmpHomogeneous_T_158 = bits(io_dpath_pmp_6_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _pmpHomogeneous_maskHomogeneous_T_18 = bits(io_dpath_pmp_6_mask, 21, 21) @[PMP.scala 99:93]
    node _pmpHomogeneous_maskHomogeneous_T_19 = bits(io_dpath_pmp_6_mask, 11, 11) @[PMP.scala 99:93]
    node pmpHomogeneous_maskHomogeneous_6 = mux(count, _pmpHomogeneous_maskHomogeneous_T_19, _pmpHomogeneous_maskHomogeneous_T_18) @[package.scala 32:76]
    node _pmpHomogeneous_T_159 = shl(io_dpath_pmp_6_addr, 2) @[PMP.scala 62:36]
    node _pmpHomogeneous_T_160 = not(_pmpHomogeneous_T_159) @[PMP.scala 62:29]
    node _pmpHomogeneous_T_161 = or(_pmpHomogeneous_T_160, UInt<32>("h3")) @[PMP.scala 62:48]
    node _pmpHomogeneous_T_162 = not(_pmpHomogeneous_T_161) @[PMP.scala 62:27]
    node _pmpHomogeneous_T_164 = shr(_pmpHomogeneous_T_162, 22) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_165 = neq(_pmpHomogeneous_T_164, UInt<10>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_171 = shr(_pmpHomogeneous_T_162, 12) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_172 = neq(_pmpHomogeneous_T_171, UInt<20>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_174 = mux(count, _pmpHomogeneous_T_172, _pmpHomogeneous_T_165) @[package.scala 32:76]
    node _pmpHomogeneous_T_175 = or(pmpHomogeneous_maskHomogeneous_6, _pmpHomogeneous_T_174) @[PMP.scala 100:21]
    node _pmpHomogeneous_T_176 = bits(io_dpath_pmp_6_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _pmpHomogeneous_T_177 = not(_pmpHomogeneous_T_176) @[PMP.scala 120:45]
    node _pmpHomogeneous_beginsAfterUpper_T_34 = lt(UInt<32>("h0"), _pmpHomogeneous_T_162) @[PMP.scala 109:32]
    node pmpHomogeneous_beginsAfterUpper_6 = not(_pmpHomogeneous_beginsAfterUpper_T_34) @[PMP.scala 109:28]
    node _pmpHomogeneous_endsBeforeUpper_T_41 = and(_pmpHomogeneous_T_162, pmpHomogeneous_pgMask) @[PMP.scala 113:53]
    node pmpHomogeneous_endsBeforeUpper_6 = lt(UInt<32>("h0"), _pmpHomogeneous_endsBeforeUpper_T_41) @[PMP.scala 113:40]
    node _pmpHomogeneous_T_178 = or(pmpHomogeneous_endsBeforeUpper_5, pmpHomogeneous_beginsAfterUpper_6) @[PMP.scala 115:21]
    node _pmpHomogeneous_T_179 = and(pmpHomogeneous_beginsAfterUpper_5, pmpHomogeneous_endsBeforeUpper_6) @[PMP.scala 115:62]
    node _pmpHomogeneous_T_180 = or(_pmpHomogeneous_T_178, _pmpHomogeneous_T_179) @[PMP.scala 115:41]
    node _pmpHomogeneous_T_181 = or(_pmpHomogeneous_T_177, _pmpHomogeneous_T_180) @[PMP.scala 120:58]
    node _pmpHomogeneous_T_182 = mux(_pmpHomogeneous_T_158, _pmpHomogeneous_T_175, _pmpHomogeneous_T_181) @[PMP.scala 120:8]
    node _pmpHomogeneous_T_183 = and(_pmpHomogeneous_T_157, _pmpHomogeneous_T_182) @[PMP.scala 140:10]
    node _pmpHomogeneous_T_184 = bits(io_dpath_pmp_7_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _pmpHomogeneous_maskHomogeneous_T_21 = bits(io_dpath_pmp_7_mask, 21, 21) @[PMP.scala 99:93]
    node _pmpHomogeneous_maskHomogeneous_T_22 = bits(io_dpath_pmp_7_mask, 11, 11) @[PMP.scala 99:93]
    node pmpHomogeneous_maskHomogeneous_7 = mux(count, _pmpHomogeneous_maskHomogeneous_T_22, _pmpHomogeneous_maskHomogeneous_T_21) @[package.scala 32:76]
    node _pmpHomogeneous_T_185 = shl(io_dpath_pmp_7_addr, 2) @[PMP.scala 62:36]
    node _pmpHomogeneous_T_186 = not(_pmpHomogeneous_T_185) @[PMP.scala 62:29]
    node _pmpHomogeneous_T_187 = or(_pmpHomogeneous_T_186, UInt<32>("h3")) @[PMP.scala 62:48]
    node _pmpHomogeneous_T_188 = not(_pmpHomogeneous_T_187) @[PMP.scala 62:27]
    node _pmpHomogeneous_T_190 = shr(_pmpHomogeneous_T_188, 22) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_191 = neq(_pmpHomogeneous_T_190, UInt<10>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_197 = shr(_pmpHomogeneous_T_188, 12) @[PMP.scala 100:66]
    node _pmpHomogeneous_T_198 = neq(_pmpHomogeneous_T_197, UInt<20>("h0")) @[PMP.scala 100:78]
    node _pmpHomogeneous_T_200 = mux(count, _pmpHomogeneous_T_198, _pmpHomogeneous_T_191) @[package.scala 32:76]
    node _pmpHomogeneous_T_201 = or(pmpHomogeneous_maskHomogeneous_7, _pmpHomogeneous_T_200) @[PMP.scala 100:21]
    node _pmpHomogeneous_T_202 = bits(io_dpath_pmp_7_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _pmpHomogeneous_T_203 = not(_pmpHomogeneous_T_202) @[PMP.scala 120:45]
    node _pmpHomogeneous_beginsAfterUpper_T_39 = lt(UInt<32>("h0"), _pmpHomogeneous_T_188) @[PMP.scala 109:32]
    node pmpHomogeneous_beginsAfterUpper_7 = not(_pmpHomogeneous_beginsAfterUpper_T_39) @[PMP.scala 109:28]
    node _pmpHomogeneous_endsBeforeUpper_T_47 = and(_pmpHomogeneous_T_188, pmpHomogeneous_pgMask) @[PMP.scala 113:53]
    node pmpHomogeneous_endsBeforeUpper_7 = lt(UInt<32>("h0"), _pmpHomogeneous_endsBeforeUpper_T_47) @[PMP.scala 113:40]
    node _pmpHomogeneous_T_204 = or(pmpHomogeneous_endsBeforeUpper_6, pmpHomogeneous_beginsAfterUpper_7) @[PMP.scala 115:21]
    node _pmpHomogeneous_T_205 = and(pmpHomogeneous_beginsAfterUpper_6, pmpHomogeneous_endsBeforeUpper_7) @[PMP.scala 115:62]
    node _pmpHomogeneous_T_206 = or(_pmpHomogeneous_T_204, _pmpHomogeneous_T_205) @[PMP.scala 115:41]
    node _pmpHomogeneous_T_207 = or(_pmpHomogeneous_T_203, _pmpHomogeneous_T_206) @[PMP.scala 120:58]
    node _pmpHomogeneous_T_208 = mux(_pmpHomogeneous_T_184, _pmpHomogeneous_T_201, _pmpHomogeneous_T_207) @[PMP.scala 120:8]
    node pmpHomogeneous = and(_pmpHomogeneous_T_183, _pmpHomogeneous_T_208) @[PMP.scala 140:10]
    node homogeneous = and(pmaHomogeneous, pmpHomogeneous) @[PTW.scala 310:36]
    node _T_111 = eq(UInt<3>("h0"), state) @[Conditional.scala 37:30]
    node _T_113 = eq(UInt<3>("h1"), state) @[Conditional.scala 37:30]
    node _T_114 = eq(UInt<3>("h2"), state) @[Conditional.scala 37:30]
    node _T_115 = eq(UInt<3>("h4"), state) @[Conditional.scala 37:30]
    node _T_118 = eq(UInt<3>("h7"), state) @[Conditional.scala 37:30]
    node _T_121 = not(homogeneous) @[PTW.scala 361:13]
    node _GEN_2103 = or(_T_121, count) @[PTW.scala 361:27 362:15 126:18]
    node _GEN_2105 = mux(_T_118, UInt<3>("h0"), state) @[Conditional.scala 39:67 PTW.scala 358:18]
    node _GEN_2109 = mux(_T_118, _GEN_2103, count) @[Conditional.scala 39:67 PTW.scala 126:18]
    node _GEN_2111 = mux(_T_115, UInt<3>("h5"), _GEN_2105) @[Conditional.scala 39:67]
    node _GEN_2112 = and(_T_115, _traverse_T_8) @[Conditional.scala 39:67 PTW.scala 196:26 350:30]
    node _GEN_2116 = mux(_T_115, count, _GEN_2109) @[Conditional.scala 39:67 PTW.scala 126:18]
    node _GEN_2118 = mux(_T_114, UInt<3>("h4"), _GEN_2111) @[Conditional.scala 39:67 PTW.scala 346:18]
    node _GEN_2119 = mux(_T_114, UInt<1>("h0"), _GEN_2112) @[Conditional.scala 39:67 PTW.scala 196:26]
    node _GEN_2123 = mux(_T_114, count, _GEN_2116) @[Conditional.scala 39:67 PTW.scala 126:18]
    node _GEN_2125 = mux(_T_113, count, _GEN_2123) @[Conditional.scala 39:67]
    node _GEN_2127 = mux(_T_113, UInt<3>("h1"), _GEN_2118) @[Conditional.scala 39:67]
    node _GEN_2128 = mux(_T_113, UInt<1>("h0"), _GEN_2119) @[Conditional.scala 39:67 PTW.scala 196:26]
    io_requestor_0_status_debug <= io_dpath_status_debug @[PTW.scala 321:28]
    io_requestor_0_pmp_0_cfg_l <= io_dpath_pmp_0_cfg_l @[PTW.scala 322:25]
    io_requestor_0_pmp_0_cfg_a <= io_dpath_pmp_0_cfg_a @[PTW.scala 322:25]
    io_requestor_0_pmp_0_cfg_x <= io_dpath_pmp_0_cfg_x @[PTW.scala 322:25]
    io_requestor_0_pmp_0_cfg_w <= io_dpath_pmp_0_cfg_w @[PTW.scala 322:25]
    io_requestor_0_pmp_0_cfg_r <= io_dpath_pmp_0_cfg_r @[PTW.scala 322:25]
    io_requestor_0_pmp_0_addr <= io_dpath_pmp_0_addr @[PTW.scala 322:25]
    io_requestor_0_pmp_0_mask <= io_dpath_pmp_0_mask @[PTW.scala 322:25]
    io_requestor_0_pmp_1_cfg_l <= io_dpath_pmp_1_cfg_l @[PTW.scala 322:25]
    io_requestor_0_pmp_1_cfg_a <= io_dpath_pmp_1_cfg_a @[PTW.scala 322:25]
    io_requestor_0_pmp_1_cfg_x <= io_dpath_pmp_1_cfg_x @[PTW.scala 322:25]
    io_requestor_0_pmp_1_cfg_w <= io_dpath_pmp_1_cfg_w @[PTW.scala 322:25]
    io_requestor_0_pmp_1_cfg_r <= io_dpath_pmp_1_cfg_r @[PTW.scala 322:25]
    io_requestor_0_pmp_1_addr <= io_dpath_pmp_1_addr @[PTW.scala 322:25]
    io_requestor_0_pmp_1_mask <= io_dpath_pmp_1_mask @[PTW.scala 322:25]
    io_requestor_0_pmp_2_cfg_l <= io_dpath_pmp_2_cfg_l @[PTW.scala 322:25]
    io_requestor_0_pmp_2_cfg_a <= io_dpath_pmp_2_cfg_a @[PTW.scala 322:25]
    io_requestor_0_pmp_2_cfg_x <= io_dpath_pmp_2_cfg_x @[PTW.scala 322:25]
    io_requestor_0_pmp_2_cfg_w <= io_dpath_pmp_2_cfg_w @[PTW.scala 322:25]
    io_requestor_0_pmp_2_cfg_r <= io_dpath_pmp_2_cfg_r @[PTW.scala 322:25]
    io_requestor_0_pmp_2_addr <= io_dpath_pmp_2_addr @[PTW.scala 322:25]
    io_requestor_0_pmp_2_mask <= io_dpath_pmp_2_mask @[PTW.scala 322:25]
    io_requestor_0_pmp_3_cfg_l <= io_dpath_pmp_3_cfg_l @[PTW.scala 322:25]
    io_requestor_0_pmp_3_cfg_a <= io_dpath_pmp_3_cfg_a @[PTW.scala 322:25]
    io_requestor_0_pmp_3_cfg_x <= io_dpath_pmp_3_cfg_x @[PTW.scala 322:25]
    io_requestor_0_pmp_3_cfg_w <= io_dpath_pmp_3_cfg_w @[PTW.scala 322:25]
    io_requestor_0_pmp_3_cfg_r <= io_dpath_pmp_3_cfg_r @[PTW.scala 322:25]
    io_requestor_0_pmp_3_addr <= io_dpath_pmp_3_addr @[PTW.scala 322:25]
    io_requestor_0_pmp_3_mask <= io_dpath_pmp_3_mask @[PTW.scala 322:25]
    io_requestor_0_pmp_4_cfg_l <= io_dpath_pmp_4_cfg_l @[PTW.scala 322:25]
    io_requestor_0_pmp_4_cfg_a <= io_dpath_pmp_4_cfg_a @[PTW.scala 322:25]
    io_requestor_0_pmp_4_cfg_x <= io_dpath_pmp_4_cfg_x @[PTW.scala 322:25]
    io_requestor_0_pmp_4_cfg_w <= io_dpath_pmp_4_cfg_w @[PTW.scala 322:25]
    io_requestor_0_pmp_4_cfg_r <= io_dpath_pmp_4_cfg_r @[PTW.scala 322:25]
    io_requestor_0_pmp_4_addr <= io_dpath_pmp_4_addr @[PTW.scala 322:25]
    io_requestor_0_pmp_4_mask <= io_dpath_pmp_4_mask @[PTW.scala 322:25]
    io_requestor_0_pmp_5_cfg_l <= io_dpath_pmp_5_cfg_l @[PTW.scala 322:25]
    io_requestor_0_pmp_5_cfg_a <= io_dpath_pmp_5_cfg_a @[PTW.scala 322:25]
    io_requestor_0_pmp_5_cfg_x <= io_dpath_pmp_5_cfg_x @[PTW.scala 322:25]
    io_requestor_0_pmp_5_cfg_w <= io_dpath_pmp_5_cfg_w @[PTW.scala 322:25]
    io_requestor_0_pmp_5_cfg_r <= io_dpath_pmp_5_cfg_r @[PTW.scala 322:25]
    io_requestor_0_pmp_5_addr <= io_dpath_pmp_5_addr @[PTW.scala 322:25]
    io_requestor_0_pmp_5_mask <= io_dpath_pmp_5_mask @[PTW.scala 322:25]
    io_requestor_0_pmp_6_cfg_l <= io_dpath_pmp_6_cfg_l @[PTW.scala 322:25]
    io_requestor_0_pmp_6_cfg_a <= io_dpath_pmp_6_cfg_a @[PTW.scala 322:25]
    io_requestor_0_pmp_6_cfg_x <= io_dpath_pmp_6_cfg_x @[PTW.scala 322:25]
    io_requestor_0_pmp_6_cfg_w <= io_dpath_pmp_6_cfg_w @[PTW.scala 322:25]
    io_requestor_0_pmp_6_cfg_r <= io_dpath_pmp_6_cfg_r @[PTW.scala 322:25]
    io_requestor_0_pmp_6_addr <= io_dpath_pmp_6_addr @[PTW.scala 322:25]
    io_requestor_0_pmp_6_mask <= io_dpath_pmp_6_mask @[PTW.scala 322:25]
    io_requestor_0_pmp_7_cfg_l <= io_dpath_pmp_7_cfg_l @[PTW.scala 322:25]
    io_requestor_0_pmp_7_cfg_a <= io_dpath_pmp_7_cfg_a @[PTW.scala 322:25]
    io_requestor_0_pmp_7_cfg_x <= io_dpath_pmp_7_cfg_x @[PTW.scala 322:25]
    io_requestor_0_pmp_7_cfg_w <= io_dpath_pmp_7_cfg_w @[PTW.scala 322:25]
    io_requestor_0_pmp_7_cfg_r <= io_dpath_pmp_7_cfg_r @[PTW.scala 322:25]
    io_requestor_0_pmp_7_addr <= io_dpath_pmp_7_addr @[PTW.scala 322:25]
    io_requestor_0_pmp_7_mask <= io_dpath_pmp_7_mask @[PTW.scala 322:25]
    io_requestor_1_status_debug <= io_dpath_status_debug @[PTW.scala 321:28]
    io_requestor_1_pmp_0_cfg_l <= io_dpath_pmp_0_cfg_l @[PTW.scala 322:25]
    io_requestor_1_pmp_0_cfg_a <= io_dpath_pmp_0_cfg_a @[PTW.scala 322:25]
    io_requestor_1_pmp_0_cfg_x <= io_dpath_pmp_0_cfg_x @[PTW.scala 322:25]
    io_requestor_1_pmp_0_cfg_w <= io_dpath_pmp_0_cfg_w @[PTW.scala 322:25]
    io_requestor_1_pmp_0_cfg_r <= io_dpath_pmp_0_cfg_r @[PTW.scala 322:25]
    io_requestor_1_pmp_0_addr <= io_dpath_pmp_0_addr @[PTW.scala 322:25]
    io_requestor_1_pmp_0_mask <= io_dpath_pmp_0_mask @[PTW.scala 322:25]
    io_requestor_1_pmp_1_cfg_l <= io_dpath_pmp_1_cfg_l @[PTW.scala 322:25]
    io_requestor_1_pmp_1_cfg_a <= io_dpath_pmp_1_cfg_a @[PTW.scala 322:25]
    io_requestor_1_pmp_1_cfg_x <= io_dpath_pmp_1_cfg_x @[PTW.scala 322:25]
    io_requestor_1_pmp_1_cfg_w <= io_dpath_pmp_1_cfg_w @[PTW.scala 322:25]
    io_requestor_1_pmp_1_cfg_r <= io_dpath_pmp_1_cfg_r @[PTW.scala 322:25]
    io_requestor_1_pmp_1_addr <= io_dpath_pmp_1_addr @[PTW.scala 322:25]
    io_requestor_1_pmp_1_mask <= io_dpath_pmp_1_mask @[PTW.scala 322:25]
    io_requestor_1_pmp_2_cfg_l <= io_dpath_pmp_2_cfg_l @[PTW.scala 322:25]
    io_requestor_1_pmp_2_cfg_a <= io_dpath_pmp_2_cfg_a @[PTW.scala 322:25]
    io_requestor_1_pmp_2_cfg_x <= io_dpath_pmp_2_cfg_x @[PTW.scala 322:25]
    io_requestor_1_pmp_2_cfg_w <= io_dpath_pmp_2_cfg_w @[PTW.scala 322:25]
    io_requestor_1_pmp_2_cfg_r <= io_dpath_pmp_2_cfg_r @[PTW.scala 322:25]
    io_requestor_1_pmp_2_addr <= io_dpath_pmp_2_addr @[PTW.scala 322:25]
    io_requestor_1_pmp_2_mask <= io_dpath_pmp_2_mask @[PTW.scala 322:25]
    io_requestor_1_pmp_3_cfg_l <= io_dpath_pmp_3_cfg_l @[PTW.scala 322:25]
    io_requestor_1_pmp_3_cfg_a <= io_dpath_pmp_3_cfg_a @[PTW.scala 322:25]
    io_requestor_1_pmp_3_cfg_x <= io_dpath_pmp_3_cfg_x @[PTW.scala 322:25]
    io_requestor_1_pmp_3_cfg_w <= io_dpath_pmp_3_cfg_w @[PTW.scala 322:25]
    io_requestor_1_pmp_3_cfg_r <= io_dpath_pmp_3_cfg_r @[PTW.scala 322:25]
    io_requestor_1_pmp_3_addr <= io_dpath_pmp_3_addr @[PTW.scala 322:25]
    io_requestor_1_pmp_3_mask <= io_dpath_pmp_3_mask @[PTW.scala 322:25]
    io_requestor_1_pmp_4_cfg_l <= io_dpath_pmp_4_cfg_l @[PTW.scala 322:25]
    io_requestor_1_pmp_4_cfg_a <= io_dpath_pmp_4_cfg_a @[PTW.scala 322:25]
    io_requestor_1_pmp_4_cfg_x <= io_dpath_pmp_4_cfg_x @[PTW.scala 322:25]
    io_requestor_1_pmp_4_cfg_w <= io_dpath_pmp_4_cfg_w @[PTW.scala 322:25]
    io_requestor_1_pmp_4_cfg_r <= io_dpath_pmp_4_cfg_r @[PTW.scala 322:25]
    io_requestor_1_pmp_4_addr <= io_dpath_pmp_4_addr @[PTW.scala 322:25]
    io_requestor_1_pmp_4_mask <= io_dpath_pmp_4_mask @[PTW.scala 322:25]
    io_requestor_1_pmp_5_cfg_l <= io_dpath_pmp_5_cfg_l @[PTW.scala 322:25]
    io_requestor_1_pmp_5_cfg_a <= io_dpath_pmp_5_cfg_a @[PTW.scala 322:25]
    io_requestor_1_pmp_5_cfg_x <= io_dpath_pmp_5_cfg_x @[PTW.scala 322:25]
    io_requestor_1_pmp_5_cfg_w <= io_dpath_pmp_5_cfg_w @[PTW.scala 322:25]
    io_requestor_1_pmp_5_cfg_r <= io_dpath_pmp_5_cfg_r @[PTW.scala 322:25]
    io_requestor_1_pmp_5_addr <= io_dpath_pmp_5_addr @[PTW.scala 322:25]
    io_requestor_1_pmp_5_mask <= io_dpath_pmp_5_mask @[PTW.scala 322:25]
    io_requestor_1_pmp_6_cfg_l <= io_dpath_pmp_6_cfg_l @[PTW.scala 322:25]
    io_requestor_1_pmp_6_cfg_a <= io_dpath_pmp_6_cfg_a @[PTW.scala 322:25]
    io_requestor_1_pmp_6_cfg_x <= io_dpath_pmp_6_cfg_x @[PTW.scala 322:25]
    io_requestor_1_pmp_6_cfg_w <= io_dpath_pmp_6_cfg_w @[PTW.scala 322:25]
    io_requestor_1_pmp_6_cfg_r <= io_dpath_pmp_6_cfg_r @[PTW.scala 322:25]
    io_requestor_1_pmp_6_addr <= io_dpath_pmp_6_addr @[PTW.scala 322:25]
    io_requestor_1_pmp_6_mask <= io_dpath_pmp_6_mask @[PTW.scala 322:25]
    io_requestor_1_pmp_7_cfg_l <= io_dpath_pmp_7_cfg_l @[PTW.scala 322:25]
    io_requestor_1_pmp_7_cfg_a <= io_dpath_pmp_7_cfg_a @[PTW.scala 322:25]
    io_requestor_1_pmp_7_cfg_x <= io_dpath_pmp_7_cfg_x @[PTW.scala 322:25]
    io_requestor_1_pmp_7_cfg_w <= io_dpath_pmp_7_cfg_w @[PTW.scala 322:25]
    io_requestor_1_pmp_7_cfg_r <= io_dpath_pmp_7_cfg_r @[PTW.scala 322:25]
    io_requestor_1_pmp_7_addr <= io_dpath_pmp_7_addr @[PTW.scala 322:25]
    io_requestor_1_pmp_7_mask <= io_dpath_pmp_7_mask @[PTW.scala 322:25]
    io_requestor_1_customCSRs_csrs_0_value <= io_dpath_customCSRs_csrs_0_value @[PTW.scala 320:32]
    io_dpath_perf_l2hit <= UInt<1>("h0") @[PTW.scala 263:27]
    io_dpath_perf_pte_miss <= mux(_T_111, UInt<1>("h0"), _GEN_2128) @[Conditional.scala 40:58 PTW.scala 196:26]
    io_dpath_perf_pte_hit <= UInt<1>("h0") @[PTW.scala 197:57]
    state_barrier.io_x <= mux(_T_111, state, _GEN_2127) @[Conditional.scala 40:58]
    state <= mux(reset, UInt<3>("h0"), state_barrier.io_y) @[PTW.scala 109:{18,18} 327:9]
    count <= mux(_T_111, UInt<1>("h0"), _GEN_2125) @[Conditional.scala 40:58 PTW.scala 334:13]
    printf(clock, _T_66, "Assertion failed: PTE Cache Hit/Miss Performance Monitor Events are lower priority than L2TLB Hit event\n    at PTW.scala:198 assert(!(io.dpath.perf.l2hit && (io.dpath.perf.pte_miss || io.dpath.perf.pte_hit)),\n") @[PTW.scala 198:9]
    stop(clock, _T_66, 1) @[PTW.scala 198:9]

  module Rocket :
    input clock : Clock
    input reset : UInt<1>
    input io_hartid : UInt<1>
    input io_interrupts_debug : UInt<1>
    input io_interrupts_mtip : UInt<1>
    input io_interrupts_msip : UInt<1>
    input io_interrupts_meip : UInt<1>
    output io_imem_might_request : UInt<1>
    output io_imem_req_valid : UInt<1>
    output io_imem_req_bits_pc : UInt<32>
    output io_imem_req_bits_speculative : UInt<1>
    output io_imem_resp_ready : UInt<1>
    input io_imem_resp_valid : UInt<1>
    input io_imem_resp_bits_pc : UInt<32>
    input io_imem_resp_bits_data : UInt<32>
    input io_imem_resp_bits_xcpt_ae_inst : UInt<1>
    input io_imem_resp_bits_replay : UInt<1>
    output io_imem_btb_update_valid : UInt<1>
    output io_imem_bht_update_valid : UInt<1>
    output io_imem_flush_icache : UInt<1>
    input io_dmem_req_ready : UInt<1>
    output io_dmem_req_valid : UInt<1>
    output io_dmem_req_bits_addr : UInt<32>
    output io_dmem_req_bits_tag : UInt<7>
    output io_dmem_req_bits_cmd : UInt<5>
    output io_dmem_req_bits_size : UInt<2>
    output io_dmem_req_bits_signed : UInt<1>
    output io_dmem_s1_kill : UInt<1>
    output io_dmem_s1_data_data : UInt<32>
    input io_dmem_s2_nack : UInt<1>
    input io_dmem_resp_valid : UInt<1>
    input io_dmem_resp_bits_tag : UInt<7>
    input io_dmem_resp_bits_data : UInt<32>
    input io_dmem_resp_bits_replay : UInt<1>
    input io_dmem_resp_bits_has_data : UInt<1>
    input io_dmem_resp_bits_data_word_bypass : UInt<32>
    input io_dmem_replay_next : UInt<1>
    input io_dmem_s2_xcpt_ma_ld : UInt<1>
    input io_dmem_s2_xcpt_ma_st : UInt<1>
    input io_dmem_s2_xcpt_pf_ld : UInt<1>
    input io_dmem_s2_xcpt_pf_st : UInt<1>
    input io_dmem_s2_xcpt_ae_ld : UInt<1>
    input io_dmem_s2_xcpt_ae_st : UInt<1>
    input io_dmem_ordered : UInt<1>
    input io_dmem_perf_grant : UInt<1>
    output io_ptw_status_debug : UInt<1>
    output io_ptw_pmp_0_cfg_l : UInt<1>
    output io_ptw_pmp_0_cfg_a : UInt<2>
    output io_ptw_pmp_0_cfg_x : UInt<1>
    output io_ptw_pmp_0_cfg_w : UInt<1>
    output io_ptw_pmp_0_cfg_r : UInt<1>
    output io_ptw_pmp_0_addr : UInt<30>
    output io_ptw_pmp_0_mask : UInt<32>
    output io_ptw_pmp_1_cfg_l : UInt<1>
    output io_ptw_pmp_1_cfg_a : UInt<2>
    output io_ptw_pmp_1_cfg_x : UInt<1>
    output io_ptw_pmp_1_cfg_w : UInt<1>
    output io_ptw_pmp_1_cfg_r : UInt<1>
    output io_ptw_pmp_1_addr : UInt<30>
    output io_ptw_pmp_1_mask : UInt<32>
    output io_ptw_pmp_2_cfg_l : UInt<1>
    output io_ptw_pmp_2_cfg_a : UInt<2>
    output io_ptw_pmp_2_cfg_x : UInt<1>
    output io_ptw_pmp_2_cfg_w : UInt<1>
    output io_ptw_pmp_2_cfg_r : UInt<1>
    output io_ptw_pmp_2_addr : UInt<30>
    output io_ptw_pmp_2_mask : UInt<32>
    output io_ptw_pmp_3_cfg_l : UInt<1>
    output io_ptw_pmp_3_cfg_a : UInt<2>
    output io_ptw_pmp_3_cfg_x : UInt<1>
    output io_ptw_pmp_3_cfg_w : UInt<1>
    output io_ptw_pmp_3_cfg_r : UInt<1>
    output io_ptw_pmp_3_addr : UInt<30>
    output io_ptw_pmp_3_mask : UInt<32>
    output io_ptw_pmp_4_cfg_l : UInt<1>
    output io_ptw_pmp_4_cfg_a : UInt<2>
    output io_ptw_pmp_4_cfg_x : UInt<1>
    output io_ptw_pmp_4_cfg_w : UInt<1>
    output io_ptw_pmp_4_cfg_r : UInt<1>
    output io_ptw_pmp_4_addr : UInt<30>
    output io_ptw_pmp_4_mask : UInt<32>
    output io_ptw_pmp_5_cfg_l : UInt<1>
    output io_ptw_pmp_5_cfg_a : UInt<2>
    output io_ptw_pmp_5_cfg_x : UInt<1>
    output io_ptw_pmp_5_cfg_w : UInt<1>
    output io_ptw_pmp_5_cfg_r : UInt<1>
    output io_ptw_pmp_5_addr : UInt<30>
    output io_ptw_pmp_5_mask : UInt<32>
    output io_ptw_pmp_6_cfg_l : UInt<1>
    output io_ptw_pmp_6_cfg_a : UInt<2>
    output io_ptw_pmp_6_cfg_x : UInt<1>
    output io_ptw_pmp_6_cfg_w : UInt<1>
    output io_ptw_pmp_6_cfg_r : UInt<1>
    output io_ptw_pmp_6_addr : UInt<30>
    output io_ptw_pmp_6_mask : UInt<32>
    output io_ptw_pmp_7_cfg_l : UInt<1>
    output io_ptw_pmp_7_cfg_a : UInt<2>
    output io_ptw_pmp_7_cfg_x : UInt<1>
    output io_ptw_pmp_7_cfg_w : UInt<1>
    output io_ptw_pmp_7_cfg_r : UInt<1>
    output io_ptw_pmp_7_addr : UInt<30>
    output io_ptw_pmp_7_mask : UInt<32>
    output io_ptw_customCSRs_csrs_0_value : UInt<32>
    output io_wfi : UInt<1>

    inst ibuf of IBuf @[RocketCore.scala 254:20]
    mem rf : @[RocketCore.scala 1015:15]
      data-type => UInt<32>
      depth => 31
      read-latency => 0
      write-latency => 1
      reader => id_rs_MPORT
      reader => id_rs_MPORT_1
      writer => MPORT
      read-under-write => undefined
    inst csr of CSRFile @[RocketCore.scala 282:19]
    inst bpu of BreakpointUnit @[RocketCore.scala 323:19]
    inst alu of ALU @[RocketCore.scala 385:19]
    inst div of MulDiv @[RocketCore.scala 409:19]
    reg id_reg_pause : UInt<1>, clock with :
      reset => (UInt<1>("h0"), id_reg_pause) @[RocketCore.scala 114:25]
    reg imem_might_request_reg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), imem_might_request_reg) @[RocketCore.scala 115:35]
    reg ex_ctrl_branch : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_branch) @[RocketCore.scala 190:20]
    reg ex_ctrl_jal : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_jal) @[RocketCore.scala 190:20]
    reg ex_ctrl_jalr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_jalr) @[RocketCore.scala 190:20]
    reg ex_ctrl_rxs2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_rxs2) @[RocketCore.scala 190:20]
    reg ex_ctrl_rxs1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_rxs1) @[RocketCore.scala 190:20]
    reg ex_ctrl_sel_alu2 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_sel_alu2) @[RocketCore.scala 190:20]
    reg ex_ctrl_sel_alu1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_sel_alu1) @[RocketCore.scala 190:20]
    reg ex_ctrl_sel_imm : UInt<3>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_sel_imm) @[RocketCore.scala 190:20]
    reg ex_ctrl_alu_fn : UInt<4>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_alu_fn) @[RocketCore.scala 190:20]
    reg ex_ctrl_mem : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_mem) @[RocketCore.scala 190:20]
    reg ex_ctrl_mem_cmd : UInt<5>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_mem_cmd) @[RocketCore.scala 190:20]
    reg ex_ctrl_div : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_div) @[RocketCore.scala 190:20]
    reg ex_ctrl_wxd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_wxd) @[RocketCore.scala 190:20]
    reg ex_ctrl_csr : UInt<3>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_csr) @[RocketCore.scala 190:20]
    reg ex_ctrl_fence_i : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_ctrl_fence_i) @[RocketCore.scala 190:20]
    reg mem_ctrl_branch : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_branch) @[RocketCore.scala 191:21]
    reg mem_ctrl_jal : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_jal) @[RocketCore.scala 191:21]
    reg mem_ctrl_jalr : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_jalr) @[RocketCore.scala 191:21]
    reg mem_ctrl_rxs2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_rxs2) @[RocketCore.scala 191:21]
    reg mem_ctrl_rxs1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_rxs1) @[RocketCore.scala 191:21]
    reg mem_ctrl_mem : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_mem) @[RocketCore.scala 191:21]
    reg mem_ctrl_div : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_div) @[RocketCore.scala 191:21]
    reg mem_ctrl_wxd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_wxd) @[RocketCore.scala 191:21]
    reg mem_ctrl_csr : UInt<3>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_csr) @[RocketCore.scala 191:21]
    reg mem_ctrl_fence_i : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_ctrl_fence_i) @[RocketCore.scala 191:21]
    reg wb_ctrl_rxs2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_rxs2) @[RocketCore.scala 192:20]
    reg wb_ctrl_rxs1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_rxs1) @[RocketCore.scala 192:20]
    reg wb_ctrl_mem : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_mem) @[RocketCore.scala 192:20]
    reg wb_ctrl_div : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_div) @[RocketCore.scala 192:20]
    reg wb_ctrl_wxd : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_wxd) @[RocketCore.scala 192:20]
    reg wb_ctrl_csr : UInt<3>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_csr) @[RocketCore.scala 192:20]
    reg wb_ctrl_fence_i : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_ctrl_fence_i) @[RocketCore.scala 192:20]
    reg ex_reg_xcpt_interrupt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_xcpt_interrupt) @[RocketCore.scala 194:35]
    reg ex_reg_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_valid) @[RocketCore.scala 195:35]
    reg ex_reg_rvc : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rvc) @[RocketCore.scala 196:35]
    reg ex_reg_xcpt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_xcpt) @[RocketCore.scala 198:35]
    reg ex_reg_flush_pipe : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_flush_pipe) @[RocketCore.scala 199:35]
    reg ex_reg_load_use : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_load_use) @[RocketCore.scala 200:35]
    reg ex_reg_cause : UInt<32>, clock with :
      reset => (UInt<1>("h0"), ex_reg_cause) @[RocketCore.scala 201:35]
    reg ex_reg_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_replay) @[RocketCore.scala 202:26]
    reg ex_reg_pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), ex_reg_pc) @[RocketCore.scala 203:22]
    reg ex_reg_mem_size : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_reg_mem_size) @[RocketCore.scala 204:28]
    reg ex_reg_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), ex_reg_inst) @[RocketCore.scala 205:24]
    reg ex_reg_raw_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), ex_reg_raw_inst) @[RocketCore.scala 206:28]
    reg mem_reg_xcpt_interrupt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_xcpt_interrupt) @[RocketCore.scala 211:36]
    reg mem_reg_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_valid) @[RocketCore.scala 212:36]
    reg mem_reg_rvc : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_rvc) @[RocketCore.scala 213:36]
    reg mem_reg_xcpt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_xcpt) @[RocketCore.scala 215:36]
    reg mem_reg_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_replay) @[RocketCore.scala 216:36]
    reg mem_reg_flush_pipe : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_flush_pipe) @[RocketCore.scala 217:36]
    reg mem_reg_cause : UInt<32>, clock with :
      reset => (UInt<1>("h0"), mem_reg_cause) @[RocketCore.scala 218:36]
    reg mem_reg_slow_bypass : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_slow_bypass) @[RocketCore.scala 219:36]
    reg mem_reg_load : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_load) @[RocketCore.scala 220:36]
    reg mem_reg_store : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_reg_store) @[RocketCore.scala 221:36]
    reg mem_reg_pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), mem_reg_pc) @[RocketCore.scala 223:23]
    reg mem_reg_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), mem_reg_inst) @[RocketCore.scala 224:25]
    reg mem_reg_raw_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), mem_reg_raw_inst) @[RocketCore.scala 226:29]
    reg mem_reg_wdata : UInt<32>, clock with :
      reset => (UInt<1>("h0"), mem_reg_wdata) @[RocketCore.scala 229:26]
    reg mem_reg_rs2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), mem_reg_rs2) @[RocketCore.scala 230:24]
    reg mem_br_taken : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mem_br_taken) @[RocketCore.scala 231:25]
    reg wb_reg_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_reg_valid) @[RocketCore.scala 235:35]
    reg wb_reg_xcpt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_reg_xcpt) @[RocketCore.scala 236:35]
    reg wb_reg_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_reg_replay) @[RocketCore.scala 237:35]
    reg wb_reg_flush_pipe : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wb_reg_flush_pipe) @[RocketCore.scala 238:35]
    reg wb_reg_cause : UInt<32>, clock with :
      reset => (UInt<1>("h0"), wb_reg_cause) @[RocketCore.scala 239:35]
    reg wb_reg_pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), wb_reg_pc) @[RocketCore.scala 241:22]
    reg wb_reg_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), wb_reg_inst) @[RocketCore.scala 243:24]
    reg wb_reg_raw_inst : UInt<32>, clock with :
      reset => (UInt<1>("h0"), wb_reg_raw_inst) @[RocketCore.scala 244:28]
    reg wb_reg_wdata : UInt<32>, clock with :
      reset => (UInt<1>("h0"), wb_reg_wdata) @[RocketCore.scala 245:25]
    node replay_wb_common = or(io_dmem_s2_nack, wb_reg_replay) @[RocketCore.scala 637:42]
    node _T_77 = and(wb_reg_valid, wb_ctrl_mem) @[RocketCore.scala 615:19]
    node _T_78 = and(_T_77, io_dmem_s2_xcpt_ma_st) @[RocketCore.scala 615:34]
    node _T_89 = or(wb_reg_xcpt, _T_78) @[RocketCore.scala 975:26]
    node _T_80 = and(_T_77, io_dmem_s2_xcpt_ma_ld) @[RocketCore.scala 616:34]
    node _T_90 = or(_T_89, _T_80) @[RocketCore.scala 975:26]
    node _T_82 = and(_T_77, io_dmem_s2_xcpt_pf_st) @[RocketCore.scala 617:34]
    node _T_91 = or(_T_90, _T_82) @[RocketCore.scala 975:26]
    node _T_84 = and(_T_77, io_dmem_s2_xcpt_pf_ld) @[RocketCore.scala 618:34]
    node _T_92 = or(_T_91, _T_84) @[RocketCore.scala 975:26]
    node _T_86 = and(_T_77, io_dmem_s2_xcpt_ae_st) @[RocketCore.scala 619:34]
    node _T_93 = or(_T_92, _T_86) @[RocketCore.scala 975:26]
    node _T_88 = and(_T_77, io_dmem_s2_xcpt_ae_ld) @[RocketCore.scala 620:34]
    node wb_xcpt = or(_T_93, _T_88) @[RocketCore.scala 975:26]
    node _take_pc_wb_T = or(replay_wb_common, wb_xcpt) @[RocketCore.scala 640:27]
    node _take_pc_wb_T_1 = or(_take_pc_wb_T, csr.io_eret) @[RocketCore.scala 640:38]
    node take_pc_wb = or(_take_pc_wb_T_1, wb_reg_flush_pipe) @[RocketCore.scala 640:53]
    node _mem_cfi_taken_T = and(mem_ctrl_branch, mem_br_taken) @[RocketCore.scala 520:40]
    node _mem_cfi_taken_T_1 = or(_mem_cfi_taken_T, mem_ctrl_jalr) @[RocketCore.scala 520:57]
    node mem_cfi_taken = or(_mem_cfi_taken_T_1, mem_ctrl_jal) @[RocketCore.scala 520:74]
    node take_pc_mem = and(mem_reg_valid, mem_cfi_taken) @[RocketCore.scala 523:32]
    node take_pc_mem_wb = or(take_pc_wb, take_pc_mem) @[RocketCore.scala 250:35]
    node _id_ctrl_decoder_bit_T = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hfe00707f")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_bit_T_1 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2000033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_3 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2001033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_5 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2003033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_7 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2002033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_9 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2004033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_11 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2005033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_13 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2006033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_15 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2007033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_16 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hf800707f")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_bit_T_17 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("h202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_19 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("h2000202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_21 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("h800202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_23 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("h6000202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_25 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("h4000202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_27 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("h8000202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_29 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("hc000202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_31 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("ha000202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_33 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("he000202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_34 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hf9f0707f")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_bit_T_35 = eq(_id_ctrl_decoder_bit_T_34, UInt<32>("h1000202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_37 = eq(_id_ctrl_decoder_bit_T_16, UInt<32>("h1800202f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_39 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h1013")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_41 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h5013")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_43 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h40005013")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_44 = eq(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7b200073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_45 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h707f")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_bit_T_46 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h100f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_48 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h1063")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_50 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h63")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_52 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h4063")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_54 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h6063")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_56 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h5063")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_58 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h7063")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_59 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7f")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_bit_T_60 = eq(_id_ctrl_decoder_bit_T_59, UInt<32>("h6f")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_62 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h67")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_64 = eq(_id_ctrl_decoder_bit_T_59, UInt<32>("h17")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_66 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h3")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_68 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h1003")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_70 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h2003")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_72 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h4003")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_74 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h5003")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_76 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h23")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_78 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h1023")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_80 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h2023")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_82 = eq(_id_ctrl_decoder_bit_T_59, UInt<32>("h37")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_84 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h13")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_86 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h2013")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_88 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h3013")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_90 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h7013")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_92 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h6013")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_94 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h4013")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_96 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h33")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_98 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h40000033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_100 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h2033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_102 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h3033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_104 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h7033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_106 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h6033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_108 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h4033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_110 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h1033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_112 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h5033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_114 = eq(_id_ctrl_decoder_bit_T, UInt<32>("h40005033")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_116 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("hf")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_117 = eq(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h73")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_118 = eq(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h100073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_119 = eq(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h30200073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_120 = eq(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h10500073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_121 = eq(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h30500073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_123 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h1073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_125 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h2073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_127 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h3073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_129 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h5073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_131 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h6073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_133 = eq(_id_ctrl_decoder_bit_T_45, UInt<32>("h7073")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_bit_T_135 = or(_id_ctrl_decoder_bit_T_1, _id_ctrl_decoder_bit_T_3) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_136 = or(_id_ctrl_decoder_bit_T_135, _id_ctrl_decoder_bit_T_5) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_137 = or(_id_ctrl_decoder_bit_T_136, _id_ctrl_decoder_bit_T_7) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_138 = or(_id_ctrl_decoder_bit_T_137, _id_ctrl_decoder_bit_T_9) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_139 = or(_id_ctrl_decoder_bit_T_138, _id_ctrl_decoder_bit_T_11) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_140 = or(_id_ctrl_decoder_bit_T_139, _id_ctrl_decoder_bit_T_13) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_141 = or(_id_ctrl_decoder_bit_T_140, _id_ctrl_decoder_bit_T_15) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_142 = or(_id_ctrl_decoder_bit_T_141, _id_ctrl_decoder_bit_T_17) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_143 = or(_id_ctrl_decoder_bit_T_142, _id_ctrl_decoder_bit_T_19) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_144 = or(_id_ctrl_decoder_bit_T_143, _id_ctrl_decoder_bit_T_21) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_145 = or(_id_ctrl_decoder_bit_T_144, _id_ctrl_decoder_bit_T_23) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_146 = or(_id_ctrl_decoder_bit_T_145, _id_ctrl_decoder_bit_T_25) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_147 = or(_id_ctrl_decoder_bit_T_146, _id_ctrl_decoder_bit_T_27) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_148 = or(_id_ctrl_decoder_bit_T_147, _id_ctrl_decoder_bit_T_29) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_149 = or(_id_ctrl_decoder_bit_T_148, _id_ctrl_decoder_bit_T_31) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_150 = or(_id_ctrl_decoder_bit_T_149, _id_ctrl_decoder_bit_T_33) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_151 = or(_id_ctrl_decoder_bit_T_150, _id_ctrl_decoder_bit_T_35) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_152 = or(_id_ctrl_decoder_bit_T_151, _id_ctrl_decoder_bit_T_37) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_153 = or(_id_ctrl_decoder_bit_T_152, _id_ctrl_decoder_bit_T_39) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_154 = or(_id_ctrl_decoder_bit_T_153, _id_ctrl_decoder_bit_T_41) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_155 = or(_id_ctrl_decoder_bit_T_154, _id_ctrl_decoder_bit_T_43) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_156 = or(_id_ctrl_decoder_bit_T_155, _id_ctrl_decoder_bit_T_44) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_157 = or(_id_ctrl_decoder_bit_T_156, _id_ctrl_decoder_bit_T_46) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_158 = or(_id_ctrl_decoder_bit_T_157, _id_ctrl_decoder_bit_T_48) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_159 = or(_id_ctrl_decoder_bit_T_158, _id_ctrl_decoder_bit_T_50) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_160 = or(_id_ctrl_decoder_bit_T_159, _id_ctrl_decoder_bit_T_52) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_161 = or(_id_ctrl_decoder_bit_T_160, _id_ctrl_decoder_bit_T_54) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_162 = or(_id_ctrl_decoder_bit_T_161, _id_ctrl_decoder_bit_T_56) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_163 = or(_id_ctrl_decoder_bit_T_162, _id_ctrl_decoder_bit_T_58) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_164 = or(_id_ctrl_decoder_bit_T_163, _id_ctrl_decoder_bit_T_60) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_165 = or(_id_ctrl_decoder_bit_T_164, _id_ctrl_decoder_bit_T_62) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_166 = or(_id_ctrl_decoder_bit_T_165, _id_ctrl_decoder_bit_T_64) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_167 = or(_id_ctrl_decoder_bit_T_166, _id_ctrl_decoder_bit_T_66) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_168 = or(_id_ctrl_decoder_bit_T_167, _id_ctrl_decoder_bit_T_68) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_169 = or(_id_ctrl_decoder_bit_T_168, _id_ctrl_decoder_bit_T_70) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_170 = or(_id_ctrl_decoder_bit_T_169, _id_ctrl_decoder_bit_T_72) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_171 = or(_id_ctrl_decoder_bit_T_170, _id_ctrl_decoder_bit_T_74) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_172 = or(_id_ctrl_decoder_bit_T_171, _id_ctrl_decoder_bit_T_76) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_173 = or(_id_ctrl_decoder_bit_T_172, _id_ctrl_decoder_bit_T_78) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_174 = or(_id_ctrl_decoder_bit_T_173, _id_ctrl_decoder_bit_T_80) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_175 = or(_id_ctrl_decoder_bit_T_174, _id_ctrl_decoder_bit_T_82) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_176 = or(_id_ctrl_decoder_bit_T_175, _id_ctrl_decoder_bit_T_84) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_177 = or(_id_ctrl_decoder_bit_T_176, _id_ctrl_decoder_bit_T_86) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_178 = or(_id_ctrl_decoder_bit_T_177, _id_ctrl_decoder_bit_T_88) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_179 = or(_id_ctrl_decoder_bit_T_178, _id_ctrl_decoder_bit_T_90) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_180 = or(_id_ctrl_decoder_bit_T_179, _id_ctrl_decoder_bit_T_92) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_181 = or(_id_ctrl_decoder_bit_T_180, _id_ctrl_decoder_bit_T_94) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_182 = or(_id_ctrl_decoder_bit_T_181, _id_ctrl_decoder_bit_T_96) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_183 = or(_id_ctrl_decoder_bit_T_182, _id_ctrl_decoder_bit_T_98) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_184 = or(_id_ctrl_decoder_bit_T_183, _id_ctrl_decoder_bit_T_100) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_185 = or(_id_ctrl_decoder_bit_T_184, _id_ctrl_decoder_bit_T_102) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_186 = or(_id_ctrl_decoder_bit_T_185, _id_ctrl_decoder_bit_T_104) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_187 = or(_id_ctrl_decoder_bit_T_186, _id_ctrl_decoder_bit_T_106) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_188 = or(_id_ctrl_decoder_bit_T_187, _id_ctrl_decoder_bit_T_108) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_189 = or(_id_ctrl_decoder_bit_T_188, _id_ctrl_decoder_bit_T_110) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_190 = or(_id_ctrl_decoder_bit_T_189, _id_ctrl_decoder_bit_T_112) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_191 = or(_id_ctrl_decoder_bit_T_190, _id_ctrl_decoder_bit_T_114) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_192 = or(_id_ctrl_decoder_bit_T_191, _id_ctrl_decoder_bit_T_116) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_193 = or(_id_ctrl_decoder_bit_T_192, _id_ctrl_decoder_bit_T_117) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_194 = or(_id_ctrl_decoder_bit_T_193, _id_ctrl_decoder_bit_T_118) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_195 = or(_id_ctrl_decoder_bit_T_194, _id_ctrl_decoder_bit_T_119) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_196 = or(_id_ctrl_decoder_bit_T_195, _id_ctrl_decoder_bit_T_120) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_197 = or(_id_ctrl_decoder_bit_T_196, _id_ctrl_decoder_bit_T_121) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_198 = or(_id_ctrl_decoder_bit_T_197, _id_ctrl_decoder_bit_T_123) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_199 = or(_id_ctrl_decoder_bit_T_198, _id_ctrl_decoder_bit_T_125) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_200 = or(_id_ctrl_decoder_bit_T_199, _id_ctrl_decoder_bit_T_127) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_201 = or(_id_ctrl_decoder_bit_T_200, _id_ctrl_decoder_bit_T_129) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_202 = or(_id_ctrl_decoder_bit_T_201, _id_ctrl_decoder_bit_T_131) @[Decode.scala 15:30]
    node id_ctrl_decoder_0 = or(_id_ctrl_decoder_bit_T_202, _id_ctrl_decoder_bit_T_133) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h54")) @[Decode.scala 14:65]
    node id_ctrl_decoder_3 = eq(_id_ctrl_decoder_T, UInt<32>("h40")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_2 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h48")) @[Decode.scala 14:65]
    node id_ctrl_decoder_4 = eq(_id_ctrl_decoder_T_2, UInt<32>("h48")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_4 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1c")) @[Decode.scala 14:65]
    node id_ctrl_decoder_5 = eq(_id_ctrl_decoder_T_4, UInt<32>("h4")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_6 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h70")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_7 = eq(_id_ctrl_decoder_T_6, UInt<32>("h20")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_8 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h64")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_9 = eq(_id_ctrl_decoder_T_8, UInt<32>("h20")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_10 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h34")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_11 = eq(_id_ctrl_decoder_T_10, UInt<32>("h20")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_13 = or(_id_ctrl_decoder_T_7, _id_ctrl_decoder_T_9) @[Decode.scala 15:30]
    node id_ctrl_decoder_6 = or(_id_ctrl_decoder_T_13, _id_ctrl_decoder_T_11) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_14 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4004")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_15 = eq(_id_ctrl_decoder_T_14, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_16 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h44")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_17 = eq(_id_ctrl_decoder_T_16, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_18 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h18")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_19 = eq(_id_ctrl_decoder_T_18, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_20 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2050")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_21 = eq(_id_ctrl_decoder_T_20, UInt<32>("h2000")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_23 = or(_id_ctrl_decoder_T_15, _id_ctrl_decoder_T_17) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_24 = or(_id_ctrl_decoder_T_23, _id_ctrl_decoder_T_19) @[Decode.scala 15:30]
    node id_ctrl_decoder_7 = or(_id_ctrl_decoder_T_24, _id_ctrl_decoder_T_21) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_25 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h58")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_26 = eq(_id_ctrl_decoder_T_25, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_27 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h20")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_28 = eq(_id_ctrl_decoder_T_27, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_29 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("hc")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_30 = eq(_id_ctrl_decoder_T_29, UInt<32>("h4")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_31 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4050")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_32 = eq(_id_ctrl_decoder_T_31, UInt<32>("h4050")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_34 = or(_id_ctrl_decoder_T_26, _id_ctrl_decoder_T_28) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_35 = or(_id_ctrl_decoder_T_34, _id_ctrl_decoder_T_30) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_36 = or(_id_ctrl_decoder_T_35, id_ctrl_decoder_4) @[Decode.scala 15:30]
    node id_ctrl_decoder_lo = or(_id_ctrl_decoder_T_36, _id_ctrl_decoder_T_32) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_38 = eq(_id_ctrl_decoder_T_2, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_39 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4008")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_40 = eq(_id_ctrl_decoder_T_39, UInt<32>("h4000")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_42 = or(_id_ctrl_decoder_T_38, _id_ctrl_decoder_T_19) @[Decode.scala 15:30]
    node id_ctrl_decoder_hi = or(_id_ctrl_decoder_T_42, _id_ctrl_decoder_T_40) @[Decode.scala 15:30]
    node id_ctrl_decoder_9 = cat(id_ctrl_decoder_hi, id_ctrl_decoder_lo) @[Cat.scala 30:58]
    node _id_ctrl_decoder_T_43 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h50")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_44 = eq(_id_ctrl_decoder_T_43, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_46 = or(_id_ctrl_decoder_T_15, _id_ctrl_decoder_T_44) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_47 = or(_id_ctrl_decoder_T_46, _id_ctrl_decoder_T_17) @[Decode.scala 15:30]
    node id_ctrl_decoder_lo_1 = or(_id_ctrl_decoder_T_47, _id_ctrl_decoder_T_19) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_48 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h24")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_49 = eq(_id_ctrl_decoder_T_48, UInt<32>("h4")) @[Decode.scala 14:121]
    node id_ctrl_decoder_hi_1 = or(_id_ctrl_decoder_T_49, id_ctrl_decoder_4) @[Decode.scala 15:30]
    node id_ctrl_decoder_10 = cat(id_ctrl_decoder_hi_1, id_ctrl_decoder_lo_1) @[Cat.scala 30:58]
    node _id_ctrl_decoder_T_51 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h8")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_52 = eq(_id_ctrl_decoder_T_51, UInt<32>("h8")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_54 = eq(_id_ctrl_decoder_T_16, UInt<32>("h40")) @[Decode.scala 14:121]
    node id_ctrl_decoder_lo_2 = or(_id_ctrl_decoder_T_52, _id_ctrl_decoder_T_54) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_57 = eq(_id_ctrl_decoder_T_16, UInt<32>("h4")) @[Decode.scala 14:121]
    node id_ctrl_decoder_hi_lo = or(_id_ctrl_decoder_T_57, _id_ctrl_decoder_T_52) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_59 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h14")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_60 = eq(_id_ctrl_decoder_T_59, UInt<32>("h10")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_61 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h30")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_62 = eq(_id_ctrl_decoder_T_61, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_64 = or(id_ctrl_decoder_5, _id_ctrl_decoder_T_60) @[Decode.scala 15:30]
    node id_ctrl_decoder_hi_hi = or(_id_ctrl_decoder_T_64, _id_ctrl_decoder_T_62) @[Decode.scala 15:30]
    node id_ctrl_decoder_hi_2 = cat(id_ctrl_decoder_hi_hi, id_ctrl_decoder_hi_lo) @[Cat.scala 30:58]
    node id_ctrl_decoder_11 = cat(id_ctrl_decoder_hi_2, id_ctrl_decoder_lo_2) @[Cat.scala 30:58]
    node _id_ctrl_decoder_T_67 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h3054")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_68 = eq(_id_ctrl_decoder_T_67, UInt<32>("h1010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_69 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1058")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_70 = eq(_id_ctrl_decoder_T_69, UInt<32>("h1040")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_71 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h7044")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_72 = eq(_id_ctrl_decoder_T_71, UInt<32>("h7000")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_73 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2001074")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_74 = eq(_id_ctrl_decoder_T_73, UInt<32>("h2001030")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_76 = or(_id_ctrl_decoder_T_68, _id_ctrl_decoder_T_70) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_77 = or(_id_ctrl_decoder_T_76, _id_ctrl_decoder_T_72) @[Decode.scala 15:30]
    node id_ctrl_decoder_lo_lo = or(_id_ctrl_decoder_T_77, _id_ctrl_decoder_T_74) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_78 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4054")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_79 = eq(_id_ctrl_decoder_T_78, UInt<32>("h40")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_80 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h3044")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_81 = eq(_id_ctrl_decoder_T_80, UInt<32>("h3000")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_82 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h6044")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_83 = eq(_id_ctrl_decoder_T_82, UInt<32>("h6000")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_84 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h6018")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_85 = eq(_id_ctrl_decoder_T_84, UInt<32>("h6000")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_86 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2002074")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_87 = eq(_id_ctrl_decoder_T_86, UInt<32>("h2002030")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_88 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40003034")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_89 = eq(_id_ctrl_decoder_T_88, UInt<32>("h40000030")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_90 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40001054")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_91 = eq(_id_ctrl_decoder_T_90, UInt<32>("h40001010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_93 = or(_id_ctrl_decoder_T_79, _id_ctrl_decoder_T_81) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_94 = or(_id_ctrl_decoder_T_93, _id_ctrl_decoder_T_83) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_95 = or(_id_ctrl_decoder_T_94, _id_ctrl_decoder_T_85) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_96 = or(_id_ctrl_decoder_T_95, _id_ctrl_decoder_T_87) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_97 = or(_id_ctrl_decoder_T_96, _id_ctrl_decoder_T_89) @[Decode.scala 15:30]
    node id_ctrl_decoder_lo_hi = or(_id_ctrl_decoder_T_97, _id_ctrl_decoder_T_91) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_98 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2002054")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_99 = eq(_id_ctrl_decoder_T_98, UInt<32>("h2010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_100 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2034")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_101 = eq(_id_ctrl_decoder_T_100, UInt<32>("h2010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_102 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40004054")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_103 = eq(_id_ctrl_decoder_T_102, UInt<32>("h4010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_104 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h5054")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_105 = eq(_id_ctrl_decoder_T_104, UInt<32>("h4010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_106 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h4058")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_107 = eq(_id_ctrl_decoder_T_106, UInt<32>("h4040")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_109 = or(_id_ctrl_decoder_T_99, _id_ctrl_decoder_T_101) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_110 = or(_id_ctrl_decoder_T_109, _id_ctrl_decoder_T_103) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_111 = or(_id_ctrl_decoder_T_110, _id_ctrl_decoder_T_105) @[Decode.scala 15:30]
    node id_ctrl_decoder_hi_lo_1 = or(_id_ctrl_decoder_T_111, _id_ctrl_decoder_T_107) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_112 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2006054")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_113 = eq(_id_ctrl_decoder_T_112, UInt<32>("h2010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_114 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h6034")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_115 = eq(_id_ctrl_decoder_T_114, UInt<32>("h2010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_116 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40003054")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_117 = eq(_id_ctrl_decoder_T_116, UInt<32>("h40001010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_119 = or(_id_ctrl_decoder_T_113, _id_ctrl_decoder_T_115) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_120 = or(_id_ctrl_decoder_T_119, _id_ctrl_decoder_T_107) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_121 = or(_id_ctrl_decoder_T_120, _id_ctrl_decoder_T_89) @[Decode.scala 15:30]
    node id_ctrl_decoder_hi_hi_1 = or(_id_ctrl_decoder_T_121, _id_ctrl_decoder_T_117) @[Decode.scala 15:30]
    node id_ctrl_decoder_lo_3 = cat(id_ctrl_decoder_lo_hi, id_ctrl_decoder_lo_lo) @[Cat.scala 30:58]
    node id_ctrl_decoder_hi_3 = cat(id_ctrl_decoder_hi_hi_1, id_ctrl_decoder_hi_lo_1) @[Cat.scala 30:58]
    node id_ctrl_decoder_13 = cat(id_ctrl_decoder_hi_3, id_ctrl_decoder_lo_3) @[Cat.scala 30:58]
    node _id_ctrl_decoder_bit_T_204 = or(_id_ctrl_decoder_bit_T_17, _id_ctrl_decoder_bit_T_19) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_205 = or(_id_ctrl_decoder_bit_T_204, _id_ctrl_decoder_bit_T_21) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_206 = or(_id_ctrl_decoder_bit_T_205, _id_ctrl_decoder_bit_T_23) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_207 = or(_id_ctrl_decoder_bit_T_206, _id_ctrl_decoder_bit_T_25) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_208 = or(_id_ctrl_decoder_bit_T_207, _id_ctrl_decoder_bit_T_27) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_209 = or(_id_ctrl_decoder_bit_T_208, _id_ctrl_decoder_bit_T_29) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_210 = or(_id_ctrl_decoder_bit_T_209, _id_ctrl_decoder_bit_T_31) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_211 = or(_id_ctrl_decoder_bit_T_210, _id_ctrl_decoder_bit_T_33) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_212 = or(_id_ctrl_decoder_bit_T_211, _id_ctrl_decoder_bit_T_35) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_213 = or(_id_ctrl_decoder_bit_T_212, _id_ctrl_decoder_bit_T_37) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_214 = or(_id_ctrl_decoder_bit_T_213, _id_ctrl_decoder_bit_T_66) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_215 = or(_id_ctrl_decoder_bit_T_214, _id_ctrl_decoder_bit_T_68) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_216 = or(_id_ctrl_decoder_bit_T_215, _id_ctrl_decoder_bit_T_70) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_217 = or(_id_ctrl_decoder_bit_T_216, _id_ctrl_decoder_bit_T_72) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_218 = or(_id_ctrl_decoder_bit_T_217, _id_ctrl_decoder_bit_T_74) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_219 = or(_id_ctrl_decoder_bit_T_218, _id_ctrl_decoder_bit_T_76) @[Decode.scala 15:30]
    node _id_ctrl_decoder_bit_T_220 = or(_id_ctrl_decoder_bit_T_219, _id_ctrl_decoder_bit_T_78) @[Decode.scala 15:30]
    node id_ctrl_decoder_14 = or(_id_ctrl_decoder_bit_T_220, _id_ctrl_decoder_bit_T_80) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_122 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h28")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_123 = eq(_id_ctrl_decoder_T_122, UInt<32>("h20")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_124 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h18000020")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_125 = eq(_id_ctrl_decoder_T_124, UInt<32>("h18000020")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_126 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h20000020")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_127 = eq(_id_ctrl_decoder_T_126, UInt<32>("h20000020")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_129 = or(_id_ctrl_decoder_T_123, _id_ctrl_decoder_T_125) @[Decode.scala 15:30]
    node id_ctrl_decoder_lo_lo_1 = or(_id_ctrl_decoder_T_129, _id_ctrl_decoder_T_127) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_130 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h10000008")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_131 = eq(_id_ctrl_decoder_T_130, UInt<32>("h10000008")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_132 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h40000008")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_133 = eq(_id_ctrl_decoder_T_132, UInt<32>("h40000008")) @[Decode.scala 14:121]
    node id_ctrl_decoder_lo_hi_1 = or(_id_ctrl_decoder_T_131, _id_ctrl_decoder_T_133) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_135 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h8000008")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_136 = eq(_id_ctrl_decoder_T_135, UInt<32>("h8000008")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_137 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h80000008")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_138 = eq(_id_ctrl_decoder_T_137, UInt<32>("h80000008")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_140 = or(_id_ctrl_decoder_T_136, _id_ctrl_decoder_T_131) @[Decode.scala 15:30]
    node id_ctrl_decoder_hi_lo_2 = or(_id_ctrl_decoder_T_140, _id_ctrl_decoder_T_138) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_141 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h18000008")) @[Decode.scala 14:65]
    node id_ctrl_decoder_hi_hi_lo = eq(_id_ctrl_decoder_T_141, UInt<32>("h8")) @[Decode.scala 14:121]
    node id_ctrl_decoder_lo_4 = cat(id_ctrl_decoder_lo_hi_1, id_ctrl_decoder_lo_lo_1) @[Cat.scala 30:58]
    node id_ctrl_decoder_hi_hi_2 = cat(UInt<1>("h0"), id_ctrl_decoder_hi_hi_lo) @[Cat.scala 30:58]
    node id_ctrl_decoder_hi_4 = cat(id_ctrl_decoder_hi_hi_2, id_ctrl_decoder_hi_lo_2) @[Cat.scala 30:58]
    node id_ctrl_decoder_15 = cat(id_ctrl_decoder_hi_4, id_ctrl_decoder_lo_4) @[Cat.scala 30:58]
    node _id_ctrl_decoder_T_143 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2000074")) @[Decode.scala 14:65]
    node id_ctrl_decoder_21 = eq(_id_ctrl_decoder_T_143, UInt<32>("h2000030")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_146 = eq(_id_ctrl_decoder_T_122, UInt<32>("h0")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_148 = eq(_id_ctrl_decoder_T_43, UInt<32>("h10")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_149 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1010")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_150 = eq(_id_ctrl_decoder_T_149, UInt<32>("h1010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_151 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2008")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_152 = eq(_id_ctrl_decoder_T_151, UInt<32>("h2008")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_153 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2010")) @[Decode.scala 14:65]
    node _id_ctrl_decoder_T_154 = eq(_id_ctrl_decoder_T_153, UInt<32>("h2010")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_156 = or(_id_ctrl_decoder_T_146, _id_ctrl_decoder_T_30) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_157 = or(_id_ctrl_decoder_T_156, _id_ctrl_decoder_T_148) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_158 = or(_id_ctrl_decoder_T_157, id_ctrl_decoder_4) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_159 = or(_id_ctrl_decoder_T_158, _id_ctrl_decoder_T_150) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_160 = or(_id_ctrl_decoder_T_159, _id_ctrl_decoder_T_152) @[Decode.scala 15:30]
    node id_ctrl_decoder_22 = or(_id_ctrl_decoder_T_160, _id_ctrl_decoder_T_154) @[Decode.scala 15:30]
    node _id_ctrl_decoder_T_161 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1050")) @[Decode.scala 14:65]
    node id_ctrl_decoder_lo_5 = eq(_id_ctrl_decoder_T_161, UInt<32>("h1050")) @[Decode.scala 14:121]
    node id_ctrl_decoder_hi_lo_3 = eq(_id_ctrl_decoder_T_20, UInt<32>("h2050")) @[Decode.scala 14:121]
    node id_ctrl_decoder_hi_hi_3 = eq(_id_ctrl_decoder_T_43, UInt<32>("h50")) @[Decode.scala 14:121]
    node id_ctrl_decoder_hi_5 = cat(id_ctrl_decoder_hi_hi_3, id_ctrl_decoder_hi_lo_3) @[Cat.scala 30:58]
    node id_ctrl_decoder_23 = cat(id_ctrl_decoder_hi_5, id_ctrl_decoder_lo_5) @[Cat.scala 30:58]
    node _id_ctrl_decoder_T_167 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h1048")) @[Decode.scala 14:65]
    node id_ctrl_decoder_24 = eq(_id_ctrl_decoder_T_167, UInt<32>("h1008")) @[Decode.scala 14:121]
    node _id_ctrl_decoder_T_169 = and(ibuf.io_inst_0_bits_inst_bits, UInt<32>("h2048")) @[Decode.scala 14:65]
    node id_ctrl_decoder_25 = eq(_id_ctrl_decoder_T_169, UInt<32>("h8")) @[Decode.scala 14:121]
    node id_ctrl_decoder_26 = eq(_id_ctrl_decoder_T_169, UInt<32>("h2008")) @[Decode.scala 14:121]
    node id_raddr2 = ibuf.io_inst_0_bits_inst_rs2 @[RocketCore.scala 267:72]
    node id_raddr1 = ibuf.io_inst_0_bits_inst_rs1 @[RocketCore.scala 267:72]
    node id_waddr = ibuf.io_inst_0_bits_inst_rd @[RocketCore.scala 267:72]
    reg id_reg_fence : UInt<1>, clock with :
      reset => (UInt<1>("h0"), id_reg_fence) @[RocketCore.scala 274:25]
    node _id_rs_T = eq(id_raddr1, UInt<5>("h0")) @[RocketCore.scala 1022:45]
    node _id_rs_T_4 = rf.id_rs_MPORT.data @[RocketCore.scala 1022:25]
    node _id_rs_T_9 = rf.id_rs_MPORT_1.data @[RocketCore.scala 1022:25]
    node _id_csr_en_T = eq(id_ctrl_decoder_23, UInt<3>("h6")) @[package.scala 15:47]
    node _id_csr_en_T_1 = eq(id_ctrl_decoder_23, UInt<3>("h7")) @[package.scala 15:47]
    node _id_csr_en_T_2 = eq(id_ctrl_decoder_23, UInt<3>("h5")) @[package.scala 15:47]
    node _id_csr_en_T_3 = or(_id_csr_en_T, _id_csr_en_T_1) @[package.scala 72:59]
    node id_csr_en = or(_id_csr_en_T_3, _id_csr_en_T_2) @[package.scala 72:59]
    node id_system_insn = eq(id_ctrl_decoder_23, UInt<3>("h4")) @[RocketCore.scala 284:36]
    node id_csr_ren = and(_id_csr_en_T_3, _id_rs_T) @[RocketCore.scala 285:54]
    node id_csr = mux(id_csr_ren, UInt<3>("h2"), id_ctrl_decoder_23) @[RocketCore.scala 286:19]
    node _id_sfence_T = eq(id_ctrl_decoder_15, UInt<5>("h14")) @[RocketCore.scala 287:50]
    node id_sfence = and(id_ctrl_decoder_14, _id_sfence_T) @[RocketCore.scala 287:31]
    node _id_csr_flush_T = or(id_sfence, id_system_insn) @[RocketCore.scala 288:32]
    node _id_csr_flush_T_1 = not(id_csr_ren) @[RocketCore.scala 288:67]
    node _id_csr_flush_T_2 = and(id_csr_en, _id_csr_flush_T_1) @[RocketCore.scala 288:64]
    node _id_csr_flush_T_3 = and(_id_csr_flush_T_2, csr.io_decode_0_write_flush) @[RocketCore.scala 288:79]
    node id_csr_flush = or(_id_csr_flush_T, _id_csr_flush_T_3) @[RocketCore.scala 288:50]
    node _id_illegal_insn_T = not(id_ctrl_decoder_0) @[RocketCore.scala 296:25]
    node _id_illegal_insn_T_2 = bits(csr.io_status_isa, 12, 12) @[RocketCore.scala 297:55]
    node _id_illegal_insn_T_3 = not(_id_illegal_insn_T_2) @[RocketCore.scala 297:37]
    node _id_illegal_insn_T_4 = and(id_ctrl_decoder_21, _id_illegal_insn_T_3) @[RocketCore.scala 297:34]
    node _id_illegal_insn_T_5 = or(_id_illegal_insn_T, _id_illegal_insn_T_4) @[RocketCore.scala 296:40]
    node _id_illegal_insn_T_6 = bits(csr.io_status_isa, 0, 0) @[RocketCore.scala 298:38]
    node _id_illegal_insn_T_7 = not(_id_illegal_insn_T_6) @[RocketCore.scala 298:20]
    node _id_illegal_insn_T_8 = and(id_ctrl_decoder_26, _id_illegal_insn_T_7) @[RocketCore.scala 298:17]
    node _id_illegal_insn_T_9 = or(_id_illegal_insn_T_5, _id_illegal_insn_T_8) @[RocketCore.scala 297:65]
    node _id_illegal_insn_T_17 = bits(csr.io_status_isa, 2, 2) @[RocketCore.scala 301:51]
    node _id_illegal_insn_T_18 = not(_id_illegal_insn_T_17) @[RocketCore.scala 301:33]
    node _id_illegal_insn_T_19 = and(ibuf.io_inst_0_bits_rvc, _id_illegal_insn_T_18) @[RocketCore.scala 301:30]
    node _id_illegal_insn_T_20 = or(_id_illegal_insn_T_9, _id_illegal_insn_T_19) @[RocketCore.scala 300:47]
    node _id_illegal_insn_T_40 = and(_id_csr_flush_T_1, csr.io_decode_0_write_illegal) @[RocketCore.scala 307:64]
    node _id_illegal_insn_T_41 = or(csr.io_decode_0_read_illegal, _id_illegal_insn_T_40) @[RocketCore.scala 307:49]
    node _id_illegal_insn_T_42 = and(id_csr_en, _id_illegal_insn_T_41) @[RocketCore.scala 307:15]
    node _id_illegal_insn_T_43 = or(_id_illegal_insn_T_20, _id_illegal_insn_T_42) @[RocketCore.scala 306:81]
    node _id_illegal_insn_T_44 = not(ibuf.io_inst_0_bits_rvc) @[RocketCore.scala 308:5]
    node _id_illegal_insn_T_46 = and(_id_csr_flush_T, csr.io_decode_0_system_illegal) @[RocketCore.scala 308:65]
    node _id_illegal_insn_T_47 = and(_id_illegal_insn_T_44, _id_illegal_insn_T_46) @[RocketCore.scala 308:31]
    node id_illegal_insn = or(_id_illegal_insn_T_43, _id_illegal_insn_T_47) @[RocketCore.scala 307:99]
    node id_amo_aq = bits(ibuf.io_inst_0_bits_inst_bits, 26, 26) @[RocketCore.scala 310:29]
    node id_amo_rl = bits(ibuf.io_inst_0_bits_inst_bits, 25, 25) @[RocketCore.scala 311:29]
    node id_fence_succ = bits(ibuf.io_inst_0_bits_inst_bits, 23, 20) @[RocketCore.scala 313:33]
    node _id_fence_next_T = and(id_ctrl_decoder_26, id_amo_aq) @[RocketCore.scala 314:52]
    node id_fence_next = or(id_ctrl_decoder_25, _id_fence_next_T) @[RocketCore.scala 314:37]
    node _id_mem_busy_T = not(io_dmem_ordered) @[RocketCore.scala 315:21]
    node id_mem_busy = or(_id_mem_busy_T, io_dmem_req_valid) @[RocketCore.scala 315:38]
    node _T = not(id_mem_busy) @[RocketCore.scala 316:9]
    node _GEN_0 = mux(_T, UInt<1>("h0"), id_reg_fence) @[RocketCore.scala 316:23 274:25 316:38]
    node _id_do_fence_x8_T_1 = and(id_ctrl_decoder_26, id_amo_rl) @[RocketCore.scala 321:33]
    node _id_do_fence_x8_T_2 = or(_id_do_fence_x8_T_1, id_ctrl_decoder_24) @[RocketCore.scala 321:46]
    node _id_do_fence_x8_T_4 = and(id_reg_fence, id_ctrl_decoder_14) @[RocketCore.scala 321:81]
    node _id_do_fence_x8_T_5 = or(_id_do_fence_x8_T_2, _id_do_fence_x8_T_4) @[RocketCore.scala 321:65]
    node id_do_fence_x8 = and(id_mem_busy, _id_do_fence_x8_T_5) @[RocketCore.scala 321:17]
    node _T_1 = or(csr.io_interrupt, bpu.io_debug_if) @[RocketCore.scala 975:26]
    node _T_2 = or(_T_1, bpu.io_xcpt_if) @[RocketCore.scala 975:26]
    node _T_4 = or(_T_2, ibuf.io_inst_0_bits_xcpt0_ae_inst) @[RocketCore.scala 975:26]
    node _T_5 = or(_T_4, ibuf.io_inst_0_bits_xcpt1_pf_inst) @[RocketCore.scala 975:26]
    node _T_6 = or(_T_5, ibuf.io_inst_0_bits_xcpt1_ae_inst) @[RocketCore.scala 975:26]
    node id_xcpt = or(_T_6, id_illegal_insn) @[RocketCore.scala 975:26]
    node _T_7 = mux(ibuf.io_inst_0_bits_xcpt1_ae_inst, UInt<2>("h1"), UInt<2>("h2")) @[Mux.scala 47:69]
    node _T_8 = mux(ibuf.io_inst_0_bits_xcpt1_pf_inst, UInt<4>("hc"), pad(_T_7, 4)) @[Mux.scala 47:69]
    node _T_9 = mux(ibuf.io_inst_0_bits_xcpt0_ae_inst, UInt<4>("h1"), _T_8) @[Mux.scala 47:69]
    node _T_11 = mux(bpu.io_xcpt_if, UInt<4>("h3"), _T_9) @[Mux.scala 47:69]
    node _T_12 = mux(bpu.io_debug_if, UInt<4>("he"), _T_11) @[Mux.scala 47:69]
    node id_cause = mux(csr.io_interrupt, csr.io_interrupt_cause, pad(_T_12, 32)) @[Mux.scala 47:69]
    node ex_waddr = bits(ex_reg_inst, 11, 7) @[RocketCore.scala 359:29]
    node mem_waddr = bits(mem_reg_inst, 11, 7) @[RocketCore.scala 360:31]
    node wb_waddr = bits(wb_reg_inst, 11, 7) @[RocketCore.scala 361:29]
    node _T_21 = and(ex_reg_valid, ex_ctrl_wxd) @[RocketCore.scala 364:19]
    node _T_22 = and(mem_reg_valid, mem_ctrl_wxd) @[RocketCore.scala 365:20]
    node _T_23 = not(mem_ctrl_mem) @[RocketCore.scala 365:39]
    node _T_24 = and(_T_22, _T_23) @[RocketCore.scala 365:36]
    node id_bypass_src_0_0 = eq(UInt<5>("h0"), id_raddr1) @[RocketCore.scala 367:82]
    node _id_bypass_src_T_1 = eq(ex_waddr, id_raddr1) @[RocketCore.scala 367:82]
    node id_bypass_src_0_1 = and(_T_21, _id_bypass_src_T_1) @[RocketCore.scala 367:74]
    node _id_bypass_src_T_2 = eq(mem_waddr, id_raddr1) @[RocketCore.scala 367:82]
    node id_bypass_src_0_2 = and(_T_24, _id_bypass_src_T_2) @[RocketCore.scala 367:74]
    node id_bypass_src_0_3 = and(_T_22, _id_bypass_src_T_2) @[RocketCore.scala 367:74]
    node id_bypass_src_1_0 = eq(UInt<5>("h0"), id_raddr2) @[RocketCore.scala 367:82]
    node _id_bypass_src_T_5 = eq(ex_waddr, id_raddr2) @[RocketCore.scala 367:82]
    node id_bypass_src_1_1 = and(_T_21, _id_bypass_src_T_5) @[RocketCore.scala 367:74]
    node _id_bypass_src_T_6 = eq(mem_waddr, id_raddr2) @[RocketCore.scala 367:82]
    node id_bypass_src_1_2 = and(_T_24, _id_bypass_src_T_6) @[RocketCore.scala 367:74]
    node id_bypass_src_1_3 = and(_T_22, _id_bypass_src_T_6) @[RocketCore.scala 367:74]
    reg ex_reg_rs_bypass_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_bypass_0) @[RocketCore.scala 371:29]
    reg ex_reg_rs_bypass_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_bypass_1) @[RocketCore.scala 371:29]
    reg ex_reg_rs_lsb_0 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_lsb_0) @[RocketCore.scala 372:26]
    reg ex_reg_rs_lsb_1 : UInt<2>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_lsb_1) @[RocketCore.scala 372:26]
    reg ex_reg_rs_msb_0 : UInt<30>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_msb_0) @[RocketCore.scala 373:26]
    reg ex_reg_rs_msb_1 : UInt<30>, clock with :
      reset => (UInt<1>("h0"), ex_reg_rs_msb_1) @[RocketCore.scala 373:26]
    node _ex_rs_T = eq(ex_reg_rs_lsb_0, UInt<2>("h1")) @[package.scala 32:86]
    node _ex_rs_T_1 = mux(_ex_rs_T, mem_reg_wdata, UInt<32>("h0")) @[package.scala 32:76]
    node _ex_rs_T_2 = eq(ex_reg_rs_lsb_0, UInt<2>("h2")) @[package.scala 32:86]
    node _ex_rs_T_3 = mux(_ex_rs_T_2, wb_reg_wdata, _ex_rs_T_1) @[package.scala 32:76]
    node _ex_rs_T_4 = eq(ex_reg_rs_lsb_0, UInt<2>("h3")) @[package.scala 32:86]
    node _ex_rs_T_5 = mux(_ex_rs_T_4, io_dmem_resp_bits_data_word_bypass, _ex_rs_T_3) @[package.scala 32:76]
    node _ex_rs_T_6 = cat(ex_reg_rs_msb_0, ex_reg_rs_lsb_0) @[Cat.scala 30:58]
    node ex_rs_0 = mux(ex_reg_rs_bypass_0, _ex_rs_T_5, _ex_rs_T_6) @[RocketCore.scala 375:14]
    node _ex_rs_T_7 = eq(ex_reg_rs_lsb_1, UInt<2>("h1")) @[package.scala 32:86]
    node _ex_rs_T_8 = mux(_ex_rs_T_7, mem_reg_wdata, UInt<32>("h0")) @[package.scala 32:76]
    node _ex_rs_T_9 = eq(ex_reg_rs_lsb_1, UInt<2>("h2")) @[package.scala 32:86]
    node _ex_rs_T_10 = mux(_ex_rs_T_9, wb_reg_wdata, _ex_rs_T_8) @[package.scala 32:76]
    node _ex_rs_T_11 = eq(ex_reg_rs_lsb_1, UInt<2>("h3")) @[package.scala 32:86]
    node _ex_rs_T_12 = mux(_ex_rs_T_11, io_dmem_resp_bits_data_word_bypass, _ex_rs_T_10) @[package.scala 32:76]
    node _ex_rs_T_13 = cat(ex_reg_rs_msb_1, ex_reg_rs_lsb_1) @[Cat.scala 30:58]
    node ex_rs_1 = mux(ex_reg_rs_bypass_1, _ex_rs_T_12, _ex_rs_T_13) @[RocketCore.scala 375:14]
    node _ex_imm_sign_T = eq(ex_ctrl_sel_imm, UInt<3>("h5")) @[RocketCore.scala 1037:24]
    node _ex_imm_sign_T_1 = bits(ex_reg_inst, 31, 31) @[RocketCore.scala 1037:48]
    node _ex_imm_sign_T_2 = asSInt(_ex_imm_sign_T_1) @[RocketCore.scala 1037:53]
    node ex_imm_sign = mux(_ex_imm_sign_T, SInt<1>("h0"), _ex_imm_sign_T_2) @[RocketCore.scala 1037:19]
    node _ex_imm_b30_20_T = eq(ex_ctrl_sel_imm, UInt<3>("h2")) @[RocketCore.scala 1038:26]
    node _ex_imm_b30_20_T_1 = bits(ex_reg_inst, 30, 20) @[RocketCore.scala 1038:41]
    node _ex_imm_b30_20_T_2 = asSInt(_ex_imm_b30_20_T_1) @[RocketCore.scala 1038:49]
    node ex_imm_b30_20 = mux(_ex_imm_b30_20_T, _ex_imm_b30_20_T_2, pad(ex_imm_sign, 11)) @[RocketCore.scala 1038:21]
    node _ex_imm_b19_12_T = neq(ex_ctrl_sel_imm, UInt<3>("h2")) @[RocketCore.scala 1039:26]
    node _ex_imm_b19_12_T_1 = neq(ex_ctrl_sel_imm, UInt<3>("h3")) @[RocketCore.scala 1039:43]
    node _ex_imm_b19_12_T_2 = and(_ex_imm_b19_12_T, _ex_imm_b19_12_T_1) @[RocketCore.scala 1039:36]
    node _ex_imm_b19_12_T_3 = bits(ex_reg_inst, 19, 12) @[RocketCore.scala 1039:65]
    node _ex_imm_b19_12_T_4 = asSInt(_ex_imm_b19_12_T_3) @[RocketCore.scala 1039:73]
    node ex_imm_b19_12 = mux(_ex_imm_b19_12_T_2, pad(ex_imm_sign, 8), _ex_imm_b19_12_T_4) @[RocketCore.scala 1039:21]
    node _ex_imm_b11_T_2 = or(_ex_imm_b30_20_T, _ex_imm_sign_T) @[RocketCore.scala 1040:33]
    node _ex_imm_b11_T_3 = eq(ex_ctrl_sel_imm, UInt<3>("h3")) @[RocketCore.scala 1041:23]
    node _ex_imm_b11_T_4 = bits(ex_reg_inst, 20, 20) @[RocketCore.scala 1041:39]
    node _ex_imm_b11_T_5 = asSInt(_ex_imm_b11_T_4) @[RocketCore.scala 1041:44]
    node _ex_imm_b11_T_6 = eq(ex_ctrl_sel_imm, UInt<3>("h1")) @[RocketCore.scala 1042:23]
    node _ex_imm_b11_T_7 = bits(ex_reg_inst, 7, 7) @[RocketCore.scala 1042:39]
    node _ex_imm_b11_T_8 = asSInt(_ex_imm_b11_T_7) @[RocketCore.scala 1042:43]
    node _ex_imm_b11_T_9 = mux(_ex_imm_b11_T_6, _ex_imm_b11_T_8, ex_imm_sign) @[RocketCore.scala 1042:18]
    node _ex_imm_b11_T_10 = mux(_ex_imm_b11_T_3, _ex_imm_b11_T_5, _ex_imm_b11_T_9) @[RocketCore.scala 1041:18]
    node ex_imm_b11 = mux(_ex_imm_b11_T_2, SInt<1>("h0"), _ex_imm_b11_T_10) @[RocketCore.scala 1040:18]
    node _ex_imm_b10_5_T_3 = bits(ex_reg_inst, 30, 25) @[RocketCore.scala 1043:66]
    node ex_imm_lo_hi_hi = mux(_ex_imm_b11_T_2, UInt<6>("h0"), _ex_imm_b10_5_T_3) @[RocketCore.scala 1043:20]
    node _ex_imm_b4_1_T_1 = eq(ex_ctrl_sel_imm, UInt<3>("h0")) @[RocketCore.scala 1045:24]
    node _ex_imm_b4_1_T_3 = or(_ex_imm_b4_1_T_1, _ex_imm_b11_T_6) @[RocketCore.scala 1045:34]
    node _ex_imm_b4_1_T_4 = bits(ex_reg_inst, 11, 8) @[RocketCore.scala 1045:57]
    node _ex_imm_b4_1_T_6 = bits(ex_reg_inst, 19, 16) @[RocketCore.scala 1046:39]
    node _ex_imm_b4_1_T_7 = bits(ex_reg_inst, 24, 21) @[RocketCore.scala 1046:52]
    node _ex_imm_b4_1_T_8 = mux(_ex_imm_sign_T, _ex_imm_b4_1_T_6, _ex_imm_b4_1_T_7) @[RocketCore.scala 1046:19]
    node _ex_imm_b4_1_T_9 = mux(_ex_imm_b4_1_T_3, _ex_imm_b4_1_T_4, _ex_imm_b4_1_T_8) @[RocketCore.scala 1045:19]
    node ex_imm_lo_hi_lo = mux(_ex_imm_b30_20_T, UInt<4>("h0"), _ex_imm_b4_1_T_9) @[RocketCore.scala 1044:19]
    node _ex_imm_b0_T_2 = eq(ex_ctrl_sel_imm, UInt<3>("h4")) @[RocketCore.scala 1048:22]
    node _ex_imm_b0_T_5 = bits(ex_reg_inst, 15, 15) @[RocketCore.scala 1049:37]
    node _ex_imm_b0_T_6 = and(_ex_imm_sign_T, _ex_imm_b0_T_5) @[RocketCore.scala 1049:17]
    node _ex_imm_b0_T_7 = mux(_ex_imm_b0_T_2, _ex_imm_b11_T_4, _ex_imm_b0_T_6) @[RocketCore.scala 1048:17]
    node ex_imm_lo_lo = mux(_ex_imm_b4_1_T_1, _ex_imm_b11_T_7, _ex_imm_b0_T_7) @[RocketCore.scala 1047:17]
    node ex_imm_lo_hi = cat(ex_imm_lo_hi_hi, ex_imm_lo_hi_lo) @[Cat.scala 30:58]
    node ex_imm_lo = cat(ex_imm_lo_hi, ex_imm_lo_lo) @[Cat.scala 30:58]
    node ex_imm_hi_lo_lo = asUInt(ex_imm_b11) @[Cat.scala 30:58]
    node ex_imm_hi_lo_hi = asUInt(ex_imm_b19_12) @[Cat.scala 30:58]
    node ex_imm_hi_lo = cat(ex_imm_hi_lo_hi, ex_imm_hi_lo_lo) @[Cat.scala 30:58]
    node ex_imm_hi_hi_lo = asUInt(ex_imm_b30_20) @[Cat.scala 30:58]
    node ex_imm_hi_hi_hi = asUInt(ex_imm_sign) @[Cat.scala 30:58]
    node ex_imm_hi_hi = cat(ex_imm_hi_hi_hi, ex_imm_hi_hi_lo) @[Cat.scala 30:58]
    node ex_imm_hi = cat(ex_imm_hi_hi, ex_imm_hi_lo) @[Cat.scala 30:58]
    node _ex_imm_T = cat(ex_imm_hi, ex_imm_lo) @[Cat.scala 30:58]
    node ex_imm = asSInt(_ex_imm_T) @[RocketCore.scala 1051:53]
    node _ex_op1_T = asSInt(ex_rs_0) @[RocketCore.scala 378:24]
    node _ex_op1_T_1 = asSInt(ex_reg_pc) @[RocketCore.scala 379:24]
    node _ex_op1_T_2 = eq(UInt<2>("h1"), ex_ctrl_sel_alu1) @[Mux.scala 80:60]
    node _ex_op1_T_3 = mux(_ex_op1_T_2, _ex_op1_T, SInt<32>("h0")) @[Mux.scala 80:57]
    node _ex_op1_T_4 = eq(UInt<2>("h2"), ex_ctrl_sel_alu1) @[Mux.scala 80:60]
    node ex_op1 = mux(_ex_op1_T_4, _ex_op1_T_1, _ex_op1_T_3) @[Mux.scala 80:57]
    node _ex_op2_T = asSInt(ex_rs_1) @[RocketCore.scala 381:24]
    node _ex_op2_T_1 = mux(ex_reg_rvc, SInt<4>("h2"), SInt<4>("h4")) @[RocketCore.scala 383:19]
    node _ex_op2_T_2 = eq(UInt<2>("h2"), ex_ctrl_sel_alu2) @[Mux.scala 80:60]
    node _ex_op2_T_3 = mux(_ex_op2_T_2, _ex_op2_T, SInt<32>("h0")) @[Mux.scala 80:57]
    node _ex_op2_T_4 = eq(UInt<2>("h3"), ex_ctrl_sel_alu2) @[Mux.scala 80:60]
    node _ex_op2_T_5 = mux(_ex_op2_T_4, ex_imm, _ex_op2_T_3) @[Mux.scala 80:57]
    node _ex_op2_T_6 = eq(UInt<2>("h1"), ex_ctrl_sel_alu2) @[Mux.scala 80:60]
    node ex_op2 = mux(_ex_op2_T_6, pad(_ex_op2_T_1, 32), _ex_op2_T_5) @[Mux.scala 80:57]
    node _ctrl_killd_T = not(ibuf.io_inst_0_valid) @[RocketCore.scala 784:17]
    node _ctrl_killd_T_1 = or(_ctrl_killd_T, ibuf.io_inst_0_bits_replay) @[RocketCore.scala 784:40]
    node _ctrl_killd_T_2 = or(_ctrl_killd_T_1, take_pc_mem_wb) @[RocketCore.scala 784:71]
    node _T_113 = neq(id_raddr1, UInt<5>("h0")) @[RocketCore.scala 714:55]
    node _T_114 = and(id_ctrl_decoder_7, _T_113) @[RocketCore.scala 714:42]
    node _data_hazard_ex_T = eq(id_raddr1, ex_waddr) @[RocketCore.scala 734:70]
    node _data_hazard_ex_T_1 = and(_T_114, _data_hazard_ex_T) @[RocketCore.scala 984:27]
    node _T_115 = neq(id_raddr2, UInt<5>("h0")) @[RocketCore.scala 715:55]
    node _T_116 = and(id_ctrl_decoder_6, _T_115) @[RocketCore.scala 715:42]
    node _data_hazard_ex_T_2 = eq(id_raddr2, ex_waddr) @[RocketCore.scala 734:70]
    node _data_hazard_ex_T_3 = and(_T_116, _data_hazard_ex_T_2) @[RocketCore.scala 984:27]
    node _data_hazard_ex_T_6 = or(_data_hazard_ex_T_1, _data_hazard_ex_T_3) @[RocketCore.scala 984:50]
    node _T_117 = neq(id_waddr, UInt<5>("h0")) @[RocketCore.scala 716:55]
    node _T_118 = and(id_ctrl_decoder_22, _T_117) @[RocketCore.scala 716:42]
    node _data_hazard_ex_T_4 = eq(id_waddr, ex_waddr) @[RocketCore.scala 734:70]
    node _data_hazard_ex_T_5 = and(_T_118, _data_hazard_ex_T_4) @[RocketCore.scala 984:27]
    node _data_hazard_ex_T_7 = or(_data_hazard_ex_T_6, _data_hazard_ex_T_5) @[RocketCore.scala 984:50]
    node data_hazard_ex = and(ex_ctrl_wxd, _data_hazard_ex_T_7) @[RocketCore.scala 734:36]
    node _ex_cannot_bypass_T = neq(ex_ctrl_csr, UInt<3>("h0")) @[RocketCore.scala 733:38]
    node _ex_cannot_bypass_T_1 = or(_ex_cannot_bypass_T, ex_ctrl_jalr) @[RocketCore.scala 733:48]
    node _ex_cannot_bypass_T_2 = or(_ex_cannot_bypass_T_1, ex_ctrl_mem) @[RocketCore.scala 733:64]
    node ex_cannot_bypass = or(_ex_cannot_bypass_T_2, ex_ctrl_div) @[RocketCore.scala 733:94]
    node _id_ex_hazard_T = and(data_hazard_ex, ex_cannot_bypass) @[RocketCore.scala 736:54]
    node id_ex_hazard = and(ex_reg_valid, _id_ex_hazard_T) @[RocketCore.scala 736:35]
    node _data_hazard_mem_T = eq(id_raddr1, mem_waddr) @[RocketCore.scala 743:72]
    node _data_hazard_mem_T_1 = and(_T_114, _data_hazard_mem_T) @[RocketCore.scala 984:27]
    node _data_hazard_mem_T_2 = eq(id_raddr2, mem_waddr) @[RocketCore.scala 743:72]
    node _data_hazard_mem_T_3 = and(_T_116, _data_hazard_mem_T_2) @[RocketCore.scala 984:27]
    node _data_hazard_mem_T_6 = or(_data_hazard_mem_T_1, _data_hazard_mem_T_3) @[RocketCore.scala 984:50]
    node _data_hazard_mem_T_4 = eq(id_waddr, mem_waddr) @[RocketCore.scala 743:72]
    node _data_hazard_mem_T_5 = and(_T_118, _data_hazard_mem_T_4) @[RocketCore.scala 984:27]
    node _data_hazard_mem_T_7 = or(_data_hazard_mem_T_6, _data_hazard_mem_T_5) @[RocketCore.scala 984:50]
    node data_hazard_mem = and(mem_ctrl_wxd, _data_hazard_mem_T_7) @[RocketCore.scala 743:38]
    node _mem_cannot_bypass_T = neq(mem_ctrl_csr, UInt<3>("h0")) @[RocketCore.scala 742:40]
    node _mem_cannot_bypass_T_1 = and(mem_ctrl_mem, mem_reg_slow_bypass) @[RocketCore.scala 742:66]
    node _mem_cannot_bypass_T_2 = or(_mem_cannot_bypass_T, _mem_cannot_bypass_T_1) @[RocketCore.scala 742:50]
    node mem_cannot_bypass = or(_mem_cannot_bypass_T_2, mem_ctrl_div) @[RocketCore.scala 742:100]
    node _id_mem_hazard_T = and(data_hazard_mem, mem_cannot_bypass) @[RocketCore.scala 745:57]
    node id_mem_hazard = and(mem_reg_valid, _id_mem_hazard_T) @[RocketCore.scala 745:37]
    node _ctrl_stalld_T = or(id_ex_hazard, id_mem_hazard) @[RocketCore.scala 772:18]
    node _data_hazard_wb_T = eq(id_raddr1, wb_waddr) @[RocketCore.scala 749:70]
    node _data_hazard_wb_T_1 = and(_T_114, _data_hazard_wb_T) @[RocketCore.scala 984:27]
    node _data_hazard_wb_T_2 = eq(id_raddr2, wb_waddr) @[RocketCore.scala 749:70]
    node _data_hazard_wb_T_3 = and(_T_116, _data_hazard_wb_T_2) @[RocketCore.scala 984:27]
    node _data_hazard_wb_T_6 = or(_data_hazard_wb_T_1, _data_hazard_wb_T_3) @[RocketCore.scala 984:50]
    node _data_hazard_wb_T_4 = eq(id_waddr, wb_waddr) @[RocketCore.scala 749:70]
    node _data_hazard_wb_T_5 = and(_T_118, _data_hazard_wb_T_4) @[RocketCore.scala 984:27]
    node _data_hazard_wb_T_7 = or(_data_hazard_wb_T_6, _data_hazard_wb_T_5) @[RocketCore.scala 984:50]
    node data_hazard_wb = and(wb_ctrl_wxd, _data_hazard_wb_T_7) @[RocketCore.scala 749:36]
    node _wb_dcache_miss_T = not(io_dmem_resp_valid) @[RocketCore.scala 491:39]
    node wb_dcache_miss = and(wb_ctrl_mem, _wb_dcache_miss_T) @[RocketCore.scala 491:36]
    node wb_set_sboard = or(wb_ctrl_div, wb_dcache_miss) @[RocketCore.scala 636:35]
    node _id_wb_hazard_T = and(data_hazard_wb, wb_set_sboard) @[RocketCore.scala 751:54]
    node id_wb_hazard = and(wb_reg_valid, _id_wb_hazard_T) @[RocketCore.scala 751:35]
    node _ctrl_stalld_T_1 = or(_ctrl_stalld_T, id_wb_hazard) @[RocketCore.scala 772:35]
    reg _r : UInt<32>, clock with :
      reset => (UInt<1>("h0"), _r) @[RocketCore.scala 1001:25]
    node _r_T = shr(_r, 1) @[RocketCore.scala 1002:35]
    node r = shl(_r_T, 1) @[RocketCore.scala 1002:40]
    node _id_sboard_hazard_T = dshr(r, id_raddr1) @[RocketCore.scala 998:35]
    node _id_sboard_hazard_T_1 = bits(_id_sboard_hazard_T, 0, 0) @[RocketCore.scala 998:35]
    node dmem_resp_valid = and(io_dmem_resp_valid, io_dmem_resp_bits_has_data) @[RocketCore.scala 646:44]
    node dmem_resp_replay = and(dmem_resp_valid, io_dmem_resp_bits_replay) @[RocketCore.scala 647:42]
    node _dmem_resp_xpu_T = bits(io_dmem_resp_bits_tag, 0, 0) @[RocketCore.scala 643:45]
    node dmem_resp_xpu = not(_dmem_resp_xpu_T) @[RocketCore.scala 643:23]
    node _T_107 = and(dmem_resp_replay, dmem_resp_xpu) @[RocketCore.scala 662:26]
    node ll_wen_x1 = and(div.io_resp_ready, div.io_resp_valid) @[Decoupled.scala 40:37]
    node ll_wen = or(_T_107, ll_wen_x1) @[RocketCore.scala 662:44 667:12]
    node dmem_resp_waddr = bits(io_dmem_resp_bits_tag, 5, 1) @[RocketCore.scala 645:46]
    node ll_waddr = mux(_T_107, dmem_resp_waddr, div.io_resp_bits_tag) @[RocketCore.scala 662:44 666:14]
    node _id_sboard_hazard_T_2 = eq(ll_waddr, id_raddr1) @[RocketCore.scala 726:70]
    node _id_sboard_hazard_T_3 = and(ll_wen, _id_sboard_hazard_T_2) @[RocketCore.scala 726:58]
    node _id_sboard_hazard_T_4 = not(_id_sboard_hazard_T_3) @[RocketCore.scala 729:80]
    node _id_sboard_hazard_T_5 = and(_id_sboard_hazard_T_1, _id_sboard_hazard_T_4) @[RocketCore.scala 729:77]
    node _id_sboard_hazard_T_6 = and(_T_114, _id_sboard_hazard_T_5) @[RocketCore.scala 984:27]
    node _id_sboard_hazard_T_7 = dshr(r, id_raddr2) @[RocketCore.scala 998:35]
    node _id_sboard_hazard_T_8 = bits(_id_sboard_hazard_T_7, 0, 0) @[RocketCore.scala 998:35]
    node _id_sboard_hazard_T_9 = eq(ll_waddr, id_raddr2) @[RocketCore.scala 726:70]
    node _id_sboard_hazard_T_10 = and(ll_wen, _id_sboard_hazard_T_9) @[RocketCore.scala 726:58]
    node _id_sboard_hazard_T_11 = not(_id_sboard_hazard_T_10) @[RocketCore.scala 729:80]
    node _id_sboard_hazard_T_12 = and(_id_sboard_hazard_T_8, _id_sboard_hazard_T_11) @[RocketCore.scala 729:77]
    node _id_sboard_hazard_T_13 = and(_T_116, _id_sboard_hazard_T_12) @[RocketCore.scala 984:27]
    node _id_sboard_hazard_T_21 = or(_id_sboard_hazard_T_6, _id_sboard_hazard_T_13) @[RocketCore.scala 984:50]
    node _id_sboard_hazard_T_14 = dshr(r, id_waddr) @[RocketCore.scala 998:35]
    node _id_sboard_hazard_T_15 = bits(_id_sboard_hazard_T_14, 0, 0) @[RocketCore.scala 998:35]
    node _id_sboard_hazard_T_16 = eq(ll_waddr, id_waddr) @[RocketCore.scala 726:70]
    node _id_sboard_hazard_T_17 = and(ll_wen, _id_sboard_hazard_T_16) @[RocketCore.scala 726:58]
    node _id_sboard_hazard_T_18 = not(_id_sboard_hazard_T_17) @[RocketCore.scala 729:80]
    node _id_sboard_hazard_T_19 = and(_id_sboard_hazard_T_15, _id_sboard_hazard_T_18) @[RocketCore.scala 729:77]
    node _id_sboard_hazard_T_20 = and(_T_118, _id_sboard_hazard_T_19) @[RocketCore.scala 984:27]
    node id_sboard_hazard = or(_id_sboard_hazard_T_21, _id_sboard_hazard_T_20) @[RocketCore.scala 984:50]
    node _ctrl_stalld_T_2 = or(_ctrl_stalld_T_1, id_sboard_hazard) @[RocketCore.scala 772:51]
    node _ctrl_stalld_T_3 = or(ex_reg_valid, mem_reg_valid) @[RocketCore.scala 773:40]
    node _ctrl_stalld_T_4 = or(_ctrl_stalld_T_3, wb_reg_valid) @[RocketCore.scala 773:57]
    node _ctrl_stalld_T_5 = and(csr.io_singleStep, _ctrl_stalld_T_4) @[RocketCore.scala 773:23]
    node _ctrl_stalld_T_6 = or(_ctrl_stalld_T_2, _ctrl_stalld_T_5) @[RocketCore.scala 772:71]
    reg blocked : UInt<1>, clock with :
      reset => (UInt<1>("h0"), blocked) @[RocketCore.scala 764:22]
    node _dcache_blocked_T = not(io_dmem_perf_grant) @[RocketCore.scala 766:16]
    node dcache_blocked = and(blocked, _dcache_blocked_T) @[RocketCore.scala 766:13]
    node _ctrl_stalld_T_13 = and(id_ctrl_decoder_14, dcache_blocked) @[RocketCore.scala 776:17]
    node _ctrl_stalld_T_14 = or(_ctrl_stalld_T_6, _ctrl_stalld_T_13) @[RocketCore.scala 775:32]
    node wb_wxd = and(wb_reg_valid, wb_ctrl_wxd) @[RocketCore.scala 635:29]
    node _ctrl_stalld_T_17 = not(wb_wxd) @[RocketCore.scala 778:65]
    node _ctrl_stalld_T_18 = and(div.io_resp_valid, _ctrl_stalld_T_17) @[RocketCore.scala 778:62]
    node _ctrl_stalld_T_19 = or(div.io_req_ready, _ctrl_stalld_T_18) @[RocketCore.scala 778:40]
    node _ctrl_stalld_T_20 = not(_ctrl_stalld_T_19) @[RocketCore.scala 778:21]
    node _ctrl_stalld_T_21 = or(_ctrl_stalld_T_20, div.io_req_valid) @[RocketCore.scala 778:75]
    node _ctrl_stalld_T_22 = and(id_ctrl_decoder_21, _ctrl_stalld_T_21) @[RocketCore.scala 778:17]
    node _ctrl_stalld_T_23 = or(_ctrl_stalld_T_14, _ctrl_stalld_T_22) @[RocketCore.scala 777:34]
    node _ctrl_stalld_T_26 = or(_ctrl_stalld_T_23, id_do_fence_x8) @[RocketCore.scala 779:15]
    node _ctrl_stalld_T_27 = or(_ctrl_stalld_T_26, csr.io_csr_stall) @[RocketCore.scala 780:17]
    node ctrl_stalld = or(_ctrl_stalld_T_27, id_reg_pause) @[RocketCore.scala 781:22]
    node _ctrl_killd_T_3 = or(_ctrl_killd_T_2, ctrl_stalld) @[RocketCore.scala 784:89]
    node ctrl_killd = or(_ctrl_killd_T_3, csr.io_interrupt) @[RocketCore.scala 784:104]
    node _ex_reg_valid_T = not(ctrl_killd) @[RocketCore.scala 423:19]
    node _ex_reg_replay_T = not(take_pc_mem_wb) @[RocketCore.scala 424:20]
    node _ex_reg_replay_T_1 = and(_ex_reg_replay_T, ibuf.io_inst_0_valid) @[RocketCore.scala 424:29]
    node _T_27 = eq(id_fence_succ, UInt<4>("h0")) @[RocketCore.scala 434:42]
    node _T_28 = and(id_ctrl_decoder_25, _T_27) @[RocketCore.scala 434:25]
    node _GEN_1 = or(_T_28, id_reg_pause) @[RocketCore.scala 114:25 434:{49,64}]
    node _GEN_2 = or(id_fence_next, _GEN_0) @[RocketCore.scala 435:{26,41}]
    node _T_29 = cat(ibuf.io_inst_0_bits_xcpt1_pf_inst, ibuf.io_inst_0_bits_xcpt1_ae_inst) @[RocketCore.scala 441:22]
    node _T_30 = orr(_T_29) @[RocketCore.scala 441:29]
    node _GEN_3 = mux(_T_30, UInt<2>("h2"), UInt<2>("h1")) @[RocketCore.scala 439:24 441:34 442:26]
    node _GEN_4 = mux(_T_30, UInt<2>("h1"), UInt<2>("h0")) @[RocketCore.scala 440:24 441:34 443:26]
    node _GEN_5 = or(_T_30, ibuf.io_inst_0_bits_rvc) @[RocketCore.scala 430:16 441:34 444:20]
    node _T_31 = cat(UInt<1>("h0"), ibuf.io_inst_0_bits_xcpt0_ae_inst) @[RocketCore.scala 446:40]
    node _T_32 = orr(_T_31) @[RocketCore.scala 446:47]
    node _T_33 = or(bpu.io_xcpt_if, _T_32) @[RocketCore.scala 446:28]
    node _GEN_6 = mux(_T_33, UInt<2>("h2"), _GEN_3) @[RocketCore.scala 446:52 447:26]
    node _GEN_7 = mux(_T_33, UInt<2>("h0"), _GEN_4) @[RocketCore.scala 446:52 448:26]
    node _GEN_8 = mux(id_xcpt, UInt<4>("h0"), id_ctrl_decoder_13) @[RocketCore.scala 429:13 436:20 437:22]
    node _GEN_10 = mux(id_xcpt, _GEN_6, id_ctrl_decoder_10) @[RocketCore.scala 429:13 436:20]
    node _GEN_11 = mux(id_xcpt, _GEN_7, id_ctrl_decoder_9) @[RocketCore.scala 429:13 436:20]
    node _GEN_12 = mux(id_xcpt, _GEN_5, ibuf.io_inst_0_bits_rvc) @[RocketCore.scala 430:16 436:20]
    node _ex_reg_flush_pipe_T = or(id_ctrl_decoder_24, id_csr_flush) @[RocketCore.scala 451:42]
    node _ex_reg_mem_size_T = bits(ibuf.io_inst_0_bits_inst_bits, 13, 12) @[RocketCore.scala 453:34]
    node _T_35 = eq(id_ctrl_decoder_15, UInt<5>("h5")) @[package.scala 15:47]
    node _T_36 = or(_id_sfence_T, _T_35) @[package.scala 72:59]
    node _ex_reg_mem_size_T_1 = cat(_T_115, _T_113) @[Cat.scala 30:58]
    node _GEN_13 = mux(_T_36, _ex_reg_mem_size_T_1, _ex_reg_mem_size_T) @[RocketCore.scala 453:21 454:59 455:23]
    node _do_bypass_T = or(id_bypass_src_0_0, id_bypass_src_0_1) @[RocketCore.scala 464:48]
    node _do_bypass_T_1 = or(_do_bypass_T, id_bypass_src_0_2) @[RocketCore.scala 464:48]
    node do_bypass = or(_do_bypass_T_1, id_bypass_src_0_3) @[RocketCore.scala 464:48]
    node _bypass_src_T = mux(id_bypass_src_0_2, UInt<2>("h2"), UInt<2>("h3")) @[Mux.scala 47:69]
    node _bypass_src_T_1 = mux(id_bypass_src_0_1, UInt<2>("h1"), _bypass_src_T) @[Mux.scala 47:69]
    node bypass_src = mux(id_bypass_src_0_0, UInt<2>("h0"), _bypass_src_T_1) @[Mux.scala 47:69]
    node _T_37 = not(do_bypass) @[RocketCore.scala 468:26]
    node _T_38 = and(id_ctrl_decoder_7, _T_37) @[RocketCore.scala 468:23]
    node _wb_valid_T = not(replay_wb_common) @[RocketCore.scala 670:34]
    node _wb_valid_T_1 = and(wb_reg_valid, _wb_valid_T) @[RocketCore.scala 670:31]
    node _wb_valid_T_2 = not(wb_xcpt) @[RocketCore.scala 670:48]
    node wb_valid = and(_wb_valid_T_1, _wb_valid_T_2) @[RocketCore.scala 670:45]
    node wb_wen = and(wb_valid, wb_ctrl_wxd) @[RocketCore.scala 671:25]
    node rf_wen = or(wb_wen, ll_wen) @[RocketCore.scala 672:23]
    node rf_waddr = mux(ll_wen, ll_waddr, wb_waddr) @[RocketCore.scala 673:21]
    node _T_108 = neq(rf_waddr, UInt<5>("h0")) @[RocketCore.scala 1027:16]
    node _T_111 = eq(rf_waddr, id_raddr1) @[RocketCore.scala 1030:20]
    node _rf_wdata_T = and(dmem_resp_valid, dmem_resp_xpu) @[RocketCore.scala 674:38]
    node ll_wdata = div.io_resp_bits_data
    node _rf_wdata_T_2 = neq(wb_ctrl_csr, UInt<3>("h0")) @[RocketCore.scala 676:34]
    node _rf_wdata_T_4 = mux(_rf_wdata_T_2, csr.io_rw_rdata, wb_reg_wdata) @[RocketCore.scala 676:21]
    node _rf_wdata_T_5 = mux(ll_wen, ll_wdata, _rf_wdata_T_4) @[RocketCore.scala 675:21]
    node rf_wdata = mux(_rf_wdata_T, io_dmem_resp_bits_data, _rf_wdata_T_5) @[RocketCore.scala 674:21]
    node _GEN_226 = mux(_T_111, rf_wdata, _id_rs_T_4) @[RocketCore.scala 1022:19 1030:{31,39}]
    node _GEN_233 = mux(_T_108, _GEN_226, _id_rs_T_4) @[RocketCore.scala 1022:19 1027:29]
    node id_rs_0 = mux(rf_wen, _GEN_233, _id_rs_T_4) @[RocketCore.scala 679:17 1022:19]
    node _ex_reg_rs_lsb_0_T = bits(id_rs_0, 1, 0) @[RocketCore.scala 469:37]
    node _ex_reg_rs_msb_0_T = shr(id_rs_0, 2) @[RocketCore.scala 470:38]
    node _GEN_14 = mux(_T_38, _ex_reg_rs_lsb_0_T, bypass_src) @[RocketCore.scala 467:24 468:38 469:26]
    node _GEN_15 = mux(_T_38, _ex_reg_rs_msb_0_T, ex_reg_rs_msb_0) @[RocketCore.scala 373:26 468:38 470:26]
    node _do_bypass_T_2 = or(id_bypass_src_1_0, id_bypass_src_1_1) @[RocketCore.scala 464:48]
    node _do_bypass_T_3 = or(_do_bypass_T_2, id_bypass_src_1_2) @[RocketCore.scala 464:48]
    node do_bypass_1 = or(_do_bypass_T_3, id_bypass_src_1_3) @[RocketCore.scala 464:48]
    node _bypass_src_T_2 = mux(id_bypass_src_1_2, UInt<2>("h2"), UInt<2>("h3")) @[Mux.scala 47:69]
    node _bypass_src_T_3 = mux(id_bypass_src_1_1, UInt<2>("h1"), _bypass_src_T_2) @[Mux.scala 47:69]
    node bypass_src_1 = mux(id_bypass_src_1_0, UInt<2>("h0"), _bypass_src_T_3) @[Mux.scala 47:69]
    node _T_39 = not(do_bypass_1) @[RocketCore.scala 468:26]
    node _T_40 = and(id_ctrl_decoder_6, _T_39) @[RocketCore.scala 468:23]
    node _T_112 = eq(rf_waddr, id_raddr2) @[RocketCore.scala 1030:20]
    node _GEN_227 = mux(_T_112, rf_wdata, _id_rs_T_9) @[RocketCore.scala 1022:19 1030:{31,39}]
    node _GEN_234 = mux(_T_108, _GEN_227, _id_rs_T_9) @[RocketCore.scala 1022:19 1027:29]
    node id_rs_1 = mux(rf_wen, _GEN_234, _id_rs_T_9) @[RocketCore.scala 679:17 1022:19]
    node _ex_reg_rs_lsb_1_T = bits(id_rs_1, 1, 0) @[RocketCore.scala 469:37]
    node _ex_reg_rs_msb_1_T = shr(id_rs_1, 2) @[RocketCore.scala 470:38]
    node _GEN_16 = mux(_T_40, _ex_reg_rs_lsb_1_T, bypass_src_1) @[RocketCore.scala 467:24 468:38 469:26]
    node _GEN_17 = mux(_T_40, _ex_reg_rs_msb_1_T, ex_reg_rs_msb_1) @[RocketCore.scala 373:26 468:38 470:26]
    node _inst_T = bits(ibuf.io_inst_0_bits_raw, 15, 0) @[RocketCore.scala 474:62]
    node inst = mux(ibuf.io_inst_0_bits_rvc, pad(_inst_T, 32), ibuf.io_inst_0_bits_raw) @[RocketCore.scala 474:21]
    node _ex_reg_rs_lsb_0_T_1 = bits(inst, 1, 0) @[RocketCore.scala 476:31]
    node _ex_reg_rs_msb_0_T_1 = shr(inst, 2) @[RocketCore.scala 477:32]
    node _GEN_18 = mux(id_illegal_insn, UInt<1>("h0"), do_bypass) @[RocketCore.scala 466:27 473:28 475:27]
    node _GEN_19 = mux(id_illegal_insn, _ex_reg_rs_lsb_0_T_1, _GEN_14) @[RocketCore.scala 473:28 476:24]
    node _GEN_20 = mux(id_illegal_insn, _ex_reg_rs_msb_0_T_1, _GEN_15) @[RocketCore.scala 473:28 477:24]
    node _GEN_52 = mux(_ex_reg_valid_T, _GEN_1, id_reg_pause) @[RocketCore.scala 428:22 114:25]
    node _GEN_53 = mux(_ex_reg_valid_T, _GEN_2, _GEN_0) @[RocketCore.scala 428:22]
    node _id_load_use_T = and(mem_reg_valid, data_hazard_mem) @[RocketCore.scala 746:32]
    node id_load_use = and(_id_load_use_T, mem_ctrl_mem) @[RocketCore.scala 746:51]
    node _T_42 = or(_ex_reg_valid_T, csr.io_interrupt) @[RocketCore.scala 480:21]
    node _T_43 = or(_T_42, ibuf.io_inst_0_bits_replay) @[RocketCore.scala 480:41]
    node _ex_pc_valid_T = or(ex_reg_valid, ex_reg_replay) @[RocketCore.scala 490:34]
    node ex_pc_valid = or(_ex_pc_valid_T, ex_reg_xcpt_interrupt) @[RocketCore.scala 490:51]
    node _replay_ex_structural_T = not(io_dmem_req_ready) @[RocketCore.scala 492:45]
    node _replay_ex_structural_T_1 = and(ex_ctrl_mem, _replay_ex_structural_T) @[RocketCore.scala 492:42]
    node _replay_ex_structural_T_2 = not(div.io_req_ready) @[RocketCore.scala 493:45]
    node _replay_ex_structural_T_3 = and(ex_ctrl_div, _replay_ex_structural_T_2) @[RocketCore.scala 493:42]
    node replay_ex_structural = or(_replay_ex_structural_T_1, _replay_ex_structural_T_3) @[RocketCore.scala 492:64]
    node replay_ex_load_use = and(wb_dcache_miss, ex_reg_load_use) @[RocketCore.scala 494:43]
    node _replay_ex_T = or(replay_ex_structural, replay_ex_load_use) @[RocketCore.scala 495:75]
    node _replay_ex_T_1 = and(ex_reg_valid, _replay_ex_T) @[RocketCore.scala 495:50]
    node replay_ex = or(ex_reg_replay, _replay_ex_T_1) @[RocketCore.scala 495:33]
    node _ctrl_killx_T = or(take_pc_mem_wb, replay_ex) @[RocketCore.scala 496:35]
    node _ctrl_killx_T_1 = not(ex_reg_valid) @[RocketCore.scala 496:51]
    node ctrl_killx = or(_ctrl_killx_T, _ctrl_killx_T_1) @[RocketCore.scala 496:48]
    node _ex_slow_bypass_T = eq(ex_ctrl_mem_cmd, UInt<5>("h7")) @[RocketCore.scala 498:40]
    node _ex_slow_bypass_T_1 = lt(ex_reg_mem_size, UInt<2>("h2")) @[RocketCore.scala 498:69]
    node ex_slow_bypass = or(_ex_slow_bypass_T, _ex_slow_bypass_T_1) @[RocketCore.scala 498:50]
    node ex_xcpt = or(ex_reg_xcpt_interrupt, ex_reg_xcpt) @[RocketCore.scala 502:28]
    node _mem_pc_valid_T = or(mem_reg_valid, mem_reg_replay) @[RocketCore.scala 508:36]
    node mem_pc_valid = or(_mem_pc_valid_T, mem_reg_xcpt_interrupt) @[RocketCore.scala 508:54]
    node _mem_br_target_T = asSInt(mem_reg_pc) @[RocketCore.scala 509:34]
    node _mem_br_target_sign_T_1 = bits(mem_reg_inst, 31, 31) @[RocketCore.scala 1037:48]
    node mem_br_target_sign = asSInt(_mem_br_target_sign_T_1) @[RocketCore.scala 1037:53]
    node mem_br_target_b30_20 = pad(mem_br_target_sign, 11) @[RocketCore.scala 1038:21]
    node _mem_br_target_b19_12_T_3 = bits(mem_reg_inst, 19, 12) @[RocketCore.scala 1039:65]
    node _mem_br_target_b19_12_T_4 = asSInt(_mem_br_target_b19_12_T_3) @[RocketCore.scala 1039:73]
    node mem_br_target_b19_12 = pad(mem_br_target_sign, 8) @[RocketCore.scala 1039:21]
    node _mem_br_target_b11_T_4 = bits(mem_reg_inst, 20, 20) @[RocketCore.scala 1041:39]
    node _mem_br_target_b11_T_5 = asSInt(_mem_br_target_b11_T_4) @[RocketCore.scala 1041:44]
    node _mem_br_target_b11_T_7 = bits(mem_reg_inst, 7, 7) @[RocketCore.scala 1042:39]
    node mem_br_target_b11 = asSInt(_mem_br_target_b11_T_7) @[RocketCore.scala 1042:43]
    node mem_br_target_lo_hi_hi = bits(mem_reg_inst, 30, 25) @[RocketCore.scala 1043:66]
    node mem_br_target_lo_hi_lo = bits(mem_reg_inst, 11, 8) @[RocketCore.scala 1045:57]
    node _mem_br_target_b4_1_T_7 = bits(mem_reg_inst, 24, 21) @[RocketCore.scala 1046:52]
    node mem_br_target_lo_hi = cat(mem_br_target_lo_hi_hi, mem_br_target_lo_hi_lo) @[Cat.scala 30:58]
    node mem_br_target_lo = cat(mem_br_target_lo_hi, UInt<1>("h0")) @[Cat.scala 30:58]
    node mem_br_target_hi_lo_lo = asUInt(mem_br_target_b11) @[Cat.scala 30:58]
    node mem_br_target_hi_lo_hi = asUInt(mem_br_target_b19_12) @[Cat.scala 30:58]
    node mem_br_target_hi_lo = cat(mem_br_target_hi_lo_hi, mem_br_target_hi_lo_lo) @[Cat.scala 30:58]
    node mem_br_target_hi_hi_lo = asUInt(mem_br_target_b30_20) @[Cat.scala 30:58]
    node mem_br_target_hi_hi_hi = asUInt(mem_br_target_sign) @[Cat.scala 30:58]
    node mem_br_target_hi_hi = cat(mem_br_target_hi_hi_hi, mem_br_target_hi_hi_lo) @[Cat.scala 30:58]
    node mem_br_target_hi = cat(mem_br_target_hi_hi, mem_br_target_hi_lo) @[Cat.scala 30:58]
    node _mem_br_target_T_2 = cat(mem_br_target_hi, mem_br_target_lo) @[Cat.scala 30:58]
    node _mem_br_target_T_3 = asSInt(_mem_br_target_T_2) @[RocketCore.scala 1051:53]
    node mem_br_target_lo_hi_1 = cat(mem_br_target_lo_hi_hi, _mem_br_target_b4_1_T_7) @[Cat.scala 30:58]
    node mem_br_target_lo_1 = cat(mem_br_target_lo_hi_1, UInt<1>("h0")) @[Cat.scala 30:58]
    node mem_br_target_hi_lo_lo_1 = asUInt(_mem_br_target_b11_T_5) @[Cat.scala 30:58]
    node mem_br_target_hi_lo_hi_1 = asUInt(_mem_br_target_b19_12_T_4) @[Cat.scala 30:58]
    node mem_br_target_hi_lo_1 = cat(mem_br_target_hi_lo_hi_1, mem_br_target_hi_lo_lo_1) @[Cat.scala 30:58]
    node mem_br_target_hi_1 = cat(mem_br_target_hi_hi, mem_br_target_hi_lo_1) @[Cat.scala 30:58]
    node _mem_br_target_T_4 = cat(mem_br_target_hi_1, mem_br_target_lo_1) @[Cat.scala 30:58]
    node _mem_br_target_T_5 = asSInt(_mem_br_target_T_4) @[RocketCore.scala 1051:53]
    node _mem_br_target_T_6 = mux(mem_reg_rvc, SInt<4>("h2"), SInt<4>("h4")) @[RocketCore.scala 512:8]
    node _mem_br_target_T_7 = mux(mem_ctrl_jal, _mem_br_target_T_5, pad(_mem_br_target_T_6, 32)) @[RocketCore.scala 511:8]
    node _mem_br_target_T_8 = mux(_mem_cfi_taken_T, _mem_br_target_T_3, _mem_br_target_T_7) @[RocketCore.scala 510:8]
    node _mem_br_target_T_9 = add(_mem_br_target_T, _mem_br_target_T_8) @[RocketCore.scala 509:41]
    node _mem_br_target_T_10 = tail(_mem_br_target_T_9, 1) @[RocketCore.scala 509:41]
    node mem_br_target = asSInt(_mem_br_target_T_10) @[RocketCore.scala 509:41]
    node _mem_npc_T_1 = asSInt(mem_reg_wdata) @[RocketCore.scala 513:106]
    node _mem_npc_T_2 = mux(mem_ctrl_jalr, _mem_npc_T_1, mem_br_target) @[RocketCore.scala 513:21]
    node _mem_npc_T_3 = and(_mem_npc_T_2, SInt<32>("h-2")) @[RocketCore.scala 513:129]
    node _mem_npc_T_4 = asSInt(_mem_npc_T_3) @[RocketCore.scala 513:129]
    node mem_npc = asUInt(_mem_npc_T_4) @[RocketCore.scala 513:141]
    node _mem_wrong_npc_T = neq(mem_npc, ex_reg_pc) @[RocketCore.scala 515:30]
    node _mem_wrong_npc_T_1 = or(ibuf.io_inst_0_valid, ibuf.io_imem_valid) @[RocketCore.scala 516:31]
    node _mem_wrong_npc_T_2 = neq(mem_npc, ibuf.io_pc) @[RocketCore.scala 516:62]
    node _mem_wrong_npc_T_3 = mux(_mem_wrong_npc_T_1, _mem_wrong_npc_T_2, UInt<1>("h1")) @[RocketCore.scala 516:8]
    node mem_wrong_npc = mux(ex_pc_valid, _mem_wrong_npc_T, _mem_wrong_npc_T_3) @[RocketCore.scala 515:8]
    node _mem_npc_misaligned_T_2 = bits(mem_npc, 1, 1) @[RocketCore.scala 517:66]
    node mem_npc_misaligned = and(_id_illegal_insn_T_18, _mem_npc_misaligned_T_2) @[RocketCore.scala 517:56]
    node _mem_int_wdata_T = not(mem_reg_xcpt) @[RocketCore.scala 518:27]
    node _mem_int_wdata_T_1 = xor(mem_ctrl_jalr, mem_npc_misaligned) @[RocketCore.scala 518:59]
    node _mem_int_wdata_T_2 = and(_mem_int_wdata_T, _mem_int_wdata_T_1) @[RocketCore.scala 518:41]
    node _mem_int_wdata_T_4 = mux(_mem_int_wdata_T_2, mem_br_target, _mem_npc_T_1) @[RocketCore.scala 518:26]
    node mem_int_wdata = asUInt(_mem_int_wdata_T_4) @[RocketCore.scala 518:119]
    node _mem_cfi_T = or(mem_ctrl_branch, mem_ctrl_jalr) @[RocketCore.scala 519:33]
    node mem_cfi = or(_mem_cfi_T, mem_ctrl_jal) @[RocketCore.scala 519:50]
    node _mem_reg_valid_T = not(ctrl_killx) @[RocketCore.scala 525:20]
    node _T_52 = and(mem_reg_valid, mem_reg_flush_pipe) @[RocketCore.scala 532:23]
    node _mem_reg_load_T = eq(ex_ctrl_mem_cmd, UInt<5>("h0")) @[Consts.scala 81:31]
    node _mem_reg_load_T_1 = eq(ex_ctrl_mem_cmd, UInt<5>("h6")) @[Consts.scala 81:48]
    node _mem_reg_load_T_2 = or(_mem_reg_load_T, _mem_reg_load_T_1) @[Consts.scala 81:41]
    node _mem_reg_load_T_4 = or(_mem_reg_load_T_2, _ex_slow_bypass_T) @[Consts.scala 81:58]
    node _mem_reg_load_T_5 = eq(ex_ctrl_mem_cmd, UInt<5>("h4")) @[package.scala 15:47]
    node _mem_reg_load_T_6 = eq(ex_ctrl_mem_cmd, UInt<5>("h9")) @[package.scala 15:47]
    node _mem_reg_load_T_7 = eq(ex_ctrl_mem_cmd, UInt<5>("ha")) @[package.scala 15:47]
    node _mem_reg_load_T_8 = eq(ex_ctrl_mem_cmd, UInt<5>("hb")) @[package.scala 15:47]
    node _mem_reg_load_T_9 = or(_mem_reg_load_T_5, _mem_reg_load_T_6) @[package.scala 72:59]
    node _mem_reg_load_T_10 = or(_mem_reg_load_T_9, _mem_reg_load_T_7) @[package.scala 72:59]
    node _mem_reg_load_T_11 = or(_mem_reg_load_T_10, _mem_reg_load_T_8) @[package.scala 72:59]
    node _mem_reg_load_T_12 = eq(ex_ctrl_mem_cmd, UInt<5>("h8")) @[package.scala 15:47]
    node _mem_reg_load_T_13 = eq(ex_ctrl_mem_cmd, UInt<5>("hc")) @[package.scala 15:47]
    node _mem_reg_load_T_14 = eq(ex_ctrl_mem_cmd, UInt<5>("hd")) @[package.scala 15:47]
    node _mem_reg_load_T_15 = eq(ex_ctrl_mem_cmd, UInt<5>("he")) @[package.scala 15:47]
    node _mem_reg_load_T_16 = eq(ex_ctrl_mem_cmd, UInt<5>("hf")) @[package.scala 15:47]
    node _mem_reg_load_T_17 = or(_mem_reg_load_T_12, _mem_reg_load_T_13) @[package.scala 72:59]
    node _mem_reg_load_T_18 = or(_mem_reg_load_T_17, _mem_reg_load_T_14) @[package.scala 72:59]
    node _mem_reg_load_T_19 = or(_mem_reg_load_T_18, _mem_reg_load_T_15) @[package.scala 72:59]
    node _mem_reg_load_T_20 = or(_mem_reg_load_T_19, _mem_reg_load_T_16) @[package.scala 72:59]
    node _mem_reg_load_T_21 = or(_mem_reg_load_T_11, _mem_reg_load_T_20) @[Consts.scala 79:44]
    node _mem_reg_load_T_22 = or(_mem_reg_load_T_4, _mem_reg_load_T_21) @[Consts.scala 81:75]
    node _mem_reg_load_T_23 = and(ex_ctrl_mem, _mem_reg_load_T_22) @[RocketCore.scala 539:33]
    node _mem_reg_store_T = eq(ex_ctrl_mem_cmd, UInt<5>("h1")) @[Consts.scala 82:32]
    node _mem_reg_store_T_1 = eq(ex_ctrl_mem_cmd, UInt<5>("h11")) @[Consts.scala 82:49]
    node _mem_reg_store_T_2 = or(_mem_reg_store_T, _mem_reg_store_T_1) @[Consts.scala 82:42]
    node _mem_reg_store_T_4 = or(_mem_reg_store_T_2, _ex_slow_bypass_T) @[Consts.scala 82:59]
    node _mem_reg_store_T_22 = or(_mem_reg_store_T_4, _mem_reg_load_T_21) @[Consts.scala 82:76]
    node _mem_reg_store_T_23 = and(ex_ctrl_mem, _mem_reg_store_T_22) @[RocketCore.scala 540:34]
    node _mem_reg_wdata_T = alu.io_out @[RocketCore.scala 552:25]
    node _T_55 = and(ex_ctrl_rxs2, ex_ctrl_mem) @[RocketCore.scala 555:24]
    node _mem_reg_rs2_T = eq(ex_reg_mem_size, UInt<2>("h0")) @[AMOALU.scala 26:19]
    node mem_reg_rs2_hi = bits(ex_rs_1, 7, 0) @[AMOALU.scala 26:66]
    node mem_reg_rs2_hi_1 = cat(mem_reg_rs2_hi, mem_reg_rs2_hi) @[Cat.scala 30:58]
    node _mem_reg_rs2_T_1 = cat(mem_reg_rs2_hi_1, mem_reg_rs2_hi_1) @[Cat.scala 30:58]
    node _mem_reg_rs2_T_2 = eq(ex_reg_mem_size, UInt<2>("h1")) @[AMOALU.scala 26:19]
    node mem_reg_rs2_hi_2 = bits(ex_rs_1, 15, 0) @[AMOALU.scala 26:66]
    node _mem_reg_rs2_T_3 = cat(mem_reg_rs2_hi_2, mem_reg_rs2_hi_2) @[Cat.scala 30:58]
    node _mem_reg_rs2_T_4 = mux(_mem_reg_rs2_T_2, _mem_reg_rs2_T_3, ex_rs_1) @[AMOALU.scala 26:13]
    node _mem_reg_rs2_T_5 = mux(_mem_reg_rs2_T, _mem_reg_rs2_T_1, _mem_reg_rs2_T_4) @[AMOALU.scala 26:13]
    node _GEN_76 = mux(_T_55, _mem_reg_rs2_T_5, mem_reg_rs2) @[RocketCore.scala 555:71 557:19 230:24]
    node _T_56 = and(ex_ctrl_jalr, csr.io_status_debug) @[RocketCore.scala 559:24]
    node _GEN_77 = or(_T_56, ex_ctrl_fence_i) @[RocketCore.scala 535:14 559:48 561:24]
    node _GEN_78 = or(_T_56, ex_reg_flush_pipe) @[RocketCore.scala 543:24 559:48 562:26]
    node _GEN_82 = mux(ex_pc_valid, ex_ctrl_branch, mem_ctrl_branch) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_83 = mux(ex_pc_valid, ex_ctrl_jal, mem_ctrl_jal) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_84 = mux(ex_pc_valid, ex_ctrl_jalr, mem_ctrl_jalr) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_85 = mux(ex_pc_valid, ex_ctrl_rxs2, mem_ctrl_rxs2) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_86 = mux(ex_pc_valid, ex_ctrl_rxs1, mem_ctrl_rxs1) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_93 = mux(ex_pc_valid, ex_ctrl_mem, mem_ctrl_mem) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_100 = mux(ex_pc_valid, ex_ctrl_div, mem_ctrl_div) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_101 = mux(ex_pc_valid, ex_ctrl_wxd, mem_ctrl_wxd) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_102 = mux(ex_pc_valid, ex_ctrl_csr, mem_ctrl_csr) @[RocketCore.scala 534:28 535:14 191:21]
    node _GEN_103 = mux(ex_pc_valid, _GEN_77, mem_ctrl_fence_i) @[RocketCore.scala 191:21 534:28]
    node _GEN_109 = mux(ex_pc_valid, ex_reg_rvc, mem_reg_rvc) @[RocketCore.scala 534:28 538:17 213:36]
    node _GEN_110 = mux(ex_pc_valid, _mem_reg_load_T_23, mem_reg_load) @[RocketCore.scala 534:28 539:18 220:36]
    node _GEN_111 = mux(ex_pc_valid, _mem_reg_store_T_23, mem_reg_store) @[RocketCore.scala 534:28 540:19 221:36]
    node _GEN_121 = mux(ex_pc_valid, _GEN_78, mem_reg_flush_pipe) @[RocketCore.scala 534:28 217:36]
    node _GEN_122 = mux(ex_pc_valid, ex_slow_bypass, mem_reg_slow_bypass) @[RocketCore.scala 534:28 544:25 219:36]
    node _GEN_124 = mux(ex_pc_valid, ex_reg_cause, mem_reg_cause) @[RocketCore.scala 534:28 547:19 218:36]
    node _GEN_125 = mux(ex_pc_valid, ex_reg_inst, mem_reg_inst) @[RocketCore.scala 534:28 548:18 224:25]
    node _GEN_126 = mux(ex_pc_valid, ex_reg_raw_inst, mem_reg_raw_inst) @[RocketCore.scala 534:28 549:22 226:29]
    node _GEN_128 = mux(ex_pc_valid, ex_reg_pc, mem_reg_pc) @[RocketCore.scala 534:28 551:16 223:23]
    node _GEN_129 = mux(ex_pc_valid, _mem_reg_wdata_T, mem_reg_wdata) @[RocketCore.scala 534:28 552:19 229:26]
    node _GEN_130 = mux(ex_pc_valid, alu.io_cmp_out, mem_br_taken) @[RocketCore.scala 534:28 553:18 231:25]
    node _GEN_131 = mux(ex_pc_valid, _GEN_76, mem_reg_rs2) @[RocketCore.scala 230:24 534:28]
    node _mem_breakpoint_T = and(mem_reg_load, bpu.io_xcpt_ld) @[RocketCore.scala 566:38]
    node _mem_breakpoint_T_1 = and(mem_reg_store, bpu.io_xcpt_st) @[RocketCore.scala 566:75]
    node mem_breakpoint = or(_mem_breakpoint_T, _mem_breakpoint_T_1) @[RocketCore.scala 566:57]
    node _mem_debug_breakpoint_T = and(mem_reg_load, bpu.io_debug_ld) @[RocketCore.scala 567:44]
    node _mem_debug_breakpoint_T_1 = and(mem_reg_store, bpu.io_debug_st) @[RocketCore.scala 567:82]
    node mem_debug_breakpoint = or(_mem_debug_breakpoint_T, _mem_debug_breakpoint_T_1) @[RocketCore.scala 567:64]
    node mem_ldst_xcpt = or(mem_debug_breakpoint, mem_breakpoint) @[RocketCore.scala 975:26]
    node mem_ldst_cause = mux(mem_debug_breakpoint, UInt<4>("he"), UInt<4>("h3")) @[Mux.scala 47:69]
    node _T_57 = or(mem_reg_xcpt_interrupt, mem_reg_xcpt) @[RocketCore.scala 573:29]
    node _T_58 = and(mem_reg_valid, mem_npc_misaligned) @[RocketCore.scala 574:20]
    node _T_59 = and(mem_reg_valid, mem_ldst_xcpt) @[RocketCore.scala 575:20]
    node _T_60 = or(_T_57, _T_58) @[RocketCore.scala 975:26]
    node mem_xcpt = or(_T_60, _T_59) @[RocketCore.scala 975:26]
    node _T_61 = mux(_T_58, UInt<4>("h0"), mem_ldst_cause) @[Mux.scala 47:69]
    node mem_cause = mux(_T_57, mem_reg_cause, pad(_T_61, 32)) @[Mux.scala 47:69]
    node dcache_kill_mem = and(_T_22, io_dmem_replay_next) @[RocketCore.scala 584:55]
    node replay_mem = or(dcache_kill_mem, mem_reg_replay) @[RocketCore.scala 586:37]
    node _killm_common_T = or(dcache_kill_mem, take_pc_wb) @[RocketCore.scala 587:38]
    node _killm_common_T_1 = or(_killm_common_T, mem_reg_xcpt) @[RocketCore.scala 587:52]
    node _killm_common_T_2 = not(mem_reg_valid) @[RocketCore.scala 587:71]
    node killm_common = or(_killm_common_T_1, _killm_common_T_2) @[RocketCore.scala 587:68]
    reg div_io_kill_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), div_io_kill_REG) @[RocketCore.scala 588:37]
    node ctrl_killm = or(killm_common, mem_xcpt) @[RocketCore.scala 589:33]
    node _wb_reg_valid_T = not(ctrl_killm) @[RocketCore.scala 592:19]
    node _wb_reg_replay_T = not(take_pc_wb) @[RocketCore.scala 593:34]
    node _T_94 = mux(_T_86, UInt<3>("h7"), UInt<3>("h5")) @[Mux.scala 47:69]
    node _T_95 = mux(_T_84, UInt<4>("hd"), pad(_T_94, 4)) @[Mux.scala 47:69]
    node _T_96 = mux(_T_82, UInt<4>("hf"), _T_95) @[Mux.scala 47:69]
    node _T_97 = mux(_T_80, UInt<4>("h4"), _T_96) @[Mux.scala 47:69]
    node _T_98 = mux(_T_78, UInt<4>("h6"), _T_97) @[Mux.scala 47:69]
    node wb_cause = mux(wb_reg_xcpt, wb_reg_cause, pad(_T_98, 32)) @[Mux.scala 47:69]
    node _T_99 = eq(wb_cause, UInt<32>("h6")) @[RocketCore.scala 979:38]
    node _T_101 = eq(wb_cause, UInt<32>("h4")) @[RocketCore.scala 979:38]
    node _T_103 = eq(wb_cause, UInt<32>("h7")) @[RocketCore.scala 979:38]
    node _T_105 = eq(wb_cause, UInt<32>("h5")) @[RocketCore.scala 979:38]
    node _csr_io_inst_0_T = bits(wb_reg_raw_inst, 1, 0) @[RocketCore.scala 687:66]
    node _csr_io_inst_0_T_1 = andr(_csr_io_inst_0_T) @[RocketCore.scala 687:73]
    node _csr_io_inst_0_T_2 = shr(wb_reg_inst, 16) @[RocketCore.scala 687:91]
    node csr_io_inst_0_hi = mux(_csr_io_inst_0_T_1, _csr_io_inst_0_T_2, UInt<16>("h0")) @[RocketCore.scala 687:50]
    node csr_io_inst_0_lo = bits(wb_reg_raw_inst, 15, 0) @[RocketCore.scala 687:119]
    node _io_fpu_time_T = csr.io_time @[RocketCore.scala 692:29]
    node _tval_valid_T = eq(wb_cause, UInt<32>("h2")) @[package.scala 15:47]
    node _tval_valid_T_1 = eq(wb_cause, UInt<32>("h3")) @[package.scala 15:47]
    node _tval_valid_T_6 = eq(wb_cause, UInt<32>("h1")) @[package.scala 15:47]
    node _tval_valid_T_7 = eq(wb_cause, UInt<32>("hd")) @[package.scala 15:47]
    node _tval_valid_T_8 = eq(wb_cause, UInt<32>("hf")) @[package.scala 15:47]
    node _tval_valid_T_9 = eq(wb_cause, UInt<32>("hc")) @[package.scala 15:47]
    node _tval_valid_T_10 = or(_tval_valid_T, _tval_valid_T_1) @[package.scala 72:59]
    node _tval_valid_T_11 = or(_tval_valid_T_10, _T_101) @[package.scala 72:59]
    node _tval_valid_T_12 = or(_tval_valid_T_11, _T_99) @[package.scala 72:59]
    node _tval_valid_T_13 = or(_tval_valid_T_12, _T_105) @[package.scala 72:59]
    node _tval_valid_T_14 = or(_tval_valid_T_13, _T_103) @[package.scala 72:59]
    node _tval_valid_T_15 = or(_tval_valid_T_14, _tval_valid_T_6) @[package.scala 72:59]
    node _tval_valid_T_16 = or(_tval_valid_T_15, _tval_valid_T_7) @[package.scala 72:59]
    node _tval_valid_T_17 = or(_tval_valid_T_16, _tval_valid_T_8) @[package.scala 72:59]
    node _tval_valid_T_18 = or(_tval_valid_T_17, _tval_valid_T_9) @[package.scala 72:59]
    node tval_valid = and(wb_xcpt, _tval_valid_T_18) @[RocketCore.scala 696:28]
    node _csr_io_rw_cmd_T = mux(wb_reg_valid, UInt<3>("h0"), UInt<3>("h4")) @[CSR.scala 132:15]
    node _csr_io_rw_cmd_T_1 = not(_csr_io_rw_cmd_T) @[CSR.scala 132:11]
    node _T_119 = dshl(UInt<1>("h1"), ll_waddr) @[RocketCore.scala 1005:62]
    node _T_120 = mux(ll_wen, _T_119, UInt<32>("h0")) @[RocketCore.scala 1005:49]
    node _T_121 = not(_T_120) @[RocketCore.scala 997:64]
    node _T_122 = and(r, _T_121) @[RocketCore.scala 997:62]
    node _GEN_242 = mux(ll_wen, _T_122, _r) @[RocketCore.scala 1009:{18,23} 1001:25]
    node _T_124 = and(wb_set_sboard, wb_wen) @[RocketCore.scala 730:28]
    node _T_125 = dshl(UInt<1>("h1"), wb_waddr) @[RocketCore.scala 1005:62]
    node _T_126 = mux(_T_124, _T_125, UInt<32>("h0")) @[RocketCore.scala 1005:49]
    node _T_127 = or(_T_122, _T_126) @[RocketCore.scala 996:60]
    node _T_128 = or(ll_wen, _T_124) @[RocketCore.scala 1008:17]
    node _GEN_243 = mux(_T_128, _T_127, _GEN_242) @[RocketCore.scala 1009:{18,23}]
    node _dcache_blocked_blocked_T_3 = and(_replay_ex_structural_T, _dcache_blocked_T) @[RocketCore.scala 765:60]
    node _dcache_blocked_blocked_T_4 = or(blocked, io_dmem_req_valid) @[RocketCore.scala 765:95]
    node _dcache_blocked_blocked_T_5 = or(_dcache_blocked_blocked_T_4, io_dmem_s2_nack) @[RocketCore.scala 765:116]
    node _io_imem_req_bits_pc_T = or(wb_xcpt, csr.io_eret) @[RocketCore.scala 789:17]
    node _io_imem_req_bits_pc_T_1 = mux(replay_wb_common, wb_reg_pc, mem_npc) @[RocketCore.scala 790:8]
    node _io_imem_flush_icache_T = and(wb_reg_valid, wb_ctrl_fence_i) @[RocketCore.scala 792:40]
    node _io_imem_flush_icache_T_1 = not(io_dmem_s2_nack) @[RocketCore.scala 792:62]
    node _io_imem_might_request_imem_might_request_reg_T = or(ex_pc_valid, mem_pc_valid) @[RocketCore.scala 794:43]
    node _io_imem_might_request_imem_might_request_reg_T_1 = bits(io_ptw_customCSRs_csrs_0_value, 1, 1) @[CustomCSRs.scala 38:61]
    node _io_imem_btb_update_valid_T_1 = and(mem_reg_valid, _wb_reg_replay_T) @[RocketCore.scala 806:45]
    node _io_imem_btb_update_valid_T_2 = and(_io_imem_btb_update_valid_T_1, mem_wrong_npc) @[RocketCore.scala 806:60]
    node _io_imem_btb_update_valid_T_3 = not(mem_cfi) @[RocketCore.scala 806:81]
    node _io_imem_btb_update_valid_T_4 = or(_io_imem_btb_update_valid_T_3, mem_cfi_taken) @[RocketCore.scala 806:90]
    node ex_dcache_tag = cat(ex_waddr, UInt<1>("h0")) @[Cat.scala 30:58]
    node _io_dmem_req_bits_signed_T = bits(ex_reg_inst, 14, 14) @[RocketCore.scala 842:42]
    node _io_rocc_cmd_bits_inst_T_5 = bits(wb_reg_inst, 19, 15) @[RocketCore.scala 856:58]
    node _io_rocc_cmd_bits_inst_T_6 = bits(wb_reg_inst, 24, 20) @[RocketCore.scala 856:58]
    node _unpause_T = bits(csr.io_time, 4, 0) @[RocketCore.scala 861:28]
    node _unpause_T_1 = eq(_unpause_T, UInt<5>("h0")) @[RocketCore.scala 861:62]
    node _unpause_T_2 = or(_unpause_T_1, csr.io_inhibit_cycle) @[RocketCore.scala 861:68]
    node unpause = or(_unpause_T_2, take_pc_mem_wb) @[RocketCore.scala 861:116]
    node _coreMonitorBundle_valid_T = not(csr.io_trace_0_exception) @[RocketCore.scala 889:55]
    node coreMonitorBundle_valid = and(csr.io_trace_0_valid, _coreMonitorBundle_valid_T) @[RocketCore.scala 889:52]
    node coreMonitorBundle_pc = csr.io_trace_0_iaddr @[RocketCore.scala 890:48]
    node _coreMonitorBundle_wrenx_T = not(wb_set_sboard) @[RocketCore.scala 891:40]
    node coreMonitorBundle_wrenx = and(wb_wen, _coreMonitorBundle_wrenx_T) @[RocketCore.scala 891:37]
    reg coreMonitorBundle_rd0val_x20 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), coreMonitorBundle_rd0val_x20) @[RocketCore.scala 896:43]
    reg coreMonitorBundle_rd0val_REG : UInt<32>, clock with :
      reset => (UInt<1>("h0"), coreMonitorBundle_rd0val_REG) @[RocketCore.scala 896:34]
    reg coreMonitorBundle_rd1val_x26 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), coreMonitorBundle_rd1val_x26) @[RocketCore.scala 898:43]
    reg coreMonitorBundle_rd1val_REG : UInt<32>, clock with :
      reset => (UInt<1>("h0"), coreMonitorBundle_rd1val_REG) @[RocketCore.scala 898:34]
    node _T_130 = mux(wb_ctrl_wxd, wb_waddr, UInt<5>("h0")) @[RocketCore.scala 934:13]
    node _T_131 = mux(coreMonitorBundle_wrenx, rf_wdata, UInt<32>("h0")) @[RocketCore.scala 935:13]
    node _T_133 = mux(wb_ctrl_rxs1, _io_rocc_cmd_bits_inst_T_5, UInt<5>("h0")) @[RocketCore.scala 937:13]
    node _T_135 = mux(wb_ctrl_rxs1, coreMonitorBundle_rd0val_REG, UInt<32>("h0")) @[RocketCore.scala 938:13]
    node _T_137 = mux(wb_ctrl_rxs2, _io_rocc_cmd_bits_inst_T_6, UInt<5>("h0")) @[RocketCore.scala 939:13]
    node _T_139 = mux(wb_ctrl_rxs2, coreMonitorBundle_rd1val_REG, UInt<32>("h0")) @[RocketCore.scala 940:13]
    node _T_141 = not(reset) @[RocketCore.scala 931:13]
    node coreMonitorBundle_inst = csr.io_trace_0_insn @[RocketCore.scala 883:31 899:26]
    io_imem_might_request <= imem_might_request_reg @[RocketCore.scala 793:25]
    io_imem_req_valid <= or(take_pc_wb, take_pc_mem) @[RocketCore.scala 250:35]
    io_imem_req_bits_pc <= mux(_io_imem_req_bits_pc_T, csr.io_evec, _io_imem_req_bits_pc_T_1) @[RocketCore.scala 789:8]
    io_imem_req_bits_speculative <= not(take_pc_wb) @[RocketCore.scala 787:35]
    io_imem_resp_ready <= ibuf.io_imem_ready @[RocketCore.scala 258:16]
    io_imem_btb_update_valid <= and(_io_imem_btb_update_valid_T_2, _io_imem_btb_update_valid_T_4) @[RocketCore.scala 806:77]
    io_imem_bht_update_valid <= and(mem_reg_valid, _wb_reg_replay_T) @[RocketCore.scala 818:45]
    io_imem_flush_icache <= and(_io_imem_flush_icache_T, _io_imem_flush_icache_T_1) @[RocketCore.scala 792:59]
    io_dmem_req_valid <= and(ex_reg_valid, ex_ctrl_mem) @[RocketCore.scala 836:41]
    io_dmem_req_bits_addr <= alu.io_adder_out @[RocketCore.scala 844:25]
    io_dmem_req_bits_tag <= pad(ex_dcache_tag, 7) @[RocketCore.scala 839:25]
    io_dmem_req_bits_cmd <= ex_ctrl_mem_cmd @[RocketCore.scala 840:25]
    io_dmem_req_bits_size <= ex_reg_mem_size @[RocketCore.scala 841:25]
    io_dmem_req_bits_signed <= not(_io_dmem_req_bits_signed_T) @[RocketCore.scala 842:30]
    io_dmem_s1_kill <= or(killm_common, mem_ldst_xcpt) @[RocketCore.scala 848:35]
    io_dmem_s1_data_data <= mem_reg_rs2 @[RocketCore.scala 847:24]
    io_ptw_status_debug <= csr.io_status_debug @[RocketCore.scala 703:17]
    io_ptw_pmp_0_cfg_l <= csr.io_pmp_0_cfg_l @[RocketCore.scala 704:14]
    io_ptw_pmp_0_cfg_a <= csr.io_pmp_0_cfg_a @[RocketCore.scala 704:14]
    io_ptw_pmp_0_cfg_x <= csr.io_pmp_0_cfg_x @[RocketCore.scala 704:14]
    io_ptw_pmp_0_cfg_w <= csr.io_pmp_0_cfg_w @[RocketCore.scala 704:14]
    io_ptw_pmp_0_cfg_r <= csr.io_pmp_0_cfg_r @[RocketCore.scala 704:14]
    io_ptw_pmp_0_addr <= csr.io_pmp_0_addr @[RocketCore.scala 704:14]
    io_ptw_pmp_0_mask <= csr.io_pmp_0_mask @[RocketCore.scala 704:14]
    io_ptw_pmp_1_cfg_l <= csr.io_pmp_1_cfg_l @[RocketCore.scala 704:14]
    io_ptw_pmp_1_cfg_a <= csr.io_pmp_1_cfg_a @[RocketCore.scala 704:14]
    io_ptw_pmp_1_cfg_x <= csr.io_pmp_1_cfg_x @[RocketCore.scala 704:14]
    io_ptw_pmp_1_cfg_w <= csr.io_pmp_1_cfg_w @[RocketCore.scala 704:14]
    io_ptw_pmp_1_cfg_r <= csr.io_pmp_1_cfg_r @[RocketCore.scala 704:14]
    io_ptw_pmp_1_addr <= csr.io_pmp_1_addr @[RocketCore.scala 704:14]
    io_ptw_pmp_1_mask <= csr.io_pmp_1_mask @[RocketCore.scala 704:14]
    io_ptw_pmp_2_cfg_l <= csr.io_pmp_2_cfg_l @[RocketCore.scala 704:14]
    io_ptw_pmp_2_cfg_a <= csr.io_pmp_2_cfg_a @[RocketCore.scala 704:14]
    io_ptw_pmp_2_cfg_x <= csr.io_pmp_2_cfg_x @[RocketCore.scala 704:14]
    io_ptw_pmp_2_cfg_w <= csr.io_pmp_2_cfg_w @[RocketCore.scala 704:14]
    io_ptw_pmp_2_cfg_r <= csr.io_pmp_2_cfg_r @[RocketCore.scala 704:14]
    io_ptw_pmp_2_addr <= csr.io_pmp_2_addr @[RocketCore.scala 704:14]
    io_ptw_pmp_2_mask <= csr.io_pmp_2_mask @[RocketCore.scala 704:14]
    io_ptw_pmp_3_cfg_l <= csr.io_pmp_3_cfg_l @[RocketCore.scala 704:14]
    io_ptw_pmp_3_cfg_a <= csr.io_pmp_3_cfg_a @[RocketCore.scala 704:14]
    io_ptw_pmp_3_cfg_x <= csr.io_pmp_3_cfg_x @[RocketCore.scala 704:14]
    io_ptw_pmp_3_cfg_w <= csr.io_pmp_3_cfg_w @[RocketCore.scala 704:14]
    io_ptw_pmp_3_cfg_r <= csr.io_pmp_3_cfg_r @[RocketCore.scala 704:14]
    io_ptw_pmp_3_addr <= csr.io_pmp_3_addr @[RocketCore.scala 704:14]
    io_ptw_pmp_3_mask <= csr.io_pmp_3_mask @[RocketCore.scala 704:14]
    io_ptw_pmp_4_cfg_l <= csr.io_pmp_4_cfg_l @[RocketCore.scala 704:14]
    io_ptw_pmp_4_cfg_a <= csr.io_pmp_4_cfg_a @[RocketCore.scala 704:14]
    io_ptw_pmp_4_cfg_x <= csr.io_pmp_4_cfg_x @[RocketCore.scala 704:14]
    io_ptw_pmp_4_cfg_w <= csr.io_pmp_4_cfg_w @[RocketCore.scala 704:14]
    io_ptw_pmp_4_cfg_r <= csr.io_pmp_4_cfg_r @[RocketCore.scala 704:14]
    io_ptw_pmp_4_addr <= csr.io_pmp_4_addr @[RocketCore.scala 704:14]
    io_ptw_pmp_4_mask <= csr.io_pmp_4_mask @[RocketCore.scala 704:14]
    io_ptw_pmp_5_cfg_l <= csr.io_pmp_5_cfg_l @[RocketCore.scala 704:14]
    io_ptw_pmp_5_cfg_a <= csr.io_pmp_5_cfg_a @[RocketCore.scala 704:14]
    io_ptw_pmp_5_cfg_x <= csr.io_pmp_5_cfg_x @[RocketCore.scala 704:14]
    io_ptw_pmp_5_cfg_w <= csr.io_pmp_5_cfg_w @[RocketCore.scala 704:14]
    io_ptw_pmp_5_cfg_r <= csr.io_pmp_5_cfg_r @[RocketCore.scala 704:14]
    io_ptw_pmp_5_addr <= csr.io_pmp_5_addr @[RocketCore.scala 704:14]
    io_ptw_pmp_5_mask <= csr.io_pmp_5_mask @[RocketCore.scala 704:14]
    io_ptw_pmp_6_cfg_l <= csr.io_pmp_6_cfg_l @[RocketCore.scala 704:14]
    io_ptw_pmp_6_cfg_a <= csr.io_pmp_6_cfg_a @[RocketCore.scala 704:14]
    io_ptw_pmp_6_cfg_x <= csr.io_pmp_6_cfg_x @[RocketCore.scala 704:14]
    io_ptw_pmp_6_cfg_w <= csr.io_pmp_6_cfg_w @[RocketCore.scala 704:14]
    io_ptw_pmp_6_cfg_r <= csr.io_pmp_6_cfg_r @[RocketCore.scala 704:14]
    io_ptw_pmp_6_addr <= csr.io_pmp_6_addr @[RocketCore.scala 704:14]
    io_ptw_pmp_6_mask <= csr.io_pmp_6_mask @[RocketCore.scala 704:14]
    io_ptw_pmp_7_cfg_l <= csr.io_pmp_7_cfg_l @[RocketCore.scala 704:14]
    io_ptw_pmp_7_cfg_a <= csr.io_pmp_7_cfg_a @[RocketCore.scala 704:14]
    io_ptw_pmp_7_cfg_x <= csr.io_pmp_7_cfg_x @[RocketCore.scala 704:14]
    io_ptw_pmp_7_cfg_w <= csr.io_pmp_7_cfg_w @[RocketCore.scala 704:14]
    io_ptw_pmp_7_cfg_r <= csr.io_pmp_7_cfg_r @[RocketCore.scala 704:14]
    io_ptw_pmp_7_addr <= csr.io_pmp_7_addr @[RocketCore.scala 704:14]
    io_ptw_pmp_7_mask <= csr.io_pmp_7_mask @[RocketCore.scala 704:14]
    io_ptw_customCSRs_csrs_0_value <= csr.io_customCSRs_0_value @[RocketCore.scala 702:79]
    io_wfi <= csr.io_status_wfi @[RocketCore.scala 864:10]
    ibuf.clock <= clock
    ibuf.reset <= reset
    ibuf.io_imem_valid <= io_imem_resp_valid @[RocketCore.scala 258:16]
    ibuf.io_imem_bits_pc <= io_imem_resp_bits_pc @[RocketCore.scala 258:16]
    ibuf.io_imem_bits_data <= io_imem_resp_bits_data @[RocketCore.scala 258:16]
    ibuf.io_imem_bits_xcpt_ae_inst <= io_imem_resp_bits_xcpt_ae_inst @[RocketCore.scala 258:16]
    ibuf.io_imem_bits_replay <= io_imem_resp_bits_replay @[RocketCore.scala 258:16]
    ibuf.io_kill <= or(take_pc_wb, take_pc_mem) @[RocketCore.scala 250:35]
    ibuf.io_inst_0_ready <= not(ctrl_stalld) @[RocketCore.scala 804:28]
    rf.id_rs_MPORT.addr <= not(id_raddr1) @[RocketCore.scala 1016:39]
    rf.id_rs_MPORT.en <= UInt<1>("h1") @[RocketCore.scala 1016:38]
    rf.id_rs_MPORT.clk <= clock @[RocketCore.scala 1016:38]
    rf.id_rs_MPORT_1.addr <= not(id_raddr2) @[RocketCore.scala 1016:39]
    rf.id_rs_MPORT_1.en <= UInt<1>("h1") @[RocketCore.scala 1016:38]
    rf.id_rs_MPORT_1.clk <= clock @[RocketCore.scala 1016:38]
    rf.MPORT.addr <= not(rf_waddr) @[RocketCore.scala 1016:39]
    rf.MPORT.en <= and(rf_wen, _T_108) @[RocketCore.scala 1015:15 679:17]
    rf.MPORT.clk <= clock @[RocketCore.scala 1027:29 1016:38]
    rf.MPORT.data <= mux(_rf_wdata_T, io_dmem_resp_bits_data, _rf_wdata_T_5) @[RocketCore.scala 674:21]
    rf.MPORT.mask <= UInt<1>("h1") @[RocketCore.scala 1027:29 1028:20]
    csr.clock <= clock
    csr.reset <= reset
    csr.io_ungated_clock <= clock @[RocketCore.scala 682:24]
    csr.io_interrupts_debug <= io_interrupts_debug @[RocketCore.scala 688:21]
    csr.io_interrupts_mtip <= io_interrupts_mtip @[RocketCore.scala 688:21]
    csr.io_interrupts_msip <= io_interrupts_msip @[RocketCore.scala 688:21]
    csr.io_interrupts_meip <= io_interrupts_meip @[RocketCore.scala 688:21]
    csr.io_hartid <= io_hartid @[RocketCore.scala 689:17]
    csr.io_rw_addr <= bits(wb_reg_inst, 31, 20) @[RocketCore.scala 705:32]
    csr.io_rw_cmd <= and(wb_ctrl_csr, _csr_io_rw_cmd_T_1) @[CSR.scala 132:9]
    csr.io_rw_wdata <= wb_reg_wdata @[RocketCore.scala 707:19]
    csr.io_decode_0_csr <= bits(ibuf.io_inst_0_bits_raw, 31, 20) @[RocketCore.scala 683:41]
    csr.io_exception <= or(_T_93, _T_88) @[RocketCore.scala 975:26]
    csr.io_retire <= and(_wb_valid_T_1, _wb_valid_T_2) @[RocketCore.scala 670:45]
    csr.io_cause <= mux(wb_reg_xcpt, wb_reg_cause, pad(_T_98, 32)) @[Mux.scala 47:69]
    csr.io_pc <= wb_reg_pc @[RocketCore.scala 695:13]
    csr.io_tval <= mux(tval_valid, wb_reg_wdata, UInt<32>("h0")) @[RocketCore.scala 700:21]
    csr.io_inst_0 <= cat(csr_io_inst_0_hi, csr_io_inst_0_lo) @[Cat.scala 30:58]
    bpu.io_status_debug <= csr.io_status_debug @[RocketCore.scala 324:17]
    bpu.io_bp_0_control_action <= csr.io_bp_0_control_action @[RocketCore.scala 325:13]
    bpu.io_bp_0_control_tmatch <= csr.io_bp_0_control_tmatch @[RocketCore.scala 325:13]
    bpu.io_bp_0_control_x <= csr.io_bp_0_control_x @[RocketCore.scala 325:13]
    bpu.io_bp_0_control_w <= csr.io_bp_0_control_w @[RocketCore.scala 325:13]
    bpu.io_bp_0_control_r <= csr.io_bp_0_control_r @[RocketCore.scala 325:13]
    bpu.io_bp_0_address <= csr.io_bp_0_address @[RocketCore.scala 325:13]
    bpu.io_pc <= ibuf.io_pc @[RocketCore.scala 326:13]
    bpu.io_ea <= mem_reg_wdata @[RocketCore.scala 327:13]
    alu.io_fn <= ex_ctrl_alu_fn @[RocketCore.scala 387:13]
    alu.io_in2 <= asUInt(ex_op2) @[RocketCore.scala 388:24]
    alu.io_in1 <= asUInt(ex_op1) @[RocketCore.scala 389:24]
    div.clock <= clock
    div.reset <= reset
    div.io_req_valid <= and(ex_reg_valid, ex_ctrl_div) @[RocketCore.scala 410:36]
    div.io_req_bits_fn <= ex_ctrl_alu_fn @[RocketCore.scala 412:22]
    div.io_req_bits_in1 <= mux(ex_reg_rs_bypass_0, _ex_rs_T_5, _ex_rs_T_6) @[RocketCore.scala 375:14]
    div.io_req_bits_in2 <= mux(ex_reg_rs_bypass_1, _ex_rs_T_12, _ex_rs_T_13) @[RocketCore.scala 375:14]
    div.io_req_bits_tag <= bits(ex_reg_inst, 11, 7) @[RocketCore.scala 359:29]
    div.io_kill <= and(killm_common, div_io_kill_REG) @[RocketCore.scala 588:31]
    div.io_resp_ready <= mux(_T_107, UInt<1>("h0"), _ctrl_stalld_T_17) @[RocketCore.scala 649:21 662:44 663:23]
    id_reg_pause <= mux(unpause, UInt<1>("h0"), _GEN_52) @[RocketCore.scala 862:{18,33}]
    imem_might_request_reg <= or(_io_imem_might_request_imem_might_request_reg_T, _io_imem_might_request_imem_might_request_reg_T_1) @[RocketCore.scala 794:59]
    ex_ctrl_branch <= mux(_ex_reg_valid_T, id_ctrl_decoder_3, ex_ctrl_branch) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_jal <= mux(_ex_reg_valid_T, id_ctrl_decoder_4, ex_ctrl_jal) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_jalr <= mux(_ex_reg_valid_T, id_ctrl_decoder_5, ex_ctrl_jalr) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_rxs2 <= mux(_ex_reg_valid_T, id_ctrl_decoder_6, ex_ctrl_rxs2) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_rxs1 <= mux(_ex_reg_valid_T, id_ctrl_decoder_7, ex_ctrl_rxs1) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_sel_alu2 <= mux(_ex_reg_valid_T, _GEN_11, ex_ctrl_sel_alu2) @[RocketCore.scala 190:20 428:22]
    ex_ctrl_sel_alu1 <= mux(_ex_reg_valid_T, _GEN_10, ex_ctrl_sel_alu1) @[RocketCore.scala 190:20 428:22]
    ex_ctrl_sel_imm <= mux(_ex_reg_valid_T, id_ctrl_decoder_11, ex_ctrl_sel_imm) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_alu_fn <= mux(_ex_reg_valid_T, _GEN_8, ex_ctrl_alu_fn) @[RocketCore.scala 190:20 428:22]
    ex_ctrl_mem <= mux(_ex_reg_valid_T, id_ctrl_decoder_14, ex_ctrl_mem) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_mem_cmd <= mux(_ex_reg_valid_T, id_ctrl_decoder_15, ex_ctrl_mem_cmd) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_div <= mux(_ex_reg_valid_T, id_ctrl_decoder_21, ex_ctrl_div) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_wxd <= mux(_ex_reg_valid_T, id_ctrl_decoder_22, ex_ctrl_wxd) @[RocketCore.scala 428:22 429:13 190:20]
    ex_ctrl_csr <= mux(_ex_reg_valid_T, id_csr, ex_ctrl_csr) @[RocketCore.scala 428:22 431:17 190:20]
    ex_ctrl_fence_i <= mux(_ex_reg_valid_T, id_ctrl_decoder_24, ex_ctrl_fence_i) @[RocketCore.scala 428:22 429:13 190:20]
    mem_ctrl_branch <= mux(_T_52, mem_ctrl_branch, _GEN_82) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_jal <= mux(_T_52, mem_ctrl_jal, _GEN_83) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_jalr <= mux(_T_52, mem_ctrl_jalr, _GEN_84) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_rxs2 <= mux(_T_52, mem_ctrl_rxs2, _GEN_85) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_rxs1 <= mux(_T_52, mem_ctrl_rxs1, _GEN_86) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_mem <= mux(_T_52, mem_ctrl_mem, _GEN_93) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_div <= mux(_T_52, mem_ctrl_div, _GEN_100) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_wxd <= mux(_T_52, mem_ctrl_wxd, _GEN_101) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_csr <= mux(_T_52, mem_ctrl_csr, _GEN_102) @[RocketCore.scala 191:21 532:46]
    mem_ctrl_fence_i <= mux(_T_52, mem_ctrl_fence_i, _GEN_103) @[RocketCore.scala 191:21 532:46]
    wb_ctrl_rxs2 <= mux(mem_pc_valid, mem_ctrl_rxs2, wb_ctrl_rxs2) @[RocketCore.scala 596:23 597:13 192:20]
    wb_ctrl_rxs1 <= mux(mem_pc_valid, mem_ctrl_rxs1, wb_ctrl_rxs1) @[RocketCore.scala 596:23 597:13 192:20]
    wb_ctrl_mem <= mux(mem_pc_valid, mem_ctrl_mem, wb_ctrl_mem) @[RocketCore.scala 596:23 597:13 192:20]
    wb_ctrl_div <= mux(mem_pc_valid, mem_ctrl_div, wb_ctrl_div) @[RocketCore.scala 596:23 597:13 192:20]
    wb_ctrl_wxd <= mux(mem_pc_valid, mem_ctrl_wxd, wb_ctrl_wxd) @[RocketCore.scala 596:23 597:13 192:20]
    wb_ctrl_csr <= mux(mem_pc_valid, mem_ctrl_csr, wb_ctrl_csr) @[RocketCore.scala 596:23 597:13 192:20]
    wb_ctrl_fence_i <= mux(mem_pc_valid, mem_ctrl_fence_i, wb_ctrl_fence_i) @[RocketCore.scala 596:23 597:13 192:20]
    ex_reg_xcpt_interrupt <= and(_ex_reg_replay_T_1, csr.io_interrupt) @[RocketCore.scala 426:62]
    ex_reg_valid <= not(ctrl_killd) @[RocketCore.scala 423:19]
    ex_reg_rvc <= mux(_ex_reg_valid_T, _GEN_12, ex_reg_rvc) @[RocketCore.scala 428:22 196:35]
    ex_reg_xcpt <= and(_ex_reg_valid_T, id_xcpt) @[RocketCore.scala 425:30]
    ex_reg_flush_pipe <= mux(_ex_reg_valid_T, _ex_reg_flush_pipe_T, ex_reg_flush_pipe) @[RocketCore.scala 428:22 451:23 199:35]
    ex_reg_load_use <= mux(_ex_reg_valid_T, id_load_use, ex_reg_load_use) @[RocketCore.scala 428:22 452:21 200:35]
    ex_reg_cause <= mux(_T_43, id_cause, ex_reg_cause) @[RocketCore.scala 480:73 481:18 201:35]
    ex_reg_replay <= and(_ex_reg_replay_T_1, ibuf.io_inst_0_bits_replay) @[RocketCore.scala 424:54]
    ex_reg_pc <= mux(_T_43, ibuf.io_pc, ex_reg_pc) @[RocketCore.scala 480:73 484:15 203:22]
    ex_reg_mem_size <= mux(_ex_reg_valid_T, _GEN_13, ex_reg_mem_size) @[RocketCore.scala 428:22 204:28]
    ex_reg_inst <= mux(_T_43, ibuf.io_inst_0_bits_inst_bits, ex_reg_inst) @[RocketCore.scala 480:73 482:17 205:24]
    ex_reg_raw_inst <= mux(_T_43, ibuf.io_inst_0_bits_raw, ex_reg_raw_inst) @[RocketCore.scala 480:73 483:21 206:28]
    mem_reg_xcpt_interrupt <= and(_ex_reg_replay_T, ex_reg_xcpt_interrupt) @[RocketCore.scala 528:45]
    mem_reg_valid <= not(ctrl_killx) @[RocketCore.scala 525:20]
    mem_reg_rvc <= mux(_T_52, mem_reg_rvc, _GEN_109) @[RocketCore.scala 213:36 532:46]
    mem_reg_xcpt <= and(_mem_reg_valid_T, ex_xcpt) @[RocketCore.scala 527:31]
    mem_reg_replay <= and(_ex_reg_replay_T, replay_ex) @[RocketCore.scala 526:37]
    mem_reg_flush_pipe <= mux(_T_52, mem_reg_flush_pipe, _GEN_121) @[RocketCore.scala 217:36 532:46]
    mem_reg_cause <= mux(_T_52, mem_reg_cause, _GEN_124) @[RocketCore.scala 218:36 532:46]
    mem_reg_slow_bypass <= mux(_T_52, mem_reg_slow_bypass, _GEN_122) @[RocketCore.scala 219:36 532:46]
    mem_reg_load <= mux(_T_52, mem_reg_load, _GEN_110) @[RocketCore.scala 220:36 532:46]
    mem_reg_store <= mux(_T_52, mem_reg_store, _GEN_111) @[RocketCore.scala 221:36 532:46]
    mem_reg_pc <= mux(_T_52, mem_reg_pc, _GEN_128) @[RocketCore.scala 223:23 532:46]
    mem_reg_inst <= mux(_T_52, mem_reg_inst, _GEN_125) @[RocketCore.scala 224:25 532:46]
    mem_reg_raw_inst <= mux(_T_52, mem_reg_raw_inst, _GEN_126) @[RocketCore.scala 226:29 532:46]
    mem_reg_wdata <= mux(_T_52, mem_reg_wdata, _GEN_129) @[RocketCore.scala 229:26 532:46]
    mem_reg_rs2 <= mux(_T_52, mem_reg_rs2, _GEN_131) @[RocketCore.scala 230:24 532:46]
    mem_br_taken <= mux(_T_52, mem_br_taken, _GEN_130) @[RocketCore.scala 231:25 532:46]
    wb_reg_valid <= not(ctrl_killm) @[RocketCore.scala 592:19]
    wb_reg_xcpt <= and(mem_xcpt, _wb_reg_replay_T) @[RocketCore.scala 594:27]
    wb_reg_replay <= and(replay_mem, _wb_reg_replay_T) @[RocketCore.scala 593:31]
    wb_reg_flush_pipe <= and(_wb_reg_valid_T, mem_reg_flush_pipe) @[RocketCore.scala 595:36]
    wb_reg_cause <= mux(mem_pc_valid, mem_cause, wb_reg_cause) @[RocketCore.scala 596:23 604:18 239:35]
    wb_reg_pc <= mux(mem_pc_valid, mem_reg_pc, wb_reg_pc) @[RocketCore.scala 596:23 608:15 241:22]
    wb_reg_inst <= mux(mem_pc_valid, mem_reg_inst, wb_reg_inst) @[RocketCore.scala 596:23 605:17 243:24]
    wb_reg_raw_inst <= mux(mem_pc_valid, mem_reg_raw_inst, wb_reg_raw_inst) @[RocketCore.scala 596:23 606:21 244:28]
    wb_reg_wdata <= mux(mem_pc_valid, mem_int_wdata, wb_reg_wdata) @[RocketCore.scala 596:23 599:18 245:25]
    id_reg_fence <= mux(reset, UInt<1>("h0"), _GEN_53) @[RocketCore.scala 274:{25,25}]
    ex_reg_rs_bypass_0 <= mux(_ex_reg_valid_T, _GEN_18, ex_reg_rs_bypass_0) @[RocketCore.scala 428:22 371:29]
    ex_reg_rs_bypass_1 <= mux(_ex_reg_valid_T, do_bypass_1, ex_reg_rs_bypass_1) @[RocketCore.scala 428:22 466:27 371:29]
    ex_reg_rs_lsb_0 <= mux(_ex_reg_valid_T, _GEN_19, ex_reg_rs_lsb_0) @[RocketCore.scala 428:22 372:26]
    ex_reg_rs_lsb_1 <= mux(_ex_reg_valid_T, _GEN_16, ex_reg_rs_lsb_1) @[RocketCore.scala 428:22 372:26]
    ex_reg_rs_msb_0 <= mux(_ex_reg_valid_T, _GEN_20, ex_reg_rs_msb_0) @[RocketCore.scala 428:22 373:26]
    ex_reg_rs_msb_1 <= mux(_ex_reg_valid_T, _GEN_17, ex_reg_rs_msb_1) @[RocketCore.scala 428:22 373:26]
    _r <= mux(reset, UInt<32>("h0"), _GEN_243) @[RocketCore.scala 1001:{25,25}]
    blocked <= and(_dcache_blocked_blocked_T_3, _dcache_blocked_blocked_T_5) @[RocketCore.scala 765:83]
    div_io_kill_REG <= and(div.io_req_ready, div.io_req_valid) @[Decoupled.scala 40:37]
    coreMonitorBundle_rd0val_x20 <= mux(ex_reg_rs_bypass_0, _ex_rs_T_5, _ex_rs_T_6) @[RocketCore.scala 375:14]
    coreMonitorBundle_rd0val_REG <= coreMonitorBundle_rd0val_x20 @[RocketCore.scala 896:34]
    coreMonitorBundle_rd1val_x26 <= mux(ex_reg_rs_bypass_1, _ex_rs_T_12, _ex_rs_T_13) @[RocketCore.scala 375:14]
    coreMonitorBundle_rd1val_REG <= coreMonitorBundle_rd1val_x26 @[RocketCore.scala 898:34]
    printf(clock, and(csr.io_trace_0_valid, _T_141), "C%d: %d [%d] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x)\n", io_hartid, _io_fpu_time_T, coreMonitorBundle_valid, coreMonitorBundle_pc, _T_130, _T_131, coreMonitorBundle_wrenx, _T_133, _T_135, _T_137, _T_139, coreMonitorBundle_inst, coreMonitorBundle_inst) @[RocketCore.scala 931:13]

  module TLB :
    input io_req_valid : UInt<1>
    input io_req_bits_vaddr : UInt<32>
    input io_req_bits_size : UInt<2>
    input io_req_bits_cmd : UInt<5>
    output io_resp_paddr : UInt<32>
    output io_resp_pf_ld : UInt<1>
    output io_resp_pf_st : UInt<1>
    output io_resp_ae_ld : UInt<1>
    output io_resp_ae_st : UInt<1>
    output io_resp_ma_ld : UInt<1>
    output io_resp_ma_st : UInt<1>
    input io_ptw_status_debug : UInt<1>
    input io_ptw_status_dprv : UInt<2>
    input io_ptw_pmp_0_cfg_l : UInt<1>
    input io_ptw_pmp_0_cfg_a : UInt<2>
    input io_ptw_pmp_0_cfg_x : UInt<1>
    input io_ptw_pmp_0_cfg_w : UInt<1>
    input io_ptw_pmp_0_cfg_r : UInt<1>
    input io_ptw_pmp_0_addr : UInt<30>
    input io_ptw_pmp_0_mask : UInt<32>
    input io_ptw_pmp_1_cfg_l : UInt<1>
    input io_ptw_pmp_1_cfg_a : UInt<2>
    input io_ptw_pmp_1_cfg_x : UInt<1>
    input io_ptw_pmp_1_cfg_w : UInt<1>
    input io_ptw_pmp_1_cfg_r : UInt<1>
    input io_ptw_pmp_1_addr : UInt<30>
    input io_ptw_pmp_1_mask : UInt<32>
    input io_ptw_pmp_2_cfg_l : UInt<1>
    input io_ptw_pmp_2_cfg_a : UInt<2>
    input io_ptw_pmp_2_cfg_x : UInt<1>
    input io_ptw_pmp_2_cfg_w : UInt<1>
    input io_ptw_pmp_2_cfg_r : UInt<1>
    input io_ptw_pmp_2_addr : UInt<30>
    input io_ptw_pmp_2_mask : UInt<32>
    input io_ptw_pmp_3_cfg_l : UInt<1>
    input io_ptw_pmp_3_cfg_a : UInt<2>
    input io_ptw_pmp_3_cfg_x : UInt<1>
    input io_ptw_pmp_3_cfg_w : UInt<1>
    input io_ptw_pmp_3_cfg_r : UInt<1>
    input io_ptw_pmp_3_addr : UInt<30>
    input io_ptw_pmp_3_mask : UInt<32>
    input io_ptw_pmp_4_cfg_l : UInt<1>
    input io_ptw_pmp_4_cfg_a : UInt<2>
    input io_ptw_pmp_4_cfg_x : UInt<1>
    input io_ptw_pmp_4_cfg_w : UInt<1>
    input io_ptw_pmp_4_cfg_r : UInt<1>
    input io_ptw_pmp_4_addr : UInt<30>
    input io_ptw_pmp_4_mask : UInt<32>
    input io_ptw_pmp_5_cfg_l : UInt<1>
    input io_ptw_pmp_5_cfg_a : UInt<2>
    input io_ptw_pmp_5_cfg_x : UInt<1>
    input io_ptw_pmp_5_cfg_w : UInt<1>
    input io_ptw_pmp_5_cfg_r : UInt<1>
    input io_ptw_pmp_5_addr : UInt<30>
    input io_ptw_pmp_5_mask : UInt<32>
    input io_ptw_pmp_6_cfg_l : UInt<1>
    input io_ptw_pmp_6_cfg_a : UInt<2>
    input io_ptw_pmp_6_cfg_x : UInt<1>
    input io_ptw_pmp_6_cfg_w : UInt<1>
    input io_ptw_pmp_6_cfg_r : UInt<1>
    input io_ptw_pmp_6_addr : UInt<30>
    input io_ptw_pmp_6_mask : UInt<32>
    input io_ptw_pmp_7_cfg_l : UInt<1>
    input io_ptw_pmp_7_cfg_a : UInt<2>
    input io_ptw_pmp_7_cfg_x : UInt<1>
    input io_ptw_pmp_7_cfg_w : UInt<1>
    input io_ptw_pmp_7_cfg_r : UInt<1>
    input io_ptw_pmp_7_addr : UInt<30>
    input io_ptw_pmp_7_mask : UInt<32>

    inst pmp of PMPChecker @[TLB.scala 193:19]
    node vpn = bits(io_req_bits_vaddr, 31, 12) @[TLB.scala 163:30]
    node mpu_ppn = shr(io_req_bits_vaddr, 12) @[TLB.scala 190:123]
    node mpu_physaddr_lo = bits(io_req_bits_vaddr, 11, 0) @[TLB.scala 191:52]
    node mpu_physaddr = cat(mpu_ppn, mpu_physaddr_lo) @[Cat.scala 30:58]
    node mpu_priv = cat(io_ptw_status_debug, io_ptw_status_dprv) @[Cat.scala 30:58]
    node _legal_address_T = xor(mpu_physaddr, UInt<32>("h3000")) @[Parameters.scala 137:31]
    node _legal_address_T_1 = cvt(_legal_address_T) @[Parameters.scala 137:49]
    node _legal_address_T_2 = and(_legal_address_T_1, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_3 = asSInt(_legal_address_T_2) @[Parameters.scala 137:52]
    node _legal_address_T_4 = eq(_legal_address_T_3, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_5 = xor(mpu_physaddr, UInt<32>("h4000")) @[Parameters.scala 137:31]
    node _legal_address_T_6 = cvt(_legal_address_T_5) @[Parameters.scala 137:49]
    node _legal_address_T_7 = and(_legal_address_T_6, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_8 = asSInt(_legal_address_T_7) @[Parameters.scala 137:52]
    node _legal_address_T_9 = eq(_legal_address_T_8, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_10 = xor(mpu_physaddr, UInt<32>("h10000000")) @[Parameters.scala 137:31]
    node _legal_address_T_11 = cvt(_legal_address_T_10) @[Parameters.scala 137:49]
    node _legal_address_T_12 = and(_legal_address_T_11, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_13 = asSInt(_legal_address_T_12) @[Parameters.scala 137:52]
    node _legal_address_T_14 = eq(_legal_address_T_13, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_15 = xor(mpu_physaddr, UInt<32>("h20000")) @[Parameters.scala 137:31]
    node _legal_address_T_16 = cvt(_legal_address_T_15) @[Parameters.scala 137:49]
    node _legal_address_T_17 = and(_legal_address_T_16, SInt<33>("h-10000")) @[Parameters.scala 137:52]
    node _legal_address_T_18 = asSInt(_legal_address_T_17) @[Parameters.scala 137:52]
    node _legal_address_T_19 = eq(_legal_address_T_18, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_20 = xor(mpu_physaddr, UInt<32>("h54000000")) @[Parameters.scala 137:31]
    node _legal_address_T_21 = cvt(_legal_address_T_20) @[Parameters.scala 137:49]
    node _legal_address_T_22 = and(_legal_address_T_21, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_23 = asSInt(_legal_address_T_22) @[Parameters.scala 137:52]
    node _legal_address_T_24 = eq(_legal_address_T_23, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_25 = xor(mpu_physaddr, UInt<32>("h100000")) @[Parameters.scala 137:31]
    node _legal_address_T_26 = cvt(_legal_address_T_25) @[Parameters.scala 137:49]
    node _legal_address_T_27 = and(_legal_address_T_26, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_28 = asSInt(_legal_address_T_27) @[Parameters.scala 137:52]
    node _legal_address_T_29 = eq(_legal_address_T_28, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_30 = xor(mpu_physaddr, UInt<32>("hc000000")) @[Parameters.scala 137:31]
    node _legal_address_T_31 = cvt(_legal_address_T_30) @[Parameters.scala 137:49]
    node _legal_address_T_32 = and(_legal_address_T_31, SInt<33>("h-4000000")) @[Parameters.scala 137:52]
    node _legal_address_T_33 = asSInt(_legal_address_T_32) @[Parameters.scala 137:52]
    node _legal_address_T_34 = eq(_legal_address_T_33, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_35 = xor(mpu_physaddr, UInt<32>("h2000000")) @[Parameters.scala 137:31]
    node _legal_address_T_36 = cvt(_legal_address_T_35) @[Parameters.scala 137:49]
    node _legal_address_T_37 = and(_legal_address_T_36, SInt<33>("h-10000")) @[Parameters.scala 137:52]
    node _legal_address_T_38 = asSInt(_legal_address_T_37) @[Parameters.scala 137:52]
    node _legal_address_T_39 = eq(_legal_address_T_38, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_41 = cvt(mpu_physaddr) @[Parameters.scala 137:49]
    node _legal_address_T_42 = and(_legal_address_T_41, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_43 = asSInt(_legal_address_T_42) @[Parameters.scala 137:52]
    node _legal_address_T_44 = eq(_legal_address_T_43, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_45 = xor(mpu_physaddr, UInt<32>("h80000000")) @[Parameters.scala 137:31]
    node _legal_address_T_46 = cvt(_legal_address_T_45) @[Parameters.scala 137:49]
    node _legal_address_T_47 = and(_legal_address_T_46, SInt<33>("h-4000")) @[Parameters.scala 137:52]
    node _legal_address_T_48 = asSInt(_legal_address_T_47) @[Parameters.scala 137:52]
    node _legal_address_T_49 = eq(_legal_address_T_48, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_50 = xor(mpu_physaddr, UInt<32>("h10000")) @[Parameters.scala 137:31]
    node _legal_address_T_51 = cvt(_legal_address_T_50) @[Parameters.scala 137:49]
    node _legal_address_T_52 = and(_legal_address_T_51, SInt<33>("h-10000")) @[Parameters.scala 137:52]
    node _legal_address_T_53 = asSInt(_legal_address_T_52) @[Parameters.scala 137:52]
    node _legal_address_T_54 = eq(_legal_address_T_53, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_55 = or(_legal_address_T_4, _legal_address_T_9) @[TLB.scala 198:67]
    node _legal_address_T_56 = or(_legal_address_T_55, _legal_address_T_14) @[TLB.scala 198:67]
    node _legal_address_T_57 = or(_legal_address_T_56, _legal_address_T_19) @[TLB.scala 198:67]
    node _legal_address_T_58 = or(_legal_address_T_57, _legal_address_T_24) @[TLB.scala 198:67]
    node _legal_address_T_59 = or(_legal_address_T_58, _legal_address_T_29) @[TLB.scala 198:67]
    node _legal_address_T_60 = or(_legal_address_T_59, _legal_address_T_34) @[TLB.scala 198:67]
    node _legal_address_T_61 = or(_legal_address_T_60, _legal_address_T_39) @[TLB.scala 198:67]
    node _legal_address_T_62 = or(_legal_address_T_61, _legal_address_T_44) @[TLB.scala 198:67]
    node _legal_address_T_63 = or(_legal_address_T_62, _legal_address_T_49) @[TLB.scala 198:67]
    node legal_address = or(_legal_address_T_63, _legal_address_T_54) @[TLB.scala 198:67]
    node _deny_access_to_debug_T = leq(mpu_priv, UInt<3>("h3")) @[TLB.scala 203:39]
    node deny_access_to_debug = and(_deny_access_to_debug_T, _legal_address_T_44) @[TLB.scala 203:48]
    node _prot_r_T_6 = not(deny_access_to_debug) @[TLB.scala 204:44]
    node _prot_r_T_7 = and(legal_address, _prot_r_T_6) @[TLB.scala 204:41]
    node prot_r = and(_prot_r_T_7, pmp.io_r) @[TLB.scala 204:66]
    node _prot_w_T_2 = and(_legal_address_T_41, SInt<33>("h9a030000")) @[Parameters.scala 137:52]
    node _prot_w_T_3 = asSInt(_prot_w_T_2) @[Parameters.scala 137:52]
    node _prot_w_T_4 = eq(_prot_w_T_3, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_w_T_7 = and(_legal_address_T_36, SInt<33>("h9a130000")) @[Parameters.scala 137:52]
    node _prot_w_T_8 = asSInt(_prot_w_T_7) @[Parameters.scala 137:52]
    node _prot_w_T_9 = eq(_prot_w_T_8, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_w_T_10 = xor(mpu_physaddr, UInt<32>("h8000000")) @[Parameters.scala 137:31]
    node _prot_w_T_11 = cvt(_prot_w_T_10) @[Parameters.scala 137:49]
    node _prot_w_T_12 = and(_prot_w_T_11, SInt<33>("h98000000")) @[Parameters.scala 137:52]
    node _prot_w_T_13 = asSInt(_prot_w_T_12) @[Parameters.scala 137:52]
    node _prot_w_T_14 = eq(_prot_w_T_13, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_w_T_17 = and(_legal_address_T_11, SInt<33>("h9a130000")) @[Parameters.scala 137:52]
    node _prot_w_T_18 = asSInt(_prot_w_T_17) @[Parameters.scala 137:52]
    node _prot_w_T_19 = eq(_prot_w_T_18, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_w_T_22 = and(_legal_address_T_46, SInt<33>("h9a130000")) @[Parameters.scala 137:52]
    node _prot_w_T_23 = asSInt(_prot_w_T_22) @[Parameters.scala 137:52]
    node _prot_w_T_24 = eq(_prot_w_T_23, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_w_T_25 = or(_prot_w_T_4, _prot_w_T_9) @[Parameters.scala 615:89]
    node _prot_w_T_26 = or(_prot_w_T_25, _prot_w_T_14) @[Parameters.scala 615:89]
    node _prot_w_T_27 = or(_prot_w_T_26, _prot_w_T_19) @[Parameters.scala 615:89]
    node _prot_w_T_28 = or(_prot_w_T_27, _prot_w_T_24) @[Parameters.scala 615:89]
    node _prot_w_T_43 = and(legal_address, _prot_w_T_28) @[TLB.scala 200:19]
    node _prot_w_T_45 = and(_prot_w_T_43, _prot_r_T_6) @[TLB.scala 205:45]
    node prot_w = and(_prot_w_T_45, pmp.io_w) @[TLB.scala 205:70]
    node _prot_x_T_35 = and(_legal_address_T_36, SInt<33>("h96130000")) @[Parameters.scala 137:52]
    node _prot_x_T_36 = asSInt(_prot_x_T_35) @[Parameters.scala 137:52]
    node _prot_x_T_37 = eq(_prot_x_T_36, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_x_T_38 = xor(mpu_physaddr, UInt<32>("h4000000")) @[Parameters.scala 137:31]
    node _prot_x_T_39 = cvt(_prot_x_T_38) @[Parameters.scala 137:49]
    node _prot_x_T_40 = and(_prot_x_T_39, SInt<33>("h94000000")) @[Parameters.scala 137:52]
    node _prot_x_T_41 = asSInt(_prot_x_T_40) @[Parameters.scala 137:52]
    node _prot_x_T_42 = eq(_prot_x_T_41, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_x_T_43 = xor(mpu_physaddr, UInt<32>("h14000000")) @[Parameters.scala 137:31]
    node _prot_x_T_44 = cvt(_prot_x_T_43) @[Parameters.scala 137:49]
    node _prot_eff_T_31 = and(_legal_address_T_41, SInt<33>("h96032000")) @[Parameters.scala 137:52]
    node _prot_eff_T_32 = asSInt(_prot_eff_T_31) @[Parameters.scala 137:52]
    node _prot_eff_T_33 = eq(_prot_eff_T_32, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_eff_T_46 = and(_prot_x_T_44, SInt<33>("h96132000")) @[Parameters.scala 137:52]
    node _prot_eff_T_47 = asSInt(_prot_eff_T_46) @[Parameters.scala 137:52]
    node _prot_eff_T_48 = eq(_prot_eff_T_47, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _prot_eff_T_49 = or(_prot_eff_T_33, _prot_x_T_37) @[Parameters.scala 615:89]
    node _prot_eff_T_50 = or(_prot_eff_T_49, _prot_x_T_42) @[Parameters.scala 615:89]
    node _prot_eff_T_51 = or(_prot_eff_T_50, _prot_eff_T_48) @[Parameters.scala 615:89]
    node prot_eff = and(legal_address, _prot_eff_T_51) @[TLB.scala 200:19]
    node pr_array_hi = mux(prot_r, UInt<2>("h3"), UInt<2>("h0")) @[Bitwise.scala 72:12]
    node pr_array = cat(pr_array_hi, UInt<5>("h0")) @[Cat.scala 30:58]
    node pw_array_hi = mux(prot_w, UInt<2>("h3"), UInt<2>("h0")) @[Bitwise.scala 72:12]
    node pw_array = cat(pw_array_hi, UInt<5>("h0")) @[Cat.scala 30:58]
    node eff_array_hi = mux(prot_eff, UInt<2>("h3"), UInt<2>("h0")) @[Bitwise.scala 72:12]
    node eff_array = cat(eff_array_hi, UInt<5>("h0")) @[Cat.scala 30:58]
    node ppp_array_hi = mux(_prot_w_T_43, UInt<2>("h3"), UInt<2>("h0")) @[Bitwise.scala 72:12]
    node ppp_array = cat(ppp_array_hi, UInt<5>("h0")) @[Cat.scala 30:58]
    node _misaligned_T = dshl(UInt<1>("h1"), io_req_bits_size) @[OneHot.scala 58:35]
    node _misaligned_T_1 = sub(_misaligned_T, UInt<4>("h1")) @[TLB.scala 283:69]
    node _misaligned_T_2 = tail(_misaligned_T_1, 1) @[TLB.scala 283:69]
    node _GEN_149 = pad(_misaligned_T_2, 32) @[TLB.scala 283:39]
    node _misaligned_T_3 = and(io_req_bits_vaddr, _GEN_149) @[TLB.scala 283:39]
    node misaligned = orr(_misaligned_T_3) @[TLB.scala 283:75]
    node _cmd_lrsc_T = eq(io_req_bits_cmd, UInt<5>("h6")) @[package.scala 15:47]
    node _cmd_lrsc_T_1 = eq(io_req_bits_cmd, UInt<5>("h7")) @[package.scala 15:47]
    node cmd_lrsc = or(_cmd_lrsc_T, _cmd_lrsc_T_1) @[package.scala 72:59]
    node _cmd_amo_logical_T = eq(io_req_bits_cmd, UInt<5>("h4")) @[package.scala 15:47]
    node _cmd_amo_logical_T_1 = eq(io_req_bits_cmd, UInt<5>("h9")) @[package.scala 15:47]
    node _cmd_amo_logical_T_2 = eq(io_req_bits_cmd, UInt<5>("ha")) @[package.scala 15:47]
    node _cmd_amo_logical_T_3 = eq(io_req_bits_cmd, UInt<5>("hb")) @[package.scala 15:47]
    node _cmd_amo_logical_T_4 = or(_cmd_amo_logical_T, _cmd_amo_logical_T_1) @[package.scala 72:59]
    node _cmd_amo_logical_T_5 = or(_cmd_amo_logical_T_4, _cmd_amo_logical_T_2) @[package.scala 72:59]
    node cmd_amo_logical = or(_cmd_amo_logical_T_5, _cmd_amo_logical_T_3) @[package.scala 72:59]
    node _cmd_amo_arithmetic_T = eq(io_req_bits_cmd, UInt<5>("h8")) @[package.scala 15:47]
    node _cmd_amo_arithmetic_T_1 = eq(io_req_bits_cmd, UInt<5>("hc")) @[package.scala 15:47]
    node _cmd_amo_arithmetic_T_2 = eq(io_req_bits_cmd, UInt<5>("hd")) @[package.scala 15:47]
    node _cmd_amo_arithmetic_T_3 = eq(io_req_bits_cmd, UInt<5>("he")) @[package.scala 15:47]
    node _cmd_amo_arithmetic_T_4 = eq(io_req_bits_cmd, UInt<5>("hf")) @[package.scala 15:47]
    node _cmd_amo_arithmetic_T_5 = or(_cmd_amo_arithmetic_T, _cmd_amo_arithmetic_T_1) @[package.scala 72:59]
    node _cmd_amo_arithmetic_T_6 = or(_cmd_amo_arithmetic_T_5, _cmd_amo_arithmetic_T_2) @[package.scala 72:59]
    node _cmd_amo_arithmetic_T_7 = or(_cmd_amo_arithmetic_T_6, _cmd_amo_arithmetic_T_3) @[package.scala 72:59]
    node cmd_amo_arithmetic = or(_cmd_amo_arithmetic_T_7, _cmd_amo_arithmetic_T_4) @[package.scala 72:59]
    node cmd_put_partial = eq(io_req_bits_cmd, UInt<5>("h11")) @[TLB.scala 297:41]
    node _cmd_read_T = eq(io_req_bits_cmd, UInt<5>("h0")) @[Consts.scala 81:31]
    node _cmd_read_T_2 = or(_cmd_read_T, _cmd_lrsc_T) @[Consts.scala 81:41]
    node _cmd_read_T_4 = or(_cmd_read_T_2, _cmd_lrsc_T_1) @[Consts.scala 81:58]
    node _cmd_read_T_21 = or(cmd_amo_logical, cmd_amo_arithmetic) @[Consts.scala 79:44]
    node cmd_read = or(_cmd_read_T_4, _cmd_read_T_21) @[Consts.scala 81:75]
    node _cmd_write_T = eq(io_req_bits_cmd, UInt<5>("h1")) @[Consts.scala 82:32]
    node _cmd_write_T_2 = or(_cmd_write_T, cmd_put_partial) @[Consts.scala 82:42]
    node _cmd_write_T_4 = or(_cmd_write_T_2, _cmd_lrsc_T_1) @[Consts.scala 82:59]
    node cmd_write = or(_cmd_write_T_4, _cmd_read_T_21) @[Consts.scala 82:76]
    node _cmd_write_perms_T = eq(io_req_bits_cmd, UInt<5>("h5")) @[package.scala 15:47]
    node _cmd_write_perms_T_1 = eq(io_req_bits_cmd, UInt<5>("h17")) @[package.scala 15:47]
    node _cmd_write_perms_T_2 = or(_cmd_write_perms_T, _cmd_write_perms_T_1) @[package.scala 72:59]
    node cmd_write_perms = or(cmd_write, _cmd_write_perms_T_2) @[TLB.scala 300:35]
    node _ae_array_T = mux(misaligned, eff_array, UInt<7>("h0")) @[TLB.scala 305:8]
    node _ae_array_T_2 = mux(cmd_lrsc, UInt<7>("h7f"), UInt<7>("h0")) @[TLB.scala 306:8]
    node ae_array = or(_ae_array_T, _ae_array_T_2) @[TLB.scala 305:37]
    node _ae_ld_array_T = not(pr_array) @[TLB.scala 307:46]
    node _ae_ld_array_T_1 = or(ae_array, _ae_ld_array_T) @[TLB.scala 307:44]
    node ae_ld_array = mux(cmd_read, _ae_ld_array_T_1, UInt<7>("h0")) @[TLB.scala 307:24]
    node _ae_st_array_T = not(pw_array) @[TLB.scala 309:37]
    node _ae_st_array_T_1 = or(ae_array, _ae_st_array_T) @[TLB.scala 309:35]
    node _ae_st_array_T_2 = mux(cmd_write_perms, _ae_st_array_T_1, UInt<7>("h0")) @[TLB.scala 309:8]
    node _ae_st_array_T_3 = not(ppp_array) @[TLB.scala 310:26]
    node _ae_st_array_T_4 = mux(cmd_put_partial, _ae_st_array_T_3, UInt<7>("h0")) @[TLB.scala 310:8]
    node _ae_st_array_T_5 = or(_ae_st_array_T_2, _ae_st_array_T_4) @[TLB.scala 309:53]
    node _ae_st_array_T_7 = mux(cmd_amo_logical, _ae_st_array_T_3, UInt<7>("h0")) @[TLB.scala 311:8]
    node _ae_st_array_T_8 = or(_ae_st_array_T_5, _ae_st_array_T_7) @[TLB.scala 310:53]
    node _ae_st_array_T_10 = mux(cmd_amo_arithmetic, _ae_st_array_T_3, UInt<7>("h0")) @[TLB.scala 312:8]
    node ae_st_array = or(_ae_st_array_T_8, _ae_st_array_T_10) @[TLB.scala 311:53]
    node _ma_ld_array_T = and(misaligned, cmd_read) @[TLB.scala 318:36]
    node _ma_ld_array_T_1 = not(eff_array) @[TLB.scala 318:49]
    node ma_ld_array = mux(_ma_ld_array_T, _ma_ld_array_T_1, UInt<7>("h0")) @[TLB.scala 318:24]
    node _ma_st_array_T = and(misaligned, cmd_write) @[TLB.scala 319:36]
    node ma_st_array = mux(_ma_st_array_T, _ma_ld_array_T_1, UInt<7>("h0")) @[TLB.scala 319:24]
    node pf_ld_array = mux(cmd_read, UInt<7>("h3f"), UInt<7>("h0")) @[TLB.scala 320:24]
    node pf_st_array = mux(cmd_write_perms, UInt<7>("h3f"), UInt<7>("h0")) @[TLB.scala 321:24]
    node _io_resp_pf_ld_T_1 = and(pf_ld_array, UInt<7>("h40")) @[TLB.scala 342:57]
    node _io_resp_pf_st_T_1 = and(pf_st_array, UInt<7>("h40")) @[TLB.scala 343:64]
    node _io_resp_ae_ld_T = and(ae_ld_array, UInt<7>("h40")) @[TLB.scala 345:33]
    node _io_resp_ae_st_T = and(ae_st_array, UInt<7>("h40")) @[TLB.scala 346:33]
    node _io_resp_ma_ld_T = and(ma_ld_array, UInt<7>("h40")) @[TLB.scala 348:33]
    node _io_resp_ma_st_T = and(ma_st_array, UInt<7>("h40")) @[TLB.scala 349:33]
    io_resp_paddr <= cat(vpn, mpu_physaddr_lo) @[Cat.scala 30:58]
    io_resp_pf_ld <= orr(_io_resp_pf_ld_T_1) @[TLB.scala 342:65]
    io_resp_pf_st <= orr(_io_resp_pf_st_T_1) @[TLB.scala 343:72]
    io_resp_ae_ld <= orr(_io_resp_ae_ld_T) @[TLB.scala 345:41]
    io_resp_ae_st <= orr(_io_resp_ae_st_T) @[TLB.scala 346:41]
    io_resp_ma_ld <= orr(_io_resp_ma_ld_T) @[TLB.scala 348:41]
    io_resp_ma_st <= orr(_io_resp_ma_st_T) @[TLB.scala 349:41]
    pmp.io_prv <= bits(mpu_priv, 1, 0) @[TLB.scala 197:14]
    pmp.io_pmp_0_cfg_l <= io_ptw_pmp_0_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_0_cfg_a <= io_ptw_pmp_0_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_0_cfg_x <= io_ptw_pmp_0_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_0_cfg_w <= io_ptw_pmp_0_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_0_cfg_r <= io_ptw_pmp_0_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_0_addr <= io_ptw_pmp_0_addr @[TLB.scala 196:14]
    pmp.io_pmp_0_mask <= io_ptw_pmp_0_mask @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_l <= io_ptw_pmp_1_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_a <= io_ptw_pmp_1_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_x <= io_ptw_pmp_1_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_w <= io_ptw_pmp_1_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_r <= io_ptw_pmp_1_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_1_addr <= io_ptw_pmp_1_addr @[TLB.scala 196:14]
    pmp.io_pmp_1_mask <= io_ptw_pmp_1_mask @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_l <= io_ptw_pmp_2_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_a <= io_ptw_pmp_2_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_x <= io_ptw_pmp_2_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_w <= io_ptw_pmp_2_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_r <= io_ptw_pmp_2_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_2_addr <= io_ptw_pmp_2_addr @[TLB.scala 196:14]
    pmp.io_pmp_2_mask <= io_ptw_pmp_2_mask @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_l <= io_ptw_pmp_3_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_a <= io_ptw_pmp_3_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_x <= io_ptw_pmp_3_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_w <= io_ptw_pmp_3_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_r <= io_ptw_pmp_3_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_3_addr <= io_ptw_pmp_3_addr @[TLB.scala 196:14]
    pmp.io_pmp_3_mask <= io_ptw_pmp_3_mask @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_l <= io_ptw_pmp_4_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_a <= io_ptw_pmp_4_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_x <= io_ptw_pmp_4_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_w <= io_ptw_pmp_4_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_r <= io_ptw_pmp_4_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_4_addr <= io_ptw_pmp_4_addr @[TLB.scala 196:14]
    pmp.io_pmp_4_mask <= io_ptw_pmp_4_mask @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_l <= io_ptw_pmp_5_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_a <= io_ptw_pmp_5_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_x <= io_ptw_pmp_5_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_w <= io_ptw_pmp_5_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_r <= io_ptw_pmp_5_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_5_addr <= io_ptw_pmp_5_addr @[TLB.scala 196:14]
    pmp.io_pmp_5_mask <= io_ptw_pmp_5_mask @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_l <= io_ptw_pmp_6_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_a <= io_ptw_pmp_6_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_x <= io_ptw_pmp_6_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_w <= io_ptw_pmp_6_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_r <= io_ptw_pmp_6_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_6_addr <= io_ptw_pmp_6_addr @[TLB.scala 196:14]
    pmp.io_pmp_6_mask <= io_ptw_pmp_6_mask @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_l <= io_ptw_pmp_7_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_a <= io_ptw_pmp_7_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_x <= io_ptw_pmp_7_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_w <= io_ptw_pmp_7_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_r <= io_ptw_pmp_7_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_7_addr <= io_ptw_pmp_7_addr @[TLB.scala 196:14]
    pmp.io_pmp_7_mask <= io_ptw_pmp_7_mask @[TLB.scala 196:14]
    pmp.io_addr <= cat(mpu_ppn, mpu_physaddr_lo) @[Cat.scala 30:58]

  module DCacheModuleImpl_Anon_1 :
    input io_in_2_valid : UInt<1>
    input io_in_2_bits_addr : UInt<32>
    input io_in_3_valid : UInt<1>
    input io_in_3_bits_addr : UInt<32>
    output io_in_5_ready : UInt<1>
    input io_in_5_valid : UInt<1>
    output io_in_7_ready : UInt<1>
    input io_in_7_valid : UInt<1>
    input io_in_7_bits_addr : UInt<32>
    output io_out_valid : UInt<1>
    output io_out_bits_write : UInt<1>
    output io_out_bits_addr : UInt<32>

    node _GEN_22 = mux(io_in_3_valid, io_in_3_bits_addr, io_in_7_bits_addr) @[Arbiter.scala 126:27 128:19]
    node _GEN_29 = or(io_in_2_valid, io_in_3_valid) @[Arbiter.scala 126:27 128:19]
    node grant_4 = not(_GEN_29) @[Arbiter.scala 31:78]
    node _io_out_valid_T = not(grant_4) @[Arbiter.scala 135:19]
    io_in_5_ready <= not(_GEN_29) @[Arbiter.scala 31:78]
    io_in_7_ready <= not(_GEN_29) @[Arbiter.scala 31:78]
    io_out_valid <= or(_io_out_valid_T, io_in_7_valid) @[Arbiter.scala 135:31]
    io_out_bits_write <= or(io_in_2_valid, io_in_3_valid) @[Arbiter.scala 126:27 128:19]
    io_out_bits_addr <= mux(io_in_2_valid, io_in_2_bits_addr, _GEN_22) @[Arbiter.scala 126:27 128:19]

  module DCacheDataArray :
    input clock : Clock
    input io_req_valid : UInt<1>
    input io_req_bits_addr : UInt<14>
    input io_req_bits_write : UInt<1>
    input io_req_bits_wdata : UInt<32>
    input io_req_bits_eccMask : UInt<4>
    output io_resp_0 : UInt<32>

    mem data_arrays_0_0 : @[DescribedSRAM.scala 19:26]
      data-type => UInt<8>
      depth => 4096
      read-latency => 1
      write-latency => 1
      reader => rdata_data
      writer => rdata_MPORT
      read-under-write => undefined
    mem data_arrays_0_1 : @[DescribedSRAM.scala 19:26]
      data-type => UInt<8>
      depth => 4096
      read-latency => 1
      write-latency => 1
      reader => rdata_data
      writer => rdata_MPORT
      read-under-write => undefined
    mem data_arrays_0_2 : @[DescribedSRAM.scala 19:26]
      data-type => UInt<8>
      depth => 4096
      read-latency => 1
      write-latency => 1
      reader => rdata_data
      writer => rdata_MPORT
      read-under-write => undefined
    mem data_arrays_0_3 : @[DescribedSRAM.scala 19:26]
      data-type => UInt<8>
      depth => 4096
      read-latency => 1
      write-latency => 1
      reader => rdata_data
      writer => rdata_MPORT
      read-under-write => undefined
    node _rdata_data_T = not(io_req_bits_write) @[DCache.scala 71:42]
    node rdata_lo = cat(data_arrays_0_1.rdata_data.data, data_arrays_0_0.rdata_data.data) @[Cat.scala 30:58]
    node rdata_hi = cat(data_arrays_0_3.rdata_data.data, data_arrays_0_2.rdata_data.data) @[Cat.scala 30:58]
    io_resp_0 <= cat(rdata_hi, rdata_lo) @[Cat.scala 30:58]
    data_arrays_0_0.rdata_data.addr <= shr(io_req_bits_addr, 2) @[DCache.scala 53:31]
    data_arrays_0_0.rdata_data.en <= and(io_req_valid, _rdata_data_T) @[DCache.scala 71:39]
    data_arrays_0_0.rdata_data.clk <= clock @[DCache.scala 71:{26,26}]
    data_arrays_0_0.rdata_MPORT.addr <= shr(io_req_bits_addr, 2) @[DCache.scala 53:31]
    data_arrays_0_0.rdata_MPORT.en <= and(io_req_valid, io_req_bits_write) @[DCache.scala 66:17]
    data_arrays_0_0.rdata_MPORT.clk <= clock @[DCache.scala 66:39]
    data_arrays_0_0.rdata_MPORT.data <= bits(io_req_bits_wdata, 7, 0) @[package.scala 202:50]
    data_arrays_0_0.rdata_MPORT.mask <= bits(io_req_bits_eccMask, 0, 0) @[DCache.scala 50:82]
    data_arrays_0_1.rdata_data.addr <= shr(io_req_bits_addr, 2) @[DCache.scala 53:31]
    data_arrays_0_1.rdata_data.en <= and(io_req_valid, _rdata_data_T) @[DCache.scala 71:39]
    data_arrays_0_1.rdata_data.clk <= clock @[DCache.scala 71:{26,26}]
    data_arrays_0_1.rdata_MPORT.addr <= shr(io_req_bits_addr, 2) @[DCache.scala 53:31]
    data_arrays_0_1.rdata_MPORT.en <= and(io_req_valid, io_req_bits_write) @[DCache.scala 66:17]
    data_arrays_0_1.rdata_MPORT.clk <= clock @[DCache.scala 66:39]
    data_arrays_0_1.rdata_MPORT.data <= bits(io_req_bits_wdata, 15, 8) @[package.scala 202:50]
    data_arrays_0_1.rdata_MPORT.mask <= bits(io_req_bits_eccMask, 1, 1) @[DCache.scala 50:82]
    data_arrays_0_2.rdata_data.addr <= shr(io_req_bits_addr, 2) @[DCache.scala 53:31]
    data_arrays_0_2.rdata_data.en <= and(io_req_valid, _rdata_data_T) @[DCache.scala 71:39]
    data_arrays_0_2.rdata_data.clk <= clock @[DCache.scala 71:{26,26}]
    data_arrays_0_2.rdata_MPORT.addr <= shr(io_req_bits_addr, 2) @[DCache.scala 53:31]
    data_arrays_0_2.rdata_MPORT.en <= and(io_req_valid, io_req_bits_write) @[DCache.scala 66:17]
    data_arrays_0_2.rdata_MPORT.clk <= clock @[DCache.scala 66:39]
    data_arrays_0_2.rdata_MPORT.data <= bits(io_req_bits_wdata, 23, 16) @[package.scala 202:50]
    data_arrays_0_2.rdata_MPORT.mask <= bits(io_req_bits_eccMask, 2, 2) @[DCache.scala 50:82]
    data_arrays_0_3.rdata_data.addr <= shr(io_req_bits_addr, 2) @[DCache.scala 53:31]
    data_arrays_0_3.rdata_data.en <= and(io_req_valid, _rdata_data_T) @[DCache.scala 71:39]
    data_arrays_0_3.rdata_data.clk <= clock @[DCache.scala 71:{26,26}]
    data_arrays_0_3.rdata_MPORT.addr <= shr(io_req_bits_addr, 2) @[DCache.scala 53:31]
    data_arrays_0_3.rdata_MPORT.en <= and(io_req_valid, io_req_bits_write) @[DCache.scala 66:17]
    data_arrays_0_3.rdata_MPORT.clk <= clock @[DCache.scala 66:39]
    data_arrays_0_3.rdata_MPORT.data <= bits(io_req_bits_wdata, 31, 24) @[package.scala 202:50]
    data_arrays_0_3.rdata_MPORT.mask <= bits(io_req_bits_eccMask, 3, 3) @[DCache.scala 50:82]

  module DCacheModuleImpl_Anon_2 :
    input io_in_0_valid : UInt<1>
    input io_in_0_bits_addr : UInt<14>
    input io_in_0_bits_write : UInt<1>
    input io_in_0_bits_wdata : UInt<32>
    input io_in_0_bits_eccMask : UInt<4>
    output io_in_1_ready : UInt<1>
    input io_in_1_valid : UInt<1>
    input io_in_1_bits_addr : UInt<14>
    input io_in_1_bits_write : UInt<1>
    input io_in_1_bits_wdata : UInt<32>
    input io_in_1_bits_eccMask : UInt<4>
    output io_in_3_ready : UInt<1>
    input io_in_3_valid : UInt<1>
    input io_in_3_bits_addr : UInt<14>
    input io_in_3_bits_wdata : UInt<32>
    input io_in_3_bits_wordMask : UInt<1>
    output io_out_valid : UInt<1>
    output io_out_bits_addr : UInt<14>
    output io_out_bits_write : UInt<1>
    output io_out_bits_wdata : UInt<32>
    output io_out_bits_eccMask : UInt<4>

    node _GEN_9 = mux(io_in_1_valid, io_in_1_bits_eccMask, UInt<4>("hf")) @[Arbiter.scala 126:27 128:19]
    node _GEN_11 = mux(io_in_1_valid, io_in_1_bits_wdata, io_in_3_bits_wdata) @[Arbiter.scala 126:27 128:19]
    node _GEN_12 = and(io_in_1_valid, io_in_1_bits_write) @[Arbiter.scala 126:27 128:19]
    node _GEN_13 = mux(io_in_1_valid, io_in_1_bits_addr, io_in_3_bits_addr) @[Arbiter.scala 126:27 128:19]
    node _grant_T = or(io_in_0_valid, io_in_1_valid) @[Arbiter.scala 31:68]
    node grant_2 = not(_grant_T) @[Arbiter.scala 31:78]
    node _io_out_valid_T = not(grant_2) @[Arbiter.scala 135:19]
    io_in_1_ready <= not(io_in_0_valid) @[Arbiter.scala 31:78]
    io_in_3_ready <= not(_grant_T) @[Arbiter.scala 31:78]
    io_out_valid <= or(_io_out_valid_T, io_in_3_valid) @[Arbiter.scala 135:31]
    io_out_bits_addr <= mux(io_in_0_valid, io_in_0_bits_addr, _GEN_13) @[Arbiter.scala 126:27 128:19]
    io_out_bits_write <= mux(io_in_0_valid, io_in_0_bits_write, _GEN_12) @[Arbiter.scala 126:27 128:19]
    io_out_bits_wdata <= mux(io_in_0_valid, io_in_0_bits_wdata, _GEN_11) @[Arbiter.scala 126:27 128:19]
    io_out_bits_eccMask <= mux(io_in_0_valid, io_in_0_bits_eccMask, _GEN_9) @[Arbiter.scala 126:27 128:19]

  module AMOALU :
    input io_cmd : UInt<5>
    input io_lhs : UInt<32>
    input io_rhs : UInt<32>
    output io_out_unmasked : UInt<32>

    node _max_T = eq(io_cmd, UInt<5>("hd")) @[AMOALU.scala 64:20]
    node _max_T_1 = eq(io_cmd, UInt<5>("hf")) @[AMOALU.scala 64:43]
    node max = or(_max_T, _max_T_1) @[AMOALU.scala 64:33]
    node _min_T = eq(io_cmd, UInt<5>("hc")) @[AMOALU.scala 65:20]
    node _min_T_1 = eq(io_cmd, UInt<5>("he")) @[AMOALU.scala 65:43]
    node min = or(_min_T, _min_T_1) @[AMOALU.scala 65:33]
    node add = eq(io_cmd, UInt<5>("h8")) @[AMOALU.scala 66:20]
    node _logic_and_T = eq(io_cmd, UInt<5>("ha")) @[AMOALU.scala 67:26]
    node _logic_and_T_1 = eq(io_cmd, UInt<5>("hb")) @[AMOALU.scala 67:48]
    node logic_and = or(_logic_and_T, _logic_and_T_1) @[AMOALU.scala 67:38]
    node _logic_xor_T = eq(io_cmd, UInt<5>("h9")) @[AMOALU.scala 68:26]
    node logic_xor = or(_logic_xor_T, _logic_and_T) @[AMOALU.scala 68:39]
    node _adder_out_T_2 = add(io_lhs, io_rhs) @[AMOALU.scala 73:21]
    node adder_out = tail(_adder_out_T_2, 1) @[AMOALU.scala 73:21]
    node _less_signed_T = and(io_cmd, UInt<5>("h2")) @[AMOALU.scala 86:17]
    node less_signed = eq(_less_signed_T, UInt<5>("h0")) @[AMOALU.scala 86:25]
    node _less_T_1 = bits(io_lhs, 31, 31) @[AMOALU.scala 88:12]
    node _less_T_2 = bits(io_rhs, 31, 31) @[AMOALU.scala 88:23]
    node _less_T_3 = eq(_less_T_1, _less_T_2) @[AMOALU.scala 88:18]
    node _less_T_6 = lt(io_lhs, io_rhs) @[AMOALU.scala 79:35]
    node _less_T_9 = mux(less_signed, _less_T_1, _less_T_2) @[AMOALU.scala 88:58]
    node less = mux(_less_T_3, _less_T_6, _less_T_9) @[AMOALU.scala 88:10]
    node _minmax_T = mux(less, min, max) @[AMOALU.scala 94:23]
    node minmax = mux(_minmax_T, io_lhs, io_rhs) @[AMOALU.scala 94:19]
    node _logic_T = and(io_lhs, io_rhs) @[AMOALU.scala 96:27]
    node _logic_T_1 = mux(logic_and, _logic_T, UInt<32>("h0")) @[AMOALU.scala 96:8]
    node _logic_T_2 = xor(io_lhs, io_rhs) @[AMOALU.scala 97:27]
    node _logic_T_3 = mux(logic_xor, _logic_T_2, UInt<32>("h0")) @[AMOALU.scala 97:8]
    node logic = or(_logic_T_1, _logic_T_3) @[AMOALU.scala 96:42]
    node _out_T = or(logic_and, logic_xor) @[AMOALU.scala 100:19]
    node _out_T_1 = mux(_out_T, logic, minmax) @[AMOALU.scala 100:8]
    io_out_unmasked <= mux(add, adder_out, _out_T_1) @[AMOALU.scala 99:8]

  module ICache :
    input clock : Clock
    input reset : UInt<1>
    input auto_master_out_a_ready : UInt<1>
    output auto_master_out_a_valid : UInt<1>
    output auto_master_out_a_bits_address : UInt<32>
    input auto_master_out_d_valid : UInt<1>
    input auto_master_out_d_bits_opcode : UInt<3>
    input auto_master_out_d_bits_size : UInt<4>
    input auto_master_out_d_bits_data : UInt<32>
    input auto_master_out_d_bits_corrupt : UInt<1>
    output io_req_ready : UInt<1>
    input io_req_valid : UInt<1>
    input io_req_bits_addr : UInt<32>
    input io_s1_paddr : UInt<32>
    input io_s1_kill : UInt<1>
    input io_s2_kill : UInt<1>
    output io_resp_valid : UInt<1>
    output io_resp_bits_data : UInt<32>
    output io_resp_bits_ae : UInt<1>
    input io_invalidate : UInt<1>

    mem tag_array_0 : @[DescribedSRAM.scala 19:26]
      data-type => UInt<21>
      depth => 64
      read-latency => 1
      write-latency => 1
      reader => tag_rdata
      writer => MPORT
      read-under-write => undefined
    mem data_arrays_0_0 : @[DescribedSRAM.scala 19:26]
      data-type => UInt<32>
      depth => 1024
      read-latency => 1
      write-latency => 1
      reader => dout
      writer => MPORT_1
      read-under-write => undefined
    node s0_valid = and(io_req_ready, io_req_valid) @[Decoupled.scala 40:37]
    reg s1_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s1_valid) @[ICache.scala 169:21]
    reg vb_array : UInt<64>, clock with :
      reset => (UInt<1>("h0"), vb_array) @[ICache.scala 231:21]
    node s1_vb_lo = bits(io_s1_paddr, 11, 6) @[ICache.scala 483:21]
    node _s1_vb_T = cat(UInt<1>("h0"), s1_vb_lo) @[Cat.scala 30:58]
    node _s1_vb_T_1 = dshr(vb_array, _s1_vb_T) @[ICache.scala 259:25]
    node s1_vb = bits(_s1_vb_T_1, 0, 0) @[ICache.scala 259:25]
    node tag = bits(tag_array_0.tag_rdata.data, 19, 0) @[package.scala 154:13]
    node s1_tag = shr(io_s1_paddr, 12) @[ICache.scala 254:30]
    node _tagMatch_T = eq(tag, s1_tag) @[ICache.scala 262:33]
    node tagMatch = and(s1_vb, _tagMatch_T) @[ICache.scala 262:26]
    node _s2_valid_T = not(io_s1_kill) @[ICache.scala 174:38]
    node _s2_valid_T_1 = and(s1_valid, _s2_valid_T) @[ICache.scala 174:35]
    reg s2_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s2_valid) @[ICache.scala 174:25]
    reg s2_hit : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s2_hit) @[ICache.scala 175:23]
    reg invalidated : UInt<1>, clock with :
      reset => (UInt<1>("h0"), invalidated) @[ICache.scala 177:24]
    reg refill_valid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), refill_valid) @[ICache.scala 178:29]
    node _s2_miss_T = not(s2_hit) @[ICache.scala 182:29]
    node _s2_miss_T_1 = and(s2_valid, _s2_miss_T) @[ICache.scala 182:26]
    node _s2_miss_T_2 = not(io_s2_kill) @[ICache.scala 182:40]
    node s2_miss = and(_s2_miss_T_1, _s2_miss_T_2) @[ICache.scala 182:37]
    reg s2_request_refill_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s2_request_refill_REG) @[ICache.scala 184:45]
    node s2_request_refill = and(s2_miss, s2_request_refill_REG) @[ICache.scala 184:35]
    node refill_fire = and(auto_master_out_a_ready, s2_request_refill) @[Decoupled.scala 40:37]
    node _s1_can_request_refill_T = or(s2_miss, refill_valid) @[ICache.scala 183:41]
    node s1_can_request_refill = not(_s1_can_request_refill_T) @[ICache.scala 183:31]
    node _refill_paddr_T = and(s1_valid, s1_can_request_refill) @[ICache.scala 185:54]
    reg refill_paddr : UInt<32>, clock with :
      reset => (UInt<1>("h0"), refill_paddr) @[Reg.scala 15:16]
    node refill_tag = shr(refill_paddr, 12) @[ICache.scala 187:33]
    node refill_idx = bits(refill_paddr, 11, 6) @[ICache.scala 483:21]
    node refill_one_beat_opdata = bits(auto_master_out_d_bits_opcode, 0, 0) @[Edges.scala 105:36]
    node refill_one_beat = and(auto_master_out_d_valid, refill_one_beat_opdata) @[ICache.scala 189:41]
    node _beats1_decode_T_1 = dshl(UInt<12>("hfff"), auto_master_out_d_bits_size) @[package.scala 234:77]
    node _beats1_decode_T_2 = bits(_beats1_decode_T_1, 11, 0) @[package.scala 234:82]
    node _beats1_decode_T_3 = not(_beats1_decode_T_2) @[package.scala 234:46]
    node beats1_decode = shr(_beats1_decode_T_3, 2) @[Edges.scala 219:59]
    node beats1 = mux(refill_one_beat_opdata, beats1_decode, UInt<10>("h0")) @[Edges.scala 220:14]
    reg counter : UInt<10>, clock with :
      reset => (UInt<1>("h0"), counter) @[Edges.scala 228:27]
    node _counter1_T = sub(counter, UInt<10>("h1")) @[Edges.scala 229:28]
    node counter1 = tail(_counter1_T, 1) @[Edges.scala 229:28]
    node first = eq(counter, UInt<10>("h0")) @[Edges.scala 230:25]
    node _last_T = eq(counter, UInt<10>("h1")) @[Edges.scala 231:25]
    node _last_T_1 = eq(beats1, UInt<10>("h0")) @[Edges.scala 231:47]
    node last = or(_last_T, _last_T_1) @[Edges.scala 231:37]
    node d_done = and(last, auto_master_out_d_valid) @[Edges.scala 232:22]
    node _count_T = not(counter1) @[Edges.scala 233:27]
    node refill_cnt = and(beats1, _count_T) @[Edges.scala 233:25]
    node _counter_T = mux(first, beats1, counter1) @[Edges.scala 235:21]
    node _GEN_3 = mux(auto_master_out_d_valid, _counter_T, counter) @[Edges.scala 234:17 235:15 228:27]
    node refill_done = and(refill_one_beat, d_done) @[ICache.scala 195:37]
    node _tag_rdata_T_1 = not(refill_done) @[ICache.scala 218:70]
    reg accruedRefillError : UInt<1>, clock with :
      reset => (UInt<1>("h0"), accruedRefillError) @[ICache.scala 219:31]
    node _refillError_T = gt(refill_cnt, UInt<10>("h0")) @[ICache.scala 220:58]
    node _refillError_T_1 = and(_refillError_T, accruedRefillError) @[ICache.scala 220:62]
    node refillError = or(auto_master_out_d_bits_corrupt, _refillError_T_1) @[ICache.scala 220:43]
    node _io_errors_bus_bits_T = shr(refill_paddr, 6) @[ICache.scala 229:40]
    node _vb_array_T = cat(UInt<1>("h0"), refill_idx) @[Cat.scala 30:58]
    node _vb_array_T_1 = not(invalidated) @[ICache.scala 235:75]
    node _vb_array_T_2 = and(refill_done, _vb_array_T_1) @[ICache.scala 235:72]
    node _vb_array_T_3 = dshl(UInt<1>("h1"), _vb_array_T) @[ICache.scala 235:32]
    node _GEN_40 = pad(vb_array, 128) @[ICache.scala 235:32]
    node _vb_array_T_4 = or(_GEN_40, _vb_array_T_3) @[ICache.scala 235:32]
    node _vb_array_T_5 = not(vb_array) @[ICache.scala 235:32]
    node _GEN_41 = pad(_vb_array_T_5, 128) @[ICache.scala 235:32]
    node _vb_array_T_6 = or(_GEN_41, _vb_array_T_3) @[ICache.scala 235:32]
    node _vb_array_T_7 = not(_vb_array_T_6) @[ICache.scala 235:32]
    node _vb_array_T_8 = mux(_vb_array_T_2, _vb_array_T_4, _vb_array_T_7) @[ICache.scala 235:32]
    node _GEN_16 = mux(refill_one_beat, _vb_array_T_8, pad(vb_array, 128)) @[ICache.scala 232:26 235:14 231:21]
    node _GEN_17 = mux(io_invalidate, UInt<128>("h0"), _GEN_16) @[ICache.scala 238:21 239:14]
    node _GEN_18 = or(io_invalidate, invalidated) @[ICache.scala 238:21 240:17 177:24]
    node tl_error = bits(tag_array_0.tag_rdata.data, 20, 20) @[package.scala 154:13]
    node s1_tl_error_0 = and(tagMatch, tl_error) @[ICache.scala 264:32]
    node wen = and(refill_one_beat, _vb_array_T_1) @[ICache.scala 285:32]
    node _mem_idx_T = shl(refill_idx, 4) @[ICache.scala 286:52]
    node _mem_idx_T_1 = or(_mem_idx_T, refill_cnt) @[ICache.scala 286:79]
    node _mem_idx_T_3 = bits(io_req_bits_addr, 11, 2) @[ICache.scala 283:31]
    node _dout_T = not(wen) @[ICache.scala 295:41]
    node s1_dout_0 = data_arrays_0_0.dout.data @[ICache.scala 296:71 297:15]
    reg s2_dout_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), s2_dout_0) @[Reg.scala 15:16]
    node _s2_tl_error_T = orr(s1_tl_error_0) @[ICache.scala 309:50]
    reg s2_tl_error : UInt<1>, clock with :
      reset => (UInt<1>("h0"), s2_tl_error) @[Reg.scala 15:16]
    node _T_20 = not(refill_valid) @[ICache.scala 473:9]
    node _GEN_38 = or(refill_fire, refill_valid) @[ICache.scala 474:22 178:29 474:37]
    node _GEN_39 = mux(refill_done, UInt<1>("h0"), _GEN_38) @[ICache.scala 475:{22,37}]
    node _GEN_42 = mux(reset, UInt<128>("h0"), _GEN_17) @[ICache.scala 231:{21,21}]
    auto_master_out_a_valid <= and(s2_miss, s2_request_refill_REG) @[ICache.scala 184:35]
    auto_master_out_a_bits_address <= shl(_io_errors_bus_bits_T, 6) @[ICache.scala 418:64]
    io_req_ready <= not(refill_one_beat) @[ICache.scala 191:19]
    io_resp_valid <= and(s2_valid, s2_hit) @[ICache.scala 336:33]
    io_resp_bits_data <= s2_dout_0 @[ICache.scala 333:25]
    io_resp_bits_ae <= s2_tl_error @[ICache.scala 334:23]
    tag_array_0.tag_rdata.addr <= bits(io_req_bits_addr, 11, 6) @[ICache.scala 218:42]
    tag_array_0.tag_rdata.en <= and(_tag_rdata_T_1, s0_valid) @[ICache.scala 218:83]
    tag_array_0.tag_rdata.clk <= clock @[ICache.scala 218:{33,33}]
    tag_array_0.MPORT.addr <= bits(refill_paddr, 11, 6) @[ICache.scala 483:21]
    tag_array_0.MPORT.en <= and(refill_one_beat, d_done) @[ICache.scala 195:37]
    tag_array_0.MPORT.clk <= clock @[ICache.scala 221:22]
    tag_array_0.MPORT.data <= cat(refillError, refill_tag) @[Cat.scala 30:58]
    tag_array_0.MPORT.mask <= UInt<1>("h1")
    data_arrays_0_0.dout.addr <= mux(refill_one_beat, _mem_idx_T_1, _mem_idx_T_3) @[ICache.scala 286:22]
    data_arrays_0_0.dout.en <= and(_dout_T, s0_valid) @[ICache.scala 295:46]
    data_arrays_0_0.dout.clk <= clock @[ICache.scala 295:{31,31}]
    data_arrays_0_0.MPORT_1.addr <= mux(refill_one_beat, _mem_idx_T_1, _mem_idx_T_3) @[ICache.scala 286:22]
    data_arrays_0_0.MPORT_1.en <= and(refill_one_beat, _vb_array_T_1) @[ICache.scala 285:32]
    data_arrays_0_0.MPORT_1.clk <= clock @[ICache.scala 290:16]
    data_arrays_0_0.MPORT_1.data <= auto_master_out_d_bits_data @[ICache.scala 291:21]
    data_arrays_0_0.MPORT_1.mask <= UInt<1>("h1")
    s1_valid <= mux(reset, UInt<1>("h0"), s0_valid) @[ICache.scala 169:{21,21} 192:12]
    vb_array <= bits(_GEN_42, 63, 0) @[ICache.scala 231:{21,21}]
    s2_valid <= mux(reset, UInt<1>("h0"), _s2_valid_T_1) @[ICache.scala 174:{25,25,25}]
    s2_hit <= and(s1_vb, _tagMatch_T) @[ICache.scala 262:26]
    invalidated <= mux(_T_20, UInt<1>("h0"), _GEN_18) @[ICache.scala 473:{24,38}]
    refill_valid <= mux(reset, UInt<1>("h0"), _GEN_39) @[ICache.scala 178:{29,29}]
    s2_request_refill_REG <= not(_s1_can_request_refill_T) @[ICache.scala 183:31]
    refill_paddr <= mux(_refill_paddr_T, io_s1_paddr, refill_paddr) @[Reg.scala 15:16 16:{19,23}]
    counter <= mux(reset, UInt<10>("h0"), _GEN_3) @[Edges.scala 228:{27,27}]
    accruedRefillError <= mux(refill_one_beat, refillError, accruedRefillError) @[ICache.scala 232:26 233:24 219:31]
    s2_dout_0 <= mux(s1_valid, s1_dout_0, s2_dout_0) @[Reg.scala 15:16 16:{19,23}]
    s2_tl_error <= mux(s1_valid, _s2_tl_error_T, s2_tl_error) @[Reg.scala 15:16 16:{19,23}]

  module ShiftQueue :
    input clock : Clock
    input reset : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits_pc : UInt<32>
    input io_enq_bits_data : UInt<32>
    input io_enq_bits_xcpt_ae_inst : UInt<1>
    input io_enq_bits_replay : UInt<1>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits_pc : UInt<32>
    output io_deq_bits_data : UInt<32>
    output io_deq_bits_xcpt_ae_inst : UInt<1>
    output io_deq_bits_replay : UInt<1>
    output io_mask : UInt<5>

    reg valid_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_0) @[ShiftQueue.scala 21:30]
    reg valid_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_1) @[ShiftQueue.scala 21:30]
    reg valid_2 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_2) @[ShiftQueue.scala 21:30]
    reg valid_3 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_3) @[ShiftQueue.scala 21:30]
    reg valid_4 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), valid_4) @[ShiftQueue.scala 21:30]
    reg elts_0_pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_0_pc) @[ShiftQueue.scala 22:25]
    reg elts_0_data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_0_data) @[ShiftQueue.scala 22:25]
    reg elts_0_xcpt_ae_inst : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_0_xcpt_ae_inst) @[ShiftQueue.scala 22:25]
    reg elts_0_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_0_replay) @[ShiftQueue.scala 22:25]
    reg elts_1_pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_1_pc) @[ShiftQueue.scala 22:25]
    reg elts_1_data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_1_data) @[ShiftQueue.scala 22:25]
    reg elts_1_xcpt_ae_inst : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_1_xcpt_ae_inst) @[ShiftQueue.scala 22:25]
    reg elts_1_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_1_replay) @[ShiftQueue.scala 22:25]
    reg elts_2_pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_2_pc) @[ShiftQueue.scala 22:25]
    reg elts_2_data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_2_data) @[ShiftQueue.scala 22:25]
    reg elts_2_xcpt_ae_inst : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_2_xcpt_ae_inst) @[ShiftQueue.scala 22:25]
    reg elts_2_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_2_replay) @[ShiftQueue.scala 22:25]
    reg elts_3_pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_3_pc) @[ShiftQueue.scala 22:25]
    reg elts_3_data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_3_data) @[ShiftQueue.scala 22:25]
    reg elts_3_xcpt_ae_inst : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_3_xcpt_ae_inst) @[ShiftQueue.scala 22:25]
    reg elts_3_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_3_replay) @[ShiftQueue.scala 22:25]
    reg elts_4_pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_4_pc) @[ShiftQueue.scala 22:25]
    reg elts_4_data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), elts_4_data) @[ShiftQueue.scala 22:25]
    reg elts_4_xcpt_ae_inst : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_4_xcpt_ae_inst) @[ShiftQueue.scala 22:25]
    reg elts_4_replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), elts_4_replay) @[ShiftQueue.scala 22:25]
    node wdata_pc = mux(valid_1, elts_1_pc, io_enq_bits_pc) @[ShiftQueue.scala 27:57]
    node wdata_data = mux(valid_1, elts_1_data, io_enq_bits_data) @[ShiftQueue.scala 27:57]
    node wdata_xcpt_ae_inst = mux(valid_1, elts_1_xcpt_ae_inst, io_enq_bits_xcpt_ae_inst) @[ShiftQueue.scala 27:57]
    node wdata_replay = mux(valid_1, elts_1_replay, io_enq_bits_replay) @[ShiftQueue.scala 27:57]
    node _wen_T = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _wen_T_2 = and(_wen_T, valid_0) @[ShiftQueue.scala 30:45]
    node _wen_T_3 = or(valid_1, _wen_T_2) @[ShiftQueue.scala 30:28]
    node _wen_T_6 = not(valid_0) @[ShiftQueue.scala 31:48]
    node _wen_T_7 = and(_wen_T, _wen_T_6) @[ShiftQueue.scala 31:45]
    node wen = mux(io_deq_ready, _wen_T_3, _wen_T_7) @[ShiftQueue.scala 29:10]
    node _valid_0_T_6 = or(_wen_T, valid_0) @[ShiftQueue.scala 37:45]
    node _valid_0_T_7 = mux(io_deq_ready, _wen_T_3, _valid_0_T_6) @[ShiftQueue.scala 35:10]
    node wdata_1_pc = mux(valid_2, elts_2_pc, io_enq_bits_pc) @[ShiftQueue.scala 27:57]
    node wdata_1_data = mux(valid_2, elts_2_data, io_enq_bits_data) @[ShiftQueue.scala 27:57]
    node wdata_1_xcpt_ae_inst = mux(valid_2, elts_2_xcpt_ae_inst, io_enq_bits_xcpt_ae_inst) @[ShiftQueue.scala 27:57]
    node wdata_1_replay = mux(valid_2, elts_2_replay, io_enq_bits_replay) @[ShiftQueue.scala 27:57]
    node _wen_T_10 = and(_wen_T, valid_1) @[ShiftQueue.scala 30:45]
    node _wen_T_11 = or(valid_2, _wen_T_10) @[ShiftQueue.scala 30:28]
    node _wen_T_14 = not(valid_1) @[ShiftQueue.scala 31:48]
    node _wen_T_15 = and(_wen_T_2, _wen_T_14) @[ShiftQueue.scala 31:45]
    node wen_1 = mux(io_deq_ready, _wen_T_11, _wen_T_15) @[ShiftQueue.scala 29:10]
    node _valid_1_T_6 = or(_wen_T_2, valid_1) @[ShiftQueue.scala 37:45]
    node _valid_1_T_7 = mux(io_deq_ready, _wen_T_11, _valid_1_T_6) @[ShiftQueue.scala 35:10]
    node wdata_2_pc = mux(valid_3, elts_3_pc, io_enq_bits_pc) @[ShiftQueue.scala 27:57]
    node wdata_2_data = mux(valid_3, elts_3_data, io_enq_bits_data) @[ShiftQueue.scala 27:57]
    node wdata_2_xcpt_ae_inst = mux(valid_3, elts_3_xcpt_ae_inst, io_enq_bits_xcpt_ae_inst) @[ShiftQueue.scala 27:57]
    node wdata_2_replay = mux(valid_3, elts_3_replay, io_enq_bits_replay) @[ShiftQueue.scala 27:57]
    node _wen_T_18 = and(_wen_T, valid_2) @[ShiftQueue.scala 30:45]
    node _wen_T_19 = or(valid_3, _wen_T_18) @[ShiftQueue.scala 30:28]
    node _wen_T_22 = not(valid_2) @[ShiftQueue.scala 31:48]
    node _wen_T_23 = and(_wen_T_10, _wen_T_22) @[ShiftQueue.scala 31:45]
    node wen_2 = mux(io_deq_ready, _wen_T_19, _wen_T_23) @[ShiftQueue.scala 29:10]
    node _valid_2_T_6 = or(_wen_T_10, valid_2) @[ShiftQueue.scala 37:45]
    node _valid_2_T_7 = mux(io_deq_ready, _wen_T_19, _valid_2_T_6) @[ShiftQueue.scala 35:10]
    node wdata_3_pc = mux(valid_4, elts_4_pc, io_enq_bits_pc) @[ShiftQueue.scala 27:57]
    node wdata_3_data = mux(valid_4, elts_4_data, io_enq_bits_data) @[ShiftQueue.scala 27:57]
    node wdata_3_xcpt_ae_inst = mux(valid_4, elts_4_xcpt_ae_inst, io_enq_bits_xcpt_ae_inst) @[ShiftQueue.scala 27:57]
    node wdata_3_replay = mux(valid_4, elts_4_replay, io_enq_bits_replay) @[ShiftQueue.scala 27:57]
    node _wen_T_26 = and(_wen_T, valid_3) @[ShiftQueue.scala 30:45]
    node _wen_T_27 = or(valid_4, _wen_T_26) @[ShiftQueue.scala 30:28]
    node _wen_T_30 = not(valid_3) @[ShiftQueue.scala 31:48]
    node _wen_T_31 = and(_wen_T_18, _wen_T_30) @[ShiftQueue.scala 31:45]
    node wen_3 = mux(io_deq_ready, _wen_T_27, _wen_T_31) @[ShiftQueue.scala 29:10]
    node _valid_3_T_6 = or(_wen_T_18, valid_3) @[ShiftQueue.scala 37:45]
    node _valid_3_T_7 = mux(io_deq_ready, _wen_T_27, _valid_3_T_6) @[ShiftQueue.scala 35:10]
    node _wen_T_34 = and(_wen_T, valid_4) @[ShiftQueue.scala 30:45]
    node _wen_T_38 = not(valid_4) @[ShiftQueue.scala 31:48]
    node _wen_T_39 = and(_wen_T_26, _wen_T_38) @[ShiftQueue.scala 31:45]
    node wen_4 = mux(io_deq_ready, _wen_T_34, _wen_T_39) @[ShiftQueue.scala 29:10]
    node _valid_4_T_6 = or(_wen_T_26, valid_4) @[ShiftQueue.scala 37:45]
    node _valid_4_T_7 = mux(io_deq_ready, _wen_T_34, _valid_4_T_6) @[ShiftQueue.scala 35:10]
    node io_mask_lo = cat(valid_1, valid_0) @[ShiftQueue.scala 53:20]
    node io_mask_hi_hi = cat(valid_4, valid_3) @[ShiftQueue.scala 53:20]
    node io_mask_hi = cat(io_mask_hi_hi, valid_2) @[ShiftQueue.scala 53:20]
    io_enq_ready <= not(valid_4) @[ShiftQueue.scala 40:19]
    io_deq_valid <= or(io_enq_valid, valid_0) @[ShiftQueue.scala 41:16 45:{25,40}]
    io_deq_bits_pc <= mux(_wen_T_6, io_enq_bits_pc, elts_0_pc) @[ShiftQueue.scala 42:15 46:{22,36}]
    io_deq_bits_data <= mux(_wen_T_6, io_enq_bits_data, elts_0_data) @[ShiftQueue.scala 42:15 46:{22,36}]
    io_deq_bits_xcpt_ae_inst <= mux(_wen_T_6, io_enq_bits_xcpt_ae_inst, elts_0_xcpt_ae_inst) @[ShiftQueue.scala 42:15 46:{22,36}]
    io_deq_bits_replay <= mux(_wen_T_6, io_enq_bits_replay, elts_0_replay) @[ShiftQueue.scala 42:15 46:{22,36}]
    io_mask <= cat(io_mask_hi, io_mask_lo) @[ShiftQueue.scala 53:20]
    valid_0 <= mux(reset, UInt<1>("h0"), _valid_0_T_7) @[ShiftQueue.scala 21:{30,30} 34:14]
    valid_1 <= mux(reset, UInt<1>("h0"), _valid_1_T_7) @[ShiftQueue.scala 21:{30,30} 34:14]
    valid_2 <= mux(reset, UInt<1>("h0"), _valid_2_T_7) @[ShiftQueue.scala 21:{30,30} 34:14]
    valid_3 <= mux(reset, UInt<1>("h0"), _valid_3_T_7) @[ShiftQueue.scala 21:{30,30} 34:14]
    valid_4 <= mux(reset, UInt<1>("h0"), _valid_4_T_7) @[ShiftQueue.scala 21:{30,30} 34:14]
    elts_0_pc <= mux(wen, wdata_pc, elts_0_pc) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_0_data <= mux(wen, wdata_data, elts_0_data) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_0_xcpt_ae_inst <= mux(wen, wdata_xcpt_ae_inst, elts_0_xcpt_ae_inst) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_0_replay <= mux(wen, wdata_replay, elts_0_replay) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_1_pc <= mux(wen_1, wdata_1_pc, elts_1_pc) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_1_data <= mux(wen_1, wdata_1_data, elts_1_data) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_1_xcpt_ae_inst <= mux(wen_1, wdata_1_xcpt_ae_inst, elts_1_xcpt_ae_inst) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_1_replay <= mux(wen_1, wdata_1_replay, elts_1_replay) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_2_pc <= mux(wen_2, wdata_2_pc, elts_2_pc) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_2_data <= mux(wen_2, wdata_2_data, elts_2_data) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_2_xcpt_ae_inst <= mux(wen_2, wdata_2_xcpt_ae_inst, elts_2_xcpt_ae_inst) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_2_replay <= mux(wen_2, wdata_2_replay, elts_2_replay) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_3_pc <= mux(wen_3, wdata_3_pc, elts_3_pc) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_3_data <= mux(wen_3, wdata_3_data, elts_3_data) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_3_xcpt_ae_inst <= mux(wen_3, wdata_3_xcpt_ae_inst, elts_3_xcpt_ae_inst) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_3_replay <= mux(wen_3, wdata_3_replay, elts_3_replay) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_4_pc <= mux(wen_4, io_enq_bits_pc, elts_4_pc) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_4_data <= mux(wen_4, io_enq_bits_data, elts_4_data) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_4_xcpt_ae_inst <= mux(wen_4, io_enq_bits_xcpt_ae_inst, elts_4_xcpt_ae_inst) @[ShiftQueue.scala 32:16 22:25 32:26]
    elts_4_replay <= mux(wen_4, io_enq_bits_replay, elts_4_replay) @[ShiftQueue.scala 32:16 22:25 32:26]

  module TLB_1 :
    input io_req_bits_vaddr : UInt<32>
    output io_resp_paddr : UInt<32>
    output io_resp_ae_inst : UInt<1>
    input io_ptw_status_debug : UInt<1>
    input io_ptw_pmp_0_cfg_l : UInt<1>
    input io_ptw_pmp_0_cfg_a : UInt<2>
    input io_ptw_pmp_0_cfg_x : UInt<1>
    input io_ptw_pmp_0_cfg_w : UInt<1>
    input io_ptw_pmp_0_cfg_r : UInt<1>
    input io_ptw_pmp_0_addr : UInt<30>
    input io_ptw_pmp_0_mask : UInt<32>
    input io_ptw_pmp_1_cfg_l : UInt<1>
    input io_ptw_pmp_1_cfg_a : UInt<2>
    input io_ptw_pmp_1_cfg_x : UInt<1>
    input io_ptw_pmp_1_cfg_w : UInt<1>
    input io_ptw_pmp_1_cfg_r : UInt<1>
    input io_ptw_pmp_1_addr : UInt<30>
    input io_ptw_pmp_1_mask : UInt<32>
    input io_ptw_pmp_2_cfg_l : UInt<1>
    input io_ptw_pmp_2_cfg_a : UInt<2>
    input io_ptw_pmp_2_cfg_x : UInt<1>
    input io_ptw_pmp_2_cfg_w : UInt<1>
    input io_ptw_pmp_2_cfg_r : UInt<1>
    input io_ptw_pmp_2_addr : UInt<30>
    input io_ptw_pmp_2_mask : UInt<32>
    input io_ptw_pmp_3_cfg_l : UInt<1>
    input io_ptw_pmp_3_cfg_a : UInt<2>
    input io_ptw_pmp_3_cfg_x : UInt<1>
    input io_ptw_pmp_3_cfg_w : UInt<1>
    input io_ptw_pmp_3_cfg_r : UInt<1>
    input io_ptw_pmp_3_addr : UInt<30>
    input io_ptw_pmp_3_mask : UInt<32>
    input io_ptw_pmp_4_cfg_l : UInt<1>
    input io_ptw_pmp_4_cfg_a : UInt<2>
    input io_ptw_pmp_4_cfg_x : UInt<1>
    input io_ptw_pmp_4_cfg_w : UInt<1>
    input io_ptw_pmp_4_cfg_r : UInt<1>
    input io_ptw_pmp_4_addr : UInt<30>
    input io_ptw_pmp_4_mask : UInt<32>
    input io_ptw_pmp_5_cfg_l : UInt<1>
    input io_ptw_pmp_5_cfg_a : UInt<2>
    input io_ptw_pmp_5_cfg_x : UInt<1>
    input io_ptw_pmp_5_cfg_w : UInt<1>
    input io_ptw_pmp_5_cfg_r : UInt<1>
    input io_ptw_pmp_5_addr : UInt<30>
    input io_ptw_pmp_5_mask : UInt<32>
    input io_ptw_pmp_6_cfg_l : UInt<1>
    input io_ptw_pmp_6_cfg_a : UInt<2>
    input io_ptw_pmp_6_cfg_x : UInt<1>
    input io_ptw_pmp_6_cfg_w : UInt<1>
    input io_ptw_pmp_6_cfg_r : UInt<1>
    input io_ptw_pmp_6_addr : UInt<30>
    input io_ptw_pmp_6_mask : UInt<32>
    input io_ptw_pmp_7_cfg_l : UInt<1>
    input io_ptw_pmp_7_cfg_a : UInt<2>
    input io_ptw_pmp_7_cfg_x : UInt<1>
    input io_ptw_pmp_7_cfg_w : UInt<1>
    input io_ptw_pmp_7_cfg_r : UInt<1>
    input io_ptw_pmp_7_addr : UInt<30>
    input io_ptw_pmp_7_mask : UInt<32>

    inst pmp of PMPChecker @[TLB.scala 193:19]
    node vpn = bits(io_req_bits_vaddr, 31, 12) @[TLB.scala 163:30]
    node mpu_ppn = shr(io_req_bits_vaddr, 12) @[TLB.scala 190:123]
    node mpu_physaddr_lo = bits(io_req_bits_vaddr, 11, 0) @[TLB.scala 191:52]
    node mpu_physaddr = cat(mpu_ppn, mpu_physaddr_lo) @[Cat.scala 30:58]
    node mpu_priv = cat(io_ptw_status_debug, UInt<2>("h3")) @[Cat.scala 30:58]
    node _legal_address_T = xor(mpu_physaddr, UInt<32>("h3000")) @[Parameters.scala 137:31]
    node _legal_address_T_1 = cvt(_legal_address_T) @[Parameters.scala 137:49]
    node _legal_address_T_2 = and(_legal_address_T_1, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_3 = asSInt(_legal_address_T_2) @[Parameters.scala 137:52]
    node _legal_address_T_4 = eq(_legal_address_T_3, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_5 = xor(mpu_physaddr, UInt<32>("h4000")) @[Parameters.scala 137:31]
    node _legal_address_T_6 = cvt(_legal_address_T_5) @[Parameters.scala 137:49]
    node _legal_address_T_7 = and(_legal_address_T_6, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_8 = asSInt(_legal_address_T_7) @[Parameters.scala 137:52]
    node _legal_address_T_9 = eq(_legal_address_T_8, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_10 = xor(mpu_physaddr, UInt<32>("h10000000")) @[Parameters.scala 137:31]
    node _legal_address_T_11 = cvt(_legal_address_T_10) @[Parameters.scala 137:49]
    node _legal_address_T_12 = and(_legal_address_T_11, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_13 = asSInt(_legal_address_T_12) @[Parameters.scala 137:52]
    node _legal_address_T_14 = eq(_legal_address_T_13, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_15 = xor(mpu_physaddr, UInt<32>("h20000")) @[Parameters.scala 137:31]
    node _legal_address_T_16 = cvt(_legal_address_T_15) @[Parameters.scala 137:49]
    node _legal_address_T_17 = and(_legal_address_T_16, SInt<33>("h-10000")) @[Parameters.scala 137:52]
    node _legal_address_T_18 = asSInt(_legal_address_T_17) @[Parameters.scala 137:52]
    node _legal_address_T_19 = eq(_legal_address_T_18, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_20 = xor(mpu_physaddr, UInt<32>("h54000000")) @[Parameters.scala 137:31]
    node _legal_address_T_21 = cvt(_legal_address_T_20) @[Parameters.scala 137:49]
    node _legal_address_T_22 = and(_legal_address_T_21, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_23 = asSInt(_legal_address_T_22) @[Parameters.scala 137:52]
    node _legal_address_T_24 = eq(_legal_address_T_23, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_25 = xor(mpu_physaddr, UInt<32>("h100000")) @[Parameters.scala 137:31]
    node _legal_address_T_26 = cvt(_legal_address_T_25) @[Parameters.scala 137:49]
    node _legal_address_T_27 = and(_legal_address_T_26, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_28 = asSInt(_legal_address_T_27) @[Parameters.scala 137:52]
    node _legal_address_T_29 = eq(_legal_address_T_28, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_30 = xor(mpu_physaddr, UInt<32>("hc000000")) @[Parameters.scala 137:31]
    node _legal_address_T_31 = cvt(_legal_address_T_30) @[Parameters.scala 137:49]
    node _legal_address_T_32 = and(_legal_address_T_31, SInt<33>("h-4000000")) @[Parameters.scala 137:52]
    node _legal_address_T_33 = asSInt(_legal_address_T_32) @[Parameters.scala 137:52]
    node _legal_address_T_34 = eq(_legal_address_T_33, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_35 = xor(mpu_physaddr, UInt<32>("h2000000")) @[Parameters.scala 137:31]
    node _legal_address_T_36 = cvt(_legal_address_T_35) @[Parameters.scala 137:49]
    node _legal_address_T_37 = and(_legal_address_T_36, SInt<33>("h-10000")) @[Parameters.scala 137:52]
    node _legal_address_T_38 = asSInt(_legal_address_T_37) @[Parameters.scala 137:52]
    node _legal_address_T_39 = eq(_legal_address_T_38, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_41 = cvt(mpu_physaddr) @[Parameters.scala 137:49]
    node _legal_address_T_42 = and(_legal_address_T_41, SInt<33>("h-1000")) @[Parameters.scala 137:52]
    node _legal_address_T_43 = asSInt(_legal_address_T_42) @[Parameters.scala 137:52]
    node _legal_address_T_44 = eq(_legal_address_T_43, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_45 = xor(mpu_physaddr, UInt<32>("h80000000")) @[Parameters.scala 137:31]
    node _legal_address_T_46 = cvt(_legal_address_T_45) @[Parameters.scala 137:49]
    node _legal_address_T_47 = and(_legal_address_T_46, SInt<33>("h-4000")) @[Parameters.scala 137:52]
    node _legal_address_T_48 = asSInt(_legal_address_T_47) @[Parameters.scala 137:52]
    node _legal_address_T_49 = eq(_legal_address_T_48, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_50 = xor(mpu_physaddr, UInt<32>("h10000")) @[Parameters.scala 137:31]
    node _legal_address_T_51 = cvt(_legal_address_T_50) @[Parameters.scala 137:49]
    node _legal_address_T_52 = and(_legal_address_T_51, SInt<33>("h-10000")) @[Parameters.scala 137:52]
    node _legal_address_T_53 = asSInt(_legal_address_T_52) @[Parameters.scala 137:52]
    node _legal_address_T_54 = eq(_legal_address_T_53, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _legal_address_T_55 = or(_legal_address_T_4, _legal_address_T_9) @[TLB.scala 198:67]
    node _legal_address_T_56 = or(_legal_address_T_55, _legal_address_T_14) @[TLB.scala 198:67]
    node _legal_address_T_57 = or(_legal_address_T_56, _legal_address_T_19) @[TLB.scala 198:67]
    node _legal_address_T_58 = or(_legal_address_T_57, _legal_address_T_24) @[TLB.scala 198:67]
    node _legal_address_T_59 = or(_legal_address_T_58, _legal_address_T_29) @[TLB.scala 198:67]
    node _legal_address_T_60 = or(_legal_address_T_59, _legal_address_T_34) @[TLB.scala 198:67]
    node _legal_address_T_61 = or(_legal_address_T_60, _legal_address_T_39) @[TLB.scala 198:67]
    node _legal_address_T_62 = or(_legal_address_T_61, _legal_address_T_44) @[TLB.scala 198:67]
    node _legal_address_T_63 = or(_legal_address_T_62, _legal_address_T_49) @[TLB.scala 198:67]
    node legal_address = or(_legal_address_T_63, _legal_address_T_54) @[TLB.scala 198:67]
    node _homogeneous_T_75 = and(_legal_address_T_41, SInt<33>("h86134000")) @[Parameters.scala 137:52]
    node _homogeneous_T_76 = asSInt(_homogeneous_T_75) @[Parameters.scala 137:52]
    node _homogeneous_T_77 = eq(_homogeneous_T_76, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _homogeneous_T_80 = and(_legal_address_T_51, SInt<33>("h96130000")) @[Parameters.scala 137:52]
    node _homogeneous_T_81 = asSInt(_homogeneous_T_80) @[Parameters.scala 137:52]
    node _homogeneous_T_82 = eq(_homogeneous_T_81, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _homogeneous_T_85 = and(_legal_address_T_16, SInt<33>("h96130000")) @[Parameters.scala 137:52]
    node _homogeneous_T_86 = asSInt(_homogeneous_T_85) @[Parameters.scala 137:52]
    node _homogeneous_T_87 = eq(_homogeneous_T_86, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _homogeneous_T_90 = and(_legal_address_T_46, SInt<33>("h96134000")) @[Parameters.scala 137:52]
    node _homogeneous_T_91 = asSInt(_homogeneous_T_90) @[Parameters.scala 137:52]
    node _homogeneous_T_92 = eq(_homogeneous_T_91, SInt<33>("h0")) @[Parameters.scala 137:67]
    node _homogeneous_T_94 = or(_homogeneous_T_77, _homogeneous_T_82) @[TLBPermissions.scala 82:66]
    node _homogeneous_T_95 = or(_homogeneous_T_94, _homogeneous_T_87) @[TLBPermissions.scala 82:66]
    node _homogeneous_T_96 = or(_homogeneous_T_95, _homogeneous_T_92) @[TLBPermissions.scala 82:66]
    node _deny_access_to_debug_T = leq(mpu_priv, UInt<3>("h3")) @[TLB.scala 203:39]
    node deny_access_to_debug = and(_deny_access_to_debug_T, _legal_address_T_44) @[TLB.scala 203:48]
    node _prot_r_T_6 = not(deny_access_to_debug) @[TLB.scala 204:44]
    node _prot_x_T_55 = and(legal_address, _homogeneous_T_96) @[TLB.scala 200:19]
    node _prot_x_T_57 = and(_prot_x_T_55, _prot_r_T_6) @[TLB.scala 209:40]
    node prot_x = and(_prot_x_T_57, pmp.io_x) @[TLB.scala 209:65]
    node px_array_hi = mux(prot_x, UInt<2>("h3"), UInt<2>("h0")) @[Bitwise.scala 72:12]
    node px_array = cat(px_array_hi, UInt<5>("h0")) @[Cat.scala 30:58]
    node _io_resp_ae_inst_T = not(px_array) @[TLB.scala 347:23]
    node _io_resp_ae_inst_T_1 = and(_io_resp_ae_inst_T, UInt<7>("h40")) @[TLB.scala 347:33]
    io_resp_paddr <= cat(vpn, mpu_physaddr_lo) @[Cat.scala 30:58]
    io_resp_ae_inst <= orr(_io_resp_ae_inst_T_1) @[TLB.scala 347:41]
    pmp.io_prv <= bits(mpu_priv, 1, 0) @[TLB.scala 197:14]
    pmp.io_pmp_0_cfg_l <= io_ptw_pmp_0_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_0_cfg_a <= io_ptw_pmp_0_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_0_cfg_x <= io_ptw_pmp_0_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_0_cfg_w <= io_ptw_pmp_0_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_0_cfg_r <= io_ptw_pmp_0_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_0_addr <= io_ptw_pmp_0_addr @[TLB.scala 196:14]
    pmp.io_pmp_0_mask <= io_ptw_pmp_0_mask @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_l <= io_ptw_pmp_1_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_a <= io_ptw_pmp_1_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_x <= io_ptw_pmp_1_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_w <= io_ptw_pmp_1_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_1_cfg_r <= io_ptw_pmp_1_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_1_addr <= io_ptw_pmp_1_addr @[TLB.scala 196:14]
    pmp.io_pmp_1_mask <= io_ptw_pmp_1_mask @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_l <= io_ptw_pmp_2_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_a <= io_ptw_pmp_2_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_x <= io_ptw_pmp_2_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_w <= io_ptw_pmp_2_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_2_cfg_r <= io_ptw_pmp_2_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_2_addr <= io_ptw_pmp_2_addr @[TLB.scala 196:14]
    pmp.io_pmp_2_mask <= io_ptw_pmp_2_mask @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_l <= io_ptw_pmp_3_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_a <= io_ptw_pmp_3_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_x <= io_ptw_pmp_3_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_w <= io_ptw_pmp_3_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_3_cfg_r <= io_ptw_pmp_3_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_3_addr <= io_ptw_pmp_3_addr @[TLB.scala 196:14]
    pmp.io_pmp_3_mask <= io_ptw_pmp_3_mask @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_l <= io_ptw_pmp_4_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_a <= io_ptw_pmp_4_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_x <= io_ptw_pmp_4_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_w <= io_ptw_pmp_4_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_4_cfg_r <= io_ptw_pmp_4_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_4_addr <= io_ptw_pmp_4_addr @[TLB.scala 196:14]
    pmp.io_pmp_4_mask <= io_ptw_pmp_4_mask @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_l <= io_ptw_pmp_5_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_a <= io_ptw_pmp_5_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_x <= io_ptw_pmp_5_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_w <= io_ptw_pmp_5_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_5_cfg_r <= io_ptw_pmp_5_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_5_addr <= io_ptw_pmp_5_addr @[TLB.scala 196:14]
    pmp.io_pmp_5_mask <= io_ptw_pmp_5_mask @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_l <= io_ptw_pmp_6_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_a <= io_ptw_pmp_6_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_x <= io_ptw_pmp_6_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_w <= io_ptw_pmp_6_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_6_cfg_r <= io_ptw_pmp_6_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_6_addr <= io_ptw_pmp_6_addr @[TLB.scala 196:14]
    pmp.io_pmp_6_mask <= io_ptw_pmp_6_mask @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_l <= io_ptw_pmp_7_cfg_l @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_a <= io_ptw_pmp_7_cfg_a @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_x <= io_ptw_pmp_7_cfg_x @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_w <= io_ptw_pmp_7_cfg_w @[TLB.scala 196:14]
    pmp.io_pmp_7_cfg_r <= io_ptw_pmp_7_cfg_r @[TLB.scala 196:14]
    pmp.io_pmp_7_addr <= io_ptw_pmp_7_addr @[TLB.scala 196:14]
    pmp.io_pmp_7_mask <= io_ptw_pmp_7_mask @[TLB.scala 196:14]
    pmp.io_addr <= cat(mpu_ppn, mpu_physaddr_lo) @[Cat.scala 30:58]

  module Repeater_7 :
    input clock : Clock
    input reset : UInt<1>
    input io_repeat : UInt<1>
    output io_full : UInt<1>
    output io_enq_ready : UInt<1>
    input io_enq_valid : UInt<1>
    input io_enq_bits_opcode : UInt<3>
    input io_enq_bits_param : UInt<3>
    input io_enq_bits_size : UInt<3>
    input io_enq_bits_source : UInt<3>
    input io_enq_bits_address : UInt<32>
    input io_enq_bits_mask : UInt<4>
    input io_deq_ready : UInt<1>
    output io_deq_valid : UInt<1>
    output io_deq_bits_opcode : UInt<3>
    output io_deq_bits_param : UInt<3>
    output io_deq_bits_size : UInt<3>
    output io_deq_bits_source : UInt<3>
    output io_deq_bits_address : UInt<32>
    output io_deq_bits_mask : UInt<4>

    reg full : UInt<1>, clock with :
      reset => (UInt<1>("h0"), full) @[Repeater.scala 19:21]
    reg saved_opcode : UInt<3>, clock with :
      reset => (UInt<1>("h0"), saved_opcode) @[Repeater.scala 20:18]
    reg saved_param : UInt<3>, clock with :
      reset => (UInt<1>("h0"), saved_param) @[Repeater.scala 20:18]
    reg saved_size : UInt<3>, clock with :
      reset => (UInt<1>("h0"), saved_size) @[Repeater.scala 20:18]
    reg saved_source : UInt<3>, clock with :
      reset => (UInt<1>("h0"), saved_source) @[Repeater.scala 20:18]
    reg saved_address : UInt<32>, clock with :
      reset => (UInt<1>("h0"), saved_address) @[Repeater.scala 20:18]
    reg saved_mask : UInt<4>, clock with :
      reset => (UInt<1>("h0"), saved_mask) @[Repeater.scala 20:18]
    node _io_enq_ready_T = not(full) @[Repeater.scala 24:35]
    node _T = and(io_enq_ready, io_enq_valid) @[Decoupled.scala 40:37]
    node _T_1 = and(_T, io_repeat) @[Repeater.scala 28:23]
    node _GEN_0 = or(_T_1, full) @[Repeater.scala 19:21 28:{38,45}]
    node _T_2 = and(io_deq_ready, io_deq_valid) @[Decoupled.scala 40:37]
    node _T_3 = not(io_repeat) @[Repeater.scala 29:26]
    node _T_4 = and(_T_2, _T_3) @[Repeater.scala 29:23]
    node _GEN_9 = mux(_T_4, UInt<1>("h0"), _GEN_0) @[Repeater.scala 29:{38,45}]
    io_full <= full @[Repeater.scala 26:11]
    io_enq_ready <= and(io_deq_ready, _io_enq_ready_T) @[Repeater.scala 24:32]
    io_deq_valid <= or(io_enq_valid, full) @[Repeater.scala 23:32]
    io_deq_bits_opcode <= mux(full, saved_opcode, io_enq_bits_opcode) @[Repeater.scala 25:21]
    io_deq_bits_param <= mux(full, saved_param, io_enq_bits_param) @[Repeater.scala 25:21]
    io_deq_bits_size <= mux(full, saved_size, io_enq_bits_size) @[Repeater.scala 25:21]
    io_deq_bits_source <= mux(full, saved_source, io_enq_bits_source) @[Repeater.scala 25:21]
    io_deq_bits_address <= mux(full, saved_address, io_enq_bits_address) @[Repeater.scala 25:21]
    io_deq_bits_mask <= mux(full, saved_mask, io_enq_bits_mask) @[Repeater.scala 25:21]
    full <= mux(reset, UInt<1>("h0"), _GEN_9) @[Repeater.scala 19:{21,21}]
    saved_opcode <= mux(_T_1, io_enq_bits_opcode, saved_opcode) @[Repeater.scala 20:18 28:{38,62}]
    saved_param <= mux(_T_1, io_enq_bits_param, saved_param) @[Repeater.scala 20:18 28:{38,62}]
    saved_size <= mux(_T_1, io_enq_bits_size, saved_size) @[Repeater.scala 20:18 28:{38,62}]
    saved_source <= mux(_T_1, io_enq_bits_source, saved_source) @[Repeater.scala 20:18 28:{38,62}]
    saved_address <= mux(_T_1, io_enq_bits_address, saved_address) @[Repeater.scala 20:18 28:{38,62}]
    saved_mask <= mux(_T_1, io_enq_bits_mask, saved_mask) @[Repeater.scala 20:18 28:{38,62}]

  module OptimizationBarrier_54 :
    input io_x : UInt<3>
    output io_y : UInt<3>

    io_y <= io_x @[package.scala 263:12]

  module IBuf :
    input clock : Clock
    input reset : UInt<1>
    output io_imem_ready : UInt<1>
    input io_imem_valid : UInt<1>
    input io_imem_bits_pc : UInt<32>
    input io_imem_bits_data : UInt<32>
    input io_imem_bits_xcpt_ae_inst : UInt<1>
    input io_imem_bits_replay : UInt<1>
    input io_kill : UInt<1>
    output io_pc : UInt<32>
    input io_inst_0_ready : UInt<1>
    output io_inst_0_valid : UInt<1>
    output io_inst_0_bits_xcpt0_ae_inst : UInt<1>
    output io_inst_0_bits_xcpt1_pf_inst : UInt<1>
    output io_inst_0_bits_xcpt1_ae_inst : UInt<1>
    output io_inst_0_bits_replay : UInt<1>
    output io_inst_0_bits_rvc : UInt<1>
    output io_inst_0_bits_inst_bits : UInt<32>
    output io_inst_0_bits_inst_rd : UInt<5>
    output io_inst_0_bits_inst_rs1 : UInt<5>
    output io_inst_0_bits_inst_rs2 : UInt<5>
    output io_inst_0_bits_raw : UInt<32>

    inst exp of RVCExpander @[IBuf.scala 86:21]
    reg nBufValid : UInt<1>, clock with :
      reset => (UInt<1>("h0"), nBufValid) @[IBuf.scala 34:47]
    reg buf__pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), buf__pc) @[IBuf.scala 35:16]
    reg buf__data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), buf__data) @[IBuf.scala 35:16]
    reg buf__xcpt_ae_inst : UInt<1>, clock with :
      reset => (UInt<1>("h0"), buf__xcpt_ae_inst) @[IBuf.scala 35:16]
    reg buf__replay : UInt<1>, clock with :
      reset => (UInt<1>("h0"), buf__replay) @[IBuf.scala 35:16]
    node pcWordBits = bits(io_imem_bits_pc, 1, 1) @[package.scala 154:13]
    node _GEN_56 = pad(pcWordBits, 2) @[IBuf.scala 41:88]
    node _nIC_T_2 = sub(UInt<2>("h2"), _GEN_56) @[IBuf.scala 41:88]
    node nIC = tail(_nIC_T_2, 1) @[IBuf.scala 41:88]
    node _nValid_T = mux(io_imem_valid, nIC, UInt<2>("h0")) @[IBuf.scala 43:19]
    node _GEN_57 = pad(nBufValid, 2) @[IBuf.scala 43:49]
    node _nValid_T_1 = add(_nValid_T, _GEN_57) @[IBuf.scala 43:49]
    node nValid = tail(_nValid_T_1, 1) @[IBuf.scala 43:49]
    node _valid_T = dshl(UInt<1>("h1"), nValid) @[OneHot.scala 58:35]
    node _valid_T_1 = sub(_valid_T, UInt<4>("h1")) @[IBuf.scala 74:33]
    node _valid_T_2 = tail(_valid_T_1, 1) @[IBuf.scala 74:33]
    node valid = bits(_valid_T_2, 1, 0) @[IBuf.scala 74:37]
    node _GEN_58 = shr(valid, 1) @[IBuf.scala 93:42]
    node _full_insn_T_2 = pad(_GEN_58, 2) @[IBuf.scala 93:42]
    node _full_insn_T_3 = bits(_full_insn_T_2, 0, 0) @[IBuf.scala 93:42]
    node _full_insn_T_4 = or(exp.io_rvc, _full_insn_T_3) @[IBuf.scala 93:34]
    node _bufMask_T = dshl(UInt<1>("h1"), nBufValid) @[OneHot.scala 58:35]
    node _bufMask_T_1 = sub(_bufMask_T, UInt<2>("h1")) @[IBuf.scala 75:37]
    node bufMask = tail(_bufMask_T_1, 1) @[IBuf.scala 75:37]
    node buf_replay = mux(buf__replay, bufMask, UInt<2>("h0")) @[IBuf.scala 77:23]
    node _full_insn_T_6 = bits(buf_replay, 0, 0) @[IBuf.scala 93:61]
    node full_insn = or(_full_insn_T_4, _full_insn_T_6) @[IBuf.scala 93:48]
    node _nReady_T_4 = mux(exp.io_rvc, UInt<2>("h1"), UInt<2>("h2")) @[IBuf.scala 102:71]
    node nReady = mux(full_insn, _nReady_T_4, UInt<2>("h0")) @[IBuf.scala 102:{56,65}]
    node _nICReady_T = sub(nReady, _GEN_57) @[IBuf.scala 42:25]
    node nICReady = tail(_nICReady_T, 1) @[IBuf.scala 42:25]
    node _io_imem_ready_T = geq(nReady, _GEN_57) @[IBuf.scala 44:47]
    node _io_imem_ready_T_1 = and(io_inst_0_ready, _io_imem_ready_T) @[IBuf.scala 44:37]
    node _io_imem_ready_T_2 = geq(nICReady, nIC) @[IBuf.scala 44:73]
    node _io_imem_ready_T_3 = sub(nIC, nICReady) @[IBuf.scala 44:92]
    node _io_imem_ready_T_4 = tail(_io_imem_ready_T_3, 1) @[IBuf.scala 44:92]
    node _io_imem_ready_T_5 = geq(UInt<2>("h1"), _io_imem_ready_T_4) @[IBuf.scala 44:85]
    node _io_imem_ready_T_6 = or(_io_imem_ready_T_2, _io_imem_ready_T_5) @[IBuf.scala 44:80]
    node _nBufValid_T_1 = not(nBufValid) @[package.scala 209:43]
    node _nBufValid_T_2 = or(_io_imem_ready_T, _nBufValid_T_1) @[package.scala 209:38]
    node _nBufValid_T_3 = sub(_GEN_57, nReady) @[IBuf.scala 48:65]
    node _nBufValid_T_4 = tail(_nBufValid_T_3, 1) @[IBuf.scala 48:65]
    node _nBufValid_T_5 = mux(_nBufValid_T_2, UInt<2>("h0"), _nBufValid_T_4) @[IBuf.scala 48:23]
    node _T_1 = and(io_imem_valid, _io_imem_ready_T) @[IBuf.scala 54:27]
    node _T_2 = lt(nICReady, nIC) @[IBuf.scala 54:62]
    node _T_3 = and(_T_1, _T_2) @[IBuf.scala 54:50]
    node _T_7 = and(_T_3, _io_imem_ready_T_5) @[IBuf.scala 54:68]
    node _shamt_T = add(_GEN_56, nICReady) @[IBuf.scala 55:32]
    node shamt = tail(_shamt_T, 1) @[IBuf.scala 55:32]
    node buf_data_data_hi = shr(io_imem_bits_data, 16) @[IBuf.scala 127:58]
    node buf_data_data_hi_1 = cat(buf_data_data_hi, buf_data_data_hi) @[Cat.scala 30:58]
    node buf_data_data = cat(buf_data_data_hi_1, io_imem_bits_data) @[Cat.scala 30:58]
    node _buf_data_T = shl(shamt, 4) @[IBuf.scala 128:19]
    node _buf_data_T_1 = dshr(buf_data_data, _buf_data_T) @[IBuf.scala 128:10]
    node _buf_data_T_2 = bits(_buf_data_T_1, 15, 0) @[IBuf.scala 58:61]
    node _buf_pc_T_1 = and(io_imem_bits_pc, UInt<32>("hfffffffc")) @[IBuf.scala 59:35]
    node _buf_pc_T_2 = shl(nICReady, 1) @[IBuf.scala 59:80]
    node _GEN_65 = pad(_buf_pc_T_2, 32) @[IBuf.scala 59:68]
    node _buf_pc_T_3 = add(io_imem_bits_pc, _GEN_65) @[IBuf.scala 59:68]
    node _buf_pc_T_4 = tail(_buf_pc_T_3, 1) @[IBuf.scala 59:68]
    node _buf_pc_T_5 = and(_buf_pc_T_4, UInt<32>("h3")) @[IBuf.scala 59:109]
    node _buf_pc_T_6 = or(_buf_pc_T_1, _buf_pc_T_5) @[IBuf.scala 59:49]
    node _GEN_0 = mux(_T_7, _io_imem_ready_T_4, _nBufValid_T_5) @[IBuf.scala 48:17 54:92 56:19]
    node _GEN_9 = mux(_T_7, _buf_pc_T_6, buf__pc) @[IBuf.scala 35:16 54:92 59:16]
    node _GEN_10 = mux(_T_7, pad(_buf_data_T_2, 32), buf__data) @[IBuf.scala 35:16 54:92 58:18]
    node _GEN_13 = mux(_T_7, io_imem_bits_xcpt_ae_inst, buf__xcpt_ae_inst) @[IBuf.scala 54:92 57:13 35:16]
    node _GEN_14 = mux(_T_7, io_imem_bits_replay, buf__replay) @[IBuf.scala 54:92 57:13 35:16]
    node _GEN_23 = mux(io_inst_0_ready, _GEN_0, pad(nBufValid, 2)) @[IBuf.scala 47:29 34:47]
    node _GEN_46 = mux(io_kill, UInt<2>("h0"), _GEN_23) @[IBuf.scala 63:20 64:17]
    node _icShiftAmt_T = add(UInt<2>("h2"), _GEN_57) @[IBuf.scala 68:32]
    node _icShiftAmt_T_1 = tail(_icShiftAmt_T, 1) @[IBuf.scala 68:32]
    node _icShiftAmt_T_2 = sub(_icShiftAmt_T_1, _GEN_56) @[IBuf.scala 68:44]
    node icShiftAmt = tail(_icShiftAmt_T_2, 1) @[IBuf.scala 68:44]
    node icData_hi = bits(io_imem_bits_data, 15, 0) @[IBuf.scala 69:87]
    node icData_lo = cat(icData_hi, icData_hi) @[Cat.scala 30:58]
    node icData_data_lo = cat(io_imem_bits_data, icData_lo) @[Cat.scala 30:58]
    node icData_data_hi = shr(icData_data_lo, 48) @[IBuf.scala 120:58]
    node icData_data_hi_1 = cat(icData_data_hi, icData_data_hi) @[Cat.scala 30:58]
    node icData_data_hi_2 = cat(icData_data_hi_1, icData_data_hi_1) @[Cat.scala 30:58]
    node icData_data = cat(icData_data_hi_2, icData_data_lo) @[Cat.scala 30:58]
    node _icData_T = shl(icShiftAmt, 4) @[IBuf.scala 121:19]
    node _icData_T_1 = dshl(icData_data, _icData_T) @[IBuf.scala 121:10]
    node icData = bits(_icData_T_1, 95, 64) @[package.scala 154:13]
    node _icMask_T_1 = shl(nBufValid, 4) @[IBuf.scala 71:65]
    node _icMask_T_2 = dshl(UInt<32>("hffffffff"), _icMask_T_1) @[IBuf.scala 71:51]
    node icMask = bits(_icMask_T_2, 31, 0) @[IBuf.scala 71:92]
    node _inst_T = and(icData, icMask) @[IBuf.scala 72:21]
    node _inst_T_1 = not(icMask) @[IBuf.scala 72:43]
    node _inst_T_2 = and(buf__data, _inst_T_1) @[IBuf.scala 72:41]
    node _xcpt_T = bits(bufMask, 0, 0) @[IBuf.scala 76:61]
    node _xcpt_T_1 = bits(bufMask, 1, 1) @[IBuf.scala 76:61]
    node xcpt_1_ae_inst = mux(_xcpt_T_1, buf__xcpt_ae_inst, io_imem_bits_xcpt_ae_inst) @[IBuf.scala 76:53]
    node _ic_replay_T = not(bufMask) @[IBuf.scala 78:65]
    node _ic_replay_T_1 = and(valid, _ic_replay_T) @[IBuf.scala 78:63]
    node _ic_replay_T_2 = mux(io_imem_bits_replay, _ic_replay_T_1, UInt<2>("h0")) @[IBuf.scala 78:35]
    node ic_replay = or(buf_replay, _ic_replay_T_2) @[IBuf.scala 78:30]
    node _io_pc_T = gt(nBufValid, UInt<1>("h0")) @[IBuf.scala 82:26]
    node _replay_T_1 = bits(ic_replay, 0, 0) @[IBuf.scala 92:29]
    node _replay_T_2 = not(exp.io_rvc) @[IBuf.scala 92:37]
    node _GEN_68 = shr(ic_replay, 1) @[IBuf.scala 92:61]
    node _replay_T_5 = pad(_GEN_68, 2) @[IBuf.scala 92:61]
    node _replay_T_6 = bits(_replay_T_5, 0, 0) @[IBuf.scala 92:61]
    node _replay_T_7 = and(_replay_T_2, _replay_T_6) @[IBuf.scala 92:49]
    node _io_inst_0_valid_T_1 = bits(valid, 0, 0) @[IBuf.scala 94:32]
    node _io_inst_0_bits_xcpt1_T_4 = cat(UInt<1>("h0"), xcpt_1_ae_inst) @[IBuf.scala 96:63]
    node _io_inst_0_bits_xcpt1_T_5 = mux(exp.io_rvc, UInt<2>("h0"), _io_inst_0_bits_xcpt1_T_4) @[IBuf.scala 96:35]
    node _GEN_70 = mux(reset, UInt<2>("h0"), _GEN_46) @[IBuf.scala 34:{47,47}]
    io_imem_ready <= and(_io_imem_ready_T_1, _io_imem_ready_T_6) @[IBuf.scala 44:60]
    io_pc <= mux(_io_pc_T, buf__pc, io_imem_bits_pc) @[IBuf.scala 82:15]
    io_inst_0_valid <= and(_io_inst_0_valid_T_1, full_insn) @[IBuf.scala 94:36]
    io_inst_0_bits_xcpt0_ae_inst <= mux(_xcpt_T, buf__xcpt_ae_inst, io_imem_bits_xcpt_ae_inst) @[IBuf.scala 76:53]
    io_inst_0_bits_xcpt1_pf_inst <= bits(_io_inst_0_bits_xcpt1_T_5, 1, 1) @[IBuf.scala 96:79]
    io_inst_0_bits_xcpt1_ae_inst <= bits(_io_inst_0_bits_xcpt1_T_5, 0, 0) @[IBuf.scala 96:79]
    io_inst_0_bits_replay <= or(_replay_T_1, _replay_T_7) @[IBuf.scala 92:33]
    io_inst_0_bits_rvc <= exp.io_rvc @[IBuf.scala 98:27]
    io_inst_0_bits_inst_bits <= exp.io_out_bits @[IBuf.scala 88:26]
    io_inst_0_bits_inst_rd <= exp.io_out_rd @[IBuf.scala 88:26]
    io_inst_0_bits_inst_rs1 <= exp.io_out_rs1 @[IBuf.scala 88:26]
    io_inst_0_bits_inst_rs2 <= exp.io_out_rs2 @[IBuf.scala 88:26]
    io_inst_0_bits_raw <= or(_inst_T, _inst_T_2) @[IBuf.scala 72:30]
    exp.io_in <= or(_inst_T, _inst_T_2) @[IBuf.scala 72:30]
    nBufValid <= bits(_GEN_70, 0, 0) @[IBuf.scala 34:{47,47}]
    buf__pc <= mux(io_inst_0_ready, _GEN_9, buf__pc) @[IBuf.scala 35:16 47:29]
    buf__data <= mux(io_inst_0_ready, _GEN_10, buf__data) @[IBuf.scala 35:16 47:29]
    buf__xcpt_ae_inst <= mux(io_inst_0_ready, _GEN_13, buf__xcpt_ae_inst) @[IBuf.scala 35:16 47:29]
    buf__replay <= mux(io_inst_0_ready, _GEN_14, buf__replay) @[IBuf.scala 35:16 47:29]

  module CSRFile :
    input clock : Clock
    input reset : UInt<1>
    input io_ungated_clock : Clock
    input io_interrupts_debug : UInt<1>
    input io_interrupts_mtip : UInt<1>
    input io_interrupts_msip : UInt<1>
    input io_interrupts_meip : UInt<1>
    input io_hartid : UInt<1>
    input io_rw_addr : UInt<12>
    input io_rw_cmd : UInt<3>
    output io_rw_rdata : UInt<32>
    input io_rw_wdata : UInt<32>
    input io_decode_0_csr : UInt<12>
    output io_decode_0_fp_illegal : UInt<1>
    output io_decode_0_fp_csr : UInt<1>
    output io_decode_0_read_illegal : UInt<1>
    output io_decode_0_write_illegal : UInt<1>
    output io_decode_0_write_flush : UInt<1>
    output io_decode_0_system_illegal : UInt<1>
    output io_csr_stall : UInt<1>
    output io_eret : UInt<1>
    output io_singleStep : UInt<1>
    output io_status_debug : UInt<1>
    output io_status_cease : UInt<1>
    output io_status_wfi : UInt<1>
    output io_status_isa : UInt<32>
    output io_status_dprv : UInt<2>
    output io_status_prv : UInt<2>
    output io_status_sd : UInt<1>
    output io_status_zero2 : UInt<27>
    output io_status_sxl : UInt<2>
    output io_status_uxl : UInt<2>
    output io_status_sd_rv32 : UInt<1>
    output io_status_zero1 : UInt<8>
    output io_status_tsr : UInt<1>
    output io_status_tw : UInt<1>
    output io_status_tvm : UInt<1>
    output io_status_mxr : UInt<1>
    output io_status_sum : UInt<1>
    output io_status_mprv : UInt<1>
    output io_status_xs : UInt<2>
    output io_status_fs : UInt<2>
    output io_status_mpp : UInt<2>
    output io_status_vs : UInt<2>
    output io_status_spp : UInt<1>
    output io_status_mpie : UInt<1>
    output io_status_hpie : UInt<1>
    output io_status_spie : UInt<1>
    output io_status_upie : UInt<1>
    output io_status_mie : UInt<1>
    output io_status_hie : UInt<1>
    output io_status_sie : UInt<1>
    output io_status_uie : UInt<1>
    output io_evec : UInt<32>
    input io_exception : UInt<1>
    input io_retire : UInt<1>
    input io_cause : UInt<32>
    input io_pc : UInt<32>
    input io_tval : UInt<32>
    output io_time : UInt<32>
    output io_interrupt : UInt<1>
    output io_interrupt_cause : UInt<32>
    output io_bp_0_control_action : UInt<1>
    output io_bp_0_control_tmatch : UInt<2>
    output io_bp_0_control_x : UInt<1>
    output io_bp_0_control_w : UInt<1>
    output io_bp_0_control_r : UInt<1>
    output io_bp_0_address : UInt<32>
    output io_pmp_0_cfg_l : UInt<1>
    output io_pmp_0_cfg_a : UInt<2>
    output io_pmp_0_cfg_x : UInt<1>
    output io_pmp_0_cfg_w : UInt<1>
    output io_pmp_0_cfg_r : UInt<1>
    output io_pmp_0_addr : UInt<30>
    output io_pmp_0_mask : UInt<32>
    output io_pmp_1_cfg_l : UInt<1>
    output io_pmp_1_cfg_a : UInt<2>
    output io_pmp_1_cfg_x : UInt<1>
    output io_pmp_1_cfg_w : UInt<1>
    output io_pmp_1_cfg_r : UInt<1>
    output io_pmp_1_addr : UInt<30>
    output io_pmp_1_mask : UInt<32>
    output io_pmp_2_cfg_l : UInt<1>
    output io_pmp_2_cfg_a : UInt<2>
    output io_pmp_2_cfg_x : UInt<1>
    output io_pmp_2_cfg_w : UInt<1>
    output io_pmp_2_cfg_r : UInt<1>
    output io_pmp_2_addr : UInt<30>
    output io_pmp_2_mask : UInt<32>
    output io_pmp_3_cfg_l : UInt<1>
    output io_pmp_3_cfg_a : UInt<2>
    output io_pmp_3_cfg_x : UInt<1>
    output io_pmp_3_cfg_w : UInt<1>
    output io_pmp_3_cfg_r : UInt<1>
    output io_pmp_3_addr : UInt<30>
    output io_pmp_3_mask : UInt<32>
    output io_pmp_4_cfg_l : UInt<1>
    output io_pmp_4_cfg_a : UInt<2>
    output io_pmp_4_cfg_x : UInt<1>
    output io_pmp_4_cfg_w : UInt<1>
    output io_pmp_4_cfg_r : UInt<1>
    output io_pmp_4_addr : UInt<30>
    output io_pmp_4_mask : UInt<32>
    output io_pmp_5_cfg_l : UInt<1>
    output io_pmp_5_cfg_a : UInt<2>
    output io_pmp_5_cfg_x : UInt<1>
    output io_pmp_5_cfg_w : UInt<1>
    output io_pmp_5_cfg_r : UInt<1>
    output io_pmp_5_addr : UInt<30>
    output io_pmp_5_mask : UInt<32>
    output io_pmp_6_cfg_l : UInt<1>
    output io_pmp_6_cfg_a : UInt<2>
    output io_pmp_6_cfg_x : UInt<1>
    output io_pmp_6_cfg_w : UInt<1>
    output io_pmp_6_cfg_r : UInt<1>
    output io_pmp_6_addr : UInt<30>
    output io_pmp_6_mask : UInt<32>
    output io_pmp_7_cfg_l : UInt<1>
    output io_pmp_7_cfg_a : UInt<2>
    output io_pmp_7_cfg_x : UInt<1>
    output io_pmp_7_cfg_w : UInt<1>
    output io_pmp_7_cfg_r : UInt<1>
    output io_pmp_7_addr : UInt<30>
    output io_pmp_7_mask : UInt<32>
    output io_inhibit_cycle : UInt<1>
    input io_inst_0 : UInt<32>
    output io_trace_0_valid : UInt<1>
    output io_trace_0_iaddr : UInt<32>
    output io_trace_0_insn : UInt<32>
    output io_trace_0_exception : UInt<1>
    output io_customCSRs_0_value : UInt<32>

    reg reg_mstatus_spp : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_spp) @[CSR.scala 319:24]
    reg reg_mstatus_mpie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_mpie) @[CSR.scala 319:24]
    reg reg_mstatus_mie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_mstatus_mie) @[CSR.scala 319:24]
    reg reg_dcsr_ebreakm : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_ebreakm) @[CSR.scala 327:21]
    reg reg_dcsr_cause : UInt<3>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_cause) @[CSR.scala 327:21]
    reg reg_dcsr_step : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_dcsr_step) @[CSR.scala 327:21]
    reg reg_debug : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_debug) @[CSR.scala 368:22]
    reg reg_dpc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_dpc) @[CSR.scala 369:20]
    reg reg_dscratch : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_dscratch) @[CSR.scala 370:25]
    reg reg_singleStepped : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_singleStepped) @[CSR.scala 372:30]
    reg reg_bp_0_control_dmode : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_dmode) @[CSR.scala 378:19]
    reg reg_bp_0_control_action : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_action) @[CSR.scala 378:19]
    reg reg_bp_0_control_tmatch : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_tmatch) @[CSR.scala 378:19]
    reg reg_bp_0_control_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_x) @[CSR.scala 378:19]
    reg reg_bp_0_control_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_w) @[CSR.scala 378:19]
    reg reg_bp_0_control_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_control_r) @[CSR.scala 378:19]
    reg reg_bp_0_address : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_bp_0_address) @[CSR.scala 378:19]
    reg reg_pmp_0_cfg_l : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_0_cfg_l) @[CSR.scala 379:20]
    reg reg_pmp_0_cfg_a : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_0_cfg_a) @[CSR.scala 379:20]
    reg reg_pmp_0_cfg_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_0_cfg_x) @[CSR.scala 379:20]
    reg reg_pmp_0_cfg_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_0_cfg_w) @[CSR.scala 379:20]
    reg reg_pmp_0_cfg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_0_cfg_r) @[CSR.scala 379:20]
    reg reg_pmp_0_addr : UInt<30>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_0_addr) @[CSR.scala 379:20]
    reg reg_pmp_1_cfg_l : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_1_cfg_l) @[CSR.scala 379:20]
    reg reg_pmp_1_cfg_a : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_1_cfg_a) @[CSR.scala 379:20]
    reg reg_pmp_1_cfg_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_1_cfg_x) @[CSR.scala 379:20]
    reg reg_pmp_1_cfg_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_1_cfg_w) @[CSR.scala 379:20]
    reg reg_pmp_1_cfg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_1_cfg_r) @[CSR.scala 379:20]
    reg reg_pmp_1_addr : UInt<30>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_1_addr) @[CSR.scala 379:20]
    reg reg_pmp_2_cfg_l : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_2_cfg_l) @[CSR.scala 379:20]
    reg reg_pmp_2_cfg_a : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_2_cfg_a) @[CSR.scala 379:20]
    reg reg_pmp_2_cfg_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_2_cfg_x) @[CSR.scala 379:20]
    reg reg_pmp_2_cfg_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_2_cfg_w) @[CSR.scala 379:20]
    reg reg_pmp_2_cfg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_2_cfg_r) @[CSR.scala 379:20]
    reg reg_pmp_2_addr : UInt<30>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_2_addr) @[CSR.scala 379:20]
    reg reg_pmp_3_cfg_l : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_3_cfg_l) @[CSR.scala 379:20]
    reg reg_pmp_3_cfg_a : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_3_cfg_a) @[CSR.scala 379:20]
    reg reg_pmp_3_cfg_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_3_cfg_x) @[CSR.scala 379:20]
    reg reg_pmp_3_cfg_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_3_cfg_w) @[CSR.scala 379:20]
    reg reg_pmp_3_cfg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_3_cfg_r) @[CSR.scala 379:20]
    reg reg_pmp_3_addr : UInt<30>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_3_addr) @[CSR.scala 379:20]
    reg reg_pmp_4_cfg_l : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_4_cfg_l) @[CSR.scala 379:20]
    reg reg_pmp_4_cfg_a : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_4_cfg_a) @[CSR.scala 379:20]
    reg reg_pmp_4_cfg_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_4_cfg_x) @[CSR.scala 379:20]
    reg reg_pmp_4_cfg_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_4_cfg_w) @[CSR.scala 379:20]
    reg reg_pmp_4_cfg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_4_cfg_r) @[CSR.scala 379:20]
    reg reg_pmp_4_addr : UInt<30>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_4_addr) @[CSR.scala 379:20]
    reg reg_pmp_5_cfg_l : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_5_cfg_l) @[CSR.scala 379:20]
    reg reg_pmp_5_cfg_a : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_5_cfg_a) @[CSR.scala 379:20]
    reg reg_pmp_5_cfg_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_5_cfg_x) @[CSR.scala 379:20]
    reg reg_pmp_5_cfg_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_5_cfg_w) @[CSR.scala 379:20]
    reg reg_pmp_5_cfg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_5_cfg_r) @[CSR.scala 379:20]
    reg reg_pmp_5_addr : UInt<30>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_5_addr) @[CSR.scala 379:20]
    reg reg_pmp_6_cfg_l : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_6_cfg_l) @[CSR.scala 379:20]
    reg reg_pmp_6_cfg_a : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_6_cfg_a) @[CSR.scala 379:20]
    reg reg_pmp_6_cfg_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_6_cfg_x) @[CSR.scala 379:20]
    reg reg_pmp_6_cfg_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_6_cfg_w) @[CSR.scala 379:20]
    reg reg_pmp_6_cfg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_6_cfg_r) @[CSR.scala 379:20]
    reg reg_pmp_6_addr : UInt<30>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_6_addr) @[CSR.scala 379:20]
    reg reg_pmp_7_cfg_l : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_7_cfg_l) @[CSR.scala 379:20]
    reg reg_pmp_7_cfg_a : UInt<2>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_7_cfg_a) @[CSR.scala 379:20]
    reg reg_pmp_7_cfg_x : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_7_cfg_x) @[CSR.scala 379:20]
    reg reg_pmp_7_cfg_w : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_7_cfg_w) @[CSR.scala 379:20]
    reg reg_pmp_7_cfg_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_7_cfg_r) @[CSR.scala 379:20]
    reg reg_pmp_7_addr : UInt<30>, clock with :
      reset => (UInt<1>("h0"), reg_pmp_7_addr) @[CSR.scala 379:20]
    reg reg_mie : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_mie) @[CSR.scala 381:20]
    reg reg_mepc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_mepc) @[CSR.scala 391:21]
    reg reg_mcause : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_mcause) @[CSR.scala 392:27]
    reg reg_mtval : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_mtval) @[CSR.scala 393:22]
    reg reg_mscratch : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_mscratch) @[CSR.scala 394:25]
    reg reg_mtvec : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_mtvec) @[CSR.scala 397:27]
    reg reg_wfi : UInt<1>, io_ungated_clock with :
      reset => (UInt<1>("h0"), reg_wfi) @[CSR.scala 427:50]
    reg reg_mcountinhibit : UInt<3>, clock with :
      reset => (UInt<1>("h0"), reg_mcountinhibit) @[CSR.scala 436:34]
    node _io_inhibit_cycle_T = bits(reg_mcountinhibit, 0, 0) @[CSR.scala 437:40]
    node x67 = bits(reg_mcountinhibit, 2, 2) @[CSR.scala 438:75]
    reg value_lo : UInt<6>, clock with :
      reset => (UInt<1>("h0"), value_lo) @[Counters.scala 45:37]
    node _GEN_34 = pad(io_retire, 6) @[Counters.scala 46:33]
    node nextSmall = add(value_lo, _GEN_34) @[Counters.scala 46:33]
    node _T_5 = not(x67) @[Counters.scala 47:9]
    node _GEN_0 = mux(_T_5, nextSmall, pad(value_lo, 7)) @[Counters.scala 47:{19,27} 45:37]
    reg value_hi : UInt<58>, clock with :
      reset => (UInt<1>("h0"), value_hi) @[Counters.scala 50:27]
    node _large_T = bits(nextSmall, 6, 6) @[Counters.scala 51:20]
    node _large_T_2 = and(_large_T, _T_5) @[Counters.scala 51:33]
    node _large_r_T = add(value_hi, UInt<58>("h1")) @[Counters.scala 51:55]
    node _large_r_T_1 = tail(_large_r_T, 1) @[Counters.scala 51:55]
    node _GEN_1 = mux(_large_T_2, _large_r_T_1, value_hi) @[Counters.scala 50:27 51:{46,50}]
    node value = cat(value_hi, value_lo) @[Cat.scala 30:58]
    node x74 = not(io_csr_stall) @[CSR.scala 440:56]
    reg value_lo_1 : UInt<6>, io_ungated_clock with :
      reset => (UInt<1>("h0"), value_lo_1) @[Counters.scala 45:37]
    node _GEN_35 = pad(x74, 6) @[Counters.scala 46:33]
    node nextSmall_1 = add(value_lo_1, _GEN_35) @[Counters.scala 46:33]
    node _T_6 = not(_io_inhibit_cycle_T) @[Counters.scala 47:9]
    node _GEN_2 = mux(_T_6, nextSmall_1, pad(value_lo_1, 7)) @[Counters.scala 47:{19,27} 45:37]
    reg value_hi_1 : UInt<58>, io_ungated_clock with :
      reset => (UInt<1>("h0"), value_hi_1) @[Counters.scala 50:27]
    node _large_T_3 = bits(nextSmall_1, 6, 6) @[Counters.scala 51:20]
    node _large_T_5 = and(_large_T_3, _T_6) @[Counters.scala 51:33]
    node _large_r_T_2 = add(value_hi_1, UInt<58>("h1")) @[Counters.scala 51:55]
    node _large_r_T_3 = tail(_large_r_T_2, 1) @[Counters.scala 51:55]
    node _GEN_3 = mux(_large_T_5, _large_r_T_3, value_hi_1) @[Counters.scala 50:27 51:{46,50}]
    node value_1 = cat(value_hi_1, value_lo_1) @[Cat.scala 30:58]
    node read_mip_lo_lo_hi = cat(io_interrupts_msip, UInt<1>("h0")) @[CSR.scala 454:22]
    node read_mip_lo_lo = cat(read_mip_lo_lo_hi, UInt<2>("h0")) @[CSR.scala 454:22]
    node read_mip_lo_hi_hi = cat(io_interrupts_mtip, UInt<1>("h0")) @[CSR.scala 454:22]
    node read_mip_lo_hi = cat(read_mip_lo_hi_hi, UInt<2>("h0")) @[CSR.scala 454:22]
    node read_mip_lo = cat(read_mip_lo_hi, read_mip_lo_lo) @[CSR.scala 454:22]
    node read_mip_hi_lo_hi = cat(io_interrupts_meip, UInt<1>("h0")) @[CSR.scala 454:22]
    node read_mip_hi_lo = cat(read_mip_hi_lo_hi, UInt<2>("h0")) @[CSR.scala 454:22]
    node read_mip_hi = cat(UInt<4>("h0"), read_mip_hi_lo) @[CSR.scala 454:22]
    node _read_mip_T = cat(read_mip_hi, read_mip_lo) @[CSR.scala 454:22]
    node read_mip = and(_read_mip_T, UInt<16>("h888")) @[CSR.scala 454:29]
    node _GEN_40 = pad(read_mip, 32) @[CSR.scala 457:56]
    node pending_interrupts = and(_GEN_40, reg_mie) @[CSR.scala 457:56]
    node d_interrupts = shl(io_interrupts_debug, 14) @[CSR.scala 458:42]
    node _m_interrupts_T_3 = not(pending_interrupts) @[CSR.scala 464:83]
    node _m_interrupts_T_5 = not(_m_interrupts_T_3) @[CSR.scala 464:81]
    node m_interrupts = mux(reg_mstatus_mie, _m_interrupts_T_5, UInt<32>("h0")) @[CSR.scala 464:25]
    node _any_T = bits(d_interrupts, 14, 14) @[CSR.scala 1177:76]
    node _any_T_1 = bits(d_interrupts, 13, 13) @[CSR.scala 1177:76]
    node _any_T_2 = bits(d_interrupts, 12, 12) @[CSR.scala 1177:76]
    node _any_T_3 = bits(d_interrupts, 11, 11) @[CSR.scala 1177:76]
    node _any_T_4 = bits(d_interrupts, 3, 3) @[CSR.scala 1177:76]
    node _any_T_5 = bits(d_interrupts, 7, 7) @[CSR.scala 1177:76]
    node _any_T_6 = bits(d_interrupts, 9, 9) @[CSR.scala 1177:76]
    node _any_T_7 = bits(d_interrupts, 1, 1) @[CSR.scala 1177:76]
    node _any_T_8 = bits(d_interrupts, 5, 5) @[CSR.scala 1177:76]
    node _any_T_9 = bits(d_interrupts, 8, 8) @[CSR.scala 1177:76]
    node _any_T_10 = bits(d_interrupts, 0, 0) @[CSR.scala 1177:76]
    node _any_T_11 = bits(d_interrupts, 4, 4) @[CSR.scala 1177:76]
    node _any_T_12 = bits(m_interrupts, 15, 15) @[CSR.scala 1177:76]
    node _any_T_13 = bits(m_interrupts, 14, 14) @[CSR.scala 1177:76]
    node _any_T_14 = bits(m_interrupts, 13, 13) @[CSR.scala 1177:76]
    node _any_T_15 = bits(m_interrupts, 12, 12) @[CSR.scala 1177:76]
    node _any_T_16 = bits(m_interrupts, 11, 11) @[CSR.scala 1177:76]
    node _any_T_17 = bits(m_interrupts, 3, 3) @[CSR.scala 1177:76]
    node _any_T_18 = bits(m_interrupts, 7, 7) @[CSR.scala 1177:76]
    node _any_T_19 = bits(m_interrupts, 9, 9) @[CSR.scala 1177:76]
    node _any_T_20 = bits(m_interrupts, 1, 1) @[CSR.scala 1177:76]
    node _any_T_21 = bits(m_interrupts, 5, 5) @[CSR.scala 1177:76]
    node _any_T_22 = bits(m_interrupts, 8, 8) @[CSR.scala 1177:76]
    node _any_T_23 = bits(m_interrupts, 0, 0) @[CSR.scala 1177:76]
    node _any_T_24 = bits(m_interrupts, 4, 4) @[CSR.scala 1177:76]
    node _any_T_38 = or(_any_T, _any_T_1) @[CSR.scala 1177:90]
    node _any_T_39 = or(_any_T_38, _any_T_2) @[CSR.scala 1177:90]
    node _any_T_40 = or(_any_T_39, _any_T_3) @[CSR.scala 1177:90]
    node _any_T_41 = or(_any_T_40, _any_T_4) @[CSR.scala 1177:90]
    node _any_T_42 = or(_any_T_41, _any_T_5) @[CSR.scala 1177:90]
    node _any_T_43 = or(_any_T_42, _any_T_6) @[CSR.scala 1177:90]
    node _any_T_44 = or(_any_T_43, _any_T_7) @[CSR.scala 1177:90]
    node _any_T_45 = or(_any_T_44, _any_T_8) @[CSR.scala 1177:90]
    node _any_T_46 = or(_any_T_45, _any_T_9) @[CSR.scala 1177:90]
    node _any_T_47 = or(_any_T_46, _any_T_10) @[CSR.scala 1177:90]
    node _any_T_48 = or(_any_T_47, _any_T_11) @[CSR.scala 1177:90]
    node _any_T_50 = or(_any_T_48, _any_T_12) @[CSR.scala 1177:90]
    node _any_T_51 = or(_any_T_50, _any_T_13) @[CSR.scala 1177:90]
    node _any_T_52 = or(_any_T_51, _any_T_14) @[CSR.scala 1177:90]
    node _any_T_53 = or(_any_T_52, _any_T_15) @[CSR.scala 1177:90]
    node _any_T_54 = or(_any_T_53, _any_T_16) @[CSR.scala 1177:90]
    node _any_T_55 = or(_any_T_54, _any_T_17) @[CSR.scala 1177:90]
    node _any_T_56 = or(_any_T_55, _any_T_18) @[CSR.scala 1177:90]
    node _any_T_57 = or(_any_T_56, _any_T_19) @[CSR.scala 1177:90]
    node _any_T_58 = or(_any_T_57, _any_T_20) @[CSR.scala 1177:90]
    node _any_T_59 = or(_any_T_58, _any_T_21) @[CSR.scala 1177:90]
    node _any_T_60 = or(_any_T_59, _any_T_22) @[CSR.scala 1177:90]
    node _any_T_61 = or(_any_T_60, _any_T_23) @[CSR.scala 1177:90]
    node anyInterrupt = or(_any_T_61, _any_T_24) @[CSR.scala 1177:90]
    node _which_T_51 = mux(_any_T_23, UInt<4>("h0"), UInt<4>("h4")) @[Mux.scala 47:69]
    node _which_T_52 = mux(_any_T_22, UInt<4>("h8"), _which_T_51) @[Mux.scala 47:69]
    node _which_T_53 = mux(_any_T_21, UInt<4>("h5"), _which_T_52) @[Mux.scala 47:69]
    node _which_T_54 = mux(_any_T_20, UInt<4>("h1"), _which_T_53) @[Mux.scala 47:69]
    node _which_T_55 = mux(_any_T_19, UInt<4>("h9"), _which_T_54) @[Mux.scala 47:69]
    node _which_T_56 = mux(_any_T_18, UInt<4>("h7"), _which_T_55) @[Mux.scala 47:69]
    node _which_T_57 = mux(_any_T_17, UInt<4>("h3"), _which_T_56) @[Mux.scala 47:69]
    node _which_T_58 = mux(_any_T_16, UInt<4>("hb"), _which_T_57) @[Mux.scala 47:69]
    node _which_T_59 = mux(_any_T_15, UInt<4>("hc"), _which_T_58) @[Mux.scala 47:69]
    node _which_T_60 = mux(_any_T_14, UInt<4>("hd"), _which_T_59) @[Mux.scala 47:69]
    node _which_T_61 = mux(_any_T_13, UInt<4>("he"), _which_T_60) @[Mux.scala 47:69]
    node _which_T_62 = mux(_any_T_12, UInt<4>("hf"), _which_T_61) @[Mux.scala 47:69]
    node _which_T_64 = mux(_any_T_11, UInt<4>("h4"), _which_T_62) @[Mux.scala 47:69]
    node _which_T_65 = mux(_any_T_10, UInt<4>("h0"), _which_T_64) @[Mux.scala 47:69]
    node _which_T_66 = mux(_any_T_9, UInt<4>("h8"), _which_T_65) @[Mux.scala 47:69]
    node _which_T_67 = mux(_any_T_8, UInt<4>("h5"), _which_T_66) @[Mux.scala 47:69]
    node _which_T_68 = mux(_any_T_7, UInt<4>("h1"), _which_T_67) @[Mux.scala 47:69]
    node _which_T_69 = mux(_any_T_6, UInt<4>("h9"), _which_T_68) @[Mux.scala 47:69]
    node _which_T_70 = mux(_any_T_5, UInt<4>("h7"), _which_T_69) @[Mux.scala 47:69]
    node _which_T_71 = mux(_any_T_4, UInt<4>("h3"), _which_T_70) @[Mux.scala 47:69]
    node _which_T_72 = mux(_any_T_3, UInt<4>("hb"), _which_T_71) @[Mux.scala 47:69]
    node _which_T_73 = mux(_any_T_2, UInt<4>("hc"), _which_T_72) @[Mux.scala 47:69]
    node _which_T_74 = mux(_any_T_1, UInt<4>("hd"), _which_T_73) @[Mux.scala 47:69]
    node whichInterrupt = mux(_any_T, UInt<4>("he"), _which_T_74) @[Mux.scala 47:69]
    node _GEN_41 = pad(whichInterrupt, 32) @[CSR.scala 468:67]
    node _interruptCause_T_3 = add(UInt<32>("h80000000"), _GEN_41) @[CSR.scala 468:67]
    node _io_interrupt_T = not(io_singleStep) @[CSR.scala 469:36]
    node _io_interrupt_T_1 = and(anyInterrupt, _io_interrupt_T) @[CSR.scala 469:33]
    node _io_interrupt_T_2 = or(_io_interrupt_T_1, reg_singleStepped) @[CSR.scala 469:51]
    node _io_interrupt_T_3 = or(reg_debug, io_status_cease) @[CSR.scala 469:88]
    node _io_interrupt_T_4 = not(_io_interrupt_T_3) @[CSR.scala 469:76]
    node pmp_mask_base_lo = bits(reg_pmp_0_cfg_a, 0, 0) @[PMP.scala 59:31]
    node pmp_mask_base = cat(reg_pmp_0_addr, pmp_mask_base_lo) @[Cat.scala 30:58]
    node _pmp_mask_T = add(pmp_mask_base, UInt<31>("h1")) @[PMP.scala 60:23]
    node _pmp_mask_T_1 = tail(_pmp_mask_T, 1) @[PMP.scala 60:23]
    node _pmp_mask_T_2 = not(_pmp_mask_T_1) @[PMP.scala 60:16]
    node pmp_mask_hi = and(pmp_mask_base, _pmp_mask_T_2) @[PMP.scala 60:14]
    node _pmp_mask_T_3 = cat(pmp_mask_hi, UInt<2>("h3")) @[Cat.scala 30:58]
    node pmp_mask_base_lo_1 = bits(reg_pmp_1_cfg_a, 0, 0) @[PMP.scala 59:31]
    node pmp_mask_base_1 = cat(reg_pmp_1_addr, pmp_mask_base_lo_1) @[Cat.scala 30:58]
    node _pmp_mask_T_4 = add(pmp_mask_base_1, UInt<31>("h1")) @[PMP.scala 60:23]
    node _pmp_mask_T_5 = tail(_pmp_mask_T_4, 1) @[PMP.scala 60:23]
    node _pmp_mask_T_6 = not(_pmp_mask_T_5) @[PMP.scala 60:16]
    node pmp_mask_hi_1 = and(pmp_mask_base_1, _pmp_mask_T_6) @[PMP.scala 60:14]
    node _pmp_mask_T_7 = cat(pmp_mask_hi_1, UInt<2>("h3")) @[Cat.scala 30:58]
    node pmp_mask_base_lo_2 = bits(reg_pmp_2_cfg_a, 0, 0) @[PMP.scala 59:31]
    node pmp_mask_base_2 = cat(reg_pmp_2_addr, pmp_mask_base_lo_2) @[Cat.scala 30:58]
    node _pmp_mask_T_8 = add(pmp_mask_base_2, UInt<31>("h1")) @[PMP.scala 60:23]
    node _pmp_mask_T_9 = tail(_pmp_mask_T_8, 1) @[PMP.scala 60:23]
    node _pmp_mask_T_10 = not(_pmp_mask_T_9) @[PMP.scala 60:16]
    node pmp_mask_hi_2 = and(pmp_mask_base_2, _pmp_mask_T_10) @[PMP.scala 60:14]
    node _pmp_mask_T_11 = cat(pmp_mask_hi_2, UInt<2>("h3")) @[Cat.scala 30:58]
    node pmp_mask_base_lo_3 = bits(reg_pmp_3_cfg_a, 0, 0) @[PMP.scala 59:31]
    node pmp_mask_base_3 = cat(reg_pmp_3_addr, pmp_mask_base_lo_3) @[Cat.scala 30:58]
    node _pmp_mask_T_12 = add(pmp_mask_base_3, UInt<31>("h1")) @[PMP.scala 60:23]
    node _pmp_mask_T_13 = tail(_pmp_mask_T_12, 1) @[PMP.scala 60:23]
    node _pmp_mask_T_14 = not(_pmp_mask_T_13) @[PMP.scala 60:16]
    node pmp_mask_hi_3 = and(pmp_mask_base_3, _pmp_mask_T_14) @[PMP.scala 60:14]
    node _pmp_mask_T_15 = cat(pmp_mask_hi_3, UInt<2>("h3")) @[Cat.scala 30:58]
    node pmp_mask_base_lo_4 = bits(reg_pmp_4_cfg_a, 0, 0) @[PMP.scala 59:31]
    node pmp_mask_base_4 = cat(reg_pmp_4_addr, pmp_mask_base_lo_4) @[Cat.scala 30:58]
    node _pmp_mask_T_16 = add(pmp_mask_base_4, UInt<31>("h1")) @[PMP.scala 60:23]
    node _pmp_mask_T_17 = tail(_pmp_mask_T_16, 1) @[PMP.scala 60:23]
    node _pmp_mask_T_18 = not(_pmp_mask_T_17) @[PMP.scala 60:16]
    node pmp_mask_hi_4 = and(pmp_mask_base_4, _pmp_mask_T_18) @[PMP.scala 60:14]
    node _pmp_mask_T_19 = cat(pmp_mask_hi_4, UInt<2>("h3")) @[Cat.scala 30:58]
    node pmp_mask_base_lo_5 = bits(reg_pmp_5_cfg_a, 0, 0) @[PMP.scala 59:31]
    node pmp_mask_base_5 = cat(reg_pmp_5_addr, pmp_mask_base_lo_5) @[Cat.scala 30:58]
    node _pmp_mask_T_20 = add(pmp_mask_base_5, UInt<31>("h1")) @[PMP.scala 60:23]
    node _pmp_mask_T_21 = tail(_pmp_mask_T_20, 1) @[PMP.scala 60:23]
    node _pmp_mask_T_22 = not(_pmp_mask_T_21) @[PMP.scala 60:16]
    node pmp_mask_hi_5 = and(pmp_mask_base_5, _pmp_mask_T_22) @[PMP.scala 60:14]
    node _pmp_mask_T_23 = cat(pmp_mask_hi_5, UInt<2>("h3")) @[Cat.scala 30:58]
    node pmp_mask_base_lo_6 = bits(reg_pmp_6_cfg_a, 0, 0) @[PMP.scala 59:31]
    node pmp_mask_base_6 = cat(reg_pmp_6_addr, pmp_mask_base_lo_6) @[Cat.scala 30:58]
    node _pmp_mask_T_24 = add(pmp_mask_base_6, UInt<31>("h1")) @[PMP.scala 60:23]
    node _pmp_mask_T_25 = tail(_pmp_mask_T_24, 1) @[PMP.scala 60:23]
    node _pmp_mask_T_26 = not(_pmp_mask_T_25) @[PMP.scala 60:16]
    node pmp_mask_hi_6 = and(pmp_mask_base_6, _pmp_mask_T_26) @[PMP.scala 60:14]
    node _pmp_mask_T_27 = cat(pmp_mask_hi_6, UInt<2>("h3")) @[Cat.scala 30:58]
    node pmp_mask_base_lo_7 = bits(reg_pmp_7_cfg_a, 0, 0) @[PMP.scala 59:31]
    node pmp_mask_base_7 = cat(reg_pmp_7_addr, pmp_mask_base_lo_7) @[Cat.scala 30:58]
    node _pmp_mask_T_28 = add(pmp_mask_base_7, UInt<31>("h1")) @[PMP.scala 60:23]
    node _pmp_mask_T_29 = tail(_pmp_mask_T_28, 1) @[PMP.scala 60:23]
    node _pmp_mask_T_30 = not(_pmp_mask_T_29) @[PMP.scala 60:16]
    node pmp_mask_hi_7 = and(pmp_mask_base_7, _pmp_mask_T_30) @[PMP.scala 60:14]
    node _pmp_mask_T_31 = cat(pmp_mask_hi_7, UInt<2>("h3")) @[Cat.scala 30:58]
    reg reg_misa : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_misa) @[CSR.scala 490:21]
    node read_mstatus_lo_lo_lo_hi = cat(io_status_hie, io_status_sie) @[CSR.scala 491:38]
    node read_mstatus_lo_lo_lo = cat(read_mstatus_lo_lo_lo_hi, io_status_uie) @[CSR.scala 491:38]
    node read_mstatus_lo_lo_hi_lo = cat(io_status_upie, io_status_mie) @[CSR.scala 491:38]
    node read_mstatus_lo_lo_hi_hi = cat(io_status_hpie, io_status_spie) @[CSR.scala 491:38]
    node read_mstatus_lo_lo_hi = cat(read_mstatus_lo_lo_hi_hi, read_mstatus_lo_lo_hi_lo) @[CSR.scala 491:38]
    node read_mstatus_lo_lo = cat(read_mstatus_lo_lo_hi, read_mstatus_lo_lo_lo) @[CSR.scala 491:38]
    node read_mstatus_lo_hi_lo_lo = cat(io_status_spp, io_status_mpie) @[CSR.scala 491:38]
    node read_mstatus_lo_hi_lo_hi = cat(io_status_mpp, io_status_vs) @[CSR.scala 491:38]
    node read_mstatus_lo_hi_lo = cat(read_mstatus_lo_hi_lo_hi, read_mstatus_lo_hi_lo_lo) @[CSR.scala 491:38]
    node read_mstatus_lo_hi_hi_lo = cat(io_status_xs, io_status_fs) @[CSR.scala 491:38]
    node read_mstatus_lo_hi_hi_hi = cat(io_status_sum, io_status_mprv) @[CSR.scala 491:38]
    node read_mstatus_lo_hi_hi = cat(read_mstatus_lo_hi_hi_hi, read_mstatus_lo_hi_hi_lo) @[CSR.scala 491:38]
    node read_mstatus_lo_hi = cat(read_mstatus_lo_hi_hi, read_mstatus_lo_hi_lo) @[CSR.scala 491:38]
    node read_mstatus_lo = cat(read_mstatus_lo_hi, read_mstatus_lo_lo) @[CSR.scala 491:38]
    node read_mstatus_hi_lo_lo_lo = cat(io_status_tvm, io_status_mxr) @[CSR.scala 491:38]
    node read_mstatus_hi_lo_lo_hi = cat(io_status_tsr, io_status_tw) @[CSR.scala 491:38]
    node read_mstatus_hi_lo_lo = cat(read_mstatus_hi_lo_lo_hi, read_mstatus_hi_lo_lo_lo) @[CSR.scala 491:38]
    node read_mstatus_hi_lo_hi_lo = cat(io_status_sd_rv32, io_status_zero1) @[CSR.scala 491:38]
    node read_mstatus_hi_lo_hi_hi = cat(io_status_sxl, io_status_uxl) @[CSR.scala 491:38]
    node read_mstatus_hi_lo_hi = cat(read_mstatus_hi_lo_hi_hi, read_mstatus_hi_lo_hi_lo) @[CSR.scala 491:38]
    node read_mstatus_hi_lo = cat(read_mstatus_hi_lo_hi, read_mstatus_hi_lo_lo) @[CSR.scala 491:38]
    node read_mstatus_hi_hi_lo_lo = cat(io_status_sd, io_status_zero2) @[CSR.scala 491:38]
    node read_mstatus_hi_hi_lo_hi = cat(io_status_dprv, io_status_prv) @[CSR.scala 491:38]
    node read_mstatus_hi_hi_lo = cat(read_mstatus_hi_hi_lo_hi, read_mstatus_hi_hi_lo_lo) @[CSR.scala 491:38]
    node read_mstatus_hi_hi_hi_lo = cat(io_status_wfi, io_status_isa) @[CSR.scala 491:38]
    node read_mstatus_hi_hi_hi_hi = cat(io_status_debug, io_status_cease) @[CSR.scala 491:38]
    node read_mstatus_hi_hi_hi = cat(read_mstatus_hi_hi_hi_hi, read_mstatus_hi_hi_hi_lo) @[CSR.scala 491:38]
    node read_mstatus_hi_hi = cat(read_mstatus_hi_hi_hi, read_mstatus_hi_hi_lo) @[CSR.scala 491:38]
    node read_mstatus_hi = cat(read_mstatus_hi_hi, read_mstatus_hi_lo) @[CSR.scala 491:38]
    node _read_mstatus_T = cat(read_mstatus_hi, read_mstatus_lo) @[CSR.scala 491:38]
    node read_mstatus = bits(_read_mstatus_T, 31, 0) @[CSR.scala 491:40]
    node _read_mtvec_T = bits(reg_mtvec, 0, 0) @[CSR.scala 1206:41]
    node _read_mtvec_T_1 = mux(_read_mtvec_T, UInt<7>("h7e"), UInt<7>("h2")) @[CSR.scala 1206:39]
    node _read_mtvec_T_3 = pad(_read_mtvec_T_1, 32) @[package.scala 165:41]
    node _read_mtvec_T_4 = not(_read_mtvec_T_3) @[package.scala 165:37]
    node read_mtvec = and(reg_mtvec, _read_mtvec_T_4) @[package.scala 165:35]
    node lo_lo_hi_2 = cat(reg_bp_0_control_x, reg_bp_0_control_w) @[CSR.scala 497:48]
    node lo_lo_2 = cat(lo_lo_hi_2, reg_bp_0_control_r) @[CSR.scala 497:48]
    node lo_2 = cat(UInt<4>("h8"), lo_lo_2) @[CSR.scala 497:48]
    node hi_lo_lo_2 = cat(UInt<2>("h0"), reg_bp_0_control_tmatch) @[CSR.scala 497:48]
    node hi_lo_hi_2 = cat(reg_bp_0_control_action, UInt<1>("h0")) @[CSR.scala 497:48]
    node hi_lo_2 = cat(hi_lo_hi_2, hi_lo_lo_2) @[CSR.scala 497:48]
    node hi_hi_hi_2 = cat(UInt<4>("h2"), reg_bp_0_control_dmode) @[CSR.scala 497:48]
    node hi_hi_2 = cat(hi_hi_hi_2, UInt<14>("h400")) @[CSR.scala 497:48]
    node hi_2 = cat(hi_hi_2, hi_lo_2) @[CSR.scala 497:48]
    node _T_7 = cat(hi_2, lo_2) @[CSR.scala 497:48]
    node _T_9 = not(reg_mepc) @[CSR.scala 1205:28]
    node _T_10 = bits(reg_misa, 2, 2) @[CSR.scala 1205:45]
    node _T_11 = mux(_T_10, UInt<2>("h1"), UInt<2>("h3")) @[CSR.scala 1205:36]
    node _GEN_483 = pad(_T_11, 32) @[CSR.scala 1205:31]
    node _T_12 = or(_T_9, _GEN_483) @[CSR.scala 1205:31]
    node _T_13 = not(_T_12) @[CSR.scala 1205:26]
    node lo_lo_hi_3 = cat(UInt<3>("h0"), reg_dcsr_step) @[CSR.scala 512:27]
    node lo_lo_3 = cat(lo_lo_hi_3, UInt<2>("h3")) @[CSR.scala 512:27]
    node lo_hi_lo_3 = cat(UInt<1>("h0"), reg_dcsr_cause) @[CSR.scala 512:27]
    node lo_hi_4 = cat(UInt<2>("h0"), lo_hi_lo_3) @[CSR.scala 512:27]
    node lo_4 = cat(lo_hi_4, lo_lo_3) @[CSR.scala 512:27]
    node hi_hi_lo_3 = cat(UInt<12>("h0"), reg_dcsr_ebreakm) @[CSR.scala 512:27]
    node hi_hi_4 = cat(UInt<4>("h4"), hi_hi_lo_3) @[CSR.scala 512:27]
    node hi_4 = cat(hi_hi_4, UInt<3>("h0")) @[CSR.scala 512:27]
    node _T_14 = cat(hi_4, lo_4) @[CSR.scala 512:27]
    node _T_15 = not(reg_dpc) @[CSR.scala 1205:28]
    node _T_18 = or(_T_15, _GEN_483) @[CSR.scala 1205:31]
    node _T_19 = not(_T_18) @[CSR.scala 1205:26]
    node _T_49 = shr(value_1, 32) @[CSR.scala 574:50]
    node _T_50 = shr(value, 32) @[CSR.scala 575:54]
    node lo_hi_5 = cat(reg_pmp_0_cfg_x, reg_pmp_0_cfg_w) @[package.scala 36:38]
    node lo_5 = cat(lo_hi_5, reg_pmp_0_cfg_r) @[package.scala 36:38]
    node hi_hi_5 = cat(reg_pmp_0_cfg_l, UInt<2>("h0")) @[package.scala 36:38]
    node hi_5 = cat(hi_hi_5, reg_pmp_0_cfg_a) @[package.scala 36:38]
    node lo_lo_4 = cat(hi_5, lo_5) @[package.scala 36:38]
    node lo_hi_6 = cat(reg_pmp_1_cfg_x, reg_pmp_1_cfg_w) @[package.scala 36:38]
    node lo_6 = cat(lo_hi_6, reg_pmp_1_cfg_r) @[package.scala 36:38]
    node hi_hi_6 = cat(reg_pmp_1_cfg_l, UInt<2>("h0")) @[package.scala 36:38]
    node hi_6 = cat(hi_hi_6, reg_pmp_1_cfg_a) @[package.scala 36:38]
    node lo_hi_7 = cat(hi_6, lo_6) @[package.scala 36:38]
    node lo_hi_8 = cat(reg_pmp_2_cfg_x, reg_pmp_2_cfg_w) @[package.scala 36:38]
    node lo_7 = cat(lo_hi_8, reg_pmp_2_cfg_r) @[package.scala 36:38]
    node hi_hi_7 = cat(reg_pmp_2_cfg_l, UInt<2>("h0")) @[package.scala 36:38]
    node hi_7 = cat(hi_hi_7, reg_pmp_2_cfg_a) @[package.scala 36:38]
    node hi_lo_4 = cat(hi_7, lo_7) @[package.scala 36:38]
    node lo_hi_9 = cat(reg_pmp_3_cfg_x, reg_pmp_3_cfg_w) @[package.scala 36:38]
    node lo_8 = cat(lo_hi_9, reg_pmp_3_cfg_r) @[package.scala 36:38]
    node hi_hi_8 = cat(reg_pmp_3_cfg_l, UInt<2>("h0")) @[package.scala 36:38]
    node hi_8 = cat(hi_hi_8, reg_pmp_3_cfg_a) @[package.scala 36:38]
    node hi_hi_9 = cat(hi_8, lo_8) @[package.scala 36:38]
    node lo_9 = cat(lo_hi_7, lo_lo_4) @[Cat.scala 30:58]
    node hi_9 = cat(hi_hi_9, hi_lo_4) @[Cat.scala 30:58]
    node _T_51 = cat(hi_9, lo_9) @[Cat.scala 30:58]
    node lo_hi_10 = cat(reg_pmp_4_cfg_x, reg_pmp_4_cfg_w) @[package.scala 36:38]
    node lo_10 = cat(lo_hi_10, reg_pmp_4_cfg_r) @[package.scala 36:38]
    node hi_hi_10 = cat(reg_pmp_4_cfg_l, UInt<2>("h0")) @[package.scala 36:38]
    node hi_10 = cat(hi_hi_10, reg_pmp_4_cfg_a) @[package.scala 36:38]
    node lo_lo_5 = cat(hi_10, lo_10) @[package.scala 36:38]
    node lo_hi_11 = cat(reg_pmp_5_cfg_x, reg_pmp_5_cfg_w) @[package.scala 36:38]
    node lo_11 = cat(lo_hi_11, reg_pmp_5_cfg_r) @[package.scala 36:38]
    node hi_hi_11 = cat(reg_pmp_5_cfg_l, UInt<2>("h0")) @[package.scala 36:38]
    node hi_11 = cat(hi_hi_11, reg_pmp_5_cfg_a) @[package.scala 36:38]
    node lo_hi_12 = cat(hi_11, lo_11) @[package.scala 36:38]
    node lo_hi_13 = cat(reg_pmp_6_cfg_x, reg_pmp_6_cfg_w) @[package.scala 36:38]
    node lo_12 = cat(lo_hi_13, reg_pmp_6_cfg_r) @[package.scala 36:38]
    node hi_hi_12 = cat(reg_pmp_6_cfg_l, UInt<2>("h0")) @[package.scala 36:38]
    node hi_12 = cat(hi_hi_12, reg_pmp_6_cfg_a) @[package.scala 36:38]
    node hi_lo_5 = cat(hi_12, lo_12) @[package.scala 36:38]
    node lo_hi_14 = cat(reg_pmp_7_cfg_x, reg_pmp_7_cfg_w) @[package.scala 36:38]
    node lo_13 = cat(lo_hi_14, reg_pmp_7_cfg_r) @[package.scala 36:38]
    node hi_hi_13 = cat(reg_pmp_7_cfg_l, UInt<2>("h0")) @[package.scala 36:38]
    node hi_13 = cat(hi_hi_13, reg_pmp_7_cfg_a) @[package.scala 36:38]
    node hi_hi_14 = cat(hi_13, lo_13) @[package.scala 36:38]
    node lo_14 = cat(lo_hi_12, lo_lo_5) @[Cat.scala 30:58]
    node hi_14 = cat(hi_hi_14, hi_lo_5) @[Cat.scala 30:58]
    node _T_52 = cat(hi_14, lo_14) @[Cat.scala 30:58]
    reg reg_custom_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg_custom_0) @[CSR.scala 628:43]
    node _T_56 = eq(io_rw_addr, UInt<12>("h7a1")) @[CSR.scala 636:73]
    node _T_57 = eq(io_rw_addr, UInt<12>("h7a2")) @[CSR.scala 636:73]
    node _T_59 = eq(io_rw_addr, UInt<12>("h301")) @[CSR.scala 636:73]
    node _T_60 = eq(io_rw_addr, UInt<12>("h300")) @[CSR.scala 636:73]
    node _T_61 = eq(io_rw_addr, UInt<12>("h305")) @[CSR.scala 636:73]
    node _T_62 = eq(io_rw_addr, UInt<12>("h344")) @[CSR.scala 636:73]
    node _T_63 = eq(io_rw_addr, UInt<12>("h304")) @[CSR.scala 636:73]
    node _T_64 = eq(io_rw_addr, UInt<12>("h340")) @[CSR.scala 636:73]
    node _T_65 = eq(io_rw_addr, UInt<12>("h341")) @[CSR.scala 636:73]
    node _T_66 = eq(io_rw_addr, UInt<12>("h343")) @[CSR.scala 636:73]
    node _T_67 = eq(io_rw_addr, UInt<12>("h342")) @[CSR.scala 636:73]
    node _T_68 = eq(io_rw_addr, UInt<12>("hf14")) @[CSR.scala 636:73]
    node _T_69 = eq(io_rw_addr, UInt<12>("h7b0")) @[CSR.scala 636:73]
    node _T_70 = eq(io_rw_addr, UInt<12>("h7b1")) @[CSR.scala 636:73]
    node _T_71 = eq(io_rw_addr, UInt<12>("h7b2")) @[CSR.scala 636:73]
    node _T_72 = eq(io_rw_addr, UInt<12>("h320")) @[CSR.scala 636:73]
    node _T_73 = eq(io_rw_addr, UInt<12>("hb00")) @[CSR.scala 636:73]
    node _T_74 = eq(io_rw_addr, UInt<12>("hb02")) @[CSR.scala 636:73]
    node _T_162 = eq(io_rw_addr, UInt<12>("hb80")) @[CSR.scala 636:73]
    node _T_163 = eq(io_rw_addr, UInt<12>("hb82")) @[CSR.scala 636:73]
    node _T_164 = eq(io_rw_addr, UInt<12>("h3a0")) @[CSR.scala 636:73]
    node _T_165 = eq(io_rw_addr, UInt<12>("h3a1")) @[CSR.scala 636:73]
    node _T_168 = eq(io_rw_addr, UInt<12>("h3b0")) @[CSR.scala 636:73]
    node _T_169 = eq(io_rw_addr, UInt<12>("h3b1")) @[CSR.scala 636:73]
    node _T_170 = eq(io_rw_addr, UInt<12>("h3b2")) @[CSR.scala 636:73]
    node _T_171 = eq(io_rw_addr, UInt<12>("h3b3")) @[CSR.scala 636:73]
    node _T_172 = eq(io_rw_addr, UInt<12>("h3b4")) @[CSR.scala 636:73]
    node _T_173 = eq(io_rw_addr, UInt<12>("h3b5")) @[CSR.scala 636:73]
    node _T_174 = eq(io_rw_addr, UInt<12>("h3b6")) @[CSR.scala 636:73]
    node _T_175 = eq(io_rw_addr, UInt<12>("h3b7")) @[CSR.scala 636:73]
    node _T_184 = eq(io_rw_addr, UInt<12>("h7c1")) @[CSR.scala 636:73]
    node _T_185 = eq(io_rw_addr, UInt<12>("hf12")) @[CSR.scala 636:73]
    node _T_187 = eq(io_rw_addr, UInt<12>("hf13")) @[CSR.scala 636:73]
    node _wdata_T = bits(io_rw_cmd, 1, 1) @[CSR.scala 1183:13]
    node _wdata_T_1 = mux(_wdata_T, io_rw_rdata, UInt<32>("h0")) @[CSR.scala 1183:9]
    node _wdata_T_2 = or(_wdata_T_1, io_rw_wdata) @[CSR.scala 1183:34]
    node _wdata_T_3 = bits(io_rw_cmd, 1, 0) @[CSR.scala 1183:53]
    node _wdata_T_4 = andr(_wdata_T_3) @[CSR.scala 1183:59]
    node _wdata_T_5 = mux(_wdata_T_4, io_rw_wdata, UInt<32>("h0")) @[CSR.scala 1183:49]
    node _wdata_T_6 = not(_wdata_T_5) @[CSR.scala 1183:45]
    node wdata = and(_wdata_T_2, _wdata_T_6) @[CSR.scala 1183:43]
    node system_insn = eq(io_rw_cmd, UInt<3>("h4")) @[CSR.scala 639:31]
    node _T_188 = shl(io_rw_addr, 20) @[CSR.scala 652:28]
    node _T_189 = and(_T_188, UInt<32>("h20100000")) @[Decode.scala 14:65]
    node _T_190 = eq(_T_189, UInt<32>("h0")) @[Decode.scala 14:121]
    node _T_192 = and(_T_188, UInt<32>("h10100000")) @[Decode.scala 14:65]
    node _T_193 = eq(_T_192, UInt<32>("h100000")) @[Decode.scala 14:121]
    node _T_195 = and(_T_188, UInt<32>("h20400000")) @[Decode.scala 14:65]
    node _T_196 = eq(_T_195, UInt<32>("h20000000")) @[Decode.scala 14:121]
    node _T_198 = and(_T_188, UInt<32>("h20200000")) @[Decode.scala 14:65]
    node _T_199 = eq(_T_198, UInt<32>("h20000000")) @[Decode.scala 14:121]
    node _T_201 = and(_T_188, UInt<32>("h30000000")) @[Decode.scala 14:65]
    node _T_202 = eq(_T_201, UInt<32>("h10000000")) @[Decode.scala 14:121]
    node insn_call = and(system_insn, _T_190) @[CSR.scala 652:95]
    node insn_break = and(system_insn, _T_193) @[CSR.scala 652:95]
    node insn_ret = and(system_insn, _T_196) @[CSR.scala 652:95]
    node insn_cease = and(system_insn, _T_199) @[CSR.scala 652:95]
    node insn_wfi = and(system_insn, _T_202) @[CSR.scala 652:95]
    node _T_209 = shl(io_decode_0_csr, 20) @[CSR.scala 659:30]
    node _T_216 = and(_T_209, UInt<32>("h20400000")) @[Decode.scala 14:65]
    node is_ret = eq(_T_216, UInt<32>("h20000000")) @[Decode.scala 14:121]
    node _io_decode_0_fp_illegal_T = eq(io_status_fs, UInt<2>("h0")) @[CSR.scala 667:39]
    node _io_decode_0_fp_illegal_T_1 = bits(reg_misa, 5, 5) @[CSR.scala 667:57]
    node _io_decode_0_fp_illegal_T_2 = not(_io_decode_0_fp_illegal_T_1) @[CSR.scala 667:48]
    node _io_decode_0_read_illegal_T_2 = eq(io_decode_0_csr, UInt<12>("h7a0")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_3 = eq(io_decode_0_csr, UInt<12>("h7a1")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_4 = eq(io_decode_0_csr, UInt<12>("h7a2")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_5 = eq(io_decode_0_csr, UInt<12>("h7a3")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_6 = eq(io_decode_0_csr, UInt<12>("h301")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_7 = eq(io_decode_0_csr, UInt<12>("h300")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_8 = eq(io_decode_0_csr, UInt<12>("h305")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_9 = eq(io_decode_0_csr, UInt<12>("h344")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_10 = eq(io_decode_0_csr, UInt<12>("h304")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_11 = eq(io_decode_0_csr, UInt<12>("h340")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_12 = eq(io_decode_0_csr, UInt<12>("h341")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_13 = eq(io_decode_0_csr, UInt<12>("h343")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_14 = eq(io_decode_0_csr, UInt<12>("h342")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_15 = eq(io_decode_0_csr, UInt<12>("hf14")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_16 = eq(io_decode_0_csr, UInt<12>("h7b0")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_17 = eq(io_decode_0_csr, UInt<12>("h7b1")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_18 = eq(io_decode_0_csr, UInt<12>("h7b2")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_19 = eq(io_decode_0_csr, UInt<12>("h320")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_20 = eq(io_decode_0_csr, UInt<12>("hb00")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_21 = eq(io_decode_0_csr, UInt<12>("hb02")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_22 = eq(io_decode_0_csr, UInt<12>("h323")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_23 = eq(io_decode_0_csr, UInt<12>("hb03")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_24 = eq(io_decode_0_csr, UInt<12>("hb83")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_25 = eq(io_decode_0_csr, UInt<12>("h324")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_26 = eq(io_decode_0_csr, UInt<12>("hb04")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_27 = eq(io_decode_0_csr, UInt<12>("hb84")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_28 = eq(io_decode_0_csr, UInt<12>("h325")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_29 = eq(io_decode_0_csr, UInt<12>("hb05")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_30 = eq(io_decode_0_csr, UInt<12>("hb85")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_31 = eq(io_decode_0_csr, UInt<12>("h326")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_32 = eq(io_decode_0_csr, UInt<12>("hb06")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_33 = eq(io_decode_0_csr, UInt<12>("hb86")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_34 = eq(io_decode_0_csr, UInt<12>("h327")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_35 = eq(io_decode_0_csr, UInt<12>("hb07")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_36 = eq(io_decode_0_csr, UInt<12>("hb87")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_37 = eq(io_decode_0_csr, UInt<12>("h328")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_38 = eq(io_decode_0_csr, UInt<12>("hb08")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_39 = eq(io_decode_0_csr, UInt<12>("hb88")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_40 = eq(io_decode_0_csr, UInt<12>("h329")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_41 = eq(io_decode_0_csr, UInt<12>("hb09")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_42 = eq(io_decode_0_csr, UInt<12>("hb89")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_43 = eq(io_decode_0_csr, UInt<12>("h32a")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_44 = eq(io_decode_0_csr, UInt<12>("hb0a")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_45 = eq(io_decode_0_csr, UInt<12>("hb8a")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_46 = eq(io_decode_0_csr, UInt<12>("h32b")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_47 = eq(io_decode_0_csr, UInt<12>("hb0b")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_48 = eq(io_decode_0_csr, UInt<12>("hb8b")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_49 = eq(io_decode_0_csr, UInt<12>("h32c")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_50 = eq(io_decode_0_csr, UInt<12>("hb0c")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_51 = eq(io_decode_0_csr, UInt<12>("hb8c")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_52 = eq(io_decode_0_csr, UInt<12>("h32d")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_53 = eq(io_decode_0_csr, UInt<12>("hb0d")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_54 = eq(io_decode_0_csr, UInt<12>("hb8d")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_55 = eq(io_decode_0_csr, UInt<12>("h32e")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_56 = eq(io_decode_0_csr, UInt<12>("hb0e")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_57 = eq(io_decode_0_csr, UInt<12>("hb8e")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_58 = eq(io_decode_0_csr, UInt<12>("h32f")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_59 = eq(io_decode_0_csr, UInt<12>("hb0f")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_60 = eq(io_decode_0_csr, UInt<12>("hb8f")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_61 = eq(io_decode_0_csr, UInt<12>("h330")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_62 = eq(io_decode_0_csr, UInt<12>("hb10")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_63 = eq(io_decode_0_csr, UInt<12>("hb90")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_64 = eq(io_decode_0_csr, UInt<12>("h331")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_65 = eq(io_decode_0_csr, UInt<12>("hb11")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_66 = eq(io_decode_0_csr, UInt<12>("hb91")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_67 = eq(io_decode_0_csr, UInt<12>("h332")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_68 = eq(io_decode_0_csr, UInt<12>("hb12")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_69 = eq(io_decode_0_csr, UInt<12>("hb92")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_70 = eq(io_decode_0_csr, UInt<12>("h333")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_71 = eq(io_decode_0_csr, UInt<12>("hb13")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_72 = eq(io_decode_0_csr, UInt<12>("hb93")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_73 = eq(io_decode_0_csr, UInt<12>("h334")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_74 = eq(io_decode_0_csr, UInt<12>("hb14")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_75 = eq(io_decode_0_csr, UInt<12>("hb94")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_76 = eq(io_decode_0_csr, UInt<12>("h335")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_77 = eq(io_decode_0_csr, UInt<12>("hb15")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_78 = eq(io_decode_0_csr, UInt<12>("hb95")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_79 = eq(io_decode_0_csr, UInt<12>("h336")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_80 = eq(io_decode_0_csr, UInt<12>("hb16")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_81 = eq(io_decode_0_csr, UInt<12>("hb96")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_82 = eq(io_decode_0_csr, UInt<12>("h337")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_83 = eq(io_decode_0_csr, UInt<12>("hb17")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_84 = eq(io_decode_0_csr, UInt<12>("hb97")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_85 = eq(io_decode_0_csr, UInt<12>("h338")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_86 = eq(io_decode_0_csr, UInt<12>("hb18")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_87 = eq(io_decode_0_csr, UInt<12>("hb98")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_88 = eq(io_decode_0_csr, UInt<12>("h339")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_89 = eq(io_decode_0_csr, UInt<12>("hb19")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_90 = eq(io_decode_0_csr, UInt<12>("hb99")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_91 = eq(io_decode_0_csr, UInt<12>("h33a")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_92 = eq(io_decode_0_csr, UInt<12>("hb1a")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_93 = eq(io_decode_0_csr, UInt<12>("hb9a")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_94 = eq(io_decode_0_csr, UInt<12>("h33b")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_95 = eq(io_decode_0_csr, UInt<12>("hb1b")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_96 = eq(io_decode_0_csr, UInt<12>("hb9b")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_97 = eq(io_decode_0_csr, UInt<12>("h33c")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_98 = eq(io_decode_0_csr, UInt<12>("hb1c")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_99 = eq(io_decode_0_csr, UInt<12>("hb9c")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_100 = eq(io_decode_0_csr, UInt<12>("h33d")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_101 = eq(io_decode_0_csr, UInt<12>("hb1d")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_102 = eq(io_decode_0_csr, UInt<12>("hb9d")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_103 = eq(io_decode_0_csr, UInt<12>("h33e")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_104 = eq(io_decode_0_csr, UInt<12>("hb1e")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_105 = eq(io_decode_0_csr, UInt<12>("hb9e")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_106 = eq(io_decode_0_csr, UInt<12>("h33f")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_107 = eq(io_decode_0_csr, UInt<12>("hb1f")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_108 = eq(io_decode_0_csr, UInt<12>("hb9f")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_109 = eq(io_decode_0_csr, UInt<12>("hb80")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_110 = eq(io_decode_0_csr, UInt<12>("hb82")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_111 = eq(io_decode_0_csr, UInt<12>("h3a0")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_112 = eq(io_decode_0_csr, UInt<12>("h3a1")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_113 = eq(io_decode_0_csr, UInt<12>("h3a2")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_114 = eq(io_decode_0_csr, UInt<12>("h3a3")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_115 = eq(io_decode_0_csr, UInt<12>("h3b0")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_116 = eq(io_decode_0_csr, UInt<12>("h3b1")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_117 = eq(io_decode_0_csr, UInt<12>("h3b2")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_118 = eq(io_decode_0_csr, UInt<12>("h3b3")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_119 = eq(io_decode_0_csr, UInt<12>("h3b4")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_120 = eq(io_decode_0_csr, UInt<12>("h3b5")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_121 = eq(io_decode_0_csr, UInt<12>("h3b6")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_122 = eq(io_decode_0_csr, UInt<12>("h3b7")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_123 = eq(io_decode_0_csr, UInt<12>("h3b8")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_124 = eq(io_decode_0_csr, UInt<12>("h3b9")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_125 = eq(io_decode_0_csr, UInt<12>("h3ba")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_126 = eq(io_decode_0_csr, UInt<12>("h3bb")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_127 = eq(io_decode_0_csr, UInt<12>("h3bc")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_128 = eq(io_decode_0_csr, UInt<12>("h3bd")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_129 = eq(io_decode_0_csr, UInt<12>("h3be")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_130 = eq(io_decode_0_csr, UInt<12>("h3bf")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_131 = eq(io_decode_0_csr, UInt<12>("h7c1")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_132 = eq(io_decode_0_csr, UInt<12>("hf12")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_133 = eq(io_decode_0_csr, UInt<12>("hf11")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_134 = eq(io_decode_0_csr, UInt<12>("hf13")) @[CSR.scala 655:99]
    node _io_decode_0_read_illegal_T_135 = or(_io_decode_0_read_illegal_T_2, _io_decode_0_read_illegal_T_3) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_136 = or(_io_decode_0_read_illegal_T_135, _io_decode_0_read_illegal_T_4) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_137 = or(_io_decode_0_read_illegal_T_136, _io_decode_0_read_illegal_T_5) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_138 = or(_io_decode_0_read_illegal_T_137, _io_decode_0_read_illegal_T_6) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_139 = or(_io_decode_0_read_illegal_T_138, _io_decode_0_read_illegal_T_7) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_140 = or(_io_decode_0_read_illegal_T_139, _io_decode_0_read_illegal_T_8) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_141 = or(_io_decode_0_read_illegal_T_140, _io_decode_0_read_illegal_T_9) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_142 = or(_io_decode_0_read_illegal_T_141, _io_decode_0_read_illegal_T_10) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_143 = or(_io_decode_0_read_illegal_T_142, _io_decode_0_read_illegal_T_11) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_144 = or(_io_decode_0_read_illegal_T_143, _io_decode_0_read_illegal_T_12) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_145 = or(_io_decode_0_read_illegal_T_144, _io_decode_0_read_illegal_T_13) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_146 = or(_io_decode_0_read_illegal_T_145, _io_decode_0_read_illegal_T_14) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_147 = or(_io_decode_0_read_illegal_T_146, _io_decode_0_read_illegal_T_15) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_148 = or(_io_decode_0_read_illegal_T_147, _io_decode_0_read_illegal_T_16) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_149 = or(_io_decode_0_read_illegal_T_148, _io_decode_0_read_illegal_T_17) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_150 = or(_io_decode_0_read_illegal_T_149, _io_decode_0_read_illegal_T_18) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_151 = or(_io_decode_0_read_illegal_T_150, _io_decode_0_read_illegal_T_19) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_152 = or(_io_decode_0_read_illegal_T_151, _io_decode_0_read_illegal_T_20) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_153 = or(_io_decode_0_read_illegal_T_152, _io_decode_0_read_illegal_T_21) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_154 = or(_io_decode_0_read_illegal_T_153, _io_decode_0_read_illegal_T_22) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_155 = or(_io_decode_0_read_illegal_T_154, _io_decode_0_read_illegal_T_23) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_156 = or(_io_decode_0_read_illegal_T_155, _io_decode_0_read_illegal_T_24) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_157 = or(_io_decode_0_read_illegal_T_156, _io_decode_0_read_illegal_T_25) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_158 = or(_io_decode_0_read_illegal_T_157, _io_decode_0_read_illegal_T_26) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_159 = or(_io_decode_0_read_illegal_T_158, _io_decode_0_read_illegal_T_27) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_160 = or(_io_decode_0_read_illegal_T_159, _io_decode_0_read_illegal_T_28) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_161 = or(_io_decode_0_read_illegal_T_160, _io_decode_0_read_illegal_T_29) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_162 = or(_io_decode_0_read_illegal_T_161, _io_decode_0_read_illegal_T_30) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_163 = or(_io_decode_0_read_illegal_T_162, _io_decode_0_read_illegal_T_31) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_164 = or(_io_decode_0_read_illegal_T_163, _io_decode_0_read_illegal_T_32) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_165 = or(_io_decode_0_read_illegal_T_164, _io_decode_0_read_illegal_T_33) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_166 = or(_io_decode_0_read_illegal_T_165, _io_decode_0_read_illegal_T_34) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_167 = or(_io_decode_0_read_illegal_T_166, _io_decode_0_read_illegal_T_35) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_168 = or(_io_decode_0_read_illegal_T_167, _io_decode_0_read_illegal_T_36) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_169 = or(_io_decode_0_read_illegal_T_168, _io_decode_0_read_illegal_T_37) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_170 = or(_io_decode_0_read_illegal_T_169, _io_decode_0_read_illegal_T_38) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_171 = or(_io_decode_0_read_illegal_T_170, _io_decode_0_read_illegal_T_39) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_172 = or(_io_decode_0_read_illegal_T_171, _io_decode_0_read_illegal_T_40) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_173 = or(_io_decode_0_read_illegal_T_172, _io_decode_0_read_illegal_T_41) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_174 = or(_io_decode_0_read_illegal_T_173, _io_decode_0_read_illegal_T_42) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_175 = or(_io_decode_0_read_illegal_T_174, _io_decode_0_read_illegal_T_43) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_176 = or(_io_decode_0_read_illegal_T_175, _io_decode_0_read_illegal_T_44) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_177 = or(_io_decode_0_read_illegal_T_176, _io_decode_0_read_illegal_T_45) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_178 = or(_io_decode_0_read_illegal_T_177, _io_decode_0_read_illegal_T_46) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_179 = or(_io_decode_0_read_illegal_T_178, _io_decode_0_read_illegal_T_47) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_180 = or(_io_decode_0_read_illegal_T_179, _io_decode_0_read_illegal_T_48) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_181 = or(_io_decode_0_read_illegal_T_180, _io_decode_0_read_illegal_T_49) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_182 = or(_io_decode_0_read_illegal_T_181, _io_decode_0_read_illegal_T_50) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_183 = or(_io_decode_0_read_illegal_T_182, _io_decode_0_read_illegal_T_51) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_184 = or(_io_decode_0_read_illegal_T_183, _io_decode_0_read_illegal_T_52) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_185 = or(_io_decode_0_read_illegal_T_184, _io_decode_0_read_illegal_T_53) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_186 = or(_io_decode_0_read_illegal_T_185, _io_decode_0_read_illegal_T_54) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_187 = or(_io_decode_0_read_illegal_T_186, _io_decode_0_read_illegal_T_55) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_188 = or(_io_decode_0_read_illegal_T_187, _io_decode_0_read_illegal_T_56) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_189 = or(_io_decode_0_read_illegal_T_188, _io_decode_0_read_illegal_T_57) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_190 = or(_io_decode_0_read_illegal_T_189, _io_decode_0_read_illegal_T_58) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_191 = or(_io_decode_0_read_illegal_T_190, _io_decode_0_read_illegal_T_59) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_192 = or(_io_decode_0_read_illegal_T_191, _io_decode_0_read_illegal_T_60) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_193 = or(_io_decode_0_read_illegal_T_192, _io_decode_0_read_illegal_T_61) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_194 = or(_io_decode_0_read_illegal_T_193, _io_decode_0_read_illegal_T_62) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_195 = or(_io_decode_0_read_illegal_T_194, _io_decode_0_read_illegal_T_63) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_196 = or(_io_decode_0_read_illegal_T_195, _io_decode_0_read_illegal_T_64) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_197 = or(_io_decode_0_read_illegal_T_196, _io_decode_0_read_illegal_T_65) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_198 = or(_io_decode_0_read_illegal_T_197, _io_decode_0_read_illegal_T_66) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_199 = or(_io_decode_0_read_illegal_T_198, _io_decode_0_read_illegal_T_67) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_200 = or(_io_decode_0_read_illegal_T_199, _io_decode_0_read_illegal_T_68) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_201 = or(_io_decode_0_read_illegal_T_200, _io_decode_0_read_illegal_T_69) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_202 = or(_io_decode_0_read_illegal_T_201, _io_decode_0_read_illegal_T_70) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_203 = or(_io_decode_0_read_illegal_T_202, _io_decode_0_read_illegal_T_71) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_204 = or(_io_decode_0_read_illegal_T_203, _io_decode_0_read_illegal_T_72) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_205 = or(_io_decode_0_read_illegal_T_204, _io_decode_0_read_illegal_T_73) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_206 = or(_io_decode_0_read_illegal_T_205, _io_decode_0_read_illegal_T_74) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_207 = or(_io_decode_0_read_illegal_T_206, _io_decode_0_read_illegal_T_75) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_208 = or(_io_decode_0_read_illegal_T_207, _io_decode_0_read_illegal_T_76) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_209 = or(_io_decode_0_read_illegal_T_208, _io_decode_0_read_illegal_T_77) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_210 = or(_io_decode_0_read_illegal_T_209, _io_decode_0_read_illegal_T_78) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_211 = or(_io_decode_0_read_illegal_T_210, _io_decode_0_read_illegal_T_79) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_212 = or(_io_decode_0_read_illegal_T_211, _io_decode_0_read_illegal_T_80) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_213 = or(_io_decode_0_read_illegal_T_212, _io_decode_0_read_illegal_T_81) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_214 = or(_io_decode_0_read_illegal_T_213, _io_decode_0_read_illegal_T_82) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_215 = or(_io_decode_0_read_illegal_T_214, _io_decode_0_read_illegal_T_83) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_216 = or(_io_decode_0_read_illegal_T_215, _io_decode_0_read_illegal_T_84) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_217 = or(_io_decode_0_read_illegal_T_216, _io_decode_0_read_illegal_T_85) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_218 = or(_io_decode_0_read_illegal_T_217, _io_decode_0_read_illegal_T_86) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_219 = or(_io_decode_0_read_illegal_T_218, _io_decode_0_read_illegal_T_87) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_220 = or(_io_decode_0_read_illegal_T_219, _io_decode_0_read_illegal_T_88) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_221 = or(_io_decode_0_read_illegal_T_220, _io_decode_0_read_illegal_T_89) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_222 = or(_io_decode_0_read_illegal_T_221, _io_decode_0_read_illegal_T_90) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_223 = or(_io_decode_0_read_illegal_T_222, _io_decode_0_read_illegal_T_91) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_224 = or(_io_decode_0_read_illegal_T_223, _io_decode_0_read_illegal_T_92) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_225 = or(_io_decode_0_read_illegal_T_224, _io_decode_0_read_illegal_T_93) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_226 = or(_io_decode_0_read_illegal_T_225, _io_decode_0_read_illegal_T_94) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_227 = or(_io_decode_0_read_illegal_T_226, _io_decode_0_read_illegal_T_95) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_228 = or(_io_decode_0_read_illegal_T_227, _io_decode_0_read_illegal_T_96) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_229 = or(_io_decode_0_read_illegal_T_228, _io_decode_0_read_illegal_T_97) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_230 = or(_io_decode_0_read_illegal_T_229, _io_decode_0_read_illegal_T_98) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_231 = or(_io_decode_0_read_illegal_T_230, _io_decode_0_read_illegal_T_99) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_232 = or(_io_decode_0_read_illegal_T_231, _io_decode_0_read_illegal_T_100) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_233 = or(_io_decode_0_read_illegal_T_232, _io_decode_0_read_illegal_T_101) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_234 = or(_io_decode_0_read_illegal_T_233, _io_decode_0_read_illegal_T_102) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_235 = or(_io_decode_0_read_illegal_T_234, _io_decode_0_read_illegal_T_103) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_236 = or(_io_decode_0_read_illegal_T_235, _io_decode_0_read_illegal_T_104) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_237 = or(_io_decode_0_read_illegal_T_236, _io_decode_0_read_illegal_T_105) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_238 = or(_io_decode_0_read_illegal_T_237, _io_decode_0_read_illegal_T_106) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_239 = or(_io_decode_0_read_illegal_T_238, _io_decode_0_read_illegal_T_107) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_240 = or(_io_decode_0_read_illegal_T_239, _io_decode_0_read_illegal_T_108) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_241 = or(_io_decode_0_read_illegal_T_240, _io_decode_0_read_illegal_T_109) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_242 = or(_io_decode_0_read_illegal_T_241, _io_decode_0_read_illegal_T_110) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_243 = or(_io_decode_0_read_illegal_T_242, _io_decode_0_read_illegal_T_111) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_244 = or(_io_decode_0_read_illegal_T_243, _io_decode_0_read_illegal_T_112) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_245 = or(_io_decode_0_read_illegal_T_244, _io_decode_0_read_illegal_T_113) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_246 = or(_io_decode_0_read_illegal_T_245, _io_decode_0_read_illegal_T_114) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_247 = or(_io_decode_0_read_illegal_T_246, _io_decode_0_read_illegal_T_115) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_248 = or(_io_decode_0_read_illegal_T_247, _io_decode_0_read_illegal_T_116) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_249 = or(_io_decode_0_read_illegal_T_248, _io_decode_0_read_illegal_T_117) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_250 = or(_io_decode_0_read_illegal_T_249, _io_decode_0_read_illegal_T_118) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_251 = or(_io_decode_0_read_illegal_T_250, _io_decode_0_read_illegal_T_119) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_252 = or(_io_decode_0_read_illegal_T_251, _io_decode_0_read_illegal_T_120) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_253 = or(_io_decode_0_read_illegal_T_252, _io_decode_0_read_illegal_T_121) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_254 = or(_io_decode_0_read_illegal_T_253, _io_decode_0_read_illegal_T_122) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_255 = or(_io_decode_0_read_illegal_T_254, _io_decode_0_read_illegal_T_123) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_256 = or(_io_decode_0_read_illegal_T_255, _io_decode_0_read_illegal_T_124) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_257 = or(_io_decode_0_read_illegal_T_256, _io_decode_0_read_illegal_T_125) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_258 = or(_io_decode_0_read_illegal_T_257, _io_decode_0_read_illegal_T_126) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_259 = or(_io_decode_0_read_illegal_T_258, _io_decode_0_read_illegal_T_127) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_260 = or(_io_decode_0_read_illegal_T_259, _io_decode_0_read_illegal_T_128) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_261 = or(_io_decode_0_read_illegal_T_260, _io_decode_0_read_illegal_T_129) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_262 = or(_io_decode_0_read_illegal_T_261, _io_decode_0_read_illegal_T_130) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_263 = or(_io_decode_0_read_illegal_T_262, _io_decode_0_read_illegal_T_131) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_264 = or(_io_decode_0_read_illegal_T_263, _io_decode_0_read_illegal_T_132) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_265 = or(_io_decode_0_read_illegal_T_264, _io_decode_0_read_illegal_T_133) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_266 = or(_io_decode_0_read_illegal_T_265, _io_decode_0_read_illegal_T_134) @[CSR.scala 655:115]
    node _io_decode_0_read_illegal_T_267 = not(_io_decode_0_read_illegal_T_266) @[CSR.scala 672:7]
    node _io_decode_0_read_illegal_T_283 = and(io_decode_0_csr, UInt<12>("hc10")) @[Decode.scala 14:65]
    node _io_decode_0_read_illegal_T_284 = eq(_io_decode_0_read_illegal_T_283, UInt<12>("h410")) @[Decode.scala 14:121]
    node _io_decode_0_read_illegal_T_287 = not(reg_debug) @[CSR.scala 675:45]
    node _io_decode_0_read_illegal_T_288 = and(_io_decode_0_read_illegal_T_284, _io_decode_0_read_illegal_T_287) @[CSR.scala 675:42]
    node _io_decode_0_read_illegal_T_289 = or(_io_decode_0_read_illegal_T_267, _io_decode_0_read_illegal_T_288) @[CSR.scala 674:148]
    node _io_decode_0_read_illegal_T_292 = and(io_decode_0_fp_csr, io_decode_0_fp_illegal) @[CSR.scala 677:21]
    node _io_decode_0_write_illegal_T = bits(io_decode_0_csr, 11, 10) @[CSR.scala 678:39]
    node _io_decode_0_write_flush_T = geq(io_decode_0_csr, UInt<12>("h340")) @[CSR.scala 679:40]
    node _io_decode_0_write_flush_T_1 = leq(io_decode_0_csr, UInt<12>("h343")) @[CSR.scala 679:71]
    node _io_decode_0_write_flush_T_2 = and(_io_decode_0_write_flush_T, _io_decode_0_write_flush_T_1) @[CSR.scala 679:57]
    node _io_decode_0_write_flush_T_3 = geq(io_decode_0_csr, UInt<12>("h140")) @[CSR.scala 679:99]
    node _io_decode_0_write_flush_T_4 = leq(io_decode_0_csr, UInt<12>("h143")) @[CSR.scala 679:130]
    node _io_decode_0_write_flush_T_5 = and(_io_decode_0_write_flush_T_3, _io_decode_0_write_flush_T_4) @[CSR.scala 679:116]
    node _io_decode_0_write_flush_T_6 = or(_io_decode_0_write_flush_T_2, _io_decode_0_write_flush_T_5) @[CSR.scala 679:85]
    node _io_decode_0_system_illegal_T_8 = bits(io_decode_0_csr, 10, 10) @[CSR.scala 683:27]
    node _io_decode_0_system_illegal_T_9 = and(is_ret, _io_decode_0_system_illegal_T_8) @[CSR.scala 683:14]
    node _io_decode_0_system_illegal_T_10 = bits(io_decode_0_csr, 7, 7) @[CSR.scala 683:45]
    node _io_decode_0_system_illegal_T_11 = and(_io_decode_0_system_illegal_T_9, _io_decode_0_system_illegal_T_10) @[CSR.scala 683:32]
    node _cause_T_2 = mux(insn_break, UInt<32>("h3"), io_cause) @[CSR.scala 689:14]
    node cause = mux(insn_call, UInt<32>("hb"), _cause_T_2) @[CSR.scala 688:8]
    node cause_lsbs = bits(cause, 7, 0) @[CSR.scala 690:25]
    node _causeIsDebugInt_T = bits(cause, 31, 31) @[CSR.scala 691:30]
    node _causeIsDebugInt_T_1 = eq(cause_lsbs, UInt<8>("he")) @[CSR.scala 691:53]
    node causeIsDebugInt = and(_causeIsDebugInt_T, _causeIsDebugInt_T_1) @[CSR.scala 691:39]
    node _causeIsDebugTrigger_T_1 = not(_causeIsDebugInt_T) @[CSR.scala 692:29]
    node causeIsDebugTrigger = and(_causeIsDebugTrigger_T_1, _causeIsDebugInt_T_1) @[CSR.scala 692:44]
    node _causeIsDebugBreak_T_2 = and(_causeIsDebugTrigger_T_1, insn_break) @[CSR.scala 693:42]
    node causeIsDebugBreak_hi = cat(reg_dcsr_ebreakm, UInt<1>("h0")) @[Cat.scala 30:58]
    node _causeIsDebugBreak_T_3 = cat(causeIsDebugBreak_hi, UInt<2>("h0")) @[Cat.scala 30:58]
    node _GEN_485 = shr(_causeIsDebugBreak_T_3, 3) @[CSR.scala 693:134]
    node _causeIsDebugBreak_T_4 = pad(_GEN_485, 4) @[CSR.scala 693:134]
    node _causeIsDebugBreak_T_5 = bits(_causeIsDebugBreak_T_4, 0, 0) @[CSR.scala 693:134]
    node causeIsDebugBreak = and(_causeIsDebugBreak_T_2, _causeIsDebugBreak_T_5) @[CSR.scala 693:56]
    node _trapToDebug_T = or(reg_singleStepped, causeIsDebugInt) @[CSR.scala 694:60]
    node _trapToDebug_T_1 = or(_trapToDebug_T, causeIsDebugTrigger) @[CSR.scala 694:79]
    node _trapToDebug_T_2 = or(_trapToDebug_T_1, causeIsDebugBreak) @[CSR.scala 694:102]
    node trapToDebug = or(_trapToDebug_T_2, reg_debug) @[CSR.scala 694:123]
    node _debugTVec_T = mux(insn_break, UInt<12>("h800"), UInt<12>("h808")) @[CSR.scala 697:37]
    node debugTVec = mux(reg_debug, _debugTVec_T, UInt<12>("h800")) @[CSR.scala 697:22]
    node _notDebugTVec_interruptOffset_T = bits(cause, 4, 0) @[CSR.scala 706:32]
    node notDebugTVec_interruptVec_lo = shl(_notDebugTVec_interruptOffset_T, 2) @[CSR.scala 706:59]
    node notDebugTVec_interruptVec_hi = shr(read_mtvec, 7) @[CSR.scala 707:33]
    node notDebugTVec_interruptVec = cat(notDebugTVec_interruptVec_hi, notDebugTVec_interruptVec_lo) @[Cat.scala 30:58]
    node _notDebugTVec_doVector_T = bits(read_mtvec, 0, 0) @[CSR.scala 708:24]
    node _notDebugTVec_doVector_T_2 = and(_notDebugTVec_doVector_T, _causeIsDebugInt_T) @[CSR.scala 708:28]
    node _notDebugTVec_doVector_T_3 = shr(cause_lsbs, 5) @[CSR.scala 708:70]
    node _notDebugTVec_doVector_T_4 = eq(_notDebugTVec_doVector_T_3, UInt<3>("h0")) @[CSR.scala 708:94]
    node notDebugTVec_doVector = and(_notDebugTVec_doVector_T_2, _notDebugTVec_doVector_T_4) @[CSR.scala 708:55]
    node _notDebugTVec_T = shr(read_mtvec, 2) @[CSR.scala 709:38]
    node _notDebugTVec_T_1 = shl(_notDebugTVec_T, 2) @[CSR.scala 709:56]
    node notDebugTVec = mux(notDebugTVec_doVector, notDebugTVec_interruptVec, _notDebugTVec_T_1) @[CSR.scala 709:8]
    node tvec = mux(trapToDebug, pad(debugTVec, 32), notDebugTVec) @[CSR.scala 723:17]
    node _io_eret_T = or(insn_call, insn_break) @[CSR.scala 726:24]
    node _io_status_sd_T = andr(io_status_fs) @[CSR.scala 729:32]
    node _io_status_sd_T_1 = andr(io_status_xs) @[CSR.scala 729:53]
    node _io_status_sd_T_2 = or(_io_status_sd_T, _io_status_sd_T_1) @[CSR.scala 729:37]
    node _io_status_sd_T_3 = andr(io_status_vs) @[CSR.scala 729:74]
    node exception = or(_io_eret_T, io_exception) @[CSR.scala 738:43]
    node _T_227 = add(insn_ret, insn_call) @[Bitwise.scala 47:55]
    node _T_229 = add(insn_break, io_exception) @[Bitwise.scala 47:55]
    node _T_231 = add(_T_227, _T_229) @[Bitwise.scala 47:55]
    node _T_233 = leq(_T_231, UInt<3>("h1")) @[CSR.scala 739:79]
    node _T_235 = or(_T_233, reset) @[CSR.scala 739:9]
    node _T_236 = not(_T_235) @[CSR.scala 739:9]
    node _T_238 = and(insn_wfi, _io_interrupt_T) @[CSR.scala 741:18]
    node _T_240 = and(_T_238, _io_decode_0_read_illegal_T_287) @[CSR.scala 741:36]
    node _GEN_46 = or(_T_240, reg_wfi) @[CSR.scala 427:50 741:{51,61}]
    node _T_241 = orr(pending_interrupts) @[CSR.scala 742:28]
    node _T_242 = or(_T_241, io_interrupts_debug) @[CSR.scala 742:32]
    node _T_243 = or(_T_242, exception) @[CSR.scala 742:55]
    node _GEN_47 = mux(_T_243, UInt<1>("h0"), _GEN_46) @[CSR.scala 742:{69,79}]
    node _T_245 = or(io_retire, exception) @[CSR.scala 745:22]
    node _GEN_48 = or(_T_245, reg_singleStepped) @[CSR.scala 372:30 745:{36,56}]
    node _T_253 = not(reg_singleStepped) @[CSR.scala 748:10]
    node _T_254 = not(io_retire) @[CSR.scala 748:42]
    node _T_255 = or(_T_253, _T_254) @[CSR.scala 748:29]
    node _T_257 = or(_T_255, reset) @[CSR.scala 748:9]
    node _T_258 = not(_T_257) @[CSR.scala 748:9]
    node _epc_T = not(io_pc) @[CSR.scala 1204:28]
    node _epc_T_1 = or(_epc_T, UInt<32>("h1")) @[CSR.scala 1204:31]
    node epc = not(_epc_T_1) @[CSR.scala 1204:26]
    node _reg_dcsr_cause_T = mux(causeIsDebugTrigger, UInt<2>("h2"), UInt<2>("h1")) @[CSR.scala 759:86]
    node _reg_dcsr_cause_T_1 = mux(causeIsDebugInt, UInt<2>("h3"), _reg_dcsr_cause_T) @[CSR.scala 759:56]
    node _reg_dcsr_cause_T_2 = mux(reg_singleStepped, UInt<3>("h4"), pad(_reg_dcsr_cause_T_1, 3)) @[CSR.scala 759:30]
    node _GEN_50 = or(_io_decode_0_read_illegal_T_287, reg_debug) @[CSR.scala 756:25 757:19 368:22]
    node _GEN_51 = mux(_io_decode_0_read_illegal_T_287, epc, reg_dpc) @[CSR.scala 756:25 758:17 369:20]
    node _GEN_52 = mux(_io_decode_0_read_illegal_T_287, _reg_dcsr_cause_T_2, reg_dcsr_cause) @[CSR.scala 327:21 756:25 759:24]
    node _GEN_66 = pad(reg_mstatus_spp, 2) @[CSR.scala 772:35 778:23 319:24]
    node _GEN_94 = mux(trapToDebug, _GEN_50, reg_debug) @[CSR.scala 368:22 755:24]
    node _GEN_95 = mux(trapToDebug, _GEN_51, reg_dpc) @[CSR.scala 369:20 755:24]
    node _GEN_96 = mux(trapToDebug, _GEN_52, reg_dcsr_cause) @[CSR.scala 327:21 755:24]
    node _GEN_109 = mux(trapToDebug, pad(reg_mstatus_spp, 2), _GEN_66) @[CSR.scala 319:24 755:24]
    node _GEN_111 = mux(trapToDebug, reg_mepc, epc) @[CSR.scala 391:21 755:24]
    node _GEN_112 = mux(trapToDebug, reg_mcause, cause) @[CSR.scala 755:24 392:27]
    node _GEN_113 = mux(trapToDebug, reg_mtval, io_tval) @[CSR.scala 393:22 755:24]
    node _GEN_114 = mux(trapToDebug, reg_mstatus_mpie, reg_mstatus_mie) @[CSR.scala 319:24 755:24]
    node _GEN_116 = and(trapToDebug, reg_mstatus_mie) @[CSR.scala 319:24 755:24]
    node _GEN_117 = mux(exception, _GEN_94, reg_debug) @[CSR.scala 754:20 368:22]
    node _GEN_118 = mux(exception, _GEN_95, reg_dpc) @[CSR.scala 369:20 754:20]
    node _GEN_119 = mux(exception, _GEN_96, reg_dcsr_cause) @[CSR.scala 754:20 327:21]
    node _GEN_132 = mux(exception, _GEN_109, pad(reg_mstatus_spp, 2)) @[CSR.scala 754:20 319:24]
    node _GEN_134 = mux(exception, _GEN_111, reg_mepc) @[CSR.scala 754:20 391:21]
    node _GEN_135 = mux(exception, _GEN_112, reg_mcause) @[CSR.scala 754:20 392:27]
    node _GEN_136 = mux(exception, _GEN_113, reg_mtval) @[CSR.scala 754:20 393:22]
    node _GEN_137 = mux(exception, _GEN_114, reg_mstatus_mpie) @[CSR.scala 754:20 319:24]
    node _GEN_139 = mux(exception, _GEN_116, reg_mstatus_mie) @[CSR.scala 754:20 319:24]
    node _T_366 = bits(io_rw_addr, 10, 10) @[CSR.scala 821:47]
    node _T_368 = bits(io_rw_addr, 7, 7) @[CSR.scala 821:65]
    node _T_369 = and(_T_366, _T_368) @[CSR.scala 821:52]
    node _GEN_148 = mux(_T_369, UInt<1>("h0"), _GEN_117) @[CSR.scala 821:70 823:17]
    node _GEN_149 = mux(_T_369, _T_19, _T_13) @[CSR.scala 821:70 824:15]
    node _GEN_152 = mux(_T_369, _GEN_139, reg_mstatus_mpie) @[CSR.scala 821:70]
    node _GEN_153 = mux(_T_369, _GEN_137, UInt<1>("h1")) @[CSR.scala 821:70]
    node _GEN_171 = mux(insn_ret, _GEN_148, _GEN_117) @[CSR.scala 813:19]
    node _GEN_174 = mux(insn_ret, _GEN_152, _GEN_139) @[CSR.scala 813:19]
    node _GEN_175 = mux(insn_ret, _GEN_153, _GEN_137) @[CSR.scala 813:19]
    reg io_status_cease_r : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_status_cease_r) @[Reg.scala 27:20]
    node _GEN_179 = or(insn_cease, io_status_cease_r) @[Reg.scala 28:19 27:20 28:23]
    node _io_rw_rdata_T_1 = mux(_T_56, _T_7, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_2 = mux(_T_57, reg_bp_0_address, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_4 = mux(_T_59, reg_misa, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_5 = mux(_T_60, read_mstatus, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_6 = mux(_T_61, read_mtvec, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_7 = mux(_T_62, read_mip, UInt<16>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_8 = mux(_T_63, reg_mie, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_9 = mux(_T_64, reg_mscratch, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_10 = mux(_T_65, _T_13, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_11 = mux(_T_66, reg_mtval, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_12 = mux(_T_67, reg_mcause, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_13 = and(_T_68, io_hartid) @[Mux.scala 27:72]
    node _io_rw_rdata_T_14 = mux(_T_69, _T_14, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_15 = mux(_T_70, _T_19, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_16 = mux(_T_71, reg_dscratch, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_17 = mux(_T_72, reg_mcountinhibit, UInt<3>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_18 = mux(_T_73, value_1, UInt<64>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_19 = mux(_T_74, value, UInt<64>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_107 = mux(_T_162, _T_49, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_108 = mux(_T_163, _T_50, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_109 = mux(_T_164, _T_51, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_110 = mux(_T_165, _T_52, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_113 = mux(_T_168, reg_pmp_0_addr, UInt<30>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_114 = mux(_T_169, reg_pmp_1_addr, UInt<30>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_115 = mux(_T_170, reg_pmp_2_addr, UInt<30>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_116 = mux(_T_171, reg_pmp_3_addr, UInt<30>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_117 = mux(_T_172, reg_pmp_4_addr, UInt<30>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_118 = mux(_T_173, reg_pmp_5_addr, UInt<30>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_119 = mux(_T_174, reg_pmp_6_addr, UInt<30>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_120 = mux(_T_175, reg_pmp_7_addr, UInt<30>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_129 = mux(_T_184, reg_custom_0, UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_130 = mux(_T_185, UInt<32>("h1"), UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_132 = mux(_T_187, UInt<32>("h20181004"), UInt<32>("h0")) @[Mux.scala 27:72]
    node _io_rw_rdata_T_134 = or(_io_rw_rdata_T_1, _io_rw_rdata_T_2) @[Mux.scala 27:72]
    node _io_rw_rdata_T_136 = or(_io_rw_rdata_T_134, _io_rw_rdata_T_4) @[Mux.scala 27:72]
    node _io_rw_rdata_T_137 = or(_io_rw_rdata_T_136, _io_rw_rdata_T_5) @[Mux.scala 27:72]
    node _io_rw_rdata_T_138 = or(_io_rw_rdata_T_137, _io_rw_rdata_T_6) @[Mux.scala 27:72]
    node _GEN_489 = pad(_io_rw_rdata_T_7, 32) @[Mux.scala 27:72]
    node _io_rw_rdata_T_139 = or(_io_rw_rdata_T_138, _GEN_489) @[Mux.scala 27:72]
    node _io_rw_rdata_T_140 = or(_io_rw_rdata_T_139, _io_rw_rdata_T_8) @[Mux.scala 27:72]
    node _io_rw_rdata_T_141 = or(_io_rw_rdata_T_140, _io_rw_rdata_T_9) @[Mux.scala 27:72]
    node _io_rw_rdata_T_142 = or(_io_rw_rdata_T_141, _io_rw_rdata_T_10) @[Mux.scala 27:72]
    node _io_rw_rdata_T_143 = or(_io_rw_rdata_T_142, _io_rw_rdata_T_11) @[Mux.scala 27:72]
    node _io_rw_rdata_T_144 = or(_io_rw_rdata_T_143, _io_rw_rdata_T_12) @[Mux.scala 27:72]
    node _GEN_490 = pad(_io_rw_rdata_T_13, 32) @[Mux.scala 27:72]
    node _io_rw_rdata_T_145 = or(_io_rw_rdata_T_144, _GEN_490) @[Mux.scala 27:72]
    node _io_rw_rdata_T_146 = or(_io_rw_rdata_T_145, _io_rw_rdata_T_14) @[Mux.scala 27:72]
    node _io_rw_rdata_T_147 = or(_io_rw_rdata_T_146, _io_rw_rdata_T_15) @[Mux.scala 27:72]
    node _io_rw_rdata_T_148 = or(_io_rw_rdata_T_147, _io_rw_rdata_T_16) @[Mux.scala 27:72]
    node _GEN_491 = pad(_io_rw_rdata_T_17, 32) @[Mux.scala 27:72]
    node _io_rw_rdata_T_149 = or(_io_rw_rdata_T_148, _GEN_491) @[Mux.scala 27:72]
    node _GEN_492 = pad(_io_rw_rdata_T_149, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_150 = or(_GEN_492, _io_rw_rdata_T_18) @[Mux.scala 27:72]
    node _io_rw_rdata_T_151 = or(_io_rw_rdata_T_150, _io_rw_rdata_T_19) @[Mux.scala 27:72]
    node _GEN_493 = pad(_io_rw_rdata_T_107, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_239 = or(_io_rw_rdata_T_151, _GEN_493) @[Mux.scala 27:72]
    node _GEN_494 = pad(_io_rw_rdata_T_108, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_240 = or(_io_rw_rdata_T_239, _GEN_494) @[Mux.scala 27:72]
    node _GEN_495 = pad(_io_rw_rdata_T_109, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_241 = or(_io_rw_rdata_T_240, _GEN_495) @[Mux.scala 27:72]
    node _GEN_496 = pad(_io_rw_rdata_T_110, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_242 = or(_io_rw_rdata_T_241, _GEN_496) @[Mux.scala 27:72]
    node _GEN_497 = pad(_io_rw_rdata_T_113, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_245 = or(_io_rw_rdata_T_242, _GEN_497) @[Mux.scala 27:72]
    node _GEN_498 = pad(_io_rw_rdata_T_114, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_246 = or(_io_rw_rdata_T_245, _GEN_498) @[Mux.scala 27:72]
    node _GEN_499 = pad(_io_rw_rdata_T_115, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_247 = or(_io_rw_rdata_T_246, _GEN_499) @[Mux.scala 27:72]
    node _GEN_500 = pad(_io_rw_rdata_T_116, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_248 = or(_io_rw_rdata_T_247, _GEN_500) @[Mux.scala 27:72]
    node _GEN_501 = pad(_io_rw_rdata_T_117, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_249 = or(_io_rw_rdata_T_248, _GEN_501) @[Mux.scala 27:72]
    node _GEN_502 = pad(_io_rw_rdata_T_118, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_250 = or(_io_rw_rdata_T_249, _GEN_502) @[Mux.scala 27:72]
    node _GEN_503 = pad(_io_rw_rdata_T_119, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_251 = or(_io_rw_rdata_T_250, _GEN_503) @[Mux.scala 27:72]
    node _GEN_504 = pad(_io_rw_rdata_T_120, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_252 = or(_io_rw_rdata_T_251, _GEN_504) @[Mux.scala 27:72]
    node _GEN_505 = pad(_io_rw_rdata_T_129, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_261 = or(_io_rw_rdata_T_252, _GEN_505) @[Mux.scala 27:72]
    node _GEN_506 = pad(_io_rw_rdata_T_130, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_262 = or(_io_rw_rdata_T_261, _GEN_506) @[Mux.scala 27:72]
    node _GEN_507 = pad(_io_rw_rdata_T_132, 64) @[Mux.scala 27:72]
    node _io_rw_rdata_T_264 = or(_io_rw_rdata_T_262, _GEN_507) @[Mux.scala 27:72]
    node _T_379 = eq(io_rw_cmd, UInt<3>("h5")) @[package.scala 15:47]
    node _T_380 = eq(io_rw_cmd, UInt<3>("h6")) @[package.scala 15:47]
    node _T_381 = eq(io_rw_cmd, UInt<3>("h7")) @[package.scala 15:47]
    node _csr_wen_T_3 = or(_T_380, _T_381) @[package.scala 72:59]
    node csr_wen = or(_csr_wen_T_3, _T_379) @[package.scala 72:59]
    node _new_mstatus_WIRE = pad(wdata, 103)
    node new_mstatus_mie = bits(_new_mstatus_WIRE, 3, 3) @[CSR.scala 899:47]
    node new_mstatus_mpie = bits(_new_mstatus_WIRE, 7, 7) @[CSR.scala 899:47]
    node _GEN_184 = mux(_T_60, new_mstatus_mie, _GEN_174) @[CSR.scala 898:39 900:23]
    node _GEN_185 = mux(_T_60, new_mstatus_mpie, _GEN_175) @[CSR.scala 898:39 901:24]
    node f = bits(wdata, 5, 5) @[CSR.scala 925:20]
    node _T_1973 = bits(io_pc, 1, 1) @[CSR.scala 927:39]
    node _T_1974 = not(_T_1973) @[CSR.scala 927:33]
    node _T_1976 = bits(wdata, 2, 2) @[CSR.scala 927:51]
    node _T_1977 = or(_T_1974, _T_1976) @[CSR.scala 927:43]
    node _reg_misa_T = not(wdata) @[CSR.scala 929:25]
    node _reg_misa_T_1 = not(f) @[CSR.scala 929:35]
    node _reg_misa_T_2 = shl(_reg_misa_T_1, 3) @[CSR.scala 929:38]
    node _GEN_508 = pad(_reg_misa_T_2, 32) @[CSR.scala 929:32]
    node _reg_misa_T_3 = or(_reg_misa_T, _GEN_508) @[CSR.scala 929:32]
    node _reg_misa_T_4 = not(_reg_misa_T_3) @[CSR.scala 929:23]
    node _reg_misa_T_5 = and(_reg_misa_T_4, UInt<32>("h1005")) @[CSR.scala 929:55]
    node _reg_misa_T_7 = and(reg_misa, UInt<32>("hffffeffa")) @[CSR.scala 929:73]
    node _reg_misa_T_8 = or(_reg_misa_T_5, _reg_misa_T_7) @[CSR.scala 929:62]
    node _GEN_187 = mux(_T_1977, _reg_misa_T_8, reg_misa) @[CSR.scala 927:64 929:20 490:21]
    node _GEN_188 = mux(_T_59, _GEN_187, reg_misa) @[CSR.scala 490:21 923:36]
    node _reg_mie_T = and(wdata, UInt<32>("h888")) @[CSR.scala 944:59]
    node _GEN_189 = mux(_T_63, _reg_mie_T, reg_mie) @[CSR.scala 381:20 944:{40,50}]
    node _reg_mepc_T_1 = or(_reg_misa_T, UInt<32>("h1")) @[CSR.scala 1204:31]
    node _reg_mepc_T_2 = not(_reg_mepc_T_1) @[CSR.scala 1204:26]
    node _GEN_190 = mux(_T_65, _reg_mepc_T_2, _GEN_134) @[CSR.scala 945:{40,51}]
    node _GEN_191 = mux(_T_64, wdata, reg_mscratch) @[CSR.scala 394:25 946:{40,55}]
    node _GEN_192 = mux(_T_61, wdata, reg_mtvec) @[CSR.scala 397:27 948:{40,52}]
    node _reg_mcause_T = and(wdata, UInt<32>("h8000000f")) @[CSR.scala 949:62]
    node _GEN_193 = mux(_T_67, _reg_mcause_T, _GEN_135) @[CSR.scala 949:{40,53}]
    node _GEN_194 = mux(_T_66, wdata, _GEN_136) @[CSR.scala 950:{40,52}]
    node _reg_mcountinhibit_T_1 = and(wdata, UInt<32>("hfffffffd")) @[CSR.scala 965:76]
    node _GEN_195 = mux(_T_72, _reg_mcountinhibit_T_1, pad(reg_mcountinhibit, 32)) @[CSR.scala 436:34 965:{47,67}]
    node hi_25 = bits(value_1, 63, 32) @[CSR.scala 1198:47]
    node _T_1978 = cat(hi_25, wdata) @[Cat.scala 30:58]
    node _hi_T = shr(_T_1978, 6) @[Counters.scala 66:28]
    node _GEN_196 = mux(_T_73, _T_1978, pad(_GEN_2, 64)) @[CSR.scala 1198:31 Counters.scala 65:11]
    node _GEN_197 = mux(_T_73, _hi_T, _GEN_3) @[CSR.scala 1198:31 Counters.scala 66:23]
    node lo_25 = bits(value_1, 31, 0) @[CSR.scala 1199:74]
    node _T_1979 = cat(wdata, lo_25) @[Cat.scala 30:58]
    node _hi_T_1 = shr(_T_1979, 6) @[Counters.scala 66:28]
    node _GEN_198 = mux(_T_162, _T_1979, _GEN_196) @[CSR.scala 1199:31 Counters.scala 65:11]
    node _GEN_199 = mux(_T_162, _hi_T_1, _GEN_197) @[CSR.scala 1199:31 Counters.scala 66:23]
    node hi_27 = bits(value, 63, 32) @[CSR.scala 1198:47]
    node _T_1980 = cat(hi_27, wdata) @[Cat.scala 30:58]
    node _hi_T_2 = shr(_T_1980, 6) @[Counters.scala 66:28]
    node _GEN_200 = mux(_T_74, _T_1980, pad(_GEN_0, 64)) @[CSR.scala 1198:31 Counters.scala 65:11]
    node _GEN_201 = mux(_T_74, _hi_T_2, _GEN_1) @[CSR.scala 1198:31 Counters.scala 66:23]
    node lo_26 = bits(value, 31, 0) @[CSR.scala 1199:74]
    node _T_1981 = cat(wdata, lo_26) @[Cat.scala 30:58]
    node _hi_T_3 = shr(_T_1981, 6) @[Counters.scala 66:28]
    node _GEN_202 = mux(_T_163, _T_1981, _GEN_200) @[CSR.scala 1199:31 Counters.scala 65:11]
    node _GEN_203 = mux(_T_163, _hi_T_3, _GEN_201) @[CSR.scala 1199:31 Counters.scala 66:23]
    node new_dcsr_ebreakm = bits(wdata, 15, 15) @[CSR.scala 981:43]
    node _GEN_204 = mux(_T_69, _T_1976, reg_dcsr_step) @[CSR.scala 327:21 980:38 982:23]
    node _GEN_205 = mux(_T_69, new_dcsr_ebreakm, reg_dcsr_ebreakm) @[CSR.scala 327:21 980:38 983:26]
    node _GEN_206 = mux(_T_70, _reg_mepc_T_2, _GEN_118) @[CSR.scala 988:{42,52}]
    node _GEN_207 = mux(_T_71, wdata, reg_dscratch) @[CSR.scala 370:25 989:{42,57}]
    node _T_1983 = not(reg_bp_0_control_dmode) @[CSR.scala 1039:37]
    node _T_1984 = or(_T_1983, reg_debug) @[CSR.scala 1039:55]
    node _GEN_209 = mux(_T_57, wdata, reg_bp_0_address) @[CSR.scala 1040:{44,57} 378:19]
    node _reg_bp_0_control_T = bits(wdata, 0, 0) @[CSR.scala 1052:41]
    node _reg_bp_0_control_T_1 = bits(wdata, 1, 1) @[CSR.scala 1052:41]
    node _reg_bp_0_control_T_7 = bits(wdata, 8, 7) @[CSR.scala 1052:41]
    node _newBPC_T_2 = mux(_wdata_T, _T_7, UInt<32>("h0")) @[CSR.scala 1183:9]
    node _newBPC_T_3 = or(_newBPC_T_2, io_rw_wdata) @[CSR.scala 1183:34]
    node _newBPC_T_8 = and(_newBPC_T_3, _wdata_T_6) @[CSR.scala 1183:43]
    node newBPC_action = bits(_newBPC_T_8, 12, 12) @[CSR.scala 1058:96]
    node newBPC_dmode = bits(_newBPC_T_8, 27, 27) @[CSR.scala 1058:96]
    node dMode = and(newBPC_dmode, reg_debug) @[CSR.scala 1059:38]
    node _GEN_210 = and(dMode, newBPC_action) @[CSR.scala 1061:{120,51,71}]
    node _GEN_212 = mux(_T_56, dMode, reg_bp_0_control_dmode) @[CSR.scala 1051:44 1060:30 378:19]
    node _GEN_215 = mux(_T_56, _GEN_210, reg_bp_0_control_action) @[CSR.scala 1051:44 378:19]
    node _GEN_218 = mux(_T_56, _reg_bp_0_control_T_7, reg_bp_0_control_tmatch) @[CSR.scala 1051:44 1052:24 378:19]
    node _GEN_223 = mux(_T_56, _T_1976, reg_bp_0_control_x) @[CSR.scala 1051:44 1052:24 378:19]
    node _GEN_224 = mux(_T_56, _reg_bp_0_control_T_1, reg_bp_0_control_w) @[CSR.scala 1051:44 1052:24 378:19]
    node _GEN_225 = mux(_T_56, _reg_bp_0_control_T, reg_bp_0_control_r) @[CSR.scala 1051:44 1052:24 378:19]
    node _GEN_226 = mux(_T_1984, _GEN_209, reg_bp_0_address) @[CSR.scala 1039:70 378:19]
    node _GEN_228 = mux(_T_1984, _GEN_212, reg_bp_0_control_dmode) @[CSR.scala 1039:70 378:19]
    node _GEN_231 = mux(_T_1984, _GEN_215, reg_bp_0_control_action) @[CSR.scala 1039:70 378:19]
    node _GEN_234 = mux(_T_1984, _GEN_218, reg_bp_0_control_tmatch) @[CSR.scala 1039:70 378:19]
    node _GEN_239 = mux(_T_1984, _GEN_223, reg_bp_0_control_x) @[CSR.scala 1039:70 378:19]
    node _GEN_240 = mux(_T_1984, _GEN_224, reg_bp_0_control_w) @[CSR.scala 1039:70 378:19]
    node _GEN_241 = mux(_T_1984, _GEN_225, reg_bp_0_control_r) @[CSR.scala 1039:70 378:19]
    node _T_1994 = not(reg_pmp_0_cfg_l) @[CSR.scala 1071:60]
    node _T_1995 = and(_T_164, _T_1994) @[CSR.scala 1071:57]
    node _newCfg_WIRE = bits(wdata, 7, 0)
    node newCfg_r = bits(_newCfg_WIRE, 0, 0) @[CSR.scala 1072:46]
    node newCfg_w = bits(_newCfg_WIRE, 1, 1) @[CSR.scala 1072:46]
    node newCfg_x = bits(_newCfg_WIRE, 2, 2) @[CSR.scala 1072:46]
    node newCfg_a = bits(_newCfg_WIRE, 4, 3) @[CSR.scala 1072:46]
    node newCfg_l = bits(_newCfg_WIRE, 7, 7) @[CSR.scala 1072:46]
    node _reg_pmp_0_cfg_w_T = and(newCfg_w, newCfg_r) @[CSR.scala 1075:31]
    node _GEN_275 = mux(_T_1995, newCfg_l, reg_pmp_0_cfg_l) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_277 = mux(_T_1995, newCfg_a, reg_pmp_0_cfg_a) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_278 = mux(_T_1995, newCfg_x, reg_pmp_0_cfg_x) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_279 = mux(_T_1995, _reg_pmp_0_cfg_w_T, reg_pmp_0_cfg_w) @[CSR.scala 1071:76 1075:19 379:20]
    node _GEN_280 = mux(_T_1995, newCfg_r, reg_pmp_0_cfg_r) @[CSR.scala 1071:76 1073:17 379:20]
    node _T_1996 = bits(reg_pmp_1_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _T_1997 = not(_T_1996) @[PMP.scala 49:13]
    node _T_1999 = and(_T_1997, pmp_mask_base_lo_1) @[PMP.scala 49:20]
    node _T_2000 = and(reg_pmp_1_cfg_l, _T_1999) @[PMP.scala 51:62]
    node _T_2001 = or(reg_pmp_0_cfg_l, _T_2000) @[PMP.scala 51:44]
    node _T_2002 = not(_T_2001) @[CSR.scala 1080:48]
    node _T_2003 = and(_T_168, _T_2002) @[CSR.scala 1080:45]
    node _GEN_281 = mux(_T_2003, wdata, pad(reg_pmp_0_addr, 32)) @[CSR.scala 1080:71 1081:18 379:20]
    node _T_2004 = not(reg_pmp_1_cfg_l) @[CSR.scala 1071:60]
    node _T_2005 = and(_T_164, _T_2004) @[CSR.scala 1071:57]
    node _newCfg_T_7 = shr(wdata, 8) @[CSR.scala 1072:53]
    node _newCfg_WIRE_1 = bits(_newCfg_T_7, 7, 0)
    node newCfg_1_r = bits(_newCfg_WIRE_1, 0, 0) @[CSR.scala 1072:46]
    node newCfg_1_w = bits(_newCfg_WIRE_1, 1, 1) @[CSR.scala 1072:46]
    node newCfg_1_x = bits(_newCfg_WIRE_1, 2, 2) @[CSR.scala 1072:46]
    node newCfg_1_a = bits(_newCfg_WIRE_1, 4, 3) @[CSR.scala 1072:46]
    node newCfg_1_l = bits(_newCfg_WIRE_1, 7, 7) @[CSR.scala 1072:46]
    node _reg_pmp_1_cfg_w_T = and(newCfg_1_w, newCfg_1_r) @[CSR.scala 1075:31]
    node _GEN_282 = mux(_T_2005, newCfg_1_l, reg_pmp_1_cfg_l) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_284 = mux(_T_2005, newCfg_1_a, reg_pmp_1_cfg_a) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_285 = mux(_T_2005, newCfg_1_x, reg_pmp_1_cfg_x) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_286 = mux(_T_2005, _reg_pmp_1_cfg_w_T, reg_pmp_1_cfg_w) @[CSR.scala 1071:76 1075:19 379:20]
    node _GEN_287 = mux(_T_2005, newCfg_1_r, reg_pmp_1_cfg_r) @[CSR.scala 1071:76 1073:17 379:20]
    node _T_2006 = bits(reg_pmp_2_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _T_2007 = not(_T_2006) @[PMP.scala 49:13]
    node _T_2009 = and(_T_2007, pmp_mask_base_lo_2) @[PMP.scala 49:20]
    node _T_2010 = and(reg_pmp_2_cfg_l, _T_2009) @[PMP.scala 51:62]
    node _T_2011 = or(reg_pmp_1_cfg_l, _T_2010) @[PMP.scala 51:44]
    node _T_2012 = not(_T_2011) @[CSR.scala 1080:48]
    node _T_2013 = and(_T_169, _T_2012) @[CSR.scala 1080:45]
    node _GEN_288 = mux(_T_2013, wdata, pad(reg_pmp_1_addr, 32)) @[CSR.scala 1080:71 1081:18 379:20]
    node _T_2014 = not(reg_pmp_2_cfg_l) @[CSR.scala 1071:60]
    node _T_2015 = and(_T_164, _T_2014) @[CSR.scala 1071:57]
    node _newCfg_T_14 = shr(wdata, 16) @[CSR.scala 1072:53]
    node _newCfg_WIRE_2 = bits(_newCfg_T_14, 7, 0)
    node newCfg_2_r = bits(_newCfg_WIRE_2, 0, 0) @[CSR.scala 1072:46]
    node newCfg_2_w = bits(_newCfg_WIRE_2, 1, 1) @[CSR.scala 1072:46]
    node newCfg_2_x = bits(_newCfg_WIRE_2, 2, 2) @[CSR.scala 1072:46]
    node newCfg_2_a = bits(_newCfg_WIRE_2, 4, 3) @[CSR.scala 1072:46]
    node newCfg_2_l = bits(_newCfg_WIRE_2, 7, 7) @[CSR.scala 1072:46]
    node _reg_pmp_2_cfg_w_T = and(newCfg_2_w, newCfg_2_r) @[CSR.scala 1075:31]
    node _GEN_289 = mux(_T_2015, newCfg_2_l, reg_pmp_2_cfg_l) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_291 = mux(_T_2015, newCfg_2_a, reg_pmp_2_cfg_a) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_292 = mux(_T_2015, newCfg_2_x, reg_pmp_2_cfg_x) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_293 = mux(_T_2015, _reg_pmp_2_cfg_w_T, reg_pmp_2_cfg_w) @[CSR.scala 1071:76 1075:19 379:20]
    node _GEN_294 = mux(_T_2015, newCfg_2_r, reg_pmp_2_cfg_r) @[CSR.scala 1071:76 1073:17 379:20]
    node _T_2016 = bits(reg_pmp_3_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _T_2017 = not(_T_2016) @[PMP.scala 49:13]
    node _T_2019 = and(_T_2017, pmp_mask_base_lo_3) @[PMP.scala 49:20]
    node _T_2020 = and(reg_pmp_3_cfg_l, _T_2019) @[PMP.scala 51:62]
    node _T_2021 = or(reg_pmp_2_cfg_l, _T_2020) @[PMP.scala 51:44]
    node _T_2022 = not(_T_2021) @[CSR.scala 1080:48]
    node _T_2023 = and(_T_170, _T_2022) @[CSR.scala 1080:45]
    node _GEN_295 = mux(_T_2023, wdata, pad(reg_pmp_2_addr, 32)) @[CSR.scala 1080:71 1081:18 379:20]
    node _T_2024 = not(reg_pmp_3_cfg_l) @[CSR.scala 1071:60]
    node _T_2025 = and(_T_164, _T_2024) @[CSR.scala 1071:57]
    node _newCfg_T_21 = shr(wdata, 24) @[CSR.scala 1072:53]
    node newCfg_3_r = bits(_newCfg_T_21, 0, 0) @[CSR.scala 1072:46]
    node newCfg_3_w = bits(_newCfg_T_21, 1, 1) @[CSR.scala 1072:46]
    node newCfg_3_x = bits(_newCfg_T_21, 2, 2) @[CSR.scala 1072:46]
    node newCfg_3_a = bits(_newCfg_T_21, 4, 3) @[CSR.scala 1072:46]
    node newCfg_3_l = bits(_newCfg_T_21, 7, 7) @[CSR.scala 1072:46]
    node _reg_pmp_3_cfg_w_T = and(newCfg_3_w, newCfg_3_r) @[CSR.scala 1075:31]
    node _GEN_296 = mux(_T_2025, newCfg_3_l, reg_pmp_3_cfg_l) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_298 = mux(_T_2025, newCfg_3_a, reg_pmp_3_cfg_a) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_299 = mux(_T_2025, newCfg_3_x, reg_pmp_3_cfg_x) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_300 = mux(_T_2025, _reg_pmp_3_cfg_w_T, reg_pmp_3_cfg_w) @[CSR.scala 1071:76 1075:19 379:20]
    node _GEN_301 = mux(_T_2025, newCfg_3_r, reg_pmp_3_cfg_r) @[CSR.scala 1071:76 1073:17 379:20]
    node _T_2026 = bits(reg_pmp_4_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _T_2027 = not(_T_2026) @[PMP.scala 49:13]
    node _T_2029 = and(_T_2027, pmp_mask_base_lo_4) @[PMP.scala 49:20]
    node _T_2030 = and(reg_pmp_4_cfg_l, _T_2029) @[PMP.scala 51:62]
    node _T_2031 = or(reg_pmp_3_cfg_l, _T_2030) @[PMP.scala 51:44]
    node _T_2032 = not(_T_2031) @[CSR.scala 1080:48]
    node _T_2033 = and(_T_171, _T_2032) @[CSR.scala 1080:45]
    node _GEN_302 = mux(_T_2033, wdata, pad(reg_pmp_3_addr, 32)) @[CSR.scala 1080:71 1081:18 379:20]
    node _T_2034 = not(reg_pmp_4_cfg_l) @[CSR.scala 1071:60]
    node _T_2035 = and(_T_165, _T_2034) @[CSR.scala 1071:57]
    node _GEN_303 = mux(_T_2035, newCfg_l, reg_pmp_4_cfg_l) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_305 = mux(_T_2035, newCfg_a, reg_pmp_4_cfg_a) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_306 = mux(_T_2035, newCfg_x, reg_pmp_4_cfg_x) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_307 = mux(_T_2035, _reg_pmp_0_cfg_w_T, reg_pmp_4_cfg_w) @[CSR.scala 1071:76 1075:19 379:20]
    node _GEN_308 = mux(_T_2035, newCfg_r, reg_pmp_4_cfg_r) @[CSR.scala 1071:76 1073:17 379:20]
    node _T_2036 = bits(reg_pmp_5_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _T_2037 = not(_T_2036) @[PMP.scala 49:13]
    node _T_2039 = and(_T_2037, pmp_mask_base_lo_5) @[PMP.scala 49:20]
    node _T_2040 = and(reg_pmp_5_cfg_l, _T_2039) @[PMP.scala 51:62]
    node _T_2041 = or(reg_pmp_4_cfg_l, _T_2040) @[PMP.scala 51:44]
    node _T_2042 = not(_T_2041) @[CSR.scala 1080:48]
    node _T_2043 = and(_T_172, _T_2042) @[CSR.scala 1080:45]
    node _GEN_309 = mux(_T_2043, wdata, pad(reg_pmp_4_addr, 32)) @[CSR.scala 1080:71 1081:18 379:20]
    node _T_2044 = not(reg_pmp_5_cfg_l) @[CSR.scala 1071:60]
    node _T_2045 = and(_T_165, _T_2044) @[CSR.scala 1071:57]
    node _GEN_310 = mux(_T_2045, newCfg_1_l, reg_pmp_5_cfg_l) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_312 = mux(_T_2045, newCfg_1_a, reg_pmp_5_cfg_a) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_313 = mux(_T_2045, newCfg_1_x, reg_pmp_5_cfg_x) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_314 = mux(_T_2045, _reg_pmp_1_cfg_w_T, reg_pmp_5_cfg_w) @[CSR.scala 1071:76 1075:19 379:20]
    node _GEN_315 = mux(_T_2045, newCfg_1_r, reg_pmp_5_cfg_r) @[CSR.scala 1071:76 1073:17 379:20]
    node _T_2046 = bits(reg_pmp_6_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _T_2047 = not(_T_2046) @[PMP.scala 49:13]
    node _T_2049 = and(_T_2047, pmp_mask_base_lo_6) @[PMP.scala 49:20]
    node _T_2050 = and(reg_pmp_6_cfg_l, _T_2049) @[PMP.scala 51:62]
    node _T_2051 = or(reg_pmp_5_cfg_l, _T_2050) @[PMP.scala 51:44]
    node _T_2052 = not(_T_2051) @[CSR.scala 1080:48]
    node _T_2053 = and(_T_173, _T_2052) @[CSR.scala 1080:45]
    node _GEN_316 = mux(_T_2053, wdata, pad(reg_pmp_5_addr, 32)) @[CSR.scala 1080:71 1081:18 379:20]
    node _T_2054 = not(reg_pmp_6_cfg_l) @[CSR.scala 1071:60]
    node _T_2055 = and(_T_165, _T_2054) @[CSR.scala 1071:57]
    node _GEN_317 = mux(_T_2055, newCfg_2_l, reg_pmp_6_cfg_l) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_319 = mux(_T_2055, newCfg_2_a, reg_pmp_6_cfg_a) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_320 = mux(_T_2055, newCfg_2_x, reg_pmp_6_cfg_x) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_321 = mux(_T_2055, _reg_pmp_2_cfg_w_T, reg_pmp_6_cfg_w) @[CSR.scala 1071:76 1075:19 379:20]
    node _GEN_322 = mux(_T_2055, newCfg_2_r, reg_pmp_6_cfg_r) @[CSR.scala 1071:76 1073:17 379:20]
    node _T_2056 = bits(reg_pmp_7_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _T_2057 = not(_T_2056) @[PMP.scala 49:13]
    node _T_2059 = and(_T_2057, pmp_mask_base_lo_7) @[PMP.scala 49:20]
    node _T_2060 = and(reg_pmp_7_cfg_l, _T_2059) @[PMP.scala 51:62]
    node _T_2061 = or(reg_pmp_6_cfg_l, _T_2060) @[PMP.scala 51:44]
    node _T_2062 = not(_T_2061) @[CSR.scala 1080:48]
    node _T_2063 = and(_T_174, _T_2062) @[CSR.scala 1080:45]
    node _GEN_323 = mux(_T_2063, wdata, pad(reg_pmp_6_addr, 32)) @[CSR.scala 1080:71 1081:18 379:20]
    node _T_2064 = not(reg_pmp_7_cfg_l) @[CSR.scala 1071:60]
    node _T_2065 = and(_T_165, _T_2064) @[CSR.scala 1071:57]
    node _GEN_324 = mux(_T_2065, newCfg_3_l, reg_pmp_7_cfg_l) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_326 = mux(_T_2065, newCfg_3_a, reg_pmp_7_cfg_a) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_327 = mux(_T_2065, newCfg_3_x, reg_pmp_7_cfg_x) @[CSR.scala 1071:76 1073:17 379:20]
    node _GEN_328 = mux(_T_2065, _reg_pmp_3_cfg_w_T, reg_pmp_7_cfg_w) @[CSR.scala 1071:76 1075:19 379:20]
    node _GEN_329 = mux(_T_2065, newCfg_3_r, reg_pmp_7_cfg_r) @[CSR.scala 1071:76 1073:17 379:20]
    node _T_2071 = or(reg_pmp_7_cfg_l, _T_2060) @[PMP.scala 51:44]
    node _T_2072 = not(_T_2071) @[CSR.scala 1080:48]
    node _T_2073 = and(_T_175, _T_2072) @[CSR.scala 1080:45]
    node _GEN_330 = mux(_T_2073, wdata, pad(reg_pmp_7_addr, 32)) @[CSR.scala 1080:71 1081:18 379:20]
    node _reg_custom_0_T = and(wdata, UInt<32>("h8")) @[CSR.scala 1087:23]
    node _reg_custom_0_T_2 = and(reg_custom_0, UInt<32>("hfffffff7")) @[CSR.scala 1087:38]
    node _reg_custom_0_T_3 = or(_reg_custom_0_T, _reg_custom_0_T_2) @[CSR.scala 1087:31]
    node _GEN_331 = mux(_T_184, _reg_custom_0_T_3, reg_custom_0) @[CSR.scala 1086:35 1087:13 628:43]
    node _GEN_339 = mux(csr_wen, _GEN_184, _GEN_174) @[CSR.scala 897:18]
    node _GEN_340 = mux(csr_wen, _GEN_185, _GEN_175) @[CSR.scala 897:18]
    node _GEN_342 = mux(csr_wen, _GEN_188, reg_misa) @[CSR.scala 897:18 490:21]
    node _GEN_346 = mux(csr_wen, _GEN_192, reg_mtvec) @[CSR.scala 897:18 397:27]
    node _GEN_347 = mux(csr_wen, _GEN_193, _GEN_135) @[CSR.scala 897:18]
    node _GEN_349 = mux(csr_wen, _GEN_195, pad(reg_mcountinhibit, 32)) @[CSR.scala 897:18 436:34]
    node _GEN_350 = mux(csr_wen, _GEN_198, pad(_GEN_2, 64)) @[CSR.scala 897:18]
    node _GEN_351 = mux(csr_wen, _GEN_199, _GEN_3) @[CSR.scala 897:18]
    node _GEN_352 = mux(csr_wen, _GEN_202, pad(_GEN_0, 64)) @[CSR.scala 897:18]
    node _GEN_353 = mux(csr_wen, _GEN_203, _GEN_1) @[CSR.scala 897:18]
    node _GEN_354 = mux(csr_wen, _GEN_204, reg_dcsr_step) @[CSR.scala 897:18 327:21]
    node _GEN_355 = mux(csr_wen, _GEN_205, reg_dcsr_ebreakm) @[CSR.scala 897:18 327:21]
    node _GEN_361 = mux(csr_wen, _GEN_228, reg_bp_0_control_dmode) @[CSR.scala 897:18 378:19]
    node _GEN_364 = mux(csr_wen, _GEN_231, reg_bp_0_control_action) @[CSR.scala 897:18 378:19]
    node _GEN_372 = mux(csr_wen, _GEN_239, reg_bp_0_control_x) @[CSR.scala 897:18 378:19]
    node _GEN_373 = mux(csr_wen, _GEN_240, reg_bp_0_control_w) @[CSR.scala 897:18 378:19]
    node _GEN_374 = mux(csr_wen, _GEN_241, reg_bp_0_control_r) @[CSR.scala 897:18 378:19]
    node _GEN_391 = mux(csr_wen, _GEN_275, reg_pmp_0_cfg_l) @[CSR.scala 897:18 379:20]
    node _GEN_393 = mux(csr_wen, _GEN_277, reg_pmp_0_cfg_a) @[CSR.scala 897:18 379:20]
    node _GEN_397 = mux(csr_wen, _GEN_281, pad(reg_pmp_0_addr, 32)) @[CSR.scala 897:18 379:20]
    node _GEN_398 = mux(csr_wen, _GEN_282, reg_pmp_1_cfg_l) @[CSR.scala 897:18 379:20]
    node _GEN_400 = mux(csr_wen, _GEN_284, reg_pmp_1_cfg_a) @[CSR.scala 897:18 379:20]
    node _GEN_404 = mux(csr_wen, _GEN_288, pad(reg_pmp_1_addr, 32)) @[CSR.scala 897:18 379:20]
    node _GEN_405 = mux(csr_wen, _GEN_289, reg_pmp_2_cfg_l) @[CSR.scala 897:18 379:20]
    node _GEN_407 = mux(csr_wen, _GEN_291, reg_pmp_2_cfg_a) @[CSR.scala 897:18 379:20]
    node _GEN_411 = mux(csr_wen, _GEN_295, pad(reg_pmp_2_addr, 32)) @[CSR.scala 897:18 379:20]
    node _GEN_412 = mux(csr_wen, _GEN_296, reg_pmp_3_cfg_l) @[CSR.scala 897:18 379:20]
    node _GEN_414 = mux(csr_wen, _GEN_298, reg_pmp_3_cfg_a) @[CSR.scala 897:18 379:20]
    node _GEN_418 = mux(csr_wen, _GEN_302, pad(reg_pmp_3_addr, 32)) @[CSR.scala 897:18 379:20]
    node _GEN_419 = mux(csr_wen, _GEN_303, reg_pmp_4_cfg_l) @[CSR.scala 897:18 379:20]
    node _GEN_421 = mux(csr_wen, _GEN_305, reg_pmp_4_cfg_a) @[CSR.scala 897:18 379:20]
    node _GEN_425 = mux(csr_wen, _GEN_309, pad(reg_pmp_4_addr, 32)) @[CSR.scala 897:18 379:20]
    node _GEN_426 = mux(csr_wen, _GEN_310, reg_pmp_5_cfg_l) @[CSR.scala 897:18 379:20]
    node _GEN_428 = mux(csr_wen, _GEN_312, reg_pmp_5_cfg_a) @[CSR.scala 897:18 379:20]
    node _GEN_432 = mux(csr_wen, _GEN_316, pad(reg_pmp_5_addr, 32)) @[CSR.scala 897:18 379:20]
    node _GEN_433 = mux(csr_wen, _GEN_317, reg_pmp_6_cfg_l) @[CSR.scala 897:18 379:20]
    node _GEN_435 = mux(csr_wen, _GEN_319, reg_pmp_6_cfg_a) @[CSR.scala 897:18 379:20]
    node _GEN_439 = mux(csr_wen, _GEN_323, pad(reg_pmp_6_addr, 32)) @[CSR.scala 897:18 379:20]
    node _GEN_440 = mux(csr_wen, _GEN_324, reg_pmp_7_cfg_l) @[CSR.scala 897:18 379:20]
    node _GEN_442 = mux(csr_wen, _GEN_326, reg_pmp_7_cfg_a) @[CSR.scala 897:18 379:20]
    node _GEN_446 = mux(csr_wen, _GEN_330, pad(reg_pmp_7_addr, 32)) @[CSR.scala 897:18 379:20]
    node _GEN_447 = mux(csr_wen, _GEN_331, reg_custom_0) @[CSR.scala 897:18 628:43]
    node _io_trace_0_valid_T = gt(io_retire, UInt<1>("h0")) @[CSR.scala 1162:26]
    node _GEN_509 = mux(reset, UInt<2>("h0"), _GEN_132) @[CSR.scala 319:{24,24}]
    node _GEN_510 = mux(reset, UInt<32>("h0"), _GEN_349) @[CSR.scala 436:{34,34}]
    node _GEN_511 = mux(reset, UInt<64>("h0"), _GEN_352) @[Counters.scala 45:{37,37}]
    node _GEN_512 = mux(reset, UInt<64>("h0"), _GEN_350) @[Counters.scala 45:{37,37}]
    io_rw_rdata <= bits(_io_rw_rdata_T_264, 31, 0) @[CSR.scala 855:15]
    io_decode_0_fp_illegal <= or(_io_decode_0_fp_illegal_T, _io_decode_0_fp_illegal_T_2) @[CSR.scala 667:45]
    io_decode_0_fp_csr <= UInt<1>("h0") @[CSR.scala 669:19]
    io_decode_0_read_illegal <= or(_io_decode_0_read_illegal_T_289, _io_decode_0_read_illegal_T_292) @[CSR.scala 676:68]
    io_decode_0_write_illegal <= andr(_io_decode_0_write_illegal_T) @[CSR.scala 678:47]
    io_decode_0_write_flush <= not(_io_decode_0_write_flush_T_6) @[CSR.scala 679:27]
    io_decode_0_system_illegal <= and(_io_decode_0_system_illegal_T_11, _io_decode_0_read_illegal_T_287) @[CSR.scala 683:49]
    io_csr_stall <= or(reg_wfi, io_status_cease) @[CSR.scala 845:27]
    io_eret <= or(_io_eret_T, insn_ret) @[CSR.scala 726:38]
    io_singleStep <= and(reg_dcsr_step, _io_decode_0_read_illegal_T_287) @[CSR.scala 727:34]
    io_status_debug <= reg_debug @[CSR.scala 730:19]
    io_status_cease <= io_status_cease_r @[CSR.scala 846:19]
    io_status_wfi <= reg_wfi @[CSR.scala 847:17]
    io_status_isa <= reg_misa @[CSR.scala 731:17]
    io_status_dprv <= UInt<2>("h3") @[CSR.scala 734:18]
    io_status_prv <= UInt<2>("h3") @[CSR.scala 728:13]
    io_status_sd <= or(_io_status_sd_T_2, _io_status_sd_T_3) @[CSR.scala 729:58]
    io_status_zero2 <= UInt<27>("h0") @[CSR.scala 728:13]
    io_status_sxl <= UInt<2>("h0") @[CSR.scala 733:17]
    io_status_uxl <= UInt<2>("h0") @[CSR.scala 732:17]
    io_status_sd_rv32 <= io_status_sd @[CSR.scala 736:23]
    io_status_zero1 <= UInt<8>("h0") @[CSR.scala 728:13]
    io_status_tsr <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_tw <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_tvm <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_mxr <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_sum <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_mprv <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_xs <= UInt<2>("h0") @[CSR.scala 728:13]
    io_status_fs <= UInt<2>("h0") @[CSR.scala 728:13]
    io_status_mpp <= UInt<2>("h3") @[CSR.scala 728:13]
    io_status_vs <= UInt<2>("h0") @[CSR.scala 728:13]
    io_status_spp <= reg_mstatus_spp @[CSR.scala 728:13]
    io_status_mpie <= reg_mstatus_mpie @[CSR.scala 728:13]
    io_status_hpie <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_spie <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_upie <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_mie <= reg_mstatus_mie @[CSR.scala 728:13]
    io_status_hie <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_sie <= UInt<1>("h0") @[CSR.scala 728:13]
    io_status_uie <= UInt<1>("h0") @[CSR.scala 728:13]
    io_evec <= mux(insn_ret, _GEN_149, tvec) @[CSR.scala 724:11 813:19]
    io_time <= bits(value_1, 31, 0) @[CSR.scala 844:11]
    io_interrupt <= and(_io_interrupt_T_2, _io_interrupt_T_4) @[CSR.scala 469:73]
    io_interrupt_cause <= tail(_interruptCause_T_3, 1) @[CSR.scala 468:67]
    io_bp_0_control_action <= reg_bp_0_control_action @[CSR.scala 471:9]
    io_bp_0_control_tmatch <= reg_bp_0_control_tmatch @[CSR.scala 471:9]
    io_bp_0_control_x <= reg_bp_0_control_x @[CSR.scala 471:9]
    io_bp_0_control_w <= reg_bp_0_control_w @[CSR.scala 471:9]
    io_bp_0_control_r <= reg_bp_0_control_r @[CSR.scala 471:9]
    io_bp_0_address <= reg_bp_0_address @[CSR.scala 471:9]
    io_pmp_0_cfg_l <= reg_pmp_0_cfg_l @[PMP.scala 26:19 27:13]
    io_pmp_0_cfg_a <= reg_pmp_0_cfg_a @[PMP.scala 26:19 27:13]
    io_pmp_0_cfg_x <= reg_pmp_0_cfg_x @[PMP.scala 26:19 27:13]
    io_pmp_0_cfg_w <= reg_pmp_0_cfg_w @[PMP.scala 26:19 27:13]
    io_pmp_0_cfg_r <= reg_pmp_0_cfg_r @[PMP.scala 26:19 27:13]
    io_pmp_0_addr <= reg_pmp_0_addr @[PMP.scala 26:19 28:14]
    io_pmp_0_mask <= bits(_pmp_mask_T_3, 31, 0) @[PMP.scala 26:19 29:14]
    io_pmp_1_cfg_l <= reg_pmp_1_cfg_l @[PMP.scala 26:19 27:13]
    io_pmp_1_cfg_a <= reg_pmp_1_cfg_a @[PMP.scala 26:19 27:13]
    io_pmp_1_cfg_x <= reg_pmp_1_cfg_x @[PMP.scala 26:19 27:13]
    io_pmp_1_cfg_w <= reg_pmp_1_cfg_w @[PMP.scala 26:19 27:13]
    io_pmp_1_cfg_r <= reg_pmp_1_cfg_r @[PMP.scala 26:19 27:13]
    io_pmp_1_addr <= reg_pmp_1_addr @[PMP.scala 26:19 28:14]
    io_pmp_1_mask <= bits(_pmp_mask_T_7, 31, 0) @[PMP.scala 26:19 29:14]
    io_pmp_2_cfg_l <= reg_pmp_2_cfg_l @[PMP.scala 26:19 27:13]
    io_pmp_2_cfg_a <= reg_pmp_2_cfg_a @[PMP.scala 26:19 27:13]
    io_pmp_2_cfg_x <= reg_pmp_2_cfg_x @[PMP.scala 26:19 27:13]
    io_pmp_2_cfg_w <= reg_pmp_2_cfg_w @[PMP.scala 26:19 27:13]
    io_pmp_2_cfg_r <= reg_pmp_2_cfg_r @[PMP.scala 26:19 27:13]
    io_pmp_2_addr <= reg_pmp_2_addr @[PMP.scala 26:19 28:14]
    io_pmp_2_mask <= bits(_pmp_mask_T_11, 31, 0) @[PMP.scala 26:19 29:14]
    io_pmp_3_cfg_l <= reg_pmp_3_cfg_l @[PMP.scala 26:19 27:13]
    io_pmp_3_cfg_a <= reg_pmp_3_cfg_a @[PMP.scala 26:19 27:13]
    io_pmp_3_cfg_x <= reg_pmp_3_cfg_x @[PMP.scala 26:19 27:13]
    io_pmp_3_cfg_w <= reg_pmp_3_cfg_w @[PMP.scala 26:19 27:13]
    io_pmp_3_cfg_r <= reg_pmp_3_cfg_r @[PMP.scala 26:19 27:13]
    io_pmp_3_addr <= reg_pmp_3_addr @[PMP.scala 26:19 28:14]
    io_pmp_3_mask <= bits(_pmp_mask_T_15, 31, 0) @[PMP.scala 26:19 29:14]
    io_pmp_4_cfg_l <= reg_pmp_4_cfg_l @[PMP.scala 26:19 27:13]
    io_pmp_4_cfg_a <= reg_pmp_4_cfg_a @[PMP.scala 26:19 27:13]
    io_pmp_4_cfg_x <= reg_pmp_4_cfg_x @[PMP.scala 26:19 27:13]
    io_pmp_4_cfg_w <= reg_pmp_4_cfg_w @[PMP.scala 26:19 27:13]
    io_pmp_4_cfg_r <= reg_pmp_4_cfg_r @[PMP.scala 26:19 27:13]
    io_pmp_4_addr <= reg_pmp_4_addr @[PMP.scala 26:19 28:14]
    io_pmp_4_mask <= bits(_pmp_mask_T_19, 31, 0) @[PMP.scala 26:19 29:14]
    io_pmp_5_cfg_l <= reg_pmp_5_cfg_l @[PMP.scala 26:19 27:13]
    io_pmp_5_cfg_a <= reg_pmp_5_cfg_a @[PMP.scala 26:19 27:13]
    io_pmp_5_cfg_x <= reg_pmp_5_cfg_x @[PMP.scala 26:19 27:13]
    io_pmp_5_cfg_w <= reg_pmp_5_cfg_w @[PMP.scala 26:19 27:13]
    io_pmp_5_cfg_r <= reg_pmp_5_cfg_r @[PMP.scala 26:19 27:13]
    io_pmp_5_addr <= reg_pmp_5_addr @[PMP.scala 26:19 28:14]
    io_pmp_5_mask <= bits(_pmp_mask_T_23, 31, 0) @[PMP.scala 26:19 29:14]
    io_pmp_6_cfg_l <= reg_pmp_6_cfg_l @[PMP.scala 26:19 27:13]
    io_pmp_6_cfg_a <= reg_pmp_6_cfg_a @[PMP.scala 26:19 27:13]
    io_pmp_6_cfg_x <= reg_pmp_6_cfg_x @[PMP.scala 26:19 27:13]
    io_pmp_6_cfg_w <= reg_pmp_6_cfg_w @[PMP.scala 26:19 27:13]
    io_pmp_6_cfg_r <= reg_pmp_6_cfg_r @[PMP.scala 26:19 27:13]
    io_pmp_6_addr <= reg_pmp_6_addr @[PMP.scala 26:19 28:14]
    io_pmp_6_mask <= bits(_pmp_mask_T_27, 31, 0) @[PMP.scala 26:19 29:14]
    io_pmp_7_cfg_l <= reg_pmp_7_cfg_l @[PMP.scala 26:19 27:13]
    io_pmp_7_cfg_a <= reg_pmp_7_cfg_a @[PMP.scala 26:19 27:13]
    io_pmp_7_cfg_x <= reg_pmp_7_cfg_x @[PMP.scala 26:19 27:13]
    io_pmp_7_cfg_w <= reg_pmp_7_cfg_w @[PMP.scala 26:19 27:13]
    io_pmp_7_cfg_r <= reg_pmp_7_cfg_r @[PMP.scala 26:19 27:13]
    io_pmp_7_addr <= reg_pmp_7_addr @[PMP.scala 26:19 28:14]
    io_pmp_7_mask <= bits(_pmp_mask_T_31, 31, 0) @[PMP.scala 26:19 29:14]
    io_inhibit_cycle <= bits(reg_mcountinhibit, 0, 0) @[CSR.scala 437:40]
    io_trace_0_valid <= or(_io_trace_0_valid_T, io_trace_0_exception) @[CSR.scala 1162:30]
    io_trace_0_iaddr <= io_pc @[CSR.scala 1164:13]
    io_trace_0_insn <= io_inst_0 @[CSR.scala 1163:12]
    io_trace_0_exception <= or(_io_eret_T, io_exception) @[CSR.scala 738:43]
    io_customCSRs_0_value <= reg_custom_0 @[CSR.scala 852:14]
    reg_mstatus_spp <= bits(_GEN_509, 0, 0) @[CSR.scala 319:{24,24}]
    reg_mstatus_mpie <= mux(reset, UInt<1>("h0"), _GEN_340) @[CSR.scala 319:{24,24}]
    reg_mstatus_mie <= mux(reset, UInt<1>("h0"), _GEN_339) @[CSR.scala 319:{24,24}]
    reg_dcsr_ebreakm <= mux(reset, UInt<1>("h0"), _GEN_355) @[CSR.scala 327:{21,21}]
    reg_dcsr_cause <= mux(reset, UInt<3>("h0"), _GEN_119) @[CSR.scala 327:{21,21}]
    reg_dcsr_step <= mux(reset, UInt<1>("h0"), _GEN_354) @[CSR.scala 327:{21,21}]
    reg_debug <= mux(reset, UInt<1>("h0"), _GEN_171) @[CSR.scala 368:{22,22}]
    reg_dpc <= mux(csr_wen, _GEN_206, _GEN_118) @[CSR.scala 897:18]
    reg_dscratch <= mux(csr_wen, _GEN_207, reg_dscratch) @[CSR.scala 897:18 370:25]
    reg_singleStepped <= mux(_io_interrupt_T, UInt<1>("h0"), _GEN_48) @[CSR.scala 746:{25,45}]
    reg_bp_0_control_dmode <= mux(reset, UInt<1>("h0"), _GEN_361) @[CSR.scala 1140:18 1142:17]
    reg_bp_0_control_action <= mux(reset, UInt<1>("h0"), _GEN_364) @[CSR.scala 1140:18 1141:18]
    reg_bp_0_control_tmatch <= mux(csr_wen, _GEN_234, reg_bp_0_control_tmatch) @[CSR.scala 897:18 378:19]
    reg_bp_0_control_x <= mux(reset, UInt<1>("h0"), _GEN_372) @[CSR.scala 1140:18 1146:13]
    reg_bp_0_control_w <= mux(reset, UInt<1>("h0"), _GEN_373) @[CSR.scala 1140:18 1145:13]
    reg_bp_0_control_r <= mux(reset, UInt<1>("h0"), _GEN_374) @[CSR.scala 1140:18 1144:13]
    reg_bp_0_address <= mux(csr_wen, _GEN_226, reg_bp_0_address) @[CSR.scala 897:18 378:19]
    reg_pmp_0_cfg_l <= mux(reset, UInt<1>("h0"), _GEN_391) @[CSR.scala 1157:18 PMP.scala 40:11]
    reg_pmp_0_cfg_a <= mux(reset, UInt<2>("h0"), _GEN_393) @[CSR.scala 1157:18 PMP.scala 39:11]
    reg_pmp_0_cfg_x <= mux(csr_wen, _GEN_278, reg_pmp_0_cfg_x) @[CSR.scala 897:18 379:20]
    reg_pmp_0_cfg_w <= mux(csr_wen, _GEN_279, reg_pmp_0_cfg_w) @[CSR.scala 897:18 379:20]
    reg_pmp_0_cfg_r <= mux(csr_wen, _GEN_280, reg_pmp_0_cfg_r) @[CSR.scala 897:18 379:20]
    reg_pmp_0_addr <= bits(_GEN_397, 29, 0)
    reg_pmp_1_cfg_l <= mux(reset, UInt<1>("h0"), _GEN_398) @[CSR.scala 1157:18 PMP.scala 40:11]
    reg_pmp_1_cfg_a <= mux(reset, UInt<2>("h0"), _GEN_400) @[CSR.scala 1157:18 PMP.scala 39:11]
    reg_pmp_1_cfg_x <= mux(csr_wen, _GEN_285, reg_pmp_1_cfg_x) @[CSR.scala 897:18 379:20]
    reg_pmp_1_cfg_w <= mux(csr_wen, _GEN_286, reg_pmp_1_cfg_w) @[CSR.scala 897:18 379:20]
    reg_pmp_1_cfg_r <= mux(csr_wen, _GEN_287, reg_pmp_1_cfg_r) @[CSR.scala 897:18 379:20]
    reg_pmp_1_addr <= bits(_GEN_404, 29, 0)
    reg_pmp_2_cfg_l <= mux(reset, UInt<1>("h0"), _GEN_405) @[CSR.scala 1157:18 PMP.scala 40:11]
    reg_pmp_2_cfg_a <= mux(reset, UInt<2>("h0"), _GEN_407) @[CSR.scala 1157:18 PMP.scala 39:11]
    reg_pmp_2_cfg_x <= mux(csr_wen, _GEN_292, reg_pmp_2_cfg_x) @[CSR.scala 897:18 379:20]
    reg_pmp_2_cfg_w <= mux(csr_wen, _GEN_293, reg_pmp_2_cfg_w) @[CSR.scala 897:18 379:20]
    reg_pmp_2_cfg_r <= mux(csr_wen, _GEN_294, reg_pmp_2_cfg_r) @[CSR.scala 897:18 379:20]
    reg_pmp_2_addr <= bits(_GEN_411, 29, 0)
    reg_pmp_3_cfg_l <= mux(reset, UInt<1>("h0"), _GEN_412) @[CSR.scala 1157:18 PMP.scala 40:11]
    reg_pmp_3_cfg_a <= mux(reset, UInt<2>("h0"), _GEN_414) @[CSR.scala 1157:18 PMP.scala 39:11]
    reg_pmp_3_cfg_x <= mux(csr_wen, _GEN_299, reg_pmp_3_cfg_x) @[CSR.scala 897:18 379:20]
    reg_pmp_3_cfg_w <= mux(csr_wen, _GEN_300, reg_pmp_3_cfg_w) @[CSR.scala 897:18 379:20]
    reg_pmp_3_cfg_r <= mux(csr_wen, _GEN_301, reg_pmp_3_cfg_r) @[CSR.scala 897:18 379:20]
    reg_pmp_3_addr <= bits(_GEN_418, 29, 0)
    reg_pmp_4_cfg_l <= mux(reset, UInt<1>("h0"), _GEN_419) @[CSR.scala 1157:18 PMP.scala 40:11]
    reg_pmp_4_cfg_a <= mux(reset, UInt<2>("h0"), _GEN_421) @[CSR.scala 1157:18 PMP.scala 39:11]
    reg_pmp_4_cfg_x <= mux(csr_wen, _GEN_306, reg_pmp_4_cfg_x) @[CSR.scala 897:18 379:20]
    reg_pmp_4_cfg_w <= mux(csr_wen, _GEN_307, reg_pmp_4_cfg_w) @[CSR.scala 897:18 379:20]
    reg_pmp_4_cfg_r <= mux(csr_wen, _GEN_308, reg_pmp_4_cfg_r) @[CSR.scala 897:18 379:20]
    reg_pmp_4_addr <= bits(_GEN_425, 29, 0)
    reg_pmp_5_cfg_l <= mux(reset, UInt<1>("h0"), _GEN_426) @[CSR.scala 1157:18 PMP.scala 40:11]
    reg_pmp_5_cfg_a <= mux(reset, UInt<2>("h0"), _GEN_428) @[CSR.scala 1157:18 PMP.scala 39:11]
    reg_pmp_5_cfg_x <= mux(csr_wen, _GEN_313, reg_pmp_5_cfg_x) @[CSR.scala 897:18 379:20]
    reg_pmp_5_cfg_w <= mux(csr_wen, _GEN_314, reg_pmp_5_cfg_w) @[CSR.scala 897:18 379:20]
    reg_pmp_5_cfg_r <= mux(csr_wen, _GEN_315, reg_pmp_5_cfg_r) @[CSR.scala 897:18 379:20]
    reg_pmp_5_addr <= bits(_GEN_432, 29, 0)
    reg_pmp_6_cfg_l <= mux(reset, UInt<1>("h0"), _GEN_433) @[CSR.scala 1157:18 PMP.scala 40:11]
    reg_pmp_6_cfg_a <= mux(reset, UInt<2>("h0"), _GEN_435) @[CSR.scala 1157:18 PMP.scala 39:11]
    reg_pmp_6_cfg_x <= mux(csr_wen, _GEN_320, reg_pmp_6_cfg_x) @[CSR.scala 897:18 379:20]
    reg_pmp_6_cfg_w <= mux(csr_wen, _GEN_321, reg_pmp_6_cfg_w) @[CSR.scala 897:18 379:20]
    reg_pmp_6_cfg_r <= mux(csr_wen, _GEN_322, reg_pmp_6_cfg_r) @[CSR.scala 897:18 379:20]
    reg_pmp_6_addr <= bits(_GEN_439, 29, 0)
    reg_pmp_7_cfg_l <= mux(reset, UInt<1>("h0"), _GEN_440) @[CSR.scala 1157:18 PMP.scala 40:11]
    reg_pmp_7_cfg_a <= mux(reset, UInt<2>("h0"), _GEN_442) @[CSR.scala 1157:18 PMP.scala 39:11]
    reg_pmp_7_cfg_x <= mux(csr_wen, _GEN_327, reg_pmp_7_cfg_x) @[CSR.scala 897:18 379:20]
    reg_pmp_7_cfg_w <= mux(csr_wen, _GEN_328, reg_pmp_7_cfg_w) @[CSR.scala 897:18 379:20]
    reg_pmp_7_cfg_r <= mux(csr_wen, _GEN_329, reg_pmp_7_cfg_r) @[CSR.scala 897:18 379:20]
    reg_pmp_7_addr <= bits(_GEN_446, 29, 0)
    reg_mie <= mux(csr_wen, _GEN_189, reg_mie) @[CSR.scala 897:18 381:20]
    reg_mepc <= mux(csr_wen, _GEN_190, _GEN_134) @[CSR.scala 897:18]
    reg_mcause <= mux(reset, UInt<32>("h0"), _GEN_347) @[CSR.scala 392:{27,27}]
    reg_mtval <= mux(csr_wen, _GEN_194, _GEN_136) @[CSR.scala 897:18]
    reg_mscratch <= mux(csr_wen, _GEN_191, reg_mscratch) @[CSR.scala 897:18 394:25]
    reg_mtvec <= mux(reset, UInt<32>("h0"), _GEN_346) @[CSR.scala 397:{27,27}]
    reg_wfi <= mux(reset, UInt<1>("h0"), _GEN_47) @[CSR.scala 427:{50,50}]
    reg_mcountinhibit <= bits(_GEN_510, 2, 0) @[CSR.scala 436:{34,34}]
    value_lo <= bits(_GEN_511, 5, 0) @[Counters.scala 45:{37,37}]
    value_hi <= mux(reset, UInt<58>("h0"), _GEN_353) @[Counters.scala 50:{27,27}]
    value_lo_1 <= bits(_GEN_512, 5, 0) @[Counters.scala 45:{37,37}]
    value_hi_1 <= mux(reset, UInt<58>("h0"), _GEN_351) @[Counters.scala 50:{27,27}]
    reg_misa <= mux(reset, UInt<32>("h40801105"), _GEN_342) @[CSR.scala 490:{21,21}]
    reg_custom_0 <= mux(reset, UInt<32>("h8"), _GEN_447) @[CSR.scala 628:{43,43}]
    io_status_cease_r <= mux(reset, UInt<1>("h0"), _GEN_179) @[Reg.scala 27:{20,20}]
    printf(clock, _T_236, "Assertion failed: these conditions must be mutually exclusive\n    at CSR.scala:739 assert(PopCount(insn_ret :: insn_call :: insn_break :: io.exception :: Nil) <= 1, \"these conditions must be mutually exclusive\")\n") @[CSR.scala 739:9]
    stop(clock, _T_236, 1) @[CSR.scala 739:9]
    printf(clock, _T_258, "Assertion failed\n    at CSR.scala:748 assert(!reg_singleStepped || io.retire === UInt(0))\n") @[CSR.scala 748:9]
    stop(clock, _T_258, 1) @[CSR.scala 748:9]

  module BreakpointUnit :
    input io_status_debug : UInt<1>
    input io_bp_0_control_action : UInt<1>
    input io_bp_0_control_tmatch : UInt<2>
    input io_bp_0_control_x : UInt<1>
    input io_bp_0_control_w : UInt<1>
    input io_bp_0_control_r : UInt<1>
    input io_bp_0_address : UInt<32>
    input io_pc : UInt<32>
    input io_ea : UInt<32>
    output io_xcpt_if : UInt<1>
    output io_xcpt_ld : UInt<1>
    output io_xcpt_st : UInt<1>
    output io_debug_if : UInt<1>
    output io_debug_ld : UInt<1>
    output io_debug_st : UInt<1>

    node en = not(io_status_debug) @[Breakpoint.scala 31:35]
    node _r_T = and(en, io_bp_0_control_r) @[Breakpoint.scala 107:16]
    node _r_T_1 = bits(io_bp_0_control_tmatch, 1, 1) @[Breakpoint.scala 69:23]
    node _r_T_2 = geq(io_ea, io_bp_0_address) @[Breakpoint.scala 66:8]
    node _r_T_3 = bits(io_bp_0_control_tmatch, 0, 0) @[Breakpoint.scala 66:36]
    node _r_T_4 = xor(_r_T_2, _r_T_3) @[Breakpoint.scala 66:20]
    node _r_T_5 = not(io_ea) @[Breakpoint.scala 63:6]
    node _r_T_6 = bits(io_bp_0_address, 0, 0) @[Breakpoint.scala 60:83]
    node r_lo_hi = and(_r_T_3, _r_T_6) @[Breakpoint.scala 60:73]
    node _r_T_7 = bits(io_bp_0_address, 1, 1) @[Breakpoint.scala 60:83]
    node r_hi_lo = and(r_lo_hi, _r_T_7) @[Breakpoint.scala 60:73]
    node _r_T_8 = bits(io_bp_0_address, 2, 2) @[Breakpoint.scala 60:83]
    node r_hi_hi = and(r_hi_lo, _r_T_8) @[Breakpoint.scala 60:73]
    node r_lo = cat(r_lo_hi, _r_T_3) @[Cat.scala 30:58]
    node r_hi = cat(r_hi_hi, r_hi_lo) @[Cat.scala 30:58]
    node _r_T_9 = cat(r_hi, r_lo) @[Cat.scala 30:58]
    node _GEN_11 = pad(_r_T_9, 32) @[Breakpoint.scala 63:9]
    node _r_T_10 = or(_r_T_5, _GEN_11) @[Breakpoint.scala 63:9]
    node _r_T_11 = not(io_bp_0_address) @[Breakpoint.scala 63:24]
    node _r_T_16 = or(_r_T_11, _GEN_11) @[Breakpoint.scala 63:33]
    node _r_T_17 = eq(_r_T_10, _r_T_16) @[Breakpoint.scala 63:19]
    node _r_T_18 = mux(_r_T_1, _r_T_4, _r_T_17) @[Breakpoint.scala 69:8]
    node r = and(_r_T, _r_T_18) @[Breakpoint.scala 107:32]
    node _w_T = and(en, io_bp_0_control_w) @[Breakpoint.scala 108:16]
    node w = and(_w_T, _r_T_18) @[Breakpoint.scala 108:32]
    node _x_T = and(en, io_bp_0_control_x) @[Breakpoint.scala 109:16]
    node _x_T_2 = geq(io_pc, io_bp_0_address) @[Breakpoint.scala 66:8]
    node _x_T_4 = xor(_x_T_2, _r_T_3) @[Breakpoint.scala 66:20]
    node _x_T_5 = not(io_pc) @[Breakpoint.scala 63:6]
    node _x_T_10 = or(_x_T_5, _GEN_11) @[Breakpoint.scala 63:9]
    node _x_T_17 = eq(_x_T_10, _r_T_16) @[Breakpoint.scala 63:19]
    node _x_T_18 = mux(_r_T_1, _x_T_4, _x_T_17) @[Breakpoint.scala 69:8]
    node x = and(_x_T, _x_T_18) @[Breakpoint.scala 109:32]
    node _io_xcpt_ld_T = not(io_bp_0_control_action) @[Breakpoint.scala 119:51]
    io_xcpt_if <= and(x, _io_xcpt_ld_T) @[Breakpoint.scala 121:{27,40} 97:14]
    io_xcpt_ld <= and(r, _io_xcpt_ld_T) @[Breakpoint.scala 119:{27,40} 98:14]
    io_xcpt_st <= and(w, _io_xcpt_ld_T) @[Breakpoint.scala 120:{27,40} 99:14]
    io_debug_if <= and(x, io_bp_0_control_action) @[Breakpoint.scala 100:15 121:{27,73}]
    io_debug_ld <= and(r, io_bp_0_control_action) @[Breakpoint.scala 101:15 119:{27,73}]
    io_debug_st <= and(w, io_bp_0_control_action) @[Breakpoint.scala 102:15 120:{27,73}]

  module ALU :
    input io_fn : UInt<4>
    input io_in2 : UInt<32>
    input io_in1 : UInt<32>
    output io_out : UInt<32>
    output io_adder_out : UInt<32>
    output io_cmp_out : UInt<1>

    node _in2_inv_T = bits(io_fn, 3, 3) @[ALU.scala 40:29]
    node _in2_inv_T_1 = not(io_in2) @[ALU.scala 61:35]
    node in2_inv = mux(_in2_inv_T, _in2_inv_T_1, io_in2) @[ALU.scala 61:20]
    node in1_xor_in2 = xor(io_in1, in2_inv) @[ALU.scala 62:28]
    node _io_adder_out_T = add(io_in1, in2_inv) @[ALU.scala 63:26]
    node _io_adder_out_T_1 = tail(_io_adder_out_T, 1) @[ALU.scala 63:26]
    node _GEN_0 = pad(_in2_inv_T, 32) @[ALU.scala 63:36]
    node _io_adder_out_T_3 = add(_io_adder_out_T_1, _GEN_0) @[ALU.scala 63:36]
    node _slt_T = bits(io_in1, 31, 31) @[ALU.scala 67:15]
    node _slt_T_1 = bits(io_in2, 31, 31) @[ALU.scala 67:34]
    node _slt_T_2 = eq(_slt_T, _slt_T_1) @[ALU.scala 67:24]
    node _slt_T_3 = bits(io_adder_out, 31, 31) @[ALU.scala 67:56]
    node _slt_T_4 = bits(io_fn, 1, 1) @[ALU.scala 42:35]
    node _slt_T_7 = mux(_slt_T_4, _slt_T_1, _slt_T) @[ALU.scala 68:8]
    node slt = mux(_slt_T_2, _slt_T_3, _slt_T_7) @[ALU.scala 67:8]
    node _io_cmp_out_T = bits(io_fn, 0, 0) @[ALU.scala 43:35]
    node _io_cmp_out_T_2 = not(_in2_inv_T) @[ALU.scala 44:26]
    node _io_cmp_out_T_3 = eq(in1_xor_in2, UInt<32>("h0")) @[ALU.scala 69:68]
    node _io_cmp_out_T_4 = mux(_io_cmp_out_T_2, _io_cmp_out_T_3, slt) @[ALU.scala 69:41]
    node shamt = bits(io_in2, 4, 0) @[ALU.scala 73:28]
    node _shin_T = eq(io_fn, UInt<4>("h5")) @[ALU.scala 81:24]
    node _shin_T_1 = eq(io_fn, UInt<4>("hb")) @[ALU.scala 81:44]
    node _shin_T_2 = or(_shin_T, _shin_T_1) @[ALU.scala 81:35]
    node _shin_T_5 = shr(io_in1, 16) @[Bitwise.scala 103:21]
    node _GEN_1 = pad(_shin_T_5, 32) @[Bitwise.scala 103:31]
    node _shin_T_6 = and(_GEN_1, UInt<32>("hffff")) @[Bitwise.scala 103:31]
    node _shin_T_7 = bits(io_in1, 15, 0) @[Bitwise.scala 103:46]
    node _shin_T_8 = shl(_shin_T_7, 16) @[Bitwise.scala 103:65]
    node _shin_T_10 = and(_shin_T_8, UInt<32>("hffff0000")) @[Bitwise.scala 103:75]
    node _shin_T_11 = or(_shin_T_6, _shin_T_10) @[Bitwise.scala 103:39]
    node _shin_T_15 = shr(_shin_T_11, 8) @[Bitwise.scala 103:21]
    node _GEN_2 = pad(_shin_T_15, 32) @[Bitwise.scala 103:31]
    node _shin_T_16 = and(_GEN_2, UInt<32>("hff00ff")) @[Bitwise.scala 103:31]
    node _shin_T_17 = bits(_shin_T_11, 23, 0) @[Bitwise.scala 103:46]
    node _shin_T_18 = shl(_shin_T_17, 8) @[Bitwise.scala 103:65]
    node _shin_T_20 = and(_shin_T_18, UInt<32>("hff00ff00")) @[Bitwise.scala 103:75]
    node _shin_T_21 = or(_shin_T_16, _shin_T_20) @[Bitwise.scala 103:39]
    node _shin_T_25 = shr(_shin_T_21, 4) @[Bitwise.scala 103:21]
    node _GEN_3 = pad(_shin_T_25, 32) @[Bitwise.scala 103:31]
    node _shin_T_26 = and(_GEN_3, UInt<32>("hf0f0f0f")) @[Bitwise.scala 103:31]
    node _shin_T_27 = bits(_shin_T_21, 27, 0) @[Bitwise.scala 103:46]
    node _shin_T_28 = shl(_shin_T_27, 4) @[Bitwise.scala 103:65]
    node _shin_T_30 = and(_shin_T_28, UInt<32>("hf0f0f0f0")) @[Bitwise.scala 103:75]
    node _shin_T_31 = or(_shin_T_26, _shin_T_30) @[Bitwise.scala 103:39]
    node _shin_T_35 = shr(_shin_T_31, 2) @[Bitwise.scala 103:21]
    node _GEN_4 = pad(_shin_T_35, 32) @[Bitwise.scala 103:31]
    node _shin_T_36 = and(_GEN_4, UInt<32>("h33333333")) @[Bitwise.scala 103:31]
    node _shin_T_37 = bits(_shin_T_31, 29, 0) @[Bitwise.scala 103:46]
    node _shin_T_38 = shl(_shin_T_37, 2) @[Bitwise.scala 103:65]
    node _shin_T_40 = and(_shin_T_38, UInt<32>("hcccccccc")) @[Bitwise.scala 103:75]
    node _shin_T_41 = or(_shin_T_36, _shin_T_40) @[Bitwise.scala 103:39]
    node _shin_T_45 = shr(_shin_T_41, 1) @[Bitwise.scala 103:21]
    node _GEN_5 = pad(_shin_T_45, 32) @[Bitwise.scala 103:31]
    node _shin_T_46 = and(_GEN_5, UInt<32>("h55555555")) @[Bitwise.scala 103:31]
    node _shin_T_47 = bits(_shin_T_41, 30, 0) @[Bitwise.scala 103:46]
    node _shin_T_48 = shl(_shin_T_47, 1) @[Bitwise.scala 103:65]
    node _shin_T_50 = and(_shin_T_48, UInt<32>("haaaaaaaa")) @[Bitwise.scala 103:75]
    node _shin_T_51 = or(_shin_T_46, _shin_T_50) @[Bitwise.scala 103:39]
    node shin = mux(_shin_T_2, io_in1, _shin_T_51) @[ALU.scala 81:17]
    node _shout_r_T_1 = bits(shin, 31, 31) @[ALU.scala 82:41]
    node shout_r_hi = and(_in2_inv_T, _shout_r_T_1) @[ALU.scala 82:35]
    node _shout_r_T_2 = cat(shout_r_hi, shin) @[Cat.scala 30:58]
    node _shout_r_T_3 = asSInt(_shout_r_T_2) @[ALU.scala 82:57]
    node _shout_r_T_4 = dshr(_shout_r_T_3, shamt) @[ALU.scala 82:64]
    node shout_r = bits(_shout_r_T_4, 31, 0) @[ALU.scala 82:73]
    node _shout_l_T_2 = shr(shout_r, 16) @[Bitwise.scala 103:21]
    node _GEN_6 = pad(_shout_l_T_2, 32) @[Bitwise.scala 103:31]
    node _shout_l_T_3 = and(_GEN_6, UInt<32>("hffff")) @[Bitwise.scala 103:31]
    node _shout_l_T_4 = bits(shout_r, 15, 0) @[Bitwise.scala 103:46]
    node _shout_l_T_5 = shl(_shout_l_T_4, 16) @[Bitwise.scala 103:65]
    node _shout_l_T_7 = and(_shout_l_T_5, UInt<32>("hffff0000")) @[Bitwise.scala 103:75]
    node _shout_l_T_8 = or(_shout_l_T_3, _shout_l_T_7) @[Bitwise.scala 103:39]
    node _shout_l_T_12 = shr(_shout_l_T_8, 8) @[Bitwise.scala 103:21]
    node _GEN_7 = pad(_shout_l_T_12, 32) @[Bitwise.scala 103:31]
    node _shout_l_T_13 = and(_GEN_7, UInt<32>("hff00ff")) @[Bitwise.scala 103:31]
    node _shout_l_T_14 = bits(_shout_l_T_8, 23, 0) @[Bitwise.scala 103:46]
    node _shout_l_T_15 = shl(_shout_l_T_14, 8) @[Bitwise.scala 103:65]
    node _shout_l_T_17 = and(_shout_l_T_15, UInt<32>("hff00ff00")) @[Bitwise.scala 103:75]
    node _shout_l_T_18 = or(_shout_l_T_13, _shout_l_T_17) @[Bitwise.scala 103:39]
    node _shout_l_T_22 = shr(_shout_l_T_18, 4) @[Bitwise.scala 103:21]
    node _GEN_8 = pad(_shout_l_T_22, 32) @[Bitwise.scala 103:31]
    node _shout_l_T_23 = and(_GEN_8, UInt<32>("hf0f0f0f")) @[Bitwise.scala 103:31]
    node _shout_l_T_24 = bits(_shout_l_T_18, 27, 0) @[Bitwise.scala 103:46]
    node _shout_l_T_25 = shl(_shout_l_T_24, 4) @[Bitwise.scala 103:65]
    node _shout_l_T_27 = and(_shout_l_T_25, UInt<32>("hf0f0f0f0")) @[Bitwise.scala 103:75]
    node _shout_l_T_28 = or(_shout_l_T_23, _shout_l_T_27) @[Bitwise.scala 103:39]
    node _shout_l_T_32 = shr(_shout_l_T_28, 2) @[Bitwise.scala 103:21]
    node _GEN_9 = pad(_shout_l_T_32, 32) @[Bitwise.scala 103:31]
    node _shout_l_T_33 = and(_GEN_9, UInt<32>("h33333333")) @[Bitwise.scala 103:31]
    node _shout_l_T_34 = bits(_shout_l_T_28, 29, 0) @[Bitwise.scala 103:46]
    node _shout_l_T_35 = shl(_shout_l_T_34, 2) @[Bitwise.scala 103:65]
    node _shout_l_T_37 = and(_shout_l_T_35, UInt<32>("hcccccccc")) @[Bitwise.scala 103:75]
    node _shout_l_T_38 = or(_shout_l_T_33, _shout_l_T_37) @[Bitwise.scala 103:39]
    node _shout_l_T_42 = shr(_shout_l_T_38, 1) @[Bitwise.scala 103:21]
    node _GEN_10 = pad(_shout_l_T_42, 32) @[Bitwise.scala 103:31]
    node _shout_l_T_43 = and(_GEN_10, UInt<32>("h55555555")) @[Bitwise.scala 103:31]
    node _shout_l_T_44 = bits(_shout_l_T_38, 30, 0) @[Bitwise.scala 103:46]
    node _shout_l_T_45 = shl(_shout_l_T_44, 1) @[Bitwise.scala 103:65]
    node _shout_l_T_47 = and(_shout_l_T_45, UInt<32>("haaaaaaaa")) @[Bitwise.scala 103:75]
    node shout_l = or(_shout_l_T_43, _shout_l_T_47) @[Bitwise.scala 103:39]
    node _shout_T_3 = mux(_shin_T_2, shout_r, UInt<32>("h0")) @[ALU.scala 84:18]
    node _shout_T_4 = eq(io_fn, UInt<4>("h1")) @[ALU.scala 85:25]
    node _shout_T_5 = mux(_shout_T_4, shout_l, UInt<32>("h0")) @[ALU.scala 85:18]
    node shout = or(_shout_T_3, _shout_T_5) @[ALU.scala 84:74]
    node _logic_T = eq(io_fn, UInt<4>("h4")) @[ALU.scala 88:25]
    node _logic_T_1 = eq(io_fn, UInt<4>("h6")) @[ALU.scala 88:45]
    node _logic_T_2 = or(_logic_T, _logic_T_1) @[ALU.scala 88:36]
    node _logic_T_3 = mux(_logic_T_2, in1_xor_in2, UInt<32>("h0")) @[ALU.scala 88:18]
    node _logic_T_5 = eq(io_fn, UInt<4>("h7")) @[ALU.scala 89:44]
    node _logic_T_6 = or(_logic_T_1, _logic_T_5) @[ALU.scala 89:35]
    node _logic_T_7 = and(io_in1, io_in2) @[ALU.scala 89:63]
    node _logic_T_8 = mux(_logic_T_6, _logic_T_7, UInt<32>("h0")) @[ALU.scala 89:18]
    node logic = or(_logic_T_3, _logic_T_8) @[ALU.scala 88:78]
    node _shift_logic_T = geq(io_fn, UInt<4>("hc")) @[ALU.scala 41:30]
    node _shift_logic_T_1 = and(_shift_logic_T, slt) @[ALU.scala 90:35]
    node _GEN_11 = pad(_shift_logic_T_1, 32) @[ALU.scala 90:43]
    node _shift_logic_T_2 = or(_GEN_11, logic) @[ALU.scala 90:43]
    node shift_logic = or(_shift_logic_T_2, shout) @[ALU.scala 90:51]
    node _out_T = eq(io_fn, UInt<4>("h0")) @[ALU.scala 91:23]
    node _out_T_1 = eq(io_fn, UInt<4>("ha")) @[ALU.scala 91:43]
    node _out_T_2 = or(_out_T, _out_T_1) @[ALU.scala 91:34]
    io_out <= mux(_out_T_2, io_adder_out, shift_logic) @[ALU.scala 91:16]
    io_adder_out <= tail(_io_adder_out_T_3, 1) @[ALU.scala 63:36]
    io_cmp_out <= xor(_io_cmp_out_T, _io_cmp_out_T_4) @[ALU.scala 69:36]

  module MulDiv :
    input clock : Clock
    input reset : UInt<1>
    output io_req_ready : UInt<1>
    input io_req_valid : UInt<1>
    input io_req_bits_fn : UInt<4>
    input io_req_bits_in1 : UInt<32>
    input io_req_bits_in2 : UInt<32>
    input io_req_bits_tag : UInt<5>
    input io_kill : UInt<1>
    input io_resp_ready : UInt<1>
    output io_resp_valid : UInt<1>
    output io_resp_bits_data : UInt<32>
    output io_resp_bits_tag : UInt<5>

    reg state : UInt<3>, clock with :
      reset => (UInt<1>("h0"), state) @[Multiplier.scala 52:22]
    reg req_tag : UInt<5>, clock with :
      reset => (UInt<1>("h0"), req_tag) @[Multiplier.scala 54:16]
    reg count : UInt<6>, clock with :
      reset => (UInt<1>("h0"), count) @[Multiplier.scala 55:18]
    reg neg_out : UInt<1>, clock with :
      reset => (UInt<1>("h0"), neg_out) @[Multiplier.scala 58:20]
    reg isHi : UInt<1>, clock with :
      reset => (UInt<1>("h0"), isHi) @[Multiplier.scala 59:17]
    reg resHi : UInt<1>, clock with :
      reset => (UInt<1>("h0"), resHi) @[Multiplier.scala 60:18]
    reg divisor : UInt<33>, clock with :
      reset => (UInt<1>("h0"), divisor) @[Multiplier.scala 61:20]
    reg remainder : UInt<66>, clock with :
      reset => (UInt<1>("h0"), remainder) @[Multiplier.scala 62:22]
    node _T = and(io_req_bits_fn, UInt<4>("h4")) @[Decode.scala 14:65]
    node cmdMul = eq(_T, UInt<4>("h0")) @[Decode.scala 14:121]
    node _T_3 = and(io_req_bits_fn, UInt<4>("h5")) @[Decode.scala 14:65]
    node _T_4 = eq(_T_3, UInt<4>("h1")) @[Decode.scala 14:121]
    node _T_5 = and(io_req_bits_fn, UInt<4>("h2")) @[Decode.scala 14:65]
    node _T_6 = eq(_T_5, UInt<4>("h2")) @[Decode.scala 14:121]
    node cmdHi = or(_T_4, _T_6) @[Decode.scala 15:30]
    node _T_9 = and(io_req_bits_fn, UInt<4>("h6")) @[Decode.scala 14:65]
    node _T_10 = eq(_T_9, UInt<4>("h0")) @[Decode.scala 14:121]
    node _T_11 = and(io_req_bits_fn, UInt<4>("h1")) @[Decode.scala 14:65]
    node _T_12 = eq(_T_11, UInt<4>("h0")) @[Decode.scala 14:121]
    node lhsSigned = or(_T_10, _T_12) @[Decode.scala 15:30]
    node _T_16 = eq(_T_3, UInt<4>("h4")) @[Decode.scala 14:121]
    node rhsSigned = or(_T_10, _T_16) @[Decode.scala 15:30]
    node _sign_T_1 = bits(io_req_bits_in1, 31, 31) @[Multiplier.scala 82:48]
    node lhs_sign = and(lhsSigned, _sign_T_1) @[Multiplier.scala 82:23]
    node hi = bits(io_req_bits_in1, 31, 16) @[Multiplier.scala 83:43]
    node lo = bits(io_req_bits_in1, 15, 0) @[Multiplier.scala 84:15]
    node lhs_in = cat(hi, lo) @[Cat.scala 30:58]
    node _sign_T_4 = bits(io_req_bits_in2, 31, 31) @[Multiplier.scala 82:48]
    node rhs_sign = and(rhsSigned, _sign_T_4) @[Multiplier.scala 82:23]
    node hi_1 = bits(io_req_bits_in2, 31, 16) @[Multiplier.scala 83:43]
    node lo_1 = bits(io_req_bits_in2, 15, 0) @[Multiplier.scala 84:15]
    node rhs_in = cat(hi_1, lo_1) @[Cat.scala 30:58]
    node _subtractor_T = bits(remainder, 64, 32) @[Multiplier.scala 89:29]
    node _subtractor_T_1 = sub(_subtractor_T, divisor) @[Multiplier.scala 89:37]
    node subtractor = tail(_subtractor_T_1, 1) @[Multiplier.scala 89:37]
    node _result_T = bits(remainder, 64, 33) @[Multiplier.scala 90:36]
    node _result_T_1 = bits(remainder, 31, 0) @[Multiplier.scala 90:57]
    node result = mux(resHi, _result_T, _result_T_1) @[Multiplier.scala 90:19]
    node _negated_remainder_T = sub(UInt<32>("h0"), result) @[Multiplier.scala 91:27]
    node negated_remainder = tail(_negated_remainder_T, 1) @[Multiplier.scala 91:27]
    node _T_23 = eq(state, UInt<3>("h1")) @[Multiplier.scala 93:39]
    node _T_24 = bits(remainder, 31, 31) @[Multiplier.scala 94:20]
    node _GEN_0 = mux(_T_24, pad(negated_remainder, 66), remainder) @[Multiplier.scala 94:27 95:17 62:22]
    node _T_25 = bits(divisor, 31, 31) @[Multiplier.scala 97:18]
    node _GEN_1 = mux(_T_25, subtractor, divisor) @[Multiplier.scala 97:25 98:15 61:20]
    node _GEN_2 = mux(_T_23, _GEN_0, remainder) @[Multiplier.scala 62:22 93:57]
    node _GEN_3 = mux(_T_23, _GEN_1, divisor) @[Multiplier.scala 61:20 93:57]
    node _GEN_4 = mux(_T_23, UInt<3>("h3"), state) @[Multiplier.scala 100:11 52:22 93:57]
    node _T_26 = eq(state, UInt<3>("h5")) @[Multiplier.scala 102:39]
    node _GEN_5 = mux(_T_26, pad(negated_remainder, 66), _GEN_2) @[Multiplier.scala 102:57 103:15]
    node _GEN_6 = mux(_T_26, UInt<3>("h7"), _GEN_4) @[Multiplier.scala 102:57 104:11]
    node _GEN_7 = mux(_T_26, UInt<1>("h0"), resHi) @[Multiplier.scala 102:57 105:11 60:18]
    node _T_27 = eq(state, UInt<3>("h2")) @[Multiplier.scala 107:39]
    node mulReg_hi = bits(remainder, 65, 33) @[Multiplier.scala 108:31]
    node mulReg = cat(mulReg_hi, _result_T_1) @[Cat.scala 30:58]
    node prod_hi = bits(remainder, 32, 32) @[Multiplier.scala 109:31]
    node mplier = bits(mulReg, 31, 0) @[Multiplier.scala 110:24]
    node _accum_T = bits(mulReg, 64, 32) @[Multiplier.scala 111:23]
    node accum = asSInt(_accum_T) @[Multiplier.scala 111:37]
    node mpcand = asSInt(divisor) @[Multiplier.scala 112:26]
    node prod_lo = bits(mplier, 7, 0) @[Multiplier.scala 113:38]
    node _prod_T = cat(prod_hi, prod_lo) @[Cat.scala 30:58]
    node _prod_T_1 = asSInt(_prod_T) @[Multiplier.scala 113:60]
    node _prod_T_2 = mul(_prod_T_1, mpcand) @[Multiplier.scala 113:67]
    node _GEN_35 = pad(accum, 42) @[Multiplier.scala 113:76]
    node _prod_T_3 = add(_prod_T_2, _GEN_35) @[Multiplier.scala 113:76]
    node _prod_T_4 = tail(_prod_T_3, 1) @[Multiplier.scala 113:76]
    node prod = asSInt(_prod_T_4) @[Multiplier.scala 113:76]
    node nextMulReg_lo = bits(mplier, 31, 8) @[Multiplier.scala 114:38]
    node nextMulReg_hi = asUInt(prod) @[Cat.scala 30:58]
    node nextMulReg = cat(nextMulReg_hi, nextMulReg_lo) @[Cat.scala 30:58]
    node _nextMplierSign_T = eq(count, UInt<6>("h2")) @[Multiplier.scala 115:32]
    node remainder_hi_lo = and(_nextMplierSign_T, neg_out) @[Multiplier.scala 115:57]
    node _eOut_T_4 = not(isHi) @[Multiplier.scala 119:7]
    node nextMulReg1_hi = bits(nextMulReg, 64, 32) @[Multiplier.scala 121:37]
    node nextMulReg1_lo = bits(nextMulReg, 31, 0) @[Multiplier.scala 121:82]
    node nextMulReg1 = cat(nextMulReg1_hi, nextMulReg1_lo) @[Cat.scala 30:58]
    node remainder_hi_hi = shr(nextMulReg1, 32) @[Multiplier.scala 122:34]
    node remainder_lo = bits(nextMulReg1, 31, 0) @[Multiplier.scala 122:67]
    node remainder_hi = cat(remainder_hi_hi, remainder_hi_lo) @[Cat.scala 30:58]
    node _remainder_T = cat(remainder_hi, remainder_lo) @[Cat.scala 30:58]
    node _count_T = add(count, UInt<6>("h1")) @[Multiplier.scala 124:20]
    node _count_T_1 = tail(_count_T, 1) @[Multiplier.scala 124:20]
    node _T_28 = eq(count, UInt<6>("h3")) @[Multiplier.scala 125:25]
    node _GEN_8 = mux(_T_28, UInt<3>("h6"), _GEN_6) @[Multiplier.scala 125:51 126:13]
    node _GEN_9 = mux(_T_28, isHi, _GEN_7) @[Multiplier.scala 125:51 127:13]
    node _GEN_10 = mux(_T_27, _remainder_T, _GEN_5) @[Multiplier.scala 107:50 122:15]
    node _GEN_11 = mux(_T_27, _count_T_1, count) @[Multiplier.scala 107:50 124:11 55:18]
    node _GEN_12 = mux(_T_27, _GEN_8, _GEN_6) @[Multiplier.scala 107:50]
    node _GEN_13 = mux(_T_27, _GEN_9, _GEN_7) @[Multiplier.scala 107:50]
    node _T_30 = eq(state, UInt<3>("h3")) @[Multiplier.scala 130:39]
    node unrolls_less = bits(subtractor, 32, 32) @[Multiplier.scala 134:28]
    node _unrolls_T = bits(remainder, 63, 32) @[Multiplier.scala 135:24]
    node _unrolls_T_1 = bits(subtractor, 31, 0) @[Multiplier.scala 135:45]
    node unrolls_hi_hi = mux(unrolls_less, _unrolls_T, _unrolls_T_1) @[Multiplier.scala 135:14]
    node unrolls_lo = not(unrolls_less) @[Multiplier.scala 135:67]
    node unrolls_hi = cat(unrolls_hi_hi, _result_T_1) @[Cat.scala 30:58]
    node unrolls_0 = cat(unrolls_hi, unrolls_lo) @[Cat.scala 30:58]
    node _T_31 = eq(count, UInt<6>("h20")) @[Multiplier.scala 139:17]
    node _state_T = mux(neg_out, UInt<3>("h5"), UInt<3>("h7")) @[Multiplier.scala 140:19]
    node _GEN_14 = mux(_T_31, _state_T, _GEN_12) @[Multiplier.scala 139:38 140:13]
    node _GEN_15 = mux(_T_31, isHi, _GEN_13) @[Multiplier.scala 139:38 141:13]
    node _divby0_T = eq(count, UInt<6>("h0")) @[Multiplier.scala 147:24]
    node divby0 = and(_divby0_T, unrolls_lo) @[Multiplier.scala 147:30]
    node _T_33 = and(divby0, _eOut_T_4) @[Multiplier.scala 160:18]
    node _GEN_16 = mux(_T_33, UInt<1>("h0"), neg_out) @[Multiplier.scala 160:{28,38} 58:20]
    node _GEN_17 = mux(_T_30, pad(unrolls_0, 66), _GEN_10) @[Multiplier.scala 130:50 138:15]
    node _GEN_18 = mux(_T_30, _GEN_14, _GEN_12) @[Multiplier.scala 130:50]
    node _GEN_19 = mux(_T_30, _GEN_15, _GEN_13) @[Multiplier.scala 130:50]
    node _GEN_20 = mux(_T_30, _count_T_1, _GEN_11) @[Multiplier.scala 130:50 145:11]
    node _GEN_21 = mux(_T_30, _GEN_16, neg_out) @[Multiplier.scala 130:50 58:20]
    node _T_34 = and(io_resp_ready, io_resp_valid) @[Decoupled.scala 40:37]
    node _T_35 = or(_T_34, io_kill) @[Multiplier.scala 162:24]
    node _GEN_22 = mux(_T_35, UInt<3>("h0"), _GEN_18) @[Multiplier.scala 162:36 163:11]
    node _T_36 = and(io_req_ready, io_req_valid) @[Decoupled.scala 40:37]
    node _state_T_1 = or(lhs_sign, rhs_sign) @[Multiplier.scala 166:46]
    node _state_T_2 = mux(_state_T_1, UInt<3>("h1"), UInt<3>("h3")) @[Multiplier.scala 166:36]
    node _state_T_3 = mux(cmdMul, UInt<3>("h2"), _state_T_2) @[Multiplier.scala 166:17]
    node _neg_out_T = neq(lhs_sign, rhs_sign) @[Multiplier.scala 170:46]
    node _neg_out_T_1 = mux(cmdHi, lhs_sign, _neg_out_T) @[Multiplier.scala 170:19]
    node _divisor_T = cat(rhs_sign, rhs_in) @[Cat.scala 30:58]
    node _GEN_23 = mux(_T_36, _state_T_3, _GEN_22) @[Multiplier.scala 165:24 166:11]
    node _loOut_T_4 = bits(result, 31, 16) @[Multiplier.scala 177:65]
    node loOut = bits(result, 15, 0) @[Multiplier.scala 177:82]
    node _io_resp_valid_T = eq(state, UInt<3>("h6")) @[Multiplier.scala 182:27]
    node _io_resp_valid_T_1 = eq(state, UInt<3>("h7")) @[Multiplier.scala 182:51]
    io_req_ready <= eq(state, UInt<3>("h0")) @[Multiplier.scala 183:25]
    io_resp_valid <= or(_io_resp_valid_T, _io_resp_valid_T_1) @[Multiplier.scala 182:42]
    io_resp_bits_data <= cat(_loOut_T_4, loOut) @[Cat.scala 30:58]
    io_resp_bits_tag <= req_tag @[Multiplier.scala 179:20]
    state <= mux(reset, UInt<3>("h0"), _GEN_23) @[Multiplier.scala 52:{22,22}]
    req_tag <= mux(_T_36, io_req_bits_tag, req_tag) @[Multiplier.scala 165:24 173:9 54:16]
    count <= mux(_T_36, UInt<6>("h0"), _GEN_20) @[Multiplier.scala 165:24 169:11]
    neg_out <= mux(_T_36, _neg_out_T_1, _GEN_21) @[Multiplier.scala 165:24 170:13]
    isHi <= mux(_T_36, cmdHi, isHi) @[Multiplier.scala 165:24 167:10 59:17]
    resHi <= mux(_T_36, UInt<1>("h0"), _GEN_19) @[Multiplier.scala 165:24 168:11]
    divisor <= mux(_T_36, _divisor_T, _GEN_3) @[Multiplier.scala 165:24 171:13]
    remainder <= mux(_T_36, pad(lhs_in, 66), _GEN_17) @[Multiplier.scala 165:24 172:15]

  module PMPChecker :
    input io_prv : UInt<2>
    input io_pmp_0_cfg_l : UInt<1>
    input io_pmp_0_cfg_a : UInt<2>
    input io_pmp_0_cfg_x : UInt<1>
    input io_pmp_0_cfg_w : UInt<1>
    input io_pmp_0_cfg_r : UInt<1>
    input io_pmp_0_addr : UInt<30>
    input io_pmp_0_mask : UInt<32>
    input io_pmp_1_cfg_l : UInt<1>
    input io_pmp_1_cfg_a : UInt<2>
    input io_pmp_1_cfg_x : UInt<1>
    input io_pmp_1_cfg_w : UInt<1>
    input io_pmp_1_cfg_r : UInt<1>
    input io_pmp_1_addr : UInt<30>
    input io_pmp_1_mask : UInt<32>
    input io_pmp_2_cfg_l : UInt<1>
    input io_pmp_2_cfg_a : UInt<2>
    input io_pmp_2_cfg_x : UInt<1>
    input io_pmp_2_cfg_w : UInt<1>
    input io_pmp_2_cfg_r : UInt<1>
    input io_pmp_2_addr : UInt<30>
    input io_pmp_2_mask : UInt<32>
    input io_pmp_3_cfg_l : UInt<1>
    input io_pmp_3_cfg_a : UInt<2>
    input io_pmp_3_cfg_x : UInt<1>
    input io_pmp_3_cfg_w : UInt<1>
    input io_pmp_3_cfg_r : UInt<1>
    input io_pmp_3_addr : UInt<30>
    input io_pmp_3_mask : UInt<32>
    input io_pmp_4_cfg_l : UInt<1>
    input io_pmp_4_cfg_a : UInt<2>
    input io_pmp_4_cfg_x : UInt<1>
    input io_pmp_4_cfg_w : UInt<1>
    input io_pmp_4_cfg_r : UInt<1>
    input io_pmp_4_addr : UInt<30>
    input io_pmp_4_mask : UInt<32>
    input io_pmp_5_cfg_l : UInt<1>
    input io_pmp_5_cfg_a : UInt<2>
    input io_pmp_5_cfg_x : UInt<1>
    input io_pmp_5_cfg_w : UInt<1>
    input io_pmp_5_cfg_r : UInt<1>
    input io_pmp_5_addr : UInt<30>
    input io_pmp_5_mask : UInt<32>
    input io_pmp_6_cfg_l : UInt<1>
    input io_pmp_6_cfg_a : UInt<2>
    input io_pmp_6_cfg_x : UInt<1>
    input io_pmp_6_cfg_w : UInt<1>
    input io_pmp_6_cfg_r : UInt<1>
    input io_pmp_6_addr : UInt<30>
    input io_pmp_6_mask : UInt<32>
    input io_pmp_7_cfg_l : UInt<1>
    input io_pmp_7_cfg_a : UInt<2>
    input io_pmp_7_cfg_x : UInt<1>
    input io_pmp_7_cfg_w : UInt<1>
    input io_pmp_7_cfg_r : UInt<1>
    input io_pmp_7_addr : UInt<30>
    input io_pmp_7_mask : UInt<32>
    input io_addr : UInt<32>
    output io_r : UInt<1>
    output io_w : UInt<1>
    output io_x : UInt<1>

    node default = gt(io_prv, UInt<2>("h1")) @[PMP.scala 157:56]
    node _res_hit_T = bits(io_pmp_7_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _res_hit_T_1 = shl(io_pmp_7_addr, 2) @[PMP.scala 62:36]
    node _res_hit_T_2 = not(_res_hit_T_1) @[PMP.scala 62:29]
    node _res_hit_T_3 = or(_res_hit_T_2, UInt<32>("h3")) @[PMP.scala 62:48]
    node _res_hit_T_4 = not(_res_hit_T_3) @[PMP.scala 62:27]
    node _res_hit_T_5 = xor(io_addr, _res_hit_T_4) @[PMP.scala 65:47]
    node _res_hit_T_6 = not(io_pmp_7_mask) @[PMP.scala 65:54]
    node _res_hit_T_7 = and(_res_hit_T_5, _res_hit_T_6) @[PMP.scala 65:52]
    node _res_hit_T_8 = eq(_res_hit_T_7, UInt<32>("h0")) @[PMP.scala 65:58]
    node _res_hit_T_9 = bits(io_pmp_7_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _res_hit_T_14 = shl(io_pmp_6_addr, 2) @[PMP.scala 62:36]
    node _res_hit_T_15 = not(_res_hit_T_14) @[PMP.scala 62:29]
    node _res_hit_T_16 = or(_res_hit_T_15, UInt<32>("h3")) @[PMP.scala 62:48]
    node _res_hit_T_17 = not(_res_hit_T_16) @[PMP.scala 62:27]
    node _res_hit_T_18 = lt(io_addr, _res_hit_T_17) @[PMP.scala 79:9]
    node _res_hit_T_19 = not(_res_hit_T_18) @[PMP.scala 90:5]
    node _res_hit_T_24 = lt(io_addr, _res_hit_T_4) @[PMP.scala 79:9]
    node _res_hit_T_25 = and(_res_hit_T_19, _res_hit_T_24) @[PMP.scala 96:48]
    node _res_hit_T_26 = and(_res_hit_T_9, _res_hit_T_25) @[PMP.scala 134:61]
    node res_hit = mux(_res_hit_T, _res_hit_T_8, _res_hit_T_26) @[PMP.scala 134:8]
    node _res_ignore_T = not(io_pmp_7_cfg_l) @[PMP.scala 165:29]
    node res_ignore = and(default, _res_ignore_T) @[PMP.scala 165:26]
    node res_cur_cfg_r = or(io_pmp_7_cfg_r, res_ignore) @[PMP.scala 183:40]
    node res_cur_cfg_w = or(io_pmp_7_cfg_w, res_ignore) @[PMP.scala 184:40]
    node res_cur_cfg_x = or(io_pmp_7_cfg_x, res_ignore) @[PMP.scala 185:40]
    node _res_T_44_cfg_x = mux(res_hit, res_cur_cfg_x, default) @[PMP.scala 186:8]
    node _res_T_44_cfg_w = mux(res_hit, res_cur_cfg_w, default) @[PMP.scala 186:8]
    node _res_T_44_cfg_r = mux(res_hit, res_cur_cfg_r, default) @[PMP.scala 186:8]
    node _res_hit_T_27 = bits(io_pmp_6_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _res_hit_T_32 = xor(io_addr, _res_hit_T_17) @[PMP.scala 65:47]
    node _res_hit_T_33 = not(io_pmp_6_mask) @[PMP.scala 65:54]
    node _res_hit_T_34 = and(_res_hit_T_32, _res_hit_T_33) @[PMP.scala 65:52]
    node _res_hit_T_35 = eq(_res_hit_T_34, UInt<32>("h0")) @[PMP.scala 65:58]
    node _res_hit_T_36 = bits(io_pmp_6_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _res_hit_T_41 = shl(io_pmp_5_addr, 2) @[PMP.scala 62:36]
    node _res_hit_T_42 = not(_res_hit_T_41) @[PMP.scala 62:29]
    node _res_hit_T_43 = or(_res_hit_T_42, UInt<32>("h3")) @[PMP.scala 62:48]
    node _res_hit_T_44 = not(_res_hit_T_43) @[PMP.scala 62:27]
    node _res_hit_T_45 = lt(io_addr, _res_hit_T_44) @[PMP.scala 79:9]
    node _res_hit_T_46 = not(_res_hit_T_45) @[PMP.scala 90:5]
    node _res_hit_T_52 = and(_res_hit_T_46, _res_hit_T_18) @[PMP.scala 96:48]
    node _res_hit_T_53 = and(_res_hit_T_36, _res_hit_T_52) @[PMP.scala 134:61]
    node res_hit_1 = mux(_res_hit_T_27, _res_hit_T_35, _res_hit_T_53) @[PMP.scala 134:8]
    node _res_ignore_T_1 = not(io_pmp_6_cfg_l) @[PMP.scala 165:29]
    node res_ignore_1 = and(default, _res_ignore_T_1) @[PMP.scala 165:26]
    node res_cur_1_cfg_r = or(io_pmp_6_cfg_r, res_ignore_1) @[PMP.scala 183:40]
    node res_cur_1_cfg_w = or(io_pmp_6_cfg_w, res_ignore_1) @[PMP.scala 184:40]
    node res_cur_1_cfg_x = or(io_pmp_6_cfg_x, res_ignore_1) @[PMP.scala 185:40]
    node _res_T_89_cfg_x = mux(res_hit_1, res_cur_1_cfg_x, _res_T_44_cfg_x) @[PMP.scala 186:8]
    node _res_T_89_cfg_w = mux(res_hit_1, res_cur_1_cfg_w, _res_T_44_cfg_w) @[PMP.scala 186:8]
    node _res_T_89_cfg_r = mux(res_hit_1, res_cur_1_cfg_r, _res_T_44_cfg_r) @[PMP.scala 186:8]
    node _res_hit_T_54 = bits(io_pmp_5_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _res_hit_T_59 = xor(io_addr, _res_hit_T_44) @[PMP.scala 65:47]
    node _res_hit_T_60 = not(io_pmp_5_mask) @[PMP.scala 65:54]
    node _res_hit_T_61 = and(_res_hit_T_59, _res_hit_T_60) @[PMP.scala 65:52]
    node _res_hit_T_62 = eq(_res_hit_T_61, UInt<32>("h0")) @[PMP.scala 65:58]
    node _res_hit_T_63 = bits(io_pmp_5_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _res_hit_T_68 = shl(io_pmp_4_addr, 2) @[PMP.scala 62:36]
    node _res_hit_T_69 = not(_res_hit_T_68) @[PMP.scala 62:29]
    node _res_hit_T_70 = or(_res_hit_T_69, UInt<32>("h3")) @[PMP.scala 62:48]
    node _res_hit_T_71 = not(_res_hit_T_70) @[PMP.scala 62:27]
    node _res_hit_T_72 = lt(io_addr, _res_hit_T_71) @[PMP.scala 79:9]
    node _res_hit_T_73 = not(_res_hit_T_72) @[PMP.scala 90:5]
    node _res_hit_T_79 = and(_res_hit_T_73, _res_hit_T_45) @[PMP.scala 96:48]
    node _res_hit_T_80 = and(_res_hit_T_63, _res_hit_T_79) @[PMP.scala 134:61]
    node res_hit_2 = mux(_res_hit_T_54, _res_hit_T_62, _res_hit_T_80) @[PMP.scala 134:8]
    node _res_ignore_T_2 = not(io_pmp_5_cfg_l) @[PMP.scala 165:29]
    node res_ignore_2 = and(default, _res_ignore_T_2) @[PMP.scala 165:26]
    node res_cur_2_cfg_r = or(io_pmp_5_cfg_r, res_ignore_2) @[PMP.scala 183:40]
    node res_cur_2_cfg_w = or(io_pmp_5_cfg_w, res_ignore_2) @[PMP.scala 184:40]
    node res_cur_2_cfg_x = or(io_pmp_5_cfg_x, res_ignore_2) @[PMP.scala 185:40]
    node _res_T_134_cfg_x = mux(res_hit_2, res_cur_2_cfg_x, _res_T_89_cfg_x) @[PMP.scala 186:8]
    node _res_T_134_cfg_w = mux(res_hit_2, res_cur_2_cfg_w, _res_T_89_cfg_w) @[PMP.scala 186:8]
    node _res_T_134_cfg_r = mux(res_hit_2, res_cur_2_cfg_r, _res_T_89_cfg_r) @[PMP.scala 186:8]
    node _res_hit_T_81 = bits(io_pmp_4_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _res_hit_T_86 = xor(io_addr, _res_hit_T_71) @[PMP.scala 65:47]
    node _res_hit_T_87 = not(io_pmp_4_mask) @[PMP.scala 65:54]
    node _res_hit_T_88 = and(_res_hit_T_86, _res_hit_T_87) @[PMP.scala 65:52]
    node _res_hit_T_89 = eq(_res_hit_T_88, UInt<32>("h0")) @[PMP.scala 65:58]
    node _res_hit_T_90 = bits(io_pmp_4_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _res_hit_T_95 = shl(io_pmp_3_addr, 2) @[PMP.scala 62:36]
    node _res_hit_T_96 = not(_res_hit_T_95) @[PMP.scala 62:29]
    node _res_hit_T_97 = or(_res_hit_T_96, UInt<32>("h3")) @[PMP.scala 62:48]
    node _res_hit_T_98 = not(_res_hit_T_97) @[PMP.scala 62:27]
    node _res_hit_T_99 = lt(io_addr, _res_hit_T_98) @[PMP.scala 79:9]
    node _res_hit_T_100 = not(_res_hit_T_99) @[PMP.scala 90:5]
    node _res_hit_T_106 = and(_res_hit_T_100, _res_hit_T_72) @[PMP.scala 96:48]
    node _res_hit_T_107 = and(_res_hit_T_90, _res_hit_T_106) @[PMP.scala 134:61]
    node res_hit_3 = mux(_res_hit_T_81, _res_hit_T_89, _res_hit_T_107) @[PMP.scala 134:8]
    node _res_ignore_T_3 = not(io_pmp_4_cfg_l) @[PMP.scala 165:29]
    node res_ignore_3 = and(default, _res_ignore_T_3) @[PMP.scala 165:26]
    node res_cur_3_cfg_r = or(io_pmp_4_cfg_r, res_ignore_3) @[PMP.scala 183:40]
    node res_cur_3_cfg_w = or(io_pmp_4_cfg_w, res_ignore_3) @[PMP.scala 184:40]
    node res_cur_3_cfg_x = or(io_pmp_4_cfg_x, res_ignore_3) @[PMP.scala 185:40]
    node _res_T_179_cfg_x = mux(res_hit_3, res_cur_3_cfg_x, _res_T_134_cfg_x) @[PMP.scala 186:8]
    node _res_T_179_cfg_w = mux(res_hit_3, res_cur_3_cfg_w, _res_T_134_cfg_w) @[PMP.scala 186:8]
    node _res_T_179_cfg_r = mux(res_hit_3, res_cur_3_cfg_r, _res_T_134_cfg_r) @[PMP.scala 186:8]
    node _res_hit_T_108 = bits(io_pmp_3_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _res_hit_T_113 = xor(io_addr, _res_hit_T_98) @[PMP.scala 65:47]
    node _res_hit_T_114 = not(io_pmp_3_mask) @[PMP.scala 65:54]
    node _res_hit_T_115 = and(_res_hit_T_113, _res_hit_T_114) @[PMP.scala 65:52]
    node _res_hit_T_116 = eq(_res_hit_T_115, UInt<32>("h0")) @[PMP.scala 65:58]
    node _res_hit_T_117 = bits(io_pmp_3_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _res_hit_T_122 = shl(io_pmp_2_addr, 2) @[PMP.scala 62:36]
    node _res_hit_T_123 = not(_res_hit_T_122) @[PMP.scala 62:29]
    node _res_hit_T_124 = or(_res_hit_T_123, UInt<32>("h3")) @[PMP.scala 62:48]
    node _res_hit_T_125 = not(_res_hit_T_124) @[PMP.scala 62:27]
    node _res_hit_T_126 = lt(io_addr, _res_hit_T_125) @[PMP.scala 79:9]
    node _res_hit_T_127 = not(_res_hit_T_126) @[PMP.scala 90:5]
    node _res_hit_T_133 = and(_res_hit_T_127, _res_hit_T_99) @[PMP.scala 96:48]
    node _res_hit_T_134 = and(_res_hit_T_117, _res_hit_T_133) @[PMP.scala 134:61]
    node res_hit_4 = mux(_res_hit_T_108, _res_hit_T_116, _res_hit_T_134) @[PMP.scala 134:8]
    node _res_ignore_T_4 = not(io_pmp_3_cfg_l) @[PMP.scala 165:29]
    node res_ignore_4 = and(default, _res_ignore_T_4) @[PMP.scala 165:26]
    node res_cur_4_cfg_r = or(io_pmp_3_cfg_r, res_ignore_4) @[PMP.scala 183:40]
    node res_cur_4_cfg_w = or(io_pmp_3_cfg_w, res_ignore_4) @[PMP.scala 184:40]
    node res_cur_4_cfg_x = or(io_pmp_3_cfg_x, res_ignore_4) @[PMP.scala 185:40]
    node _res_T_224_cfg_x = mux(res_hit_4, res_cur_4_cfg_x, _res_T_179_cfg_x) @[PMP.scala 186:8]
    node _res_T_224_cfg_w = mux(res_hit_4, res_cur_4_cfg_w, _res_T_179_cfg_w) @[PMP.scala 186:8]
    node _res_T_224_cfg_r = mux(res_hit_4, res_cur_4_cfg_r, _res_T_179_cfg_r) @[PMP.scala 186:8]
    node _res_hit_T_135 = bits(io_pmp_2_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _res_hit_T_140 = xor(io_addr, _res_hit_T_125) @[PMP.scala 65:47]
    node _res_hit_T_141 = not(io_pmp_2_mask) @[PMP.scala 65:54]
    node _res_hit_T_142 = and(_res_hit_T_140, _res_hit_T_141) @[PMP.scala 65:52]
    node _res_hit_T_143 = eq(_res_hit_T_142, UInt<32>("h0")) @[PMP.scala 65:58]
    node _res_hit_T_144 = bits(io_pmp_2_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _res_hit_T_149 = shl(io_pmp_1_addr, 2) @[PMP.scala 62:36]
    node _res_hit_T_150 = not(_res_hit_T_149) @[PMP.scala 62:29]
    node _res_hit_T_151 = or(_res_hit_T_150, UInt<32>("h3")) @[PMP.scala 62:48]
    node _res_hit_T_152 = not(_res_hit_T_151) @[PMP.scala 62:27]
    node _res_hit_T_153 = lt(io_addr, _res_hit_T_152) @[PMP.scala 79:9]
    node _res_hit_T_154 = not(_res_hit_T_153) @[PMP.scala 90:5]
    node _res_hit_T_160 = and(_res_hit_T_154, _res_hit_T_126) @[PMP.scala 96:48]
    node _res_hit_T_161 = and(_res_hit_T_144, _res_hit_T_160) @[PMP.scala 134:61]
    node res_hit_5 = mux(_res_hit_T_135, _res_hit_T_143, _res_hit_T_161) @[PMP.scala 134:8]
    node _res_ignore_T_5 = not(io_pmp_2_cfg_l) @[PMP.scala 165:29]
    node res_ignore_5 = and(default, _res_ignore_T_5) @[PMP.scala 165:26]
    node res_cur_5_cfg_r = or(io_pmp_2_cfg_r, res_ignore_5) @[PMP.scala 183:40]
    node res_cur_5_cfg_w = or(io_pmp_2_cfg_w, res_ignore_5) @[PMP.scala 184:40]
    node res_cur_5_cfg_x = or(io_pmp_2_cfg_x, res_ignore_5) @[PMP.scala 185:40]
    node _res_T_269_cfg_x = mux(res_hit_5, res_cur_5_cfg_x, _res_T_224_cfg_x) @[PMP.scala 186:8]
    node _res_T_269_cfg_w = mux(res_hit_5, res_cur_5_cfg_w, _res_T_224_cfg_w) @[PMP.scala 186:8]
    node _res_T_269_cfg_r = mux(res_hit_5, res_cur_5_cfg_r, _res_T_224_cfg_r) @[PMP.scala 186:8]
    node _res_hit_T_162 = bits(io_pmp_1_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _res_hit_T_167 = xor(io_addr, _res_hit_T_152) @[PMP.scala 65:47]
    node _res_hit_T_168 = not(io_pmp_1_mask) @[PMP.scala 65:54]
    node _res_hit_T_169 = and(_res_hit_T_167, _res_hit_T_168) @[PMP.scala 65:52]
    node _res_hit_T_170 = eq(_res_hit_T_169, UInt<32>("h0")) @[PMP.scala 65:58]
    node _res_hit_T_171 = bits(io_pmp_1_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _res_hit_T_176 = shl(io_pmp_0_addr, 2) @[PMP.scala 62:36]
    node _res_hit_T_177 = not(_res_hit_T_176) @[PMP.scala 62:29]
    node _res_hit_T_178 = or(_res_hit_T_177, UInt<32>("h3")) @[PMP.scala 62:48]
    node _res_hit_T_179 = not(_res_hit_T_178) @[PMP.scala 62:27]
    node _res_hit_T_180 = lt(io_addr, _res_hit_T_179) @[PMP.scala 79:9]
    node _res_hit_T_181 = not(_res_hit_T_180) @[PMP.scala 90:5]
    node _res_hit_T_187 = and(_res_hit_T_181, _res_hit_T_153) @[PMP.scala 96:48]
    node _res_hit_T_188 = and(_res_hit_T_171, _res_hit_T_187) @[PMP.scala 134:61]
    node res_hit_6 = mux(_res_hit_T_162, _res_hit_T_170, _res_hit_T_188) @[PMP.scala 134:8]
    node _res_ignore_T_6 = not(io_pmp_1_cfg_l) @[PMP.scala 165:29]
    node res_ignore_6 = and(default, _res_ignore_T_6) @[PMP.scala 165:26]
    node res_cur_6_cfg_r = or(io_pmp_1_cfg_r, res_ignore_6) @[PMP.scala 183:40]
    node res_cur_6_cfg_w = or(io_pmp_1_cfg_w, res_ignore_6) @[PMP.scala 184:40]
    node res_cur_6_cfg_x = or(io_pmp_1_cfg_x, res_ignore_6) @[PMP.scala 185:40]
    node _res_T_314_cfg_x = mux(res_hit_6, res_cur_6_cfg_x, _res_T_269_cfg_x) @[PMP.scala 186:8]
    node _res_T_314_cfg_w = mux(res_hit_6, res_cur_6_cfg_w, _res_T_269_cfg_w) @[PMP.scala 186:8]
    node _res_T_314_cfg_r = mux(res_hit_6, res_cur_6_cfg_r, _res_T_269_cfg_r) @[PMP.scala 186:8]
    node _res_hit_T_189 = bits(io_pmp_0_cfg_a, 1, 1) @[PMP.scala 47:20]
    node _res_hit_T_194 = xor(io_addr, _res_hit_T_179) @[PMP.scala 65:47]
    node _res_hit_T_195 = not(io_pmp_0_mask) @[PMP.scala 65:54]
    node _res_hit_T_196 = and(_res_hit_T_194, _res_hit_T_195) @[PMP.scala 65:52]
    node _res_hit_T_197 = eq(_res_hit_T_196, UInt<32>("h0")) @[PMP.scala 65:58]
    node _res_hit_T_198 = bits(io_pmp_0_cfg_a, 0, 0) @[PMP.scala 48:26]
    node _res_hit_T_215 = and(_res_hit_T_198, _res_hit_T_180) @[PMP.scala 134:61]
    node res_hit_7 = mux(_res_hit_T_189, _res_hit_T_197, _res_hit_T_215) @[PMP.scala 134:8]
    node _res_ignore_T_7 = not(io_pmp_0_cfg_l) @[PMP.scala 165:29]
    node res_ignore_7 = and(default, _res_ignore_T_7) @[PMP.scala 165:26]
    node res_cur_7_cfg_r = or(io_pmp_0_cfg_r, res_ignore_7) @[PMP.scala 183:40]
    node res_cur_7_cfg_w = or(io_pmp_0_cfg_w, res_ignore_7) @[PMP.scala 184:40]
    node res_cur_7_cfg_x = or(io_pmp_0_cfg_x, res_ignore_7) @[PMP.scala 185:40]
    io_r <= mux(res_hit_7, res_cur_7_cfg_r, _res_T_314_cfg_r) @[PMP.scala 186:8]
    io_w <= mux(res_hit_7, res_cur_7_cfg_w, _res_T_314_cfg_w) @[PMP.scala 186:8]
    io_x <= mux(res_hit_7, res_cur_7_cfg_x, _res_T_314_cfg_x) @[PMP.scala 186:8]

  module RVCExpander :
    input io_in : UInt<32>
    output io_out_bits : UInt<32>
    output io_out_rd : UInt<5>
    output io_out_rs1 : UInt<5>
    output io_out_rs2 : UInt<5>
    output io_rvc : UInt<1>

    node _io_rvc_T = bits(io_in, 1, 0) @[RVC.scala 163:20]
    node _io_out_s_opc_T = bits(io_in, 12, 5) @[RVC.scala 53:22]
    node _io_out_s_opc_T_1 = orr(_io_out_s_opc_T) @[RVC.scala 53:29]
    node io_out_s_lo_lo = mux(_io_out_s_opc_T_1, UInt<7>("h13"), UInt<7>("h1f")) @[RVC.scala 53:20]
    node io_out_s_hi_hi_hi = bits(io_in, 10, 7) @[RVC.scala 34:26]
    node io_out_s_hi_hi_lo = bits(io_in, 12, 11) @[RVC.scala 34:35]
    node io_out_s_hi_lo = bits(io_in, 5, 5) @[RVC.scala 34:45]
    node io_out_s_lo_hi = bits(io_in, 6, 6) @[RVC.scala 34:51]
    node io_out_s_lo = cat(io_out_s_lo_hi, UInt<2>("h0")) @[Cat.scala 30:58]
    node io_out_s_hi_hi = cat(io_out_s_hi_hi_hi, io_out_s_hi_hi_lo) @[Cat.scala 30:58]
    node io_out_s_hi = cat(io_out_s_hi_hi, io_out_s_hi_lo) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_1 = cat(io_out_s_hi, io_out_s_lo) @[Cat.scala 30:58]
    node io_out_s_lo_1 = bits(io_in, 4, 2) @[RVC.scala 31:29]
    node io_out_s_lo_hi_1 = cat(UInt<2>("h1"), io_out_s_lo_1) @[Cat.scala 30:58]
    node io_out_s_lo_2 = cat(io_out_s_lo_hi_1, io_out_s_lo_lo) @[Cat.scala 30:58]
    node io_out_s_hi_hi_1 = cat(io_out_s_hi_hi_hi_1, UInt<5>("h2")) @[Cat.scala 30:58]
    node io_out_s_hi_1 = cat(io_out_s_hi_hi_1, UInt<3>("h0")) @[Cat.scala 30:58]
    node _io_out_s_T = cat(io_out_s_hi_1, io_out_s_lo_2) @[Cat.scala 30:58]
    node io_out_s_hi_hi_2 = bits(io_in, 6, 5) @[RVC.scala 36:20]
    node io_out_s_hi_lo_1 = bits(io_in, 12, 10) @[RVC.scala 36:28]
    node io_out_s_hi_2 = cat(io_out_s_hi_hi_2, io_out_s_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_2 = cat(io_out_s_hi_2, UInt<3>("h0")) @[Cat.scala 30:58]
    node io_out_s_lo_5 = bits(io_in, 9, 7) @[RVC.scala 30:29]
    node io_out_s_hi_hi_lo_1 = cat(UInt<2>("h1"), io_out_s_lo_5) @[Cat.scala 30:58]
    node io_out_s_lo_7 = cat(io_out_s_lo_hi_1, UInt<7>("h7")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_3 = cat(io_out_s_hi_hi_hi_2, io_out_s_hi_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_3 = cat(io_out_s_hi_hi_3, UInt<3>("h3")) @[Cat.scala 30:58]
    node _io_out_s_T_4 = cat(io_out_s_hi_3, io_out_s_lo_7) @[Cat.scala 30:58]
    node io_out_s_hi_4 = cat(io_out_s_hi_lo, io_out_s_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_3 = cat(io_out_s_hi_4, io_out_s_lo) @[Cat.scala 30:58]
    node io_out_s_lo_14 = cat(io_out_s_lo_hi_1, UInt<7>("h3")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_5 = cat(io_out_s_hi_hi_hi_3, io_out_s_hi_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_5 = cat(io_out_s_hi_hi_5, UInt<3>("h2")) @[Cat.scala 30:58]
    node _io_out_s_T_9 = cat(io_out_s_hi_5, io_out_s_lo_14) @[Cat.scala 30:58]
    node _io_out_s_T_14 = cat(io_out_s_hi_5, io_out_s_lo_7) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_5 = shr(io_out_s_hi_hi_hi_3, 5) @[RVC.scala 63:32]
    node io_out_s_lo_hi_lo = bits(io_out_s_hi_hi_hi_3, 4, 0) @[RVC.scala 63:65]
    node io_out_s_lo_hi_9 = cat(UInt<3>("h2"), io_out_s_lo_hi_lo) @[Cat.scala 30:58]
    node io_out_s_lo_29 = cat(io_out_s_lo_hi_9, UInt<7>("h3f")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_10 = cat(io_out_s_hi_hi_hi_5, io_out_s_lo_hi_1) @[Cat.scala 30:58]
    node io_out_s_hi_10 = cat(io_out_s_hi_hi_10, io_out_s_hi_hi_lo_1) @[Cat.scala 30:58]
    node _io_out_s_T_21 = cat(io_out_s_hi_10, io_out_s_lo_29) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_6 = shr(io_out_s_hi_hi_hi_2, 5) @[RVC.scala 66:30]
    node io_out_s_lo_hi_lo_1 = bits(io_out_s_hi_hi_hi_2, 4, 0) @[RVC.scala 66:63]
    node io_out_s_lo_hi_10 = cat(UInt<3>("h3"), io_out_s_lo_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_lo_35 = cat(io_out_s_lo_hi_10, UInt<7>("h27")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_13 = cat(io_out_s_hi_hi_hi_6, io_out_s_lo_hi_1) @[Cat.scala 30:58]
    node io_out_s_hi_13 = cat(io_out_s_hi_hi_13, io_out_s_hi_hi_lo_1) @[Cat.scala 30:58]
    node _io_out_s_T_28 = cat(io_out_s_hi_13, io_out_s_lo_35) @[Cat.scala 30:58]
    node io_out_s_lo_43 = cat(io_out_s_lo_hi_9, UInt<7>("h23")) @[Cat.scala 30:58]
    node _io_out_s_T_35 = cat(io_out_s_hi_10, io_out_s_lo_43) @[Cat.scala 30:58]
    node io_out_s_lo_51 = cat(io_out_s_lo_hi_9, UInt<7>("h27")) @[Cat.scala 30:58]
    node _io_out_s_T_42 = cat(io_out_s_hi_10, io_out_s_lo_51) @[Cat.scala 30:58]
    node _io_out_s_T_47 = bits(io_in, 12, 12) @[RVC.scala 43:30]
    node io_out_s_hi_20 = mux(_io_out_s_T_47, UInt<7>("h7f"), UInt<7>("h0")) @[Bitwise.scala 72:12]
    node io_out_s_lo_55 = bits(io_in, 6, 2) @[RVC.scala 43:38]
    node io_out_s_hi_hi_hi_9 = cat(io_out_s_hi_20, io_out_s_lo_55) @[Cat.scala 30:58]
    node io_out_s_hi_hi_lo_8 = bits(io_in, 11, 7) @[RVC.scala 33:13]
    node io_out_s_lo_56 = cat(io_out_s_hi_hi_lo_8, UInt<7>("h13")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_20 = cat(io_out_s_hi_hi_hi_9, io_out_s_hi_hi_lo_8) @[Cat.scala 30:58]
    node io_out_s_hi_21 = cat(io_out_s_hi_hi_20, UInt<3>("h0")) @[Cat.scala 30:58]
    node io_out_s_8_bits = cat(io_out_s_hi_21, io_out_s_lo_56) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_hi = mux(_io_out_s_T_47, UInt<10>("h3ff"), UInt<10>("h0")) @[Bitwise.scala 72:12]
    node io_out_s_hi_hi_hi_lo = bits(io_in, 8, 8) @[RVC.scala 44:36]
    node io_out_s_hi_hi_lo_9 = bits(io_in, 10, 9) @[RVC.scala 44:42]
    node io_out_s_hi_lo_lo = bits(io_in, 7, 7) @[RVC.scala 44:57]
    node io_out_s_lo_hi_hi = bits(io_in, 2, 2) @[RVC.scala 44:63]
    node io_out_s_lo_hi_lo_4 = bits(io_in, 11, 11) @[RVC.scala 44:69]
    node io_out_s_lo_lo_hi = bits(io_in, 5, 3) @[RVC.scala 44:76]
    node io_out_s_lo_lo_1 = cat(io_out_s_lo_lo_hi, UInt<1>("h0")) @[Cat.scala 30:58]
    node io_out_s_lo_hi_18 = cat(io_out_s_lo_hi_hi, io_out_s_lo_hi_lo_4) @[Cat.scala 30:58]
    node io_out_s_lo_58 = cat(io_out_s_lo_hi_18, io_out_s_lo_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_lo_16 = cat(io_out_s_lo_hi, io_out_s_hi_lo_lo) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_10 = cat(io_out_s_hi_hi_hi_hi, io_out_s_hi_hi_hi_lo) @[Cat.scala 30:58]
    node io_out_s_hi_hi_21 = cat(io_out_s_hi_hi_hi_10, io_out_s_hi_hi_lo_9) @[Cat.scala 30:58]
    node io_out_s_hi_22 = cat(io_out_s_hi_hi_21, io_out_s_hi_lo_16) @[Cat.scala 30:58]
    node _io_out_s_T_56 = cat(io_out_s_hi_22, io_out_s_lo_58) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_11 = bits(_io_out_s_T_56, 20, 20) @[RVC.scala 81:36]
    node io_out_s_hi_hi_lo_11 = bits(_io_out_s_T_56, 10, 1) @[RVC.scala 81:46]
    node io_out_s_hi_lo_19 = bits(_io_out_s_T_56, 11, 11) @[RVC.scala 81:58]
    node io_out_s_lo_hi_hi_4 = bits(_io_out_s_T_56, 19, 12) @[RVC.scala 81:68]
    node io_out_s_lo_hi_22 = cat(io_out_s_lo_hi_hi_4, UInt<5>("h1")) @[Cat.scala 30:58]
    node io_out_s_lo_62 = cat(io_out_s_lo_hi_22, UInt<7>("h6f")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_25 = cat(io_out_s_hi_hi_hi_11, io_out_s_hi_hi_lo_11) @[Cat.scala 30:58]
    node io_out_s_hi_26 = cat(io_out_s_hi_hi_25, io_out_s_hi_lo_19) @[Cat.scala 30:58]
    node io_out_s_9_bits = cat(io_out_s_hi_26, io_out_s_lo_62) @[Cat.scala 30:58]
    node io_out_s_hi_hi_26 = cat(io_out_s_hi_hi_hi_9, UInt<5>("h0")) @[Cat.scala 30:58]
    node io_out_s_hi_28 = cat(io_out_s_hi_hi_26, UInt<3>("h0")) @[Cat.scala 30:58]
    node io_out_s_10_bits = cat(io_out_s_hi_28, io_out_s_lo_56) @[Cat.scala 30:58]
    node _io_out_s_opc_T_5 = orr(io_out_s_hi_hi_hi_9) @[RVC.scala 90:29]
    node io_out_s_me_lo = mux(_io_out_s_opc_T_5, UInt<7>("h37"), UInt<7>("h3f")) @[RVC.scala 90:20]
    node io_out_s_me_hi_hi = mux(_io_out_s_T_47, UInt<15>("h7fff"), UInt<15>("h0")) @[Bitwise.scala 72:12]
    node io_out_s_me_hi = cat(io_out_s_me_hi_hi, io_out_s_lo_55) @[Cat.scala 30:58]
    node _io_out_s_me_T_2 = cat(io_out_s_me_hi, UInt<12>("h0")) @[Cat.scala 30:58]
    node io_out_s_me_hi_hi_1 = bits(_io_out_s_me_T_2, 31, 12) @[RVC.scala 91:31]
    node io_out_s_me_hi_1 = cat(io_out_s_me_hi_hi_1, io_out_s_hi_hi_lo_8) @[Cat.scala 30:58]
    node io_out_s_me_bits = cat(io_out_s_me_hi_1, io_out_s_me_lo) @[Cat.scala 30:58]
    node _io_out_s_T_77 = eq(io_out_s_hi_hi_lo_8, UInt<5>("h0")) @[RVC.scala 92:14]
    node _io_out_s_T_79 = eq(io_out_s_hi_hi_lo_8, UInt<5>("h2")) @[RVC.scala 92:27]
    node _io_out_s_T_80 = or(_io_out_s_T_77, _io_out_s_T_79) @[RVC.scala 92:21]
    node io_out_s_lo_lo_5 = mux(_io_out_s_opc_T_5, UInt<7>("h13"), UInt<7>("h1f")) @[RVC.scala 86:20]
    node io_out_s_hi_hi_hi_16 = mux(_io_out_s_T_47, UInt<3>("h7"), UInt<3>("h0")) @[Bitwise.scala 72:12]
    node io_out_s_hi_hi_lo_14 = bits(io_in, 4, 3) @[RVC.scala 42:42]
    node io_out_s_lo_hi_24 = cat(io_out_s_lo_hi_hi, io_out_s_lo_hi) @[Cat.scala 30:58]
    node io_out_s_lo_67 = cat(io_out_s_lo_hi_24, UInt<4>("h0")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_27 = cat(io_out_s_hi_hi_hi_16, io_out_s_hi_hi_lo_14) @[Cat.scala 30:58]
    node io_out_s_hi_29 = cat(io_out_s_hi_hi_27, io_out_s_hi_lo) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_17 = cat(io_out_s_hi_29, io_out_s_lo_67) @[Cat.scala 30:58]
    node io_out_s_lo_68 = cat(io_out_s_hi_hi_lo_8, io_out_s_lo_lo_5) @[Cat.scala 30:58]
    node io_out_s_hi_hi_28 = cat(io_out_s_hi_hi_hi_17, io_out_s_hi_hi_lo_8) @[Cat.scala 30:58]
    node io_out_s_hi_30 = cat(io_out_s_hi_hi_28, UInt<3>("h0")) @[Cat.scala 30:58]
    node io_out_s_res_bits = cat(io_out_s_hi_30, io_out_s_lo_68) @[Cat.scala 30:58]
    node io_out_s_11_bits = mux(_io_out_s_T_80, io_out_s_res_bits, io_out_s_me_bits) @[RVC.scala 92:10]
    node io_out_s_hi_hi_hi_18 = cat(_io_out_s_T_47, io_out_s_lo_55) @[Cat.scala 30:58]
    node io_out_s_lo_73 = cat(io_out_s_hi_hi_lo_1, UInt<7>("h13")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_29 = cat(io_out_s_hi_hi_hi_18, io_out_s_hi_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_32 = cat(io_out_s_hi_hi_29, UInt<3>("h5")) @[Cat.scala 30:58]
    node _io_out_s_T_88 = cat(io_out_s_hi_32, io_out_s_lo_73) @[Cat.scala 30:58]
    node _GEN_0 = pad(_io_out_s_T_88, 31) @[RVC.scala 99:23]
    node _io_out_s_T_90 = or(_GEN_0, UInt<31>("h40000000")) @[RVC.scala 99:23]
    node io_out_s_hi_hi_31 = cat(io_out_s_hi_hi_hi_9, io_out_s_hi_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_36 = cat(io_out_s_hi_hi_31, UInt<3>("h7")) @[Cat.scala 30:58]
    node _io_out_s_T_93 = cat(io_out_s_hi_36, io_out_s_lo_73) @[Cat.scala 30:58]
    node _io_out_s_funct_T = cat(_io_out_s_T_47, io_out_s_hi_hi_2) @[Cat.scala 30:58]
    node _io_out_s_funct_T_1 = eq(_io_out_s_funct_T, UInt<3>("h1")) @[package.scala 32:86]
    node _io_out_s_funct_T_2 = mux(_io_out_s_funct_T_1, UInt<3>("h4"), UInt<3>("h0")) @[package.scala 32:76]
    node _io_out_s_funct_T_3 = eq(_io_out_s_funct_T, UInt<3>("h2")) @[package.scala 32:86]
    node _io_out_s_funct_T_4 = mux(_io_out_s_funct_T_3, UInt<3>("h6"), _io_out_s_funct_T_2) @[package.scala 32:76]
    node _io_out_s_funct_T_5 = eq(_io_out_s_funct_T, UInt<3>("h3")) @[package.scala 32:86]
    node _io_out_s_funct_T_6 = mux(_io_out_s_funct_T_5, UInt<3>("h7"), _io_out_s_funct_T_4) @[package.scala 32:76]
    node _io_out_s_funct_T_7 = eq(_io_out_s_funct_T, UInt<3>("h4")) @[package.scala 32:86]
    node _io_out_s_funct_T_8 = mux(_io_out_s_funct_T_7, UInt<3>("h0"), _io_out_s_funct_T_6) @[package.scala 32:76]
    node _io_out_s_funct_T_9 = eq(_io_out_s_funct_T, UInt<3>("h5")) @[package.scala 32:86]
    node _io_out_s_funct_T_10 = mux(_io_out_s_funct_T_9, UInt<3>("h0"), _io_out_s_funct_T_8) @[package.scala 32:76]
    node _io_out_s_funct_T_11 = eq(_io_out_s_funct_T, UInt<3>("h6")) @[package.scala 32:86]
    node _io_out_s_funct_T_12 = mux(_io_out_s_funct_T_11, UInt<3>("h2"), _io_out_s_funct_T_10) @[package.scala 32:76]
    node _io_out_s_funct_T_13 = eq(_io_out_s_funct_T, UInt<3>("h7")) @[package.scala 32:86]
    node io_out_s_hi_lo_22 = mux(_io_out_s_funct_T_13, UInt<3>("h3"), _io_out_s_funct_T_12) @[package.scala 32:76]
    node _io_out_s_sub_T_1 = eq(io_out_s_hi_hi_2, UInt<2>("h0")) @[RVC.scala 103:30]
    node io_out_s_sub = mux(_io_out_s_sub_T_1, UInt<31>("h40000000"), UInt<31>("h0")) @[RVC.scala 103:22]
    node io_out_s_lo_lo_6 = mux(_io_out_s_T_47, UInt<7>("h3b"), UInt<7>("h33")) @[RVC.scala 104:22]
    node io_out_s_lo_85 = cat(io_out_s_hi_hi_lo_1, io_out_s_lo_lo_6) @[Cat.scala 30:58]
    node io_out_s_hi_hi_32 = cat(io_out_s_lo_hi_1, io_out_s_hi_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_37 = cat(io_out_s_hi_hi_32, io_out_s_hi_lo_22) @[Cat.scala 30:58]
    node _io_out_s_T_94 = cat(io_out_s_hi_37, io_out_s_lo_85) @[Cat.scala 30:58]
    node _GEN_1 = pad(_io_out_s_T_94, 31) @[RVC.scala 105:43]
    node _io_out_s_T_95 = or(_GEN_1, io_out_s_sub) @[RVC.scala 105:43]
    node _io_out_s_T_96 = bits(io_in, 11, 10) @[RVC.scala 107:42]
    node _io_out_s_T_97 = eq(_io_out_s_T_96, UInt<2>("h1")) @[package.scala 32:86]
    node _io_out_s_T_98 = mux(_io_out_s_T_97, _io_out_s_T_90, pad(_io_out_s_T_88, 31)) @[package.scala 32:76]
    node _io_out_s_T_99 = eq(_io_out_s_T_96, UInt<2>("h2")) @[package.scala 32:86]
    node _io_out_s_T_100 = mux(_io_out_s_T_99, _io_out_s_T_93, pad(_io_out_s_T_98, 32)) @[package.scala 32:76]
    node _io_out_s_T_101 = eq(_io_out_s_T_96, UInt<2>("h3")) @[package.scala 32:86]
    node io_out_s_12_bits = mux(_io_out_s_T_101, pad(_io_out_s_T_95, 32), _io_out_s_T_100) @[package.scala 32:76]
    node io_out_s_lo_hi_34 = cat(io_out_s_lo_hi_hi_4, UInt<5>("h0")) @[Cat.scala 30:58]
    node io_out_s_lo_93 = cat(io_out_s_lo_hi_34, UInt<7>("h6f")) @[Cat.scala 30:58]
    node io_out_s_13_bits = cat(io_out_s_hi_26, io_out_s_lo_93) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_27 = mux(_io_out_s_T_47, UInt<5>("h1f"), UInt<5>("h0")) @[Bitwise.scala 72:12]
    node io_out_s_lo_hi_35 = cat(_io_out_s_T_96, io_out_s_hi_hi_lo_14) @[Cat.scala 30:58]
    node io_out_s_lo_96 = cat(io_out_s_lo_hi_35, UInt<1>("h0")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_38 = cat(io_out_s_hi_hi_hi_27, io_out_s_hi_hi_2) @[Cat.scala 30:58]
    node io_out_s_hi_43 = cat(io_out_s_hi_hi_38, io_out_s_lo_hi_hi) @[Cat.scala 30:58]
    node _io_out_s_T_125 = cat(io_out_s_hi_43, io_out_s_lo_96) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_28 = bits(_io_out_s_T_125, 12, 12) @[RVC.scala 95:29]
    node io_out_s_hi_hi_lo_27 = bits(_io_out_s_T_125, 10, 5) @[RVC.scala 95:39]
    node io_out_s_lo_hi_lo_16 = bits(_io_out_s_T_125, 4, 1) @[RVC.scala 95:71]
    node io_out_s_lo_lo_hi_8 = bits(_io_out_s_T_125, 11, 11) @[RVC.scala 95:82]
    node io_out_s_lo_lo_11 = cat(io_out_s_lo_lo_hi_8, UInt<7>("h63")) @[Cat.scala 30:58]
    node io_out_s_lo_hi_39 = cat(UInt<3>("h0"), io_out_s_lo_hi_lo_16) @[Cat.scala 30:58]
    node io_out_s_lo_101 = cat(io_out_s_lo_hi_39, io_out_s_lo_lo_11) @[Cat.scala 30:58]
    node io_out_s_hi_lo_32 = cat(UInt<5>("h0"), io_out_s_hi_hi_lo_1) @[Cat.scala 30:58]
    node io_out_s_hi_hi_42 = cat(io_out_s_hi_hi_hi_28, io_out_s_hi_hi_lo_27) @[Cat.scala 30:58]
    node io_out_s_hi_47 = cat(io_out_s_hi_hi_42, io_out_s_hi_lo_32) @[Cat.scala 30:58]
    node io_out_s_14_bits = cat(io_out_s_hi_47, io_out_s_lo_101) @[Cat.scala 30:58]
    node io_out_s_lo_hi_44 = cat(UInt<3>("h1"), io_out_s_lo_hi_lo_16) @[Cat.scala 30:58]
    node io_out_s_lo_109 = cat(io_out_s_lo_hi_44, io_out_s_lo_lo_11) @[Cat.scala 30:58]
    node io_out_s_15_bits = cat(io_out_s_hi_47, io_out_s_lo_109) @[Cat.scala 30:58]
    node _io_out_s_load_opc_T_1 = orr(io_out_s_hi_hi_lo_8) @[RVC.scala 113:27]
    node io_out_s_lo_lo_13 = mux(_io_out_s_load_opc_T_1, UInt<7>("h3"), UInt<7>("h1f")) @[RVC.scala 113:23]
    node io_out_s_hi_hi_48 = cat(io_out_s_hi_hi_hi_18, io_out_s_hi_hi_lo_8) @[Cat.scala 30:58]
    node io_out_s_hi_54 = cat(io_out_s_hi_hi_48, UInt<3>("h1")) @[Cat.scala 30:58]
    node _io_out_s_T_154 = cat(io_out_s_hi_54, io_out_s_lo_56) @[Cat.scala 30:58]
    node io_out_s_lo_113 = cat(io_out_s_hi_hi_2, UInt<3>("h0")) @[Cat.scala 30:58]
    node io_out_s_hi_55 = cat(io_out_s_lo_1, _io_out_s_T_47) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_38 = cat(io_out_s_hi_55, io_out_s_lo_113) @[Cat.scala 30:58]
    node io_out_s_lo_114 = cat(io_out_s_hi_hi_lo_8, UInt<7>("h7")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_50 = cat(io_out_s_hi_hi_hi_38, UInt<5>("h2")) @[Cat.scala 30:58]
    node io_out_s_hi_56 = cat(io_out_s_hi_hi_50, UInt<3>("h3")) @[Cat.scala 30:58]
    node _io_out_s_T_159 = cat(io_out_s_hi_56, io_out_s_lo_114) @[Cat.scala 30:58]
    node io_out_s_hi_hi_51 = bits(io_in, 3, 2) @[RVC.scala 37:22]
    node io_out_s_lo_hi_48 = bits(io_in, 6, 4) @[RVC.scala 37:37]
    node io_out_s_lo_115 = cat(io_out_s_lo_hi_48, UInt<2>("h0")) @[Cat.scala 30:58]
    node io_out_s_hi_57 = cat(io_out_s_hi_hi_51, _io_out_s_T_47) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_39 = cat(io_out_s_hi_57, io_out_s_lo_115) @[Cat.scala 30:58]
    node io_out_s_lo_116 = cat(io_out_s_hi_hi_lo_8, io_out_s_lo_lo_13) @[Cat.scala 30:58]
    node io_out_s_hi_hi_52 = cat(io_out_s_hi_hi_hi_39, UInt<5>("h2")) @[Cat.scala 30:58]
    node io_out_s_hi_58 = cat(io_out_s_hi_hi_52, UInt<3>("h2")) @[Cat.scala 30:58]
    node _io_out_s_T_163 = cat(io_out_s_hi_58, io_out_s_lo_116) @[Cat.scala 30:58]
    node _io_out_s_T_167 = cat(io_out_s_hi_58, io_out_s_lo_114) @[Cat.scala 30:58]
    node io_out_s_mv_lo = cat(io_out_s_hi_hi_lo_8, UInt<7>("h33")) @[Cat.scala 30:58]
    node io_out_s_mv_hi_hi = cat(io_out_s_lo_55, UInt<5>("h0")) @[Cat.scala 30:58]
    node io_out_s_mv_hi = cat(io_out_s_mv_hi_hi, UInt<3>("h0")) @[Cat.scala 30:58]
    node _io_out_s_mv_T = cat(io_out_s_mv_hi, io_out_s_mv_lo) @[Cat.scala 30:58]
    node io_out_s_add_hi_hi = cat(io_out_s_lo_55, io_out_s_hi_hi_lo_8) @[Cat.scala 30:58]
    node io_out_s_add_hi = cat(io_out_s_add_hi_hi, UInt<3>("h0")) @[Cat.scala 30:58]
    node _io_out_s_add_T = cat(io_out_s_add_hi, io_out_s_mv_lo) @[Cat.scala 30:58]
    node io_out_s_jr = cat(io_out_s_add_hi, UInt<12>("h67")) @[Cat.scala 30:58]
    node io_out_s_reserved_hi = shr(io_out_s_jr, 7) @[RVC.scala 133:29]
    node io_out_s_reserved = cat(io_out_s_reserved_hi, UInt<7>("h1f")) @[Cat.scala 30:58]
    node _io_out_s_jr_reserved_T_2 = mux(_io_out_s_load_opc_T_1, io_out_s_jr, io_out_s_reserved) @[RVC.scala 134:33]
    node _io_out_s_jr_mv_T_1 = orr(io_out_s_lo_55) @[RVC.scala 135:27]
    node io_out_s_mv_bits = pad(_io_out_s_mv_T, 32) @[RVC.scala 21:19 22:14]
    node io_out_s_jr_reserved_bits = pad(_io_out_s_jr_reserved_T_2, 32) @[RVC.scala 21:19 22:14]
    node io_out_s_jr_mv_bits = mux(_io_out_s_jr_mv_T_1, io_out_s_mv_bits, io_out_s_jr_reserved_bits) @[RVC.scala 135:22]
    node io_out_s_jr_mv_rd = mux(_io_out_s_jr_mv_T_1, io_out_s_hi_hi_lo_8, UInt<5>("h0")) @[RVC.scala 135:22]
    node io_out_s_jr_mv_rs1 = mux(_io_out_s_jr_mv_T_1, UInt<5>("h0"), io_out_s_hi_hi_lo_8) @[RVC.scala 135:22]
    node io_out_s_jalr = cat(io_out_s_add_hi, UInt<12>("he7")) @[Cat.scala 30:58]
    node _io_out_s_ebreak_T = cat(io_out_s_reserved_hi, UInt<7>("h73")) @[Cat.scala 30:58]
    node io_out_s_ebreak = or(_io_out_s_ebreak_T, UInt<25>("h100000")) @[RVC.scala 137:46]
    node _io_out_s_jalr_ebreak_T_2 = mux(_io_out_s_load_opc_T_1, io_out_s_jalr, io_out_s_ebreak) @[RVC.scala 138:33]
    node io_out_s_add_bits = pad(_io_out_s_add_T, 32) @[RVC.scala 21:19 22:14]
    node io_out_s_jalr_ebreak_bits = pad(_io_out_s_jalr_ebreak_T_2, 32) @[RVC.scala 21:19 22:14]
    node io_out_s_jalr_add_bits = mux(_io_out_s_jr_mv_T_1, io_out_s_add_bits, io_out_s_jalr_ebreak_bits) @[RVC.scala 139:25]
    node io_out_s_jalr_add_rd = mux(_io_out_s_jr_mv_T_1, io_out_s_hi_hi_lo_8, UInt<5>("h1")) @[RVC.scala 139:25]
    node io_out_s_20_bits = mux(_io_out_s_T_47, io_out_s_jalr_add_bits, io_out_s_jr_mv_bits) @[RVC.scala 140:10]
    node io_out_s_20_rd = mux(_io_out_s_T_47, io_out_s_jalr_add_rd, io_out_s_jr_mv_rd) @[RVC.scala 140:10]
    node io_out_s_20_rs1 = mux(_io_out_s_T_47, io_out_s_hi_hi_lo_8, io_out_s_jr_mv_rs1) @[RVC.scala 140:10]
    node io_out_s_hi_61 = cat(io_out_s_lo_5, io_out_s_hi_lo_1) @[Cat.scala 30:58]
    node _io_out_s_T_172 = cat(io_out_s_hi_61, UInt<3>("h0")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_41 = shr(_io_out_s_T_172, 5) @[RVC.scala 124:34]
    node io_out_s_lo_hi_lo_23 = bits(_io_out_s_T_172, 4, 0) @[RVC.scala 124:66]
    node io_out_s_lo_hi_52 = cat(UInt<3>("h3"), io_out_s_lo_hi_lo_23) @[Cat.scala 30:58]
    node io_out_s_lo_119 = cat(io_out_s_lo_hi_52, UInt<7>("h27")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_57 = cat(io_out_s_hi_hi_hi_41, io_out_s_lo_55) @[Cat.scala 30:58]
    node io_out_s_hi_63 = cat(io_out_s_hi_hi_57, UInt<5>("h2")) @[Cat.scala 30:58]
    node _io_out_s_T_174 = cat(io_out_s_hi_63, io_out_s_lo_119) @[Cat.scala 30:58]
    node io_out_s_hi_hi_58 = bits(io_in, 8, 7) @[RVC.scala 39:22]
    node io_out_s_hi_lo_43 = bits(io_in, 12, 9) @[RVC.scala 39:30]
    node io_out_s_hi_64 = cat(io_out_s_hi_hi_58, io_out_s_hi_lo_43) @[Cat.scala 30:58]
    node _io_out_s_T_178 = cat(io_out_s_hi_64, UInt<2>("h0")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_hi_42 = shr(_io_out_s_T_178, 5) @[RVC.scala 123:33]
    node io_out_s_lo_hi_lo_24 = bits(_io_out_s_T_178, 4, 0) @[RVC.scala 123:65]
    node io_out_s_lo_hi_53 = cat(UInt<3>("h2"), io_out_s_lo_hi_lo_24) @[Cat.scala 30:58]
    node io_out_s_lo_120 = cat(io_out_s_lo_hi_53, UInt<7>("h23")) @[Cat.scala 30:58]
    node io_out_s_hi_hi_60 = cat(io_out_s_hi_hi_hi_42, io_out_s_lo_55) @[Cat.scala 30:58]
    node io_out_s_hi_66 = cat(io_out_s_hi_hi_60, UInt<5>("h2")) @[Cat.scala 30:58]
    node _io_out_s_T_180 = cat(io_out_s_hi_66, io_out_s_lo_120) @[Cat.scala 30:58]
    node io_out_s_lo_121 = cat(io_out_s_lo_hi_53, UInt<7>("h27")) @[Cat.scala 30:58]
    node _io_out_s_T_186 = cat(io_out_s_hi_66, io_out_s_lo_121) @[Cat.scala 30:58]
    node io_out_s_24_rs1 = bits(io_in, 19, 15) @[RVC.scala 20:57]
    node io_out_s_24_rs2 = bits(io_in, 24, 20) @[RVC.scala 20:79]
    node io_out_lo = bits(io_in, 15, 13) @[RVC.scala 151:20]
    node _io_out_T = cat(_io_rvc_T, io_out_lo) @[Cat.scala 30:58]
    node _io_out_T_1 = eq(_io_out_T, UInt<5>("h1")) @[package.scala 32:86]
    node io_out_s_1_bits = pad(_io_out_s_T_4, 32) @[RVC.scala 21:19 22:14]
    node io_out_s_0_bits = pad(_io_out_s_T, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_2_bits = mux(_io_out_T_1, io_out_s_1_bits, io_out_s_0_bits) @[package.scala 32:76]
    node _io_out_T_2_rs1 = mux(_io_out_T_1, io_out_s_hi_hi_lo_1, UInt<5>("h2")) @[package.scala 32:76]
    node _io_out_T_3 = eq(_io_out_T, UInt<5>("h2")) @[package.scala 32:86]
    node io_out_s_2_bits = pad(_io_out_s_T_9, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_4_bits = mux(_io_out_T_3, io_out_s_2_bits, _io_out_T_2_bits) @[package.scala 32:76]
    node _io_out_T_4_rs1 = mux(_io_out_T_3, io_out_s_hi_hi_lo_1, _io_out_T_2_rs1) @[package.scala 32:76]
    node _io_out_T_5 = eq(_io_out_T, UInt<5>("h3")) @[package.scala 32:86]
    node io_out_s_3_bits = pad(_io_out_s_T_14, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_6_bits = mux(_io_out_T_5, io_out_s_3_bits, _io_out_T_4_bits) @[package.scala 32:76]
    node _io_out_T_6_rs1 = mux(_io_out_T_5, io_out_s_hi_hi_lo_1, _io_out_T_4_rs1) @[package.scala 32:76]
    node _io_out_T_7 = eq(_io_out_T, UInt<5>("h4")) @[package.scala 32:86]
    node io_out_s_4_bits = pad(_io_out_s_T_21, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_8_bits = mux(_io_out_T_7, io_out_s_4_bits, _io_out_T_6_bits) @[package.scala 32:76]
    node _io_out_T_8_rs1 = mux(_io_out_T_7, io_out_s_hi_hi_lo_1, _io_out_T_6_rs1) @[package.scala 32:76]
    node _io_out_T_9 = eq(_io_out_T, UInt<5>("h5")) @[package.scala 32:86]
    node io_out_s_5_bits = pad(_io_out_s_T_28, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_10_bits = mux(_io_out_T_9, io_out_s_5_bits, _io_out_T_8_bits) @[package.scala 32:76]
    node _io_out_T_10_rs1 = mux(_io_out_T_9, io_out_s_hi_hi_lo_1, _io_out_T_8_rs1) @[package.scala 32:76]
    node _io_out_T_11 = eq(_io_out_T, UInt<5>("h6")) @[package.scala 32:86]
    node io_out_s_6_bits = pad(_io_out_s_T_35, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_12_bits = mux(_io_out_T_11, io_out_s_6_bits, _io_out_T_10_bits) @[package.scala 32:76]
    node _io_out_T_12_rs1 = mux(_io_out_T_11, io_out_s_hi_hi_lo_1, _io_out_T_10_rs1) @[package.scala 32:76]
    node _io_out_T_13 = eq(_io_out_T, UInt<5>("h7")) @[package.scala 32:86]
    node io_out_s_7_bits = pad(_io_out_s_T_42, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_14_bits = mux(_io_out_T_13, io_out_s_7_bits, _io_out_T_12_bits) @[package.scala 32:76]
    node _io_out_T_14_rs1 = mux(_io_out_T_13, io_out_s_hi_hi_lo_1, _io_out_T_12_rs1) @[package.scala 32:76]
    node _io_out_T_15 = eq(_io_out_T, UInt<5>("h8")) @[package.scala 32:86]
    node _io_out_T_16_bits = mux(_io_out_T_15, io_out_s_8_bits, _io_out_T_14_bits) @[package.scala 32:76]
    node _io_out_T_16_rd = mux(_io_out_T_15, io_out_s_hi_hi_lo_8, io_out_s_lo_hi_1) @[package.scala 32:76]
    node _io_out_T_16_rs1 = mux(_io_out_T_15, io_out_s_hi_hi_lo_8, _io_out_T_14_rs1) @[package.scala 32:76]
    node _io_out_T_17 = eq(_io_out_T, UInt<5>("h9")) @[package.scala 32:86]
    node _io_out_T_18_bits = mux(_io_out_T_17, io_out_s_9_bits, _io_out_T_16_bits) @[package.scala 32:76]
    node _io_out_T_18_rd = mux(_io_out_T_17, UInt<5>("h1"), _io_out_T_16_rd) @[package.scala 32:76]
    node _io_out_T_18_rs1 = mux(_io_out_T_17, io_out_s_hi_hi_lo_8, _io_out_T_16_rs1) @[package.scala 32:76]
    node _io_out_T_19 = eq(_io_out_T, UInt<5>("ha")) @[package.scala 32:86]
    node _io_out_T_20_bits = mux(_io_out_T_19, io_out_s_10_bits, _io_out_T_18_bits) @[package.scala 32:76]
    node _io_out_T_20_rd = mux(_io_out_T_19, io_out_s_hi_hi_lo_8, _io_out_T_18_rd) @[package.scala 32:76]
    node _io_out_T_20_rs1 = mux(_io_out_T_19, UInt<5>("h0"), _io_out_T_18_rs1) @[package.scala 32:76]
    node _io_out_T_21 = eq(_io_out_T, UInt<5>("hb")) @[package.scala 32:86]
    node _io_out_T_22_bits = mux(_io_out_T_21, io_out_s_11_bits, _io_out_T_20_bits) @[package.scala 32:76]
    node _io_out_T_22_rd = mux(_io_out_T_21, io_out_s_hi_hi_lo_8, _io_out_T_20_rd) @[package.scala 32:76]
    node _io_out_T_22_rs1 = mux(_io_out_T_21, io_out_s_hi_hi_lo_8, _io_out_T_20_rs1) @[package.scala 32:76]
    node _io_out_T_23 = eq(_io_out_T, UInt<5>("hc")) @[package.scala 32:86]
    node _io_out_T_24_bits = mux(_io_out_T_23, io_out_s_12_bits, _io_out_T_22_bits) @[package.scala 32:76]
    node _io_out_T_24_rd = mux(_io_out_T_23, io_out_s_hi_hi_lo_1, _io_out_T_22_rd) @[package.scala 32:76]
    node _io_out_T_24_rs1 = mux(_io_out_T_23, io_out_s_hi_hi_lo_1, _io_out_T_22_rs1) @[package.scala 32:76]
    node _io_out_T_25 = eq(_io_out_T, UInt<5>("hd")) @[package.scala 32:86]
    node _io_out_T_26_bits = mux(_io_out_T_25, io_out_s_13_bits, _io_out_T_24_bits) @[package.scala 32:76]
    node _io_out_T_26_rd = mux(_io_out_T_25, UInt<5>("h0"), _io_out_T_24_rd) @[package.scala 32:76]
    node _io_out_T_26_rs1 = mux(_io_out_T_25, io_out_s_hi_hi_lo_1, _io_out_T_24_rs1) @[package.scala 32:76]
    node _io_out_T_27 = eq(_io_out_T, UInt<5>("he")) @[package.scala 32:86]
    node _io_out_T_28_bits = mux(_io_out_T_27, io_out_s_14_bits, _io_out_T_26_bits) @[package.scala 32:76]
    node _io_out_T_28_rd = mux(_io_out_T_27, io_out_s_hi_hi_lo_1, _io_out_T_26_rd) @[package.scala 32:76]
    node _io_out_T_28_rs1 = mux(_io_out_T_27, io_out_s_hi_hi_lo_1, _io_out_T_26_rs1) @[package.scala 32:76]
    node _io_out_T_28_rs2 = mux(_io_out_T_27, UInt<5>("h0"), io_out_s_lo_hi_1) @[package.scala 32:76]
    node _io_out_T_29 = eq(_io_out_T, UInt<5>("hf")) @[package.scala 32:86]
    node _io_out_T_30_bits = mux(_io_out_T_29, io_out_s_15_bits, _io_out_T_28_bits) @[package.scala 32:76]
    node _io_out_T_30_rd = mux(_io_out_T_29, UInt<5>("h0"), _io_out_T_28_rd) @[package.scala 32:76]
    node _io_out_T_30_rs1 = mux(_io_out_T_29, io_out_s_hi_hi_lo_1, _io_out_T_28_rs1) @[package.scala 32:76]
    node _io_out_T_30_rs2 = mux(_io_out_T_29, UInt<5>("h0"), _io_out_T_28_rs2) @[package.scala 32:76]
    node _io_out_T_31 = eq(_io_out_T, UInt<5>("h10")) @[package.scala 32:86]
    node io_out_s_16_bits = pad(_io_out_s_T_154, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_32_bits = mux(_io_out_T_31, io_out_s_16_bits, _io_out_T_30_bits) @[package.scala 32:76]
    node _io_out_T_32_rd = mux(_io_out_T_31, io_out_s_hi_hi_lo_8, _io_out_T_30_rd) @[package.scala 32:76]
    node _io_out_T_32_rs1 = mux(_io_out_T_31, io_out_s_hi_hi_lo_8, _io_out_T_30_rs1) @[package.scala 32:76]
    node _io_out_T_32_rs2 = mux(_io_out_T_31, io_out_s_lo_55, _io_out_T_30_rs2) @[package.scala 32:76]
    node _io_out_T_33 = eq(_io_out_T, UInt<5>("h11")) @[package.scala 32:86]
    node io_out_s_17_bits = pad(_io_out_s_T_159, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_34_bits = mux(_io_out_T_33, io_out_s_17_bits, _io_out_T_32_bits) @[package.scala 32:76]
    node _io_out_T_34_rd = mux(_io_out_T_33, io_out_s_hi_hi_lo_8, _io_out_T_32_rd) @[package.scala 32:76]
    node _io_out_T_34_rs1 = mux(_io_out_T_33, UInt<5>("h2"), _io_out_T_32_rs1) @[package.scala 32:76]
    node _io_out_T_34_rs2 = mux(_io_out_T_33, io_out_s_lo_55, _io_out_T_32_rs2) @[package.scala 32:76]
    node _io_out_T_35 = eq(_io_out_T, UInt<5>("h12")) @[package.scala 32:86]
    node io_out_s_18_bits = pad(_io_out_s_T_163, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_36_bits = mux(_io_out_T_35, io_out_s_18_bits, _io_out_T_34_bits) @[package.scala 32:76]
    node _io_out_T_36_rd = mux(_io_out_T_35, io_out_s_hi_hi_lo_8, _io_out_T_34_rd) @[package.scala 32:76]
    node _io_out_T_36_rs1 = mux(_io_out_T_35, UInt<5>("h2"), _io_out_T_34_rs1) @[package.scala 32:76]
    node _io_out_T_36_rs2 = mux(_io_out_T_35, io_out_s_lo_55, _io_out_T_34_rs2) @[package.scala 32:76]
    node _io_out_T_37 = eq(_io_out_T, UInt<5>("h13")) @[package.scala 32:86]
    node io_out_s_19_bits = pad(_io_out_s_T_167, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_38_bits = mux(_io_out_T_37, io_out_s_19_bits, _io_out_T_36_bits) @[package.scala 32:76]
    node _io_out_T_38_rd = mux(_io_out_T_37, io_out_s_hi_hi_lo_8, _io_out_T_36_rd) @[package.scala 32:76]
    node _io_out_T_38_rs1 = mux(_io_out_T_37, UInt<5>("h2"), _io_out_T_36_rs1) @[package.scala 32:76]
    node _io_out_T_38_rs2 = mux(_io_out_T_37, io_out_s_lo_55, _io_out_T_36_rs2) @[package.scala 32:76]
    node _io_out_T_39 = eq(_io_out_T, UInt<5>("h14")) @[package.scala 32:86]
    node _io_out_T_40_bits = mux(_io_out_T_39, io_out_s_20_bits, _io_out_T_38_bits) @[package.scala 32:76]
    node _io_out_T_40_rd = mux(_io_out_T_39, io_out_s_20_rd, _io_out_T_38_rd) @[package.scala 32:76]
    node _io_out_T_40_rs1 = mux(_io_out_T_39, io_out_s_20_rs1, _io_out_T_38_rs1) @[package.scala 32:76]
    node _io_out_T_40_rs2 = mux(_io_out_T_39, io_out_s_lo_55, _io_out_T_38_rs2) @[package.scala 32:76]
    node _io_out_T_41 = eq(_io_out_T, UInt<5>("h15")) @[package.scala 32:86]
    node io_out_s_21_bits = pad(_io_out_s_T_174, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_42_bits = mux(_io_out_T_41, io_out_s_21_bits, _io_out_T_40_bits) @[package.scala 32:76]
    node _io_out_T_42_rd = mux(_io_out_T_41, io_out_s_hi_hi_lo_8, _io_out_T_40_rd) @[package.scala 32:76]
    node _io_out_T_42_rs1 = mux(_io_out_T_41, UInt<5>("h2"), _io_out_T_40_rs1) @[package.scala 32:76]
    node _io_out_T_42_rs2 = mux(_io_out_T_41, io_out_s_lo_55, _io_out_T_40_rs2) @[package.scala 32:76]
    node _io_out_T_43 = eq(_io_out_T, UInt<5>("h16")) @[package.scala 32:86]
    node io_out_s_22_bits = pad(_io_out_s_T_180, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_44_bits = mux(_io_out_T_43, io_out_s_22_bits, _io_out_T_42_bits) @[package.scala 32:76]
    node _io_out_T_44_rd = mux(_io_out_T_43, io_out_s_hi_hi_lo_8, _io_out_T_42_rd) @[package.scala 32:76]
    node _io_out_T_44_rs1 = mux(_io_out_T_43, UInt<5>("h2"), _io_out_T_42_rs1) @[package.scala 32:76]
    node _io_out_T_44_rs2 = mux(_io_out_T_43, io_out_s_lo_55, _io_out_T_42_rs2) @[package.scala 32:76]
    node _io_out_T_45 = eq(_io_out_T, UInt<5>("h17")) @[package.scala 32:86]
    node io_out_s_23_bits = pad(_io_out_s_T_186, 32) @[RVC.scala 21:19 22:14]
    node _io_out_T_46_bits = mux(_io_out_T_45, io_out_s_23_bits, _io_out_T_44_bits) @[package.scala 32:76]
    node _io_out_T_46_rd = mux(_io_out_T_45, io_out_s_hi_hi_lo_8, _io_out_T_44_rd) @[package.scala 32:76]
    node _io_out_T_46_rs1 = mux(_io_out_T_45, UInt<5>("h2"), _io_out_T_44_rs1) @[package.scala 32:76]
    node _io_out_T_46_rs2 = mux(_io_out_T_45, io_out_s_lo_55, _io_out_T_44_rs2) @[package.scala 32:76]
    node _io_out_T_47 = eq(_io_out_T, UInt<5>("h18")) @[package.scala 32:86]
    node _io_out_T_48_bits = mux(_io_out_T_47, io_in, _io_out_T_46_bits) @[package.scala 32:76]
    node _io_out_T_48_rd = mux(_io_out_T_47, io_out_s_hi_hi_lo_8, _io_out_T_46_rd) @[package.scala 32:76]
    node _io_out_T_48_rs1 = mux(_io_out_T_47, io_out_s_24_rs1, _io_out_T_46_rs1) @[package.scala 32:76]
    node _io_out_T_48_rs2 = mux(_io_out_T_47, io_out_s_24_rs2, _io_out_T_46_rs2) @[package.scala 32:76]
    node _io_out_T_49 = eq(_io_out_T, UInt<5>("h19")) @[package.scala 32:86]
    node _io_out_T_50_bits = mux(_io_out_T_49, io_in, _io_out_T_48_bits) @[package.scala 32:76]
    node _io_out_T_50_rd = mux(_io_out_T_49, io_out_s_hi_hi_lo_8, _io_out_T_48_rd) @[package.scala 32:76]
    node _io_out_T_50_rs1 = mux(_io_out_T_49, io_out_s_24_rs1, _io_out_T_48_rs1) @[package.scala 32:76]
    node _io_out_T_50_rs2 = mux(_io_out_T_49, io_out_s_24_rs2, _io_out_T_48_rs2) @[package.scala 32:76]
    node _io_out_T_51 = eq(_io_out_T, UInt<5>("h1a")) @[package.scala 32:86]
    node _io_out_T_52_bits = mux(_io_out_T_51, io_in, _io_out_T_50_bits) @[package.scala 32:76]
    node _io_out_T_52_rd = mux(_io_out_T_51, io_out_s_hi_hi_lo_8, _io_out_T_50_rd) @[package.scala 32:76]
    node _io_out_T_52_rs1 = mux(_io_out_T_51, io_out_s_24_rs1, _io_out_T_50_rs1) @[package.scala 32:76]
    node _io_out_T_52_rs2 = mux(_io_out_T_51, io_out_s_24_rs2, _io_out_T_50_rs2) @[package.scala 32:76]
    node _io_out_T_53 = eq(_io_out_T, UInt<5>("h1b")) @[package.scala 32:86]
    node _io_out_T_54_bits = mux(_io_out_T_53, io_in, _io_out_T_52_bits) @[package.scala 32:76]
    node _io_out_T_54_rd = mux(_io_out_T_53, io_out_s_hi_hi_lo_8, _io_out_T_52_rd) @[package.scala 32:76]
    node _io_out_T_54_rs1 = mux(_io_out_T_53, io_out_s_24_rs1, _io_out_T_52_rs1) @[package.scala 32:76]
    node _io_out_T_54_rs2 = mux(_io_out_T_53, io_out_s_24_rs2, _io_out_T_52_rs2) @[package.scala 32:76]
    node _io_out_T_55 = eq(_io_out_T, UInt<5>("h1c")) @[package.scala 32:86]
    node _io_out_T_56_bits = mux(_io_out_T_55, io_in, _io_out_T_54_bits) @[package.scala 32:76]
    node _io_out_T_56_rd = mux(_io_out_T_55, io_out_s_hi_hi_lo_8, _io_out_T_54_rd) @[package.scala 32:76]
    node _io_out_T_56_rs1 = mux(_io_out_T_55, io_out_s_24_rs1, _io_out_T_54_rs1) @[package.scala 32:76]
    node _io_out_T_56_rs2 = mux(_io_out_T_55, io_out_s_24_rs2, _io_out_T_54_rs2) @[package.scala 32:76]
    node _io_out_T_57 = eq(_io_out_T, UInt<5>("h1d")) @[package.scala 32:86]
    node _io_out_T_58_bits = mux(_io_out_T_57, io_in, _io_out_T_56_bits) @[package.scala 32:76]
    node _io_out_T_58_rd = mux(_io_out_T_57, io_out_s_hi_hi_lo_8, _io_out_T_56_rd) @[package.scala 32:76]
    node _io_out_T_58_rs1 = mux(_io_out_T_57, io_out_s_24_rs1, _io_out_T_56_rs1) @[package.scala 32:76]
    node _io_out_T_58_rs2 = mux(_io_out_T_57, io_out_s_24_rs2, _io_out_T_56_rs2) @[package.scala 32:76]
    node _io_out_T_59 = eq(_io_out_T, UInt<5>("h1e")) @[package.scala 32:86]
    node _io_out_T_60_bits = mux(_io_out_T_59, io_in, _io_out_T_58_bits) @[package.scala 32:76]
    node _io_out_T_60_rd = mux(_io_out_T_59, io_out_s_hi_hi_lo_8, _io_out_T_58_rd) @[package.scala 32:76]
    node _io_out_T_60_rs1 = mux(_io_out_T_59, io_out_s_24_rs1, _io_out_T_58_rs1) @[package.scala 32:76]
    node _io_out_T_60_rs2 = mux(_io_out_T_59, io_out_s_24_rs2, _io_out_T_58_rs2) @[package.scala 32:76]
    node _io_out_T_61 = eq(_io_out_T, UInt<5>("h1f")) @[package.scala 32:86]
    io_out_bits <= mux(_io_out_T_61, io_in, _io_out_T_60_bits) @[package.scala 32:76]
    io_out_rd <= mux(_io_out_T_61, io_out_s_hi_hi_lo_8, _io_out_T_60_rd) @[package.scala 32:76]
    io_out_rs1 <= mux(_io_out_T_61, io_out_s_24_rs1, _io_out_T_60_rs1) @[package.scala 32:76]
    io_out_rs2 <= mux(_io_out_T_61, io_out_s_24_rs2, _io_out_T_60_rs2) @[package.scala 32:76]
    io_rvc <= neq(_io_rvc_T, UInt<2>("h3")) @[RVC.scala 163:26]
