circuit ServTopWithRam :
  module State :
    input clock : Clock
    input reset : UInt<1>
    input io_csr_newIrq : UInt<1>
    output io_csr_trapTaken : UInt<1>
    output io_csr_pendingIrq : UInt<1>
    input io_dbus_ack : UInt<1>
    output io_dbus_cyc : UInt<1>
    input io_ibus_ack : UInt<1>
    output io_ram_writeRequest : UInt<1>
    output io_ram_readRequest : UInt<1>
    input io_ram_ready : UInt<1>
    output io_rf_writeEnable : UInt<1>
    input io_decode_takeBranch : UInt<1>
    input io_decode_eOp : UInt<1>
    input io_decode_eBreak : UInt<1>
    input io_decode_branchOp : UInt<1>
    input io_decode_memOp : UInt<1>
    input io_decode_shiftOp : UInt<1>
    input io_decode_sltOp : UInt<1>
    input io_decode_rdOp : UInt<1>
    output io_count_enabled : UInt<1>
    output io_count_init : UInt<1>
    output io_count_count0 : UInt<1>
    output io_count_count0To3 : UInt<1>
    output io_count_count12To31 : UInt<1>
    output io_count_count1 : UInt<1>
    output io_count_count2 : UInt<1>
    output io_count_count3 : UInt<1>
    output io_count_count7 : UInt<1>
    output io_count_done : UInt<1>
    output io_bufreg_hold : UInt<1>
    output io_control_pcEnable : UInt<1>
    output io_control_jump : UInt<1>
    output io_control_trap : UInt<1>
    output io_alu_shiftAmountEnable : UInt<1>
    input io_alu_shiftDone : UInt<1>
    output io_mem_byteCount : UInt<2>
    input io_mem_misaligned : UInt<1>
    input io_lsb1 : UInt<1>

    reg init : UInt<1>, clock with :
      reset => (UInt<1>("h0"), init) @[State.scala 17:17]
    reg countDone : UInt<1>, clock with :
      reset => (UInt<1>("h0"), countDone) @[State.scala 21:22]
    reg countEnabled : UInt<1>, clock with :
      reset => (UInt<1>("h0"), countEnabled) @[State.scala 23:25]
    node _GEN_0 = mux(io_ram_ready, UInt<1>("h1"), countEnabled) @[State.scala 25:22 23:25 25:37]
    node _GEN_1 = mux(countDone, UInt<1>("h0"), _GEN_0) @[State.scala 26:{19,34}]
    reg count : UInt<3>, clock with :
      reset => (UInt<1>("h0"), count) @[State.scala 28:22]
    reg countR : UInt<4>, clock with :
      reset => (UInt<1>("h0"), countR) @[State.scala 29:23]
    node _count_T = bits(countR, 3, 3) @[State.scala 31:26]
    node _count_T_1 = add(count, _count_T) @[State.scala 31:18]
    node _count_T_2 = tail(_count_T_1, 1) @[State.scala 31:18]
    node _countR_T = bits(countR, 2, 0) @[State.scala 33:40]
    node _countR_T_1 = bits(countR, 3, 3) @[State.scala 33:55]
    node _countR_T_2 = cat(_countR_T, _countR_T_1) @[State.scala 33:46]
    node _GEN_2 = mux(countEnabled, _countR_T_2, countR) @[State.scala 33:22 29:23 33:31]
    node _countDone_T = eq(count, UInt<3>("h7")) @[State.scala 34:23]
    node _countDone_T_1 = bits(countR, 2, 2) @[State.scala 34:41]
    node _countDone_T_2 = and(_countDone_T, _countDone_T_1) @[State.scala 34:32]
    node _stageTwoRequest_T = and(countDone, init) @[State.scala 37:43]
    reg stageTwoRequest : UInt<1>, clock with :
      reset => (UInt<1>("h0"), stageTwoRequest) @[State.scala 37:32]
    node _io_control_pcEnable_T = eq(init, UInt<1>("h0")) @[State.scala 40:41]
    node _io_control_pcEnable_T_1 = and(countEnabled, _io_control_pcEnable_T) @[State.scala 40:39]
    node _io_mem_byteCount_T = bits(count, 2, 1) @[State.scala 42:28]
    node _io_count_count0To3_T = eq(count, UInt<1>("h0")) @[State.scala 44:31]
    node _io_count_count12To31_T = bits(count, 2, 2) @[State.scala 45:32]
    node _io_count_count12To31_T_1 = bits(count, 1, 0) @[State.scala 45:45]
    node _io_count_count12To31_T_2 = eq(_io_count_count12To31_T_1, UInt<2>("h3")) @[State.scala 45:51]
    node _io_count_count12To31_T_3 = or(_io_count_count12To31_T, _io_count_count12To31_T_2) @[State.scala 45:36]
    node _io_count_count0_T = eq(count, UInt<1>("h0")) @[State.scala 46:29]
    node _io_count_count0_T_1 = bits(countR, 0, 0) @[State.scala 46:47]
    node _io_count_count0_T_2 = and(_io_count_count0_T, _io_count_count0_T_1) @[State.scala 46:38]
    node _io_count_count1_T = eq(count, UInt<1>("h0")) @[State.scala 47:29]
    node _io_count_count1_T_1 = bits(countR, 1, 1) @[State.scala 47:47]
    node _io_count_count1_T_2 = and(_io_count_count1_T, _io_count_count1_T_1) @[State.scala 47:38]
    node _io_count_count2_T = eq(count, UInt<1>("h0")) @[State.scala 48:29]
    node _io_count_count2_T_1 = bits(countR, 2, 2) @[State.scala 48:47]
    node _io_count_count2_T_2 = and(_io_count_count2_T, _io_count_count2_T_1) @[State.scala 48:38]
    node _io_count_count3_T = eq(count, UInt<1>("h0")) @[State.scala 49:29]
    node _io_count_count3_T_1 = bits(countR, 3, 3) @[State.scala 49:47]
    node _io_count_count3_T_2 = and(_io_count_count3_T, _io_count_count3_T_1) @[State.scala 49:38]
    node _count4_T = eq(count, UInt<1>("h1")) @[State.scala 50:23]
    node _count4_T_1 = bits(countR, 0, 0) @[State.scala 50:41]
    node count4 = and(_count4_T, _count4_T_1) @[State.scala 50:32]
    node _io_count_count7_T = eq(count, UInt<1>("h1")) @[State.scala 51:29]
    node _io_count_count7_T_1 = bits(countR, 3, 3) @[State.scala 51:47]
    node _io_count_count7_T_2 = and(_io_count_count7_T, _io_count_count7_T_1) @[State.scala 51:38]
    node _io_alu_shiftAmountEnable_T = or(io_count_count0To3, count4) @[State.scala 53:51]
    node _io_alu_shiftAmountEnable_T_1 = and(_io_alu_shiftAmountEnable_T, init) @[State.scala 53:62]
    node _twoStageOp_T = or(io_decode_sltOp, io_decode_memOp) @[State.scala 56:36]
    node _twoStageOp_T_1 = or(_twoStageOp_T, io_decode_branchOp) @[State.scala 56:55]
    node twoStageOp = or(_twoStageOp_T_1, io_decode_shiftOp) @[State.scala 56:77]
    reg stageTwoPending : UInt<1>, clock with :
      reset => (UInt<1>("h0"), stageTwoPending) @[State.scala 58:32]
    node _GEN_3 = mux(countEnabled, init, stageTwoPending) @[State.scala 59:22 58:32 59:40]
    node _io_dbus_cyc_T = eq(countEnabled, UInt<1>("h0")) @[State.scala 61:18]
    node _io_dbus_cyc_T_1 = and(_io_dbus_cyc_T, stageTwoPending) @[State.scala 61:32]
    node _io_dbus_cyc_T_2 = and(_io_dbus_cyc_T_1, io_decode_memOp) @[State.scala 61:51]
    node _io_dbus_cyc_T_3 = eq(io_mem_misaligned, UInt<1>("h0")) @[State.scala 61:73]
    node _io_dbus_cyc_T_4 = and(_io_dbus_cyc_T_2, _io_dbus_cyc_T_3) @[State.scala 61:70]
    node _trapPending_T = bits(io_lsb1, 0, 0) @[State.scala 63:69]
    node _trapPending_T_1 = and(io_control_jump, _trapPending_T) @[State.scala 63:52]
    node _trapPending_T_2 = or(_trapPending_T_1, io_mem_misaligned) @[State.scala 63:73]
    node trapPending = and(UInt<1>("h1"), _trapPending_T_2) @[State.scala 63:31]
    node _io_ram_readRequest_T = and(stageTwoRequest, trapPending) @[State.scala 67:57]
    node _io_ram_readRequest_T_1 = or(io_ibus_ack, _io_ram_readRequest_T) @[State.scala 67:37]
    node _io_ram_writeRequest_T = and(io_decode_shiftOp, io_alu_shiftDone) @[State.scala 71:24]
    node _io_ram_writeRequest_T_1 = and(_io_ram_writeRequest_T, stageTwoPending) @[State.scala 71:44]
    node _io_ram_writeRequest_T_2 = and(io_decode_memOp, io_dbus_ack) @[State.scala 72:22]
    node _io_ram_writeRequest_T_3 = or(_io_ram_writeRequest_T_1, _io_ram_writeRequest_T_2) @[State.scala 71:64]
    node _io_ram_writeRequest_T_4 = or(io_decode_sltOp, io_decode_branchOp) @[State.scala 73:42]
    node _io_ram_writeRequest_T_5 = and(stageTwoRequest, _io_ram_writeRequest_T_4) @[State.scala 73:22]
    node _io_ram_writeRequest_T_6 = eq(trapPending, UInt<1>("h0")) @[State.scala 73:68]
    node _io_ram_writeRequest_T_7 = and(_io_ram_writeRequest_T_5, _io_ram_writeRequest_T_6) @[State.scala 73:65]
    node _io_ram_writeRequest_T_8 = or(_io_ram_writeRequest_T_3, _io_ram_writeRequest_T_7) @[State.scala 72:38]
    node _io_rf_writeEnable_T = and(io_decode_rdOp, countEnabled) @[State.scala 75:39]
    node _io_rf_writeEnable_T_1 = eq(init, UInt<1>("h0")) @[State.scala 75:58]
    node _io_rf_writeEnable_T_2 = and(_io_rf_writeEnable_T, _io_rf_writeEnable_T_1) @[State.scala 75:55]
    node _io_bufreg_hold_T = eq(countEnabled, UInt<1>("h0")) @[State.scala 78:21]
    node _io_bufreg_hold_T_1 = eq(io_decode_shiftOp, UInt<1>("h0")) @[State.scala 78:58]
    node _io_bufreg_hold_T_2 = or(stageTwoRequest, _io_bufreg_hold_T_1) @[State.scala 78:55]
    node _io_bufreg_hold_T_3 = and(_io_bufreg_hold_T, _io_bufreg_hold_T_2) @[State.scala 78:35]
    reg controlJump : UInt<1>, clock with :
      reset => (UInt<1>("h0"), controlJump) @[State.scala 81:28]
    node _controlJump_T = and(init, io_decode_takeBranch) @[State.scala 82:41]
    node _GEN_4 = mux(countDone, _controlJump_T, controlJump) @[State.scala 82:19 81:28 82:33]
    reg pendingIrq : UInt<1>, clock with :
      reset => (UInt<1>("h0"), pendingIrq) @[State.scala 85:23]
    node _T = eq(stageTwoPending, UInt<1>("h0")) @[State.scala 89:24]
    node _T_1 = and(io_ram_ready, _T) @[State.scala 89:21]
    node _init_T = eq(pendingIrq, UInt<1>("h0")) @[State.scala 90:27]
    node _init_T_1 = and(twoStageOp, _init_T) @[State.scala 90:24]
    node _GEN_5 = mux(_T_1, _init_T_1, init) @[State.scala 89:42 90:10 17:17]
    node _GEN_6 = mux(countDone, UInt<1>("h0"), _GEN_5) @[State.scala 92:{19,26}]
    reg irqSync : UInt<1>, clock with :
      reset => (UInt<1>("h0"), irqSync) @[State.scala 95:22]
    reg misalignedTrapSync : UInt<1>, clock with :
      reset => (UInt<1>("h0"), misalignedTrapSync) @[State.scala 96:33]
    node _io_control_trap_T = or(io_decode_eOp, pendingIrq) @[State.scala 97:38]
    node _io_control_trap_T_1 = or(_io_control_trap_T, misalignedTrapSync) @[State.scala 97:51]
    node _io_csr_trapTaken_T = and(io_ibus_ack, io_control_trap) @[State.scala 98:37]
    node _GEN_7 = mux(io_ibus_ack, UInt<1>("h0"), irqSync) @[State.scala 100:{23,33} 95:22]
    node _GEN_8 = mux(io_csr_newIrq, UInt<1>("h1"), _GEN_7) @[State.scala 101:{25,35}]
    node _GEN_9 = mux(io_ibus_ack, irqSync, pendingIrq) @[State.scala 103:{23,36} 85:23]
    node _GEN_10 = mux(stageTwoRequest, trapPending, misalignedTrapSync) @[State.scala 105:{27,48} 96:33]
    node _GEN_11 = mux(io_ibus_ack, UInt<1>("h0"), _GEN_10) @[State.scala 106:{23,44}]
    io_csr_trapTaken <= _io_csr_trapTaken_T @[State.scala 98:22]
    io_csr_pendingIrq <= pendingIrq @[State.scala 86:21]
    io_dbus_cyc <= _io_dbus_cyc_T_4 @[State.scala 61:15]
    io_ram_writeRequest <= _io_ram_writeRequest_T_8 @[State.scala 70:23]
    io_ram_readRequest <= _io_ram_readRequest_T_1 @[State.scala 67:22]
    io_rf_writeEnable <= _io_rf_writeEnable_T_2 @[State.scala 75:21]
    io_count_enabled <= countEnabled @[State.scala 24:20]
    io_count_init <= init @[State.scala 18:17]
    io_count_count0 <= _io_count_count0_T_2 @[State.scala 46:19]
    io_count_count0To3 <= _io_count_count0To3_T @[State.scala 44:22]
    io_count_count12To31 <= _io_count_count12To31_T_3 @[State.scala 45:24]
    io_count_count1 <= _io_count_count1_T_2 @[State.scala 47:19]
    io_count_count2 <= _io_count_count2_T_2 @[State.scala 48:19]
    io_count_count3 <= _io_count_count3_T_2 @[State.scala 49:19]
    io_count_count7 <= _io_count_count7_T_2 @[State.scala 51:19]
    io_count_done <= countDone @[State.scala 22:17]
    io_bufreg_hold <= _io_bufreg_hold_T_3 @[State.scala 78:18]
    io_control_pcEnable <= _io_control_pcEnable_T_1 @[State.scala 40:23]
    io_control_jump <= controlJump @[State.scala 83:19]
    io_control_trap <= _io_control_trap_T_1 @[State.scala 97:21]
    io_alu_shiftAmountEnable <= _io_alu_shiftAmountEnable_T_1 @[State.scala 53:28]
    io_mem_byteCount <= _io_mem_byteCount_T @[State.scala 42:20]
    init <= _GEN_6
    countDone <= _countDone_T_2 @[State.scala 34:13]
    countEnabled <= _GEN_1
    count <= mux(reset, UInt<3>("h0"), _count_T_2) @[State.scala 28:{22,22} 31:9]
    countR <= mux(reset, UInt<4>("h1"), _GEN_2) @[State.scala 29:{23,23}]
    stageTwoRequest <= _stageTwoRequest_T @[State.scala 37:32]
    stageTwoPending <= mux(reset, UInt<1>("h0"), _GEN_3) @[State.scala 58:{32,32}]
    controlJump <= mux(reset, UInt<1>("h0"), _GEN_4) @[State.scala 81:{28,28}]
    pendingIrq <= _GEN_9
    irqSync <= _GEN_8
    misalignedTrapSync <= _GEN_11

  module Decode :
    input clock : Clock
    input reset : UInt<1>
    input io_count_enabled : UInt<1>
    input io_count_init : UInt<1>
    input io_count_count0 : UInt<1>
    input io_count_count0To3 : UInt<1>
    input io_count_count12To31 : UInt<1>
    input io_count_count1 : UInt<1>
    input io_count_count2 : UInt<1>
    input io_count_count3 : UInt<1>
    input io_count_count7 : UInt<1>
    input io_count_done : UInt<1>
    output io_state_takeBranch : UInt<1>
    output io_state_eOp : UInt<1>
    output io_state_eBreak : UInt<1>
    output io_state_branchOp : UInt<1>
    output io_state_memOp : UInt<1>
    output io_state_shiftOp : UInt<1>
    output io_state_sltOp : UInt<1>
    output io_state_rdOp : UInt<1>
    output io_bufreg_loop : UInt<1>
    output io_bufreg_rs1En : UInt<1>
    output io_bufreg_immEn : UInt<1>
    output io_bufreg_clearLsb : UInt<1>
    output io_control_jalOrJalr : UInt<1>
    output io_control_uType : UInt<1>
    output io_control_pcRel : UInt<1>
    output io_control_mRet : UInt<1>
    output io_alu_doSubtract : UInt<1>
    output io_alu_boolOp : UInt<2>
    output io_alu_cmpEqual : UInt<1>
    output io_alu_cmpUnsigned : UInt<1>
    output io_alu_shiftSigned : UInt<1>
    output io_alu_shiftRight : UInt<1>
    output io_alu_rdSelect : UInt<4>
    output io_alu_opBIsRS2 : UInt<1>
    input io_alu_cmpResult : UInt<1>
    output io_rf_rdAddress : UInt<5>
    output io_rf_rs1Address : UInt<5>
    output io_rf_rs2Address : UInt<5>
    output io_rf_csrAddress : UInt<2>
    output io_rf_csrEnable : UInt<1>
    output io_mem_memOp : UInt<1>
    output io_mem_signed : UInt<1>
    output io_mem_word : UInt<1>
    output io_mem_half : UInt<1>
    output io_mem_cmd : UInt<1>
    output io_csr_mStatusEn : UInt<1>
    output io_csr_mieEn : UInt<1>
    output io_csr_mcauseEn : UInt<1>
    output io_csr_source : UInt<2>
    output io_csr_dSel : UInt<1>
    output io_csr_imm : UInt<1>
    output io_csr_eOp : UInt<1>
    output io_csr_eBreak : UInt<1>
    output io_csr_mRet : UInt<1>
    input io_top_wbRdt : UInt<32>
    input io_top_wbEn : UInt<1>
    output io_top_imm : UInt<1>
    output io_top_rdCsrEn : UInt<1>
    output io_top_rdAluEn : UInt<1>

    node _io_rf_rdAddress_T = bits(io_top_wbRdt, 11, 7) @[Decode.scala 15:45]
    reg io_rf_rdAddress_r : UInt<5>, clock with :
      reset => (UInt<1>("h0"), io_rf_rdAddress_r) @[Reg.scala 15:16]
    node _GEN_0 = mux(io_top_wbEn, _io_rf_rdAddress_T, io_rf_rdAddress_r) @[Reg.scala 15:16 16:{19,23}]
    node _rs1Address_T = bits(io_top_wbRdt, 19, 15) @[Decode.scala 16:42]
    reg rs1Address : UInt<5>, clock with :
      reset => (UInt<1>("h0"), rs1Address) @[Reg.scala 15:16]
    node _GEN_1 = mux(io_top_wbEn, _rs1Address_T, rs1Address) @[Reg.scala 15:16 16:{19,23}]
    node _io_rf_rs2Address_T = bits(io_top_wbRdt, 24, 20) @[Decode.scala 18:45]
    reg io_rf_rs2Address_r : UInt<5>, clock with :
      reset => (UInt<1>("h0"), io_rf_rs2Address_r) @[Reg.scala 15:16]
    node _GEN_2 = mux(io_top_wbEn, _io_rf_rs2Address_T, io_rf_rs2Address_r) @[Reg.scala 15:16 16:{19,23}]
    node _funct3_T = bits(io_top_wbRdt, 14, 12) @[Decode.scala 20:38]
    reg funct3 : UInt<3>, clock with :
      reset => (UInt<1>("h0"), funct3) @[Reg.scala 15:16]
    node _GEN_3 = mux(io_top_wbEn, _funct3_T, funct3) @[Reg.scala 15:16 16:{19,23}]
    node _imm30_T = bits(io_top_wbRdt, 30, 30) @[Decode.scala 21:38]
    reg imm30 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), imm30) @[Reg.scala 15:16]
    node _GEN_4 = mux(io_top_wbEn, _imm30_T, imm30) @[Reg.scala 15:16 16:{19,23}]
    node _opcode_T = bits(io_top_wbRdt, 6, 2) @[Decode.scala 22:38]
    reg opcode : UInt<5>, clock with :
      reset => (UInt<1>("h0"), opcode) @[Reg.scala 15:16]
    node _GEN_5 = mux(io_top_wbEn, _opcode_T, opcode) @[Reg.scala 15:16 16:{19,23}]
    node _op20_T = bits(io_top_wbRdt, 20, 20) @[Decode.scala 23:36]
    reg op20 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), op20) @[Reg.scala 15:16]
    node _GEN_6 = mux(io_top_wbEn, _op20_T, op20) @[Reg.scala 15:16 16:{19,23}]
    node _op21_T = bits(io_top_wbRdt, 21, 21) @[Decode.scala 24:36]
    reg op21 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), op21) @[Reg.scala 15:16]
    node _GEN_7 = mux(io_top_wbEn, _op21_T, op21) @[Reg.scala 15:16 16:{19,23}]
    node _op22_T = bits(io_top_wbRdt, 22, 22) @[Decode.scala 25:36]
    reg op22 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), op22) @[Reg.scala 15:16]
    node _GEN_8 = mux(io_top_wbEn, _op22_T, op22) @[Reg.scala 15:16 16:{19,23}]
    node _op26_T = bits(io_top_wbRdt, 26, 26) @[Decode.scala 26:36]
    reg op26 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), op26) @[Reg.scala 15:16]
    node _GEN_9 = mux(io_top_wbEn, _op26_T, op26) @[Reg.scala 15:16 16:{19,23}]
    node _signbit_T = bits(io_top_wbRdt, 31, 31) @[Decode.scala 29:39]
    reg signbit : UInt<1>, clock with :
      reset => (UInt<1>("h0"), signbit) @[Reg.scala 15:16]
    node _GEN_10 = mux(io_top_wbEn, _signbit_T, signbit) @[Reg.scala 15:16 16:{19,23}]
    node _imm19_12_20_T = bits(io_top_wbRdt, 19, 12) @[Decode.scala 30:43]
    node _imm19_12_20_T_1 = bits(io_top_wbRdt, 20, 20) @[Decode.scala 30:66]
    node _imm19_12_20_T_2 = cat(_imm19_12_20_T, _imm19_12_20_T_1) @[Decode.scala 30:51]
    reg imm19_12_20 : UInt<9>, clock with :
      reset => (UInt<1>("h0"), imm19_12_20) @[Reg.scala 15:16]
    node _GEN_11 = mux(io_top_wbEn, _imm19_12_20_T_2, imm19_12_20) @[Reg.scala 15:16 16:{19,23}]
    node _imm7_T = bits(io_top_wbRdt, 7, 7) @[Decode.scala 31:36]
    reg imm7 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), imm7) @[Reg.scala 15:16]
    node _GEN_12 = mux(io_top_wbEn, _imm7_T, imm7) @[Reg.scala 15:16 16:{19,23}]
    node _imm30_25_T = bits(io_top_wbRdt, 30, 25) @[Decode.scala 32:40]
    reg imm30_25 : UInt<6>, clock with :
      reset => (UInt<1>("h0"), imm30_25) @[Reg.scala 15:16]
    node _GEN_13 = mux(io_top_wbEn, _imm30_25_T, imm30_25) @[Reg.scala 15:16 16:{19,23}]
    node _imm24_20_T = bits(io_top_wbRdt, 24, 20) @[Decode.scala 33:40]
    reg imm24_20 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), imm24_20) @[Reg.scala 15:16]
    node _GEN_14 = mux(io_top_wbEn, _imm24_20_T, imm24_20) @[Reg.scala 15:16 16:{19,23}]
    node _imm11_7_T = bits(io_top_wbRdt, 11, 7) @[Decode.scala 34:40]
    reg imm11_7 : UInt<5>, clock with :
      reset => (UInt<1>("h0"), imm11_7) @[Reg.scala 15:16]
    node _GEN_15 = mux(io_top_wbEn, _imm11_7_T, imm11_7) @[Reg.scala 15:16 16:{19,23}]
    node m3 = bits(opcode, 4, 4) @[Decode.scala 35:18]
    node _m2_T = bits(opcode, 4, 4) @[Decode.scala 36:19]
    node _m2_T_1 = bits(opcode, 0, 0) @[Decode.scala 36:32]
    node _m2_T_2 = eq(_m2_T_1, UInt<1>("h0")) @[Decode.scala 36:25]
    node _m2_T_3 = and(_m2_T, _m2_T_2) @[Decode.scala 36:23]
    node _m2_T_4 = bits(opcode, 1, 0) @[Decode.scala 36:48]
    node _m2_T_5 = eq(_m2_T_4, UInt<1>("h0")) @[Decode.scala 36:54]
    node _m2_T_6 = bits(opcode, 2, 1) @[Decode.scala 36:72]
    node _m2_T_7 = eq(_m2_T_6, UInt<1>("h0")) @[Decode.scala 36:78]
    node _m2_T_8 = or(_m2_T_5, _m2_T_7) @[Decode.scala 36:63]
    node m2 = cat(_m2_T_3, _m2_T_8) @[Decode.scala 36:37]
    node _csrOp_T = bits(opcode, 4, 4) @[Decode.scala 37:21]
    node _csrOp_T_1 = bits(opcode, 2, 2) @[Decode.scala 37:34]
    node _csrOp_T_2 = and(_csrOp_T, _csrOp_T_1) @[Decode.scala 37:25]
    node _csrOp_T_3 = orr(funct3) @[Decode.scala 37:51]
    node csrOp = and(_csrOp_T_2, _csrOp_T_3) @[Decode.scala 37:38]
    node _imm19_12_20_T_3 = bits(imm24_20, 0, 0) @[Decode.scala 39:45]
    node _imm19_12_20_T_4 = mux(m3, signbit, _imm19_12_20_T_3) @[Decode.scala 39:23]
    node _imm19_12_20_T_5 = bits(imm19_12_20, 8, 1) @[Decode.scala 39:64]
    node _imm19_12_20_T_6 = cat(_imm19_12_20_T_4, _imm19_12_20_T_5) @[Decode.scala 39:50]
    node _imm30_25_T_1 = bits(m2, 1, 1) @[Decode.scala 41:26]
    node _imm30_25_T_2 = bits(m2, 0, 0) @[Decode.scala 41:43]
    node _imm30_25_T_3 = bits(imm19_12_20, 0, 0) @[Decode.scala 41:68]
    node _imm30_25_T_4 = mux(_imm30_25_T_2, signbit, _imm30_25_T_3) @[Decode.scala 41:40]
    node _imm30_25_T_5 = mux(_imm30_25_T_1, imm7, _imm30_25_T_4) @[Decode.scala 41:23]
    node _imm30_25_T_6 = bits(imm30_25, 5, 1) @[Decode.scala 41:85]
    node _imm30_25_T_7 = cat(_imm30_25_T_5, _imm30_25_T_6) @[Decode.scala 41:74]
    node _imm24_20_T_1 = bits(imm30_25, 0, 0) @[Decode.scala 42:28]
    node _imm24_20_T_2 = bits(imm24_20, 4, 1) @[Decode.scala 42:43]
    node _imm24_20_T_3 = cat(_imm24_20_T_1, _imm24_20_T_2) @[Decode.scala 42:32]
    node _imm11_7_T_1 = bits(imm30_25, 0, 0) @[Decode.scala 43:28]
    node _imm11_7_T_2 = bits(imm11_7, 4, 1) @[Decode.scala 43:42]
    node _imm11_7_T_3 = cat(_imm11_7_T_1, _imm11_7_T_2) @[Decode.scala 43:32]
    node _T = and(csrOp, io_csr_dSel) @[Decode.scala 44:16]
    node _rs1Address_T_1 = bits(rs1Address, 4, 1) @[Decode.scala 45:43]
    node _rs1Address_T_2 = cat(UInt<1>("h0"), _rs1Address_T_1) @[Decode.scala 45:30]
    node _GEN_16 = mux(_T, _rs1Address_T_2, _GEN_1) @[Decode.scala 44:32 45:18]
    node _GEN_17 = mux(io_count_enabled, _imm19_12_20_T_6, _GEN_11) @[Decode.scala 38:26 39:17]
    node _GEN_18 = mux(io_count_enabled, signbit, _GEN_12) @[Decode.scala 38:26 40:17]
    node _GEN_19 = mux(io_count_enabled, _imm30_25_T_7, _GEN_13) @[Decode.scala 38:26 41:17]
    node _GEN_20 = mux(io_count_enabled, _imm24_20_T_3, _GEN_14) @[Decode.scala 38:26 42:17]
    node _GEN_21 = mux(io_count_enabled, _imm11_7_T_3, _GEN_15) @[Decode.scala 38:26 43:17]
    node _GEN_22 = mux(io_count_enabled, _GEN_16, _GEN_1) @[Decode.scala 38:26]
    node _opOrOpimm_T = bits(opcode, 4, 4) @[Decode.scala 49:28]
    node _opOrOpimm_T_1 = eq(_opOrOpimm_T, UInt<1>("h0")) @[Decode.scala 49:21]
    node _opOrOpimm_T_2 = bits(opcode, 2, 2) @[Decode.scala 49:42]
    node _opOrOpimm_T_3 = and(_opOrOpimm_T_1, _opOrOpimm_T_2) @[Decode.scala 49:32]
    node _opOrOpimm_T_4 = bits(opcode, 0, 0) @[Decode.scala 49:56]
    node _opOrOpimm_T_5 = eq(_opOrOpimm_T_4, UInt<1>("h0")) @[Decode.scala 49:49]
    node opOrOpimm = and(_opOrOpimm_T_3, _opOrOpimm_T_5) @[Decode.scala 49:46]
    node _io_state_memOp_T = bits(opcode, 4, 4) @[Decode.scala 50:28]
    node _io_state_memOp_T_1 = eq(_io_state_memOp_T, UInt<1>("h0")) @[Decode.scala 50:21]
    node _io_state_memOp_T_2 = bits(opcode, 2, 2) @[Decode.scala 50:42]
    node _io_state_memOp_T_3 = eq(_io_state_memOp_T_2, UInt<1>("h0")) @[Decode.scala 50:35]
    node _io_state_memOp_T_4 = and(_io_state_memOp_T_1, _io_state_memOp_T_3) @[Decode.scala 50:32]
    node _io_state_memOp_T_5 = bits(opcode, 0, 0) @[Decode.scala 50:56]
    node _io_state_memOp_T_6 = eq(_io_state_memOp_T_5, UInt<1>("h0")) @[Decode.scala 50:49]
    node _io_state_memOp_T_7 = and(_io_state_memOp_T_4, _io_state_memOp_T_6) @[Decode.scala 50:46]
    node _io_state_shiftOp_T = bits(funct3, 1, 0) @[Decode.scala 52:44]
    node _io_state_shiftOp_T_1 = eq(_io_state_shiftOp_T, UInt<1>("h1")) @[Decode.scala 52:50]
    node _io_state_shiftOp_T_2 = and(opOrOpimm, _io_state_shiftOp_T_1) @[Decode.scala 52:34]
    node _io_state_sltOp_T = bits(funct3, 2, 1) @[Decode.scala 53:44]
    node _io_state_sltOp_T_1 = eq(_io_state_sltOp_T, UInt<1>("h1")) @[Decode.scala 53:50]
    node _io_state_sltOp_T_2 = and(opOrOpimm, _io_state_sltOp_T_1) @[Decode.scala 53:34]
    node _io_state_branchOp_T = bits(opcode, 4, 4) @[Decode.scala 54:30]
    node _io_state_branchOp_T_1 = bits(opcode, 2, 2) @[Decode.scala 54:43]
    node _io_state_branchOp_T_2 = eq(_io_state_branchOp_T_1, UInt<1>("h0")) @[Decode.scala 54:36]
    node _io_state_branchOp_T_3 = and(_io_state_branchOp_T, _io_state_branchOp_T_2) @[Decode.scala 54:34]
    node _io_state_eOp_T = bits(opcode, 4, 4) @[Decode.scala 58:25]
    node _io_state_eOp_T_1 = bits(opcode, 2, 2) @[Decode.scala 58:37]
    node _io_state_eOp_T_2 = and(_io_state_eOp_T, _io_state_eOp_T_1) @[Decode.scala 58:29]
    node _io_state_eOp_T_3 = eq(op21, UInt<1>("h0")) @[Decode.scala 58:43]
    node _io_state_eOp_T_4 = and(_io_state_eOp_T_2, _io_state_eOp_T_3) @[Decode.scala 58:41]
    node _io_state_eOp_T_5 = orr(funct3) @[Decode.scala 58:63]
    node _io_state_eOp_T_6 = eq(_io_state_eOp_T_5, UInt<1>("h0")) @[Decode.scala 58:51]
    node _io_state_eOp_T_7 = and(_io_state_eOp_T_4, _io_state_eOp_T_6) @[Decode.scala 58:49]
    node _io_bufreg_rs1En_T = bits(opcode, 4, 4) @[Decode.scala 68:29]
    node _io_bufreg_rs1En_T_1 = eq(_io_bufreg_rs1En_T, UInt<1>("h0")) @[Decode.scala 68:22]
    node _io_bufreg_rs1En_T_2 = bits(opcode, 1, 1) @[Decode.scala 68:44]
    node _io_bufreg_rs1En_T_3 = eq(_io_bufreg_rs1En_T_2, UInt<1>("h0")) @[Decode.scala 68:37]
    node _io_bufreg_rs1En_T_4 = bits(opcode, 0, 0) @[Decode.scala 68:57]
    node _io_bufreg_rs1En_T_5 = and(_io_bufreg_rs1En_T_3, _io_bufreg_rs1En_T_4) @[Decode.scala 68:48]
    node _io_bufreg_rs1En_T_6 = or(_io_bufreg_rs1En_T_1, _io_bufreg_rs1En_T_5) @[Decode.scala 68:33]
    node _io_bufreg_immEn_T = bits(opcode, 2, 2) @[Decode.scala 69:29]
    node _io_bufreg_immEn_T_1 = eq(_io_bufreg_immEn_T, UInt<1>("h0")) @[Decode.scala 69:22]
    node _io_bufreg_clearLsb_T = bits(opcode, 4, 4) @[Decode.scala 77:31]
    node _io_bufreg_clearLsb_T_1 = bits(opcode, 1, 0) @[Decode.scala 77:46]
    node _io_bufreg_clearLsb_T_2 = eq(_io_bufreg_clearLsb_T_1, UInt<1>("h0")) @[Decode.scala 77:52]
    node _io_bufreg_clearLsb_T_3 = bits(opcode, 1, 0) @[Decode.scala 77:71]
    node _io_bufreg_clearLsb_T_4 = eq(_io_bufreg_clearLsb_T_3, UInt<2>("h3")) @[Decode.scala 77:77]
    node _io_bufreg_clearLsb_T_5 = or(_io_bufreg_clearLsb_T_2, _io_bufreg_clearLsb_T_4) @[Decode.scala 77:61]
    node _io_bufreg_clearLsb_T_6 = and(_io_bufreg_clearLsb_T, _io_bufreg_clearLsb_T_5) @[Decode.scala 77:35]
    node _io_state_takeBranch_T = bits(opcode, 4, 4) @[Decode.scala 85:32]
    node _io_state_takeBranch_T_1 = bits(opcode, 2, 2) @[Decode.scala 85:46]
    node _io_state_takeBranch_T_2 = eq(_io_state_takeBranch_T_1, UInt<1>("h0")) @[Decode.scala 85:39]
    node _io_state_takeBranch_T_3 = and(_io_state_takeBranch_T, _io_state_takeBranch_T_2) @[Decode.scala 85:36]
    node _io_state_takeBranch_T_4 = bits(opcode, 0, 0) @[Decode.scala 85:60]
    node _io_state_takeBranch_T_5 = bits(funct3, 0, 0) @[Decode.scala 85:93]
    node _io_state_takeBranch_T_6 = xor(io_alu_cmpResult, _io_state_takeBranch_T_5) @[Decode.scala 85:85]
    node _io_state_takeBranch_T_7 = or(_io_state_takeBranch_T_4, _io_state_takeBranch_T_6) @[Decode.scala 85:64]
    node _io_state_takeBranch_T_8 = and(_io_state_takeBranch_T_3, _io_state_takeBranch_T_7) @[Decode.scala 85:50]
    node _io_control_uType_T = bits(opcode, 4, 4) @[Decode.scala 87:34]
    node _io_control_uType_T_1 = eq(_io_control_uType_T, UInt<1>("h0")) @[Decode.scala 87:27]
    node _io_control_uType_T_2 = bits(opcode, 2, 2) @[Decode.scala 87:47]
    node _io_control_uType_T_3 = and(_io_control_uType_T_1, _io_control_uType_T_2) @[Decode.scala 87:38]
    node _io_control_uType_T_4 = bits(opcode, 0, 0) @[Decode.scala 87:60]
    node _io_control_uType_T_5 = and(_io_control_uType_T_3, _io_control_uType_T_4) @[Decode.scala 87:51]
    node _io_control_jalOrJalr_T = bits(opcode, 4, 4) @[Decode.scala 88:34]
    node _io_control_jalOrJalr_T_1 = bits(opcode, 0, 0) @[Decode.scala 88:60]
    node _io_control_jalOrJalr_T_2 = and(_io_control_jalOrJalr_T, _io_control_jalOrJalr_T_1) @[Decode.scala 88:38]
    node _io_control_pcRel_T = bits(opcode, 2, 0) @[Decode.scala 92:30]
    node _io_control_pcRel_T_1 = eq(_io_control_pcRel_T, UInt<1>("h0")) @[Decode.scala 92:36]
    node _io_control_pcRel_T_2 = bits(opcode, 1, 0) @[Decode.scala 92:55]
    node _io_control_pcRel_T_3 = eq(_io_control_pcRel_T_2, UInt<2>("h3")) @[Decode.scala 92:61]
    node _io_control_pcRel_T_4 = or(_io_control_pcRel_T_1, _io_control_pcRel_T_3) @[Decode.scala 92:45]
    node _io_control_pcRel_T_5 = bits(opcode, 4, 3) @[Decode.scala 92:80]
    node _io_control_pcRel_T_6 = eq(_io_control_pcRel_T_5, UInt<1>("h0")) @[Decode.scala 92:86]
    node _io_control_pcRel_T_7 = or(_io_control_pcRel_T_4, _io_control_pcRel_T_6) @[Decode.scala 92:70]
    node _io_control_mRet_T = bits(opcode, 4, 4) @[Decode.scala 94:28]
    node _io_control_mRet_T_1 = bits(opcode, 2, 2) @[Decode.scala 94:41]
    node _io_control_mRet_T_2 = and(_io_control_mRet_T, _io_control_mRet_T_1) @[Decode.scala 94:32]
    node _io_control_mRet_T_3 = and(_io_control_mRet_T_2, op21) @[Decode.scala 94:45]
    node _io_control_mRet_T_4 = orr(funct3) @[Decode.scala 94:67]
    node _io_control_mRet_T_5 = eq(_io_control_mRet_T_4, UInt<1>("h0")) @[Decode.scala 94:56]
    node _io_control_mRet_T_6 = and(_io_control_mRet_T_3, _io_control_mRet_T_5) @[Decode.scala 94:53]
    node _io_state_rdOp_T = bits(opcode, 2, 2) @[Decode.scala 100:27]
    node _io_state_rdOp_T_1 = bits(opcode, 2, 2) @[Decode.scala 101:29]
    node _io_state_rdOp_T_2 = eq(_io_state_rdOp_T_1, UInt<1>("h0")) @[Decode.scala 101:22]
    node _io_state_rdOp_T_3 = bits(opcode, 4, 4) @[Decode.scala 101:42]
    node _io_state_rdOp_T_4 = and(_io_state_rdOp_T_2, _io_state_rdOp_T_3) @[Decode.scala 101:33]
    node _io_state_rdOp_T_5 = bits(opcode, 0, 0) @[Decode.scala 101:55]
    node _io_state_rdOp_T_6 = and(_io_state_rdOp_T_4, _io_state_rdOp_T_5) @[Decode.scala 101:46]
    node _io_state_rdOp_T_7 = or(_io_state_rdOp_T, _io_state_rdOp_T_6) @[Decode.scala 100:31]
    node _io_state_rdOp_T_8 = bits(opcode, 2, 2) @[Decode.scala 102:29]
    node _io_state_rdOp_T_9 = eq(_io_state_rdOp_T_8, UInt<1>("h0")) @[Decode.scala 102:22]
    node _io_state_rdOp_T_10 = bits(opcode, 3, 3) @[Decode.scala 102:43]
    node _io_state_rdOp_T_11 = eq(_io_state_rdOp_T_10, UInt<1>("h0")) @[Decode.scala 102:36]
    node _io_state_rdOp_T_12 = and(_io_state_rdOp_T_9, _io_state_rdOp_T_11) @[Decode.scala 102:33]
    node _io_state_rdOp_T_13 = bits(opcode, 0, 0) @[Decode.scala 102:57]
    node _io_state_rdOp_T_14 = eq(_io_state_rdOp_T_13, UInt<1>("h0")) @[Decode.scala 102:50]
    node _io_state_rdOp_T_15 = and(_io_state_rdOp_T_12, _io_state_rdOp_T_14) @[Decode.scala 102:47]
    node _io_state_rdOp_T_16 = or(_io_state_rdOp_T_7, _io_state_rdOp_T_15) @[Decode.scala 101:60]
    node _io_state_rdOp_T_17 = orr(io_rf_rdAddress) @[Decode.scala 102:85]
    node _io_state_rdOp_T_18 = and(_io_state_rdOp_T_16, _io_state_rdOp_T_17) @[Decode.scala 102:63]
    node _io_alu_doSubtract_T = bits(opcode, 3, 3) @[Decode.scala 103:30]
    node _io_alu_doSubtract_T_1 = and(_io_alu_doSubtract_T, imm30) @[Decode.scala 103:34]
    node _csrValid_T = eq(op22, UInt<1>("h0")) @[Decode.scala 118:35]
    node _csrValid_T_1 = and(op26, _csrValid_T) @[Decode.scala 118:32]
    node _csrValid_T_2 = eq(op21, UInt<1>("h0")) @[Decode.scala 118:44]
    node _csrValid_T_3 = and(_csrValid_T_1, _csrValid_T_2) @[Decode.scala 118:41]
    node csrValid = or(op20, _csrValid_T_3) @[Decode.scala 118:23]
    node _io_rf_csrEnable_T = and(csrOp, csrValid) @[Decode.scala 123:28]
    node _io_csr_mStatusEn_T = eq(op26, UInt<1>("h0")) @[Decode.scala 124:32]
    node _io_csr_mStatusEn_T_1 = and(csrOp, _io_csr_mStatusEn_T) @[Decode.scala 124:29]
    node _io_csr_mStatusEn_T_2 = eq(op22, UInt<1>("h0")) @[Decode.scala 124:41]
    node _io_csr_mStatusEn_T_3 = and(_io_csr_mStatusEn_T_1, _io_csr_mStatusEn_T_2) @[Decode.scala 124:38]
    node _io_csr_mieEn_T = eq(op26, UInt<1>("h0")) @[Decode.scala 125:32]
    node _io_csr_mieEn_T_1 = and(csrOp, _io_csr_mieEn_T) @[Decode.scala 125:29]
    node _io_csr_mieEn_T_2 = and(_io_csr_mieEn_T_1, op22) @[Decode.scala 125:38]
    node _io_csr_mieEn_T_3 = eq(op20, UInt<1>("h0")) @[Decode.scala 125:50]
    node _io_csr_mieEn_T_4 = and(_io_csr_mieEn_T_2, _io_csr_mieEn_T_3) @[Decode.scala 125:47]
    node _io_csr_mcauseEn_T = and(csrOp, op21) @[Decode.scala 126:38]
    node _io_csr_mcauseEn_T_1 = eq(op20, UInt<1>("h0")) @[Decode.scala 126:50]
    node _io_csr_mcauseEn_T_2 = and(_io_csr_mcauseEn_T, _io_csr_mcauseEn_T_1) @[Decode.scala 126:47]
    node _io_csr_source_T = bits(funct3, 1, 0) @[Decode.scala 127:26]
    node _io_csr_dSel_T = bits(funct3, 2, 2) @[Decode.scala 128:24]
    node _io_csr_imm_T = bits(rs1Address, 0, 0) @[Decode.scala 129:27]
    node _io_rf_csrAddress_T = eq(op20, UInt<1>("h0")) @[Decode.scala 131:14]
    node _io_rf_csrAddress_T_1 = and(op26, _io_rf_csrAddress_T) @[Decode.scala 131:11]
    node _io_rf_csrAddress_T_2 = eq(op21, UInt<1>("h0")) @[Decode.scala 132:14]
    node _io_rf_csrAddress_T_3 = and(op26, _io_rf_csrAddress_T_2) @[Decode.scala 132:11]
    node _io_rf_csrAddress_T_4 = mux(op26, UInt<2>("h3"), UInt<2>("h1")) @[Mux.scala 98:16]
    node _io_rf_csrAddress_T_5 = mux(_io_rf_csrAddress_T_3, UInt<2>("h2"), _io_rf_csrAddress_T_4) @[Mux.scala 98:16]
    node _io_rf_csrAddress_T_6 = mux(_io_rf_csrAddress_T_1, UInt<2>("h0"), _io_rf_csrAddress_T_5) @[Mux.scala 98:16]
    node _io_alu_cmpEqual_T = bits(funct3, 2, 1) @[Decode.scala 135:28]
    node _io_alu_cmpEqual_T_1 = eq(_io_alu_cmpEqual_T, UInt<1>("h0")) @[Decode.scala 135:34]
    node _io_alu_cmpUnsigned_T = bits(funct3, 0, 0) @[Decode.scala 136:32]
    node _io_alu_cmpUnsigned_T_1 = bits(funct3, 1, 1) @[Decode.scala 136:45]
    node _io_alu_cmpUnsigned_T_2 = and(_io_alu_cmpUnsigned_T, _io_alu_cmpUnsigned_T_1) @[Decode.scala 136:36]
    node _io_alu_cmpUnsigned_T_3 = bits(funct3, 1, 1) @[Decode.scala 136:60]
    node _io_alu_cmpUnsigned_T_4 = bits(funct3, 2, 2) @[Decode.scala 136:73]
    node _io_alu_cmpUnsigned_T_5 = and(_io_alu_cmpUnsigned_T_3, _io_alu_cmpUnsigned_T_4) @[Decode.scala 136:64]
    node _io_alu_cmpUnsigned_T_6 = or(_io_alu_cmpUnsigned_T_2, _io_alu_cmpUnsigned_T_5) @[Decode.scala 136:50]
    node _io_alu_shiftRight_T = bits(funct3, 2, 2) @[Decode.scala 138:30]
    node _io_alu_boolOp_T = bits(funct3, 1, 0) @[Decode.scala 139:43]
    node _aluBool_T = bits(funct3, 2, 2) @[Decode.scala 141:23]
    node _aluBool_T_1 = bits(funct3, 1, 0) @[Decode.scala 141:38]
    node _aluBool_T_2 = eq(_aluBool_T_1, UInt<1>("h1")) @[Decode.scala 141:44]
    node _aluBool_T_3 = eq(_aluBool_T_2, UInt<1>("h0")) @[Decode.scala 141:30]
    node aluBool = and(_aluBool_T, _aluBool_T_3) @[Decode.scala 141:27]
    node _aluSlt_T = bits(funct3, 2, 1) @[Decode.scala 142:22]
    node aluSlt = eq(_aluSlt_T, UInt<1>("h1")) @[Decode.scala 142:28]
    node _aluShift_T = bits(funct3, 1, 0) @[Decode.scala 143:24]
    node aluShift = eq(_aluShift_T, UInt<1>("h1")) @[Decode.scala 143:30]
    node aluAddSub = eq(funct3, UInt<1>("h0")) @[Decode.scala 144:26]
    node _io_alu_rdSelect_T = cat(aluBool, aluSlt) @[Decode.scala 145:37]
    node _io_alu_rdSelect_T_1 = cat(_io_alu_rdSelect_T, aluShift) @[Decode.scala 145:47]
    node _io_alu_rdSelect_T_2 = cat(_io_alu_rdSelect_T_1, aluAddSub) @[Decode.scala 145:59]
    node _io_mem_cmd_T = bits(opcode, 3, 3) @[Decode.scala 147:23]
    node _io_mem_signed_T = bits(funct3, 2, 2) @[Decode.scala 148:27]
    node _io_mem_signed_T_1 = eq(_io_mem_signed_T, UInt<1>("h0")) @[Decode.scala 148:20]
    node _io_mem_word_T = bits(funct3, 1, 1) @[Decode.scala 149:24]
    node _io_mem_half_T = bits(funct3, 0, 0) @[Decode.scala 150:24]
    node _m1_T = bits(opcode, 3, 0) @[Decode.scala 154:18]
    node m1 = eq(_m1_T, UInt<4>("h8")) @[Decode.scala 154:24]
    node _io_top_imm_T = bits(imm11_7, 0, 0) @[Decode.scala 155:60]
    node _io_top_imm_T_1 = bits(imm24_20, 0, 0) @[Decode.scala 155:73]
    node _io_top_imm_T_2 = mux(m1, _io_top_imm_T, _io_top_imm_T_1) @[Decode.scala 155:48]
    node _io_top_imm_T_3 = mux(io_count_done, signbit, _io_top_imm_T_2) @[Decode.scala 155:20]
    node _io_alu_opBIsRS2_T = bits(opcode, 3, 3) @[Decode.scala 159:28]
    node _io_top_rdAluEn_T = bits(opcode, 0, 0) @[Decode.scala 161:28]
    node _io_top_rdAluEn_T_1 = eq(_io_top_rdAluEn_T, UInt<1>("h0")) @[Decode.scala 161:21]
    node _io_top_rdAluEn_T_2 = bits(opcode, 2, 2) @[Decode.scala 161:41]
    node _io_top_rdAluEn_T_3 = and(_io_top_rdAluEn_T_1, _io_top_rdAluEn_T_2) @[Decode.scala 161:32]
    node _io_top_rdAluEn_T_4 = bits(opcode, 4, 4) @[Decode.scala 161:55]
    node _io_top_rdAluEn_T_5 = eq(_io_top_rdAluEn_T_4, UInt<1>("h0")) @[Decode.scala 161:48]
    node _io_top_rdAluEn_T_6 = and(_io_top_rdAluEn_T_3, _io_top_rdAluEn_T_5) @[Decode.scala 161:45]
    node _io_alu_boolOp_WIRE = _io_alu_boolOp_T @[Decode.scala 139:{36,36}]
    node _io_alu_boolOp_WIRE_1 = _io_alu_boolOp_WIRE @[Decode.scala 139:{36,36}]
    node _io_alu_rdSelect_WIRE = _io_alu_rdSelect_T_2 @[Decode.scala 145:{28,28}]
    node _io_alu_rdSelect_WIRE_1 = _io_alu_rdSelect_WIRE @[Decode.scala 145:{28,28}]
    io_state_takeBranch <= _io_state_takeBranch_T_8 @[Decode.scala 85:23]
    io_state_eOp <= _io_state_eOp_T_7 @[Decode.scala 58:16]
    io_state_eBreak <= op20 @[Decode.scala 61:19]
    io_state_branchOp <= _io_state_branchOp_T_3 @[Decode.scala 54:21]
    io_state_memOp <= _io_state_memOp_T_7 @[Decode.scala 50:18]
    io_state_shiftOp <= _io_state_shiftOp_T_2 @[Decode.scala 52:21]
    io_state_sltOp <= _io_state_sltOp_T_2 @[Decode.scala 53:21]
    io_state_rdOp <= _io_state_rdOp_T_18 @[Decode.scala 100:17]
    io_bufreg_loop <= opOrOpimm @[Decode.scala 72:18]
    io_bufreg_rs1En <= _io_bufreg_rs1En_T_6 @[Decode.scala 68:19]
    io_bufreg_immEn <= _io_bufreg_immEn_T_1 @[Decode.scala 69:19]
    io_bufreg_clearLsb <= _io_bufreg_clearLsb_T_6 @[Decode.scala 77:22]
    io_control_jalOrJalr <= _io_control_jalOrJalr_T_2 @[Decode.scala 88:24]
    io_control_uType <= _io_control_uType_T_5 @[Decode.scala 87:24]
    io_control_pcRel <= _io_control_pcRel_T_7 @[Decode.scala 92:20]
    io_control_mRet <= _io_control_mRet_T_6 @[Decode.scala 94:19]
    io_alu_doSubtract <= _io_alu_doSubtract_T_1 @[Decode.scala 103:21]
    io_alu_boolOp <= _io_alu_boolOp_WIRE_1 @[Decode.scala 139:17]
    io_alu_cmpEqual <= _io_alu_cmpEqual_T_1 @[Decode.scala 135:19]
    io_alu_cmpUnsigned <= _io_alu_cmpUnsigned_T_6 @[Decode.scala 136:22]
    io_alu_shiftSigned <= imm30 @[Decode.scala 137:22]
    io_alu_shiftRight <= _io_alu_shiftRight_T @[Decode.scala 138:21]
    io_alu_rdSelect <= _io_alu_rdSelect_WIRE_1 @[Decode.scala 145:19]
    io_alu_opBIsRS2 <= _io_alu_opBIsRS2_T @[Decode.scala 159:19]
    io_rf_rdAddress <= io_rf_rdAddress_r @[Decode.scala 15:20]
    io_rf_rs1Address <= rs1Address @[Decode.scala 17:20]
    io_rf_rs2Address <= io_rf_rs2Address_r @[Decode.scala 18:20]
    io_rf_csrAddress <= _io_rf_csrAddress_T_6 @[Decode.scala 130:20]
    io_rf_csrEnable <= _io_rf_csrEnable_T @[Decode.scala 123:19]
    io_mem_memOp <= io_state_memOp @[Decode.scala 51:18]
    io_mem_signed <= _io_mem_signed_T_1 @[Decode.scala 148:17]
    io_mem_word <= _io_mem_word_T @[Decode.scala 149:15]
    io_mem_half <= _io_mem_half_T @[Decode.scala 150:15]
    io_mem_cmd <= _io_mem_cmd_T @[Decode.scala 147:14]
    io_csr_mStatusEn <= _io_csr_mStatusEn_T_3 @[Decode.scala 124:20]
    io_csr_mieEn <= _io_csr_mieEn_T_4 @[Decode.scala 125:20]
    io_csr_mcauseEn <= _io_csr_mcauseEn_T_2 @[Decode.scala 126:20]
    io_csr_source <= _io_csr_source_T @[Decode.scala 127:17]
    io_csr_dSel <= _io_csr_dSel_T @[Decode.scala 128:15]
    io_csr_imm <= _io_csr_imm_T @[Decode.scala 129:14]
    io_csr_eOp <= io_state_eOp @[Decode.scala 59:14]
    io_csr_eBreak <= io_state_eBreak @[Decode.scala 62:17]
    io_csr_mRet <= io_control_mRet @[Decode.scala 95:15]
    io_top_imm <= _io_top_imm_T_3 @[Decode.scala 155:14]
    io_top_rdCsrEn <= csrOp @[Decode.scala 121:18]
    io_top_rdAluEn <= _io_top_rdAluEn_T_6 @[Decode.scala 161:18]
    io_rf_rdAddress_r <= _GEN_0
    rs1Address <= _GEN_22
    io_rf_rs2Address_r <= _GEN_2
    funct3 <= _GEN_3
    imm30 <= _GEN_4
    opcode <= _GEN_5
    op20 <= _GEN_6
    op21 <= _GEN_7
    op22 <= _GEN_8
    op26 <= _GEN_9
    signbit <= _GEN_10
    imm19_12_20 <= _GEN_17
    imm7 <= _GEN_18
    imm30_25 <= _GEN_19
    imm24_20 <= _GEN_20
    imm11_7 <= _GEN_21

  module BufReg :
    input clock : Clock
    input reset : UInt<1>
    input io_count_enabled : UInt<1>
    input io_count_init : UInt<1>
    input io_count_count0 : UInt<1>
    input io_count_count0To3 : UInt<1>
    input io_count_count12To31 : UInt<1>
    input io_count_count1 : UInt<1>
    input io_count_count2 : UInt<1>
    input io_count_count3 : UInt<1>
    input io_count_count7 : UInt<1>
    input io_count_done : UInt<1>
    input io_decode_loop : UInt<1>
    input io_decode_rs1En : UInt<1>
    input io_decode_immEn : UInt<1>
    input io_decode_clearLsb : UInt<1>
    input io_state_hold : UInt<1>
    input io_imm : UInt<1>
    input io_rs1 : UInt<1>
    output io_lsb : UInt<2>
    output io_dataBusAddress : UInt<32>
    output io_dataOut : UInt<1>

    node enabled = eq(io_state_hold, UInt<1>("h0")) @[BufReg.scala 13:17]
    node clearLsb = and(io_count_count0, io_decode_clearLsb) @[BufReg.scala 15:34]
    reg carry : UInt<1>, clock with :
      reset => (UInt<1>("h0"), carry) @[BufReg.scala 17:18]
    node _c_q_T = and(io_rs1, io_decode_rs1En) @[BufReg.scala 18:21]
    node _c_q_T_1 = and(io_imm, io_decode_immEn) @[BufReg.scala 18:51]
    node _c_q_T_2 = eq(clearLsb, UInt<1>("h0")) @[BufReg.scala 18:71]
    node _c_q_T_3 = and(_c_q_T_1, _c_q_T_2) @[BufReg.scala 18:69]
    node _c_q_T_4 = add(_c_q_T, _c_q_T_3) @[BufReg.scala 18:40]
    node _c_q_T_5 = add(_c_q_T_4, carry) @[BufReg.scala 18:82]
    node c_q = tail(_c_q_T_5, 1) @[BufReg.scala 18:82]
    node q = bits(c_q, 0, 0) @[BufReg.scala 19:14]
    node _carry_T = bits(c_q, 1, 1) @[BufReg.scala 21:15]
    node _carry_T_1 = and(_carry_T, io_count_init) @[BufReg.scala 21:19]
    reg data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), data) @[BufReg.scala 23:17]
    node _newData_T = eq(io_count_init, UInt<1>("h0")) @[BufReg.scala 24:39]
    node _newData_T_1 = and(io_decode_loop, _newData_T) @[BufReg.scala 24:36]
    node _newData_T_2 = bits(data, 0, 0) @[BufReg.scala 24:59]
    node newData = mux(_newData_T_1, _newData_T_2, q) @[BufReg.scala 24:20]
    node _data_T = bits(data, 31, 1) @[BufReg.scala 25:42]
    node _data_T_1 = cat(newData, _data_T) @[BufReg.scala 25:35]
    node _GEN_0 = mux(enabled, _data_T_1, data) @[BufReg.scala 23:17 25:{17,24}]
    node _io_dataOut_T = bits(data, 0, 0) @[BufReg.scala 26:21]
    node _io_dataBusAddress_T = bits(data, 31, 2) @[BufReg.scala 27:28]
    node _io_dataBusAddress_T_1 = cat(_io_dataBusAddress_T, UInt<2>("h0")) @[BufReg.scala 27:35]
    reg lsb_1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lsb_1) @[BufReg.scala 29:28]
    reg lsb_0 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), lsb_0) @[BufReg.scala 29:44]
    node _io_lsb_T = cat(lsb_1, lsb_0) @[BufReg.scala 30:19]
    node _GEN_1 = mux(io_count_count0, q, lsb_0) @[BufReg.scala 31:25 32:11 29:44]
    node _GEN_2 = mux(io_count_count1, q, lsb_1) @[BufReg.scala 34:25 35:11 29:28]
    io_lsb <= _io_lsb_T @[BufReg.scala 30:10]
    io_dataBusAddress <= _io_dataBusAddress_T_1 @[BufReg.scala 27:21]
    io_dataOut <= _io_dataOut_T @[BufReg.scala 26:14]
    carry <= _carry_T_1 @[BufReg.scala 21:9]
    data <= _GEN_0
    lsb_1 <= _GEN_2
    lsb_0 <= _GEN_1

  module Control :
    input clock : Clock
    input reset : UInt<1>
    input io_state_pcEnable : UInt<1>
    input io_state_jump : UInt<1>
    input io_state_trap : UInt<1>
    input io_count_enabled : UInt<1>
    input io_count_init : UInt<1>
    input io_count_count0 : UInt<1>
    input io_count_count0To3 : UInt<1>
    input io_count_count12To31 : UInt<1>
    input io_count_count1 : UInt<1>
    input io_count_count2 : UInt<1>
    input io_count_count3 : UInt<1>
    input io_count_count7 : UInt<1>
    input io_count_done : UInt<1>
    input io_decode_jalOrJalr : UInt<1>
    input io_decode_uType : UInt<1>
    input io_decode_pcRel : UInt<1>
    input io_decode_mRet : UInt<1>
    output io_ibus_address : UInt<32>
    output io_ibus_cyc : UInt<1>
    input io_ibus_ack : UInt<1>
    input io_data_imm : UInt<1>
    input io_data_buf : UInt<1>
    input io_data_csrPc : UInt<1>
    output io_data_rd : UInt<1>
    output io_data_badPc : UInt<1>

    node trap = or(io_state_trap, io_decode_mRet) @[Control.scala 14:28]
    reg enablePc : UInt<1>, clock with :
      reset => (UInt<1>("h0"), enablePc) @[Control.scala 16:25]
    reg pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), pc) @[Control.scala 17:19]
    node offsetA = and(io_decode_pcRel, pc) @[Control.scala 19:33]
    node _offsetB_T = and(io_data_imm, io_count_count12To31) @[Control.scala 20:50]
    node offsetB = mux(io_decode_uType, _offsetB_T, io_data_buf) @[Control.scala 20:20]
    reg pcPlusOffsetCarry : UInt<1>, clock with :
      reset => (UInt<1>("h0"), pcPlusOffsetCarry) @[Control.scala 21:30]
    node _pcPlusOffset_T = add(offsetA, offsetB) @[Control.scala 22:30]
    node _pcPlusOffset_T_1 = add(_pcPlusOffset_T, pcPlusOffsetCarry) @[Control.scala 22:41]
    node pcPlusOffset = tail(_pcPlusOffset_T_1, 1) @[Control.scala 22:41]
    node _pcPlusOffsetCarry_T = bits(pcPlusOffset, 1, 1) @[Control.scala 23:56]
    node _pcPlusOffsetCarry_T_1 = and(io_state_pcEnable, _pcPlusOffsetCarry_T) @[Control.scala 23:42]
    node _pcPlusOffsetAligned_T = bits(pcPlusOffset, 0, 0) @[Control.scala 24:41]
    node pcPlusOffsetAligned = and(_pcPlusOffsetAligned_T, enablePc) @[Control.scala 24:45]
    reg pcPlus4Carry : UInt<1>, clock with :
      reset => (UInt<1>("h0"), pcPlus4Carry) @[Control.scala 28:25]
    node _pcPlus4_T = bits(pc, 0, 0) @[Control.scala 29:19]
    node _pcPlus4_T_1 = add(_pcPlus4_T, io_count_count2) @[Control.scala 29:23]
    node _pcPlus4_T_2 = add(_pcPlus4_T_1, pcPlus4Carry) @[Control.scala 29:32]
    node pcPlus4 = tail(_pcPlus4_T_2, 1) @[Control.scala 29:32]
    node _pcPlus4Carry_T = bits(pcPlus4, 1, 1) @[Control.scala 30:46]
    node _pcPlus4Carry_T_1 = and(io_state_pcEnable, _pcPlus4Carry_T) @[Control.scala 30:37]
    node _newPc_T = and(io_data_csrPc, enablePc) @[Control.scala 35:30]
    node _newPc_T_1 = bits(pcPlus4, 0, 0) @[Control.scala 39:23]
    node _GEN_0 = mux(io_state_jump, pcPlusOffsetAligned, _newPc_T_1) @[Control.scala 36:32 37:13 39:13]
    node _GEN_1 = mux(io_state_trap, _newPc_T, _GEN_0) @[Control.scala 34:25 35:13]
    node _io_data_rd_T = and(io_decode_uType, pcPlusOffsetAligned) @[Control.scala 49:34]
    node _io_data_rd_T_1 = bits(pcPlus4, 0, 0) @[Control.scala 49:67]
    node _io_data_rd_T_2 = and(_io_data_rd_T_1, io_decode_jalOrJalr) @[Control.scala 49:71]
    node _io_data_rd_T_3 = or(_io_data_rd_T, _io_data_rd_T_2) @[Control.scala 49:57]
    node _io_ibus_cyc_T = eq(io_state_pcEnable, UInt<1>("h0")) @[Control.scala 51:30]
    node _io_ibus_cyc_T_1 = and(enablePc, _io_ibus_cyc_T) @[Control.scala 51:27]
    node _pc_T = bits(pc, 31, 1) @[Control.scala 56:22]
    node newPc = _GEN_1 @[Control.scala 32:19]
    node _pc_T_1 = cat(newPc, _pc_T) @[Control.scala 56:17]
    node _T = and(io_ibus_cyc, io_ibus_ack) @[Control.scala 57:27]
    node _GEN_2 = mux(_T, UInt<1>("h0"), enablePc) @[Control.scala 57:43 58:14 16:25]
    node _GEN_3 = mux(io_state_pcEnable, UInt<1>("h1"), _GEN_2) @[Control.scala 54:27 55:14]
    node _GEN_4 = mux(io_state_pcEnable, _pc_T_1, pc) @[Control.scala 17:19 54:27 56:8]
    io_ibus_address <= pc @[Control.scala 52:19]
    io_ibus_cyc <= _io_ibus_cyc_T_1 @[Control.scala 51:15]
    io_data_rd <= _io_data_rd_T_3 @[Control.scala 49:14]
    io_data_badPc <= pcPlusOffsetAligned @[Control.scala 25:17]
    enablePc <= mux(reset, UInt<1>("h1"), _GEN_3) @[Control.scala 16:{25,25}]
    pc <= mux(reset, UInt<32>("h0"), _GEN_4) @[Control.scala 17:{19,19}]
    pcPlusOffsetCarry <= _pcPlusOffsetCarry_T_1 @[Control.scala 23:21]
    pcPlus4Carry <= _pcPlus4Carry_T_1 @[Control.scala 30:16]

  module SerialShift :
    input clock : Clock
    input reset : UInt<1>
    input io_load : UInt<1>
    input io_shiftAmount : UInt<5>
    input io_shamt_msb : UInt<1>
    input io_signbit : UInt<1>
    input io_right : UInt<1>
    output io_done : UInt<1>
    input io_d : UInt<1>
    output io_q : UInt<1>

    reg cnt : UInt<6>, clock with :
      reset => (UInt<1>("h0"), cnt) @[Alu.scala 121:16]
    reg signbit : UInt<1>, clock with :
      reset => (UInt<1>("h0"), signbit) @[Alu.scala 122:20]
    node _wrapped_T = head(cnt, 1) @[Alu.scala 123:33]
    node _wrapped_T_1 = eq(io_right, UInt<1>("h0")) @[Alu.scala 123:54]
    node _wrapped_T_2 = and(io_shamt_msb, _wrapped_T_1) @[Alu.scala 123:52]
    node _wrapped_T_3 = or(_wrapped_T, _wrapped_T_2) @[Alu.scala 123:37]
    reg wrapped : UInt<1>, clock with :
      reset => (UInt<1>("h0"), wrapped) @[Alu.scala 123:24]
    node _signbit_T = and(io_signbit, io_right) @[Alu.scala 127:27]
    node _cnt_T = add(cnt, UInt<1>("h1")) @[Alu.scala 129:16]
    node _cnt_T_1 = tail(_cnt_T, 1) @[Alu.scala 129:16]
    node _GEN_0 = mux(io_load, UInt<1>("h0"), _cnt_T_1) @[Alu.scala 125:17 126:9 129:9]
    node _GEN_1 = mux(io_load, _signbit_T, signbit) @[Alu.scala 125:17 127:13 122:20]
    node _io_done_T = bits(cnt, 4, 0) @[Alu.scala 132:17]
    node _io_done_T_1 = eq(_io_done_T, io_shiftAmount) @[Alu.scala 132:24]
    node _io_q_T = neq(io_right, wrapped) @[Alu.scala 133:24]
    node _io_q_T_1 = mux(_io_q_T, io_d, signbit) @[Alu.scala 133:14]
    io_done <= _io_done_T_1 @[Alu.scala 132:11]
    io_q <= _io_q_T_1 @[Alu.scala 133:8]
    cnt <= _GEN_0
    signbit <= _GEN_1
    wrapped <= _wrapped_T_3 @[Alu.scala 123:24]

  module Alu :
    input clock : Clock
    input reset : UInt<1>
    input io_decode_doSubtract : UInt<1>
    input io_decode_boolOp : UInt<2>
    input io_decode_cmpEqual : UInt<1>
    input io_decode_cmpUnsigned : UInt<1>
    input io_decode_shiftSigned : UInt<1>
    input io_decode_shiftRight : UInt<1>
    input io_decode_rdSelect : UInt<4>
    input io_decode_opBIsRS2 : UInt<1>
    output io_decode_cmpResult : UInt<1>
    input io_data_rs1 : UInt<1>
    input io_data_rs2 : UInt<1>
    input io_data_imm : UInt<1>
    input io_data_buffer : UInt<1>
    output io_data_rd : UInt<1>
    input io_count_enabled : UInt<1>
    input io_count_init : UInt<1>
    input io_count_count0 : UInt<1>
    input io_count_count0To3 : UInt<1>
    input io_count_count12To31 : UInt<1>
    input io_count_count1 : UInt<1>
    input io_count_count2 : UInt<1>
    input io_count_count3 : UInt<1>
    input io_count_count7 : UInt<1>
    input io_count_done : UInt<1>
    input io_state_shiftAmountEnable : UInt<1>
    output io_state_shiftDone : UInt<1>

    inst shift of SerialShift @[Alu.scala 66:21]
    node operandB = mux(io_decode_opBIsRS2, io_data_rs2, io_data_imm) @[Alu.scala 46:21]
    reg negativeBCarry : UInt<1>, clock with :
      reset => (UInt<1>("h0"), negativeBCarry) @[Alu.scala 50:27]
    node _negativeBCarryAndResult_T = not(operandB) @[Alu.scala 51:33]
    node _negativeBCarryAndResult_T_1 = add(_negativeBCarryAndResult_T, io_count_count0) @[Alu.scala 51:43]
    node _negativeBCarryAndResult_T_2 = add(_negativeBCarryAndResult_T_1, negativeBCarry) @[Alu.scala 51:52]
    node negativeBCarryAndResult = tail(_negativeBCarryAndResult_T_2, 1) @[Alu.scala 51:52]
    node _negativeBCarry_T = bits(negativeBCarryAndResult, 1, 1) @[Alu.scala 52:63]
    node _negativeBCarry_T_1 = and(io_count_enabled, _negativeBCarry_T) @[Alu.scala 52:38]
    node negativeB = bits(negativeBCarryAndResult, 0, 0) @[Alu.scala 53:42]
    node addB = mux(io_decode_doSubtract, negativeB, operandB) @[Alu.scala 57:17]
    reg addCarry : UInt<1>, clock with :
      reset => (UInt<1>("h0"), addCarry) @[Alu.scala 58:21]
    node _addCarryNextAndResult_T = add(io_data_rs1, addB) @[Alu.scala 59:43]
    node _addCarryNextAndResult_T_1 = add(_addCarryNextAndResult_T, addCarry) @[Alu.scala 59:51]
    node addCarryNextAndResult = tail(_addCarryNextAndResult_T_1, 1) @[Alu.scala 59:51]
    node _addCarry_T = bits(addCarryNextAndResult, 1, 1) @[Alu.scala 60:55]
    node _addCarry_T_1 = and(io_count_enabled, _addCarry_T) @[Alu.scala 60:32]
    node resultAdd = bits(addCarryNextAndResult, 0, 0) @[Alu.scala 61:40]
    node shiftAmountSerial = mux(io_decode_shiftRight, operandB, negativeB) @[Alu.scala 64:30]
    reg shiftAmount : UInt<5>, clock with :
      reset => (UInt<1>("h0"), shiftAmount) @[Alu.scala 65:24]
    reg shiftAmountMSB : UInt<1>, clock with :
      reset => (UInt<1>("h0"), shiftAmountMSB) @[Alu.scala 69:27]
    node _shiftAmount_T = bits(shiftAmount, 4, 1) @[Alu.scala 72:52]
    node _shiftAmount_T_1 = cat(shiftAmountSerial, _shiftAmount_T) @[Alu.scala 72:38]
    node _GEN_0 = mux(io_state_shiftAmountEnable, negativeB, shiftAmountMSB) @[Alu.scala 70:36 71:20 69:27]
    node _GEN_1 = mux(io_state_shiftAmountEnable, _shiftAmount_T_1, shiftAmount) @[Alu.scala 70:36 72:17 65:24]
    node _shift_io_signbit_T = and(io_decode_shiftSigned, io_data_rs1) @[Alu.scala 75:45]
    node equal = eq(io_data_rs1, operandB) @[Alu.scala 82:27]
    reg equalBuf : UInt<1>, clock with :
      reset => (UInt<1>("h0"), equalBuf) @[Alu.scala 83:21]
    node resultEqual = and(equal, equalBuf) @[Alu.scala 84:27]
    node _equalBuf_T = not(io_count_enabled) @[Alu.scala 85:29]
    node _equalBuf_T_1 = or(resultEqual, _equalBuf_T) @[Alu.scala 85:27]
    reg ltBuf : UInt<1>, clock with :
      reset => (UInt<1>("h0"), ltBuf) @[Alu.scala 88:18]
    node _ltSign_T = eq(io_decode_cmpUnsigned, UInt<1>("h0")) @[Alu.scala 89:32]
    node ltSign = and(io_count_done, _ltSign_T) @[Alu.scala 89:30]
    node _resultLt_T = xor(operandB, ltSign) @[Alu.scala 90:45]
    node resultLt = mux(equal, ltBuf, _resultLt_T) @[Alu.scala 90:21]
    node _ltBuf_T = and(resultLt, io_count_enabled) @[Alu.scala 91:21]
    reg resultLtBuf : UInt<1>, clock with :
      reset => (UInt<1>("h0"), resultLtBuf) @[Alu.scala 92:24]
    node _GEN_2 = mux(io_count_enabled, resultLt, resultLtBuf) @[Alu.scala 92:24 93:{26,40}]
    node _io_decode_cmpResult_T = mux(io_decode_cmpEqual, resultEqual, resultLt) @[Alu.scala 95:29]
    node _resultBool_T = asUInt(io_decode_boolOp) @[Alu.scala 99:59]
    node _resultBool_T_1 = cat(_resultBool_T, io_data_rs1) @[Alu.scala 99:62]
    node _resultBool_T_2 = cat(_resultBool_T_1, operandB) @[Alu.scala 99:77]
    node _resultBool_T_3 = dshr(UInt<16>("h8e96"), _resultBool_T_2) @[Alu.scala 99:35]
    node resultBool = bits(_resultBool_T_3, 0, 0) @[Alu.scala 99:35]
    node _io_data_rd_T = asUInt(io_decode_rdSelect) @[Alu.scala 103:31]
    node _io_data_rd_T_1 = bits(_io_data_rd_T, 0, 0) @[Alu.scala 103:33]
    node _io_data_rd_T_2 = and(_io_data_rd_T_1, resultAdd) @[Alu.scala 103:37]
    node _io_data_rd_T_3 = asUInt(io_decode_rdSelect) @[Alu.scala 104:31]
    node _io_data_rd_T_4 = bits(_io_data_rd_T_3, 1, 1) @[Alu.scala 104:33]
    node _io_data_rd_T_5 = and(_io_data_rd_T_4, shift.io_q) @[Alu.scala 104:37]
    node _io_data_rd_T_6 = or(_io_data_rd_T_2, _io_data_rd_T_5) @[Alu.scala 103:60]
    node _io_data_rd_T_7 = asUInt(io_decode_rdSelect) @[Alu.scala 105:31]
    node _io_data_rd_T_8 = bits(_io_data_rd_T_7, 2, 2) @[Alu.scala 105:33]
    node _io_data_rd_T_9 = and(_io_data_rd_T_8, resultLtBuf) @[Alu.scala 105:37]
    node _io_data_rd_T_10 = and(_io_data_rd_T_9, io_count_count0) @[Alu.scala 105:51]
    node _io_data_rd_T_11 = or(_io_data_rd_T_6, _io_data_rd_T_10) @[Alu.scala 104:60]
    node _io_data_rd_T_12 = asUInt(io_decode_rdSelect) @[Alu.scala 106:31]
    node _io_data_rd_T_13 = bits(_io_data_rd_T_12, 3, 3) @[Alu.scala 106:33]
    node _io_data_rd_T_14 = and(_io_data_rd_T_13, resultBool) @[Alu.scala 106:37]
    node _io_data_rd_T_15 = or(_io_data_rd_T_11, _io_data_rd_T_14) @[Alu.scala 105:60]
    io_decode_cmpResult <= _io_decode_cmpResult_T @[Alu.scala 95:23]
    io_data_rd <= _io_data_rd_T_15 @[Alu.scala 102:14]
    io_state_shiftDone <= shift.io_done @[Alu.scala 78:22]
    negativeBCarry <= _negativeBCarry_T_1 @[Alu.scala 52:18]
    addCarry <= _addCarry_T_1 @[Alu.scala 60:12]
    shiftAmount <= _GEN_1
    shift.clock <= clock
    shift.reset <= reset
    shift.io_load <= io_count_init @[Alu.scala 67:17]
    shift.io_shiftAmount <= shiftAmount @[Alu.scala 68:24]
    shift.io_shamt_msb <= shiftAmountMSB @[Alu.scala 74:22]
    shift.io_signbit <= _shift_io_signbit_T @[Alu.scala 75:20]
    shift.io_right <= io_decode_shiftRight @[Alu.scala 76:18]
    shift.io_d <= io_data_buffer @[Alu.scala 77:14]
    shiftAmountMSB <= _GEN_0
    equalBuf <= _equalBuf_T_1 @[Alu.scala 85:12]
    ltBuf <= _ltBuf_T @[Alu.scala 91:9]
    resultLtBuf <= _GEN_2

  module RegisterFileInterface :
    input clock : Clock
    input reset : UInt<1>
    output io_rf_write0_addr : UInt<6>
    output io_rf_write0_enable : UInt<1>
    output io_rf_write0_data : UInt<1>
    output io_rf_write1_addr : UInt<6>
    output io_rf_write1_enable : UInt<1>
    output io_rf_write1_data : UInt<1>
    output io_rf_read0_addr : UInt<6>
    input io_rf_read0_data : UInt<1>
    output io_rf_read1_addr : UInt<6>
    input io_rf_read1_data : UInt<1>
    input io_trap_doTrap : UInt<1>
    input io_trap_mRet : UInt<1>
    input io_trap_mePC : UInt<1>
    input io_trap_memMisaligned : UInt<1>
    input io_trap_bufRegData : UInt<1>
    input io_trap_badPC : UInt<1>
    output io_trap_csrPC : UInt<1>
    output io_csr_readData : UInt<1>
    input io_csr_writeData : UInt<1>
    input io_rd_writeEnable : UInt<1>
    input io_rd_controlData : UInt<1>
    input io_rd_aluData : UInt<1>
    input io_rd_aluEnable : UInt<1>
    input io_rd_csrData : UInt<1>
    input io_rd_csrEnable : UInt<1>
    input io_rd_memData : UInt<1>
    output io_rs1Data : UInt<1>
    output io_rs2Data : UInt<1>
    input io_decode_rdAddress : UInt<5>
    input io_decode_rs1Address : UInt<5>
    input io_decode_rs2Address : UInt<5>
    input io_decode_csrAddress : UInt<2>
    input io_decode_csrEnable : UInt<1>

    node MepcAddress = cat(UInt<4>("h8"), UInt<2>("h2")) @[RegisterFileInterface.scala 22:31]
    node MtvalAddress = cat(UInt<4>("h8"), UInt<2>("h3")) @[RegisterFileInterface.scala 23:32]
    node MtvecAddress = cat(UInt<4>("h8"), UInt<2>("h1")) @[RegisterFileInterface.scala 24:32]
    node _rd_T = and(io_rd_aluData, io_rd_aluEnable) @[RegisterFileInterface.scala 30:22]
    node _rd_T_1 = or(io_rd_controlData, _rd_T) @[RegisterFileInterface.scala 29:32]
    node _rd_T_2 = and(io_rd_csrData, io_rd_csrEnable) @[RegisterFileInterface.scala 31:22]
    node _rd_T_3 = or(_rd_T_1, _rd_T_2) @[RegisterFileInterface.scala 30:41]
    node rd = or(_rd_T_3, io_rd_memData) @[RegisterFileInterface.scala 31:41]
    node mtval = mux(io_trap_memMisaligned, io_trap_bufRegData, io_trap_badPC) @[RegisterFileInterface.scala 35:20]
    node _io_rf_write0_data_T = mux(io_trap_doTrap, mtval, rd) @[RegisterFileInterface.scala 36:29]
    node _io_rf_write0_addr_T = mux(io_trap_doTrap, MtvalAddress, io_decode_rdAddress) @[RegisterFileInterface.scala 37:29]
    node _io_rf_write0_enable_T = or(io_trap_doTrap, io_rd_writeEnable) @[RegisterFileInterface.scala 38:43]
    node _io_rf_write1_data_T = mux(io_trap_doTrap, io_trap_mePC, io_csr_writeData) @[RegisterFileInterface.scala 41:29]
    node _io_rf_write1_addr_T = mux(io_trap_doTrap, MepcAddress, io_decode_csrAddress) @[RegisterFileInterface.scala 42:29]
    node _io_rf_write1_enable_T = or(io_trap_doTrap, io_decode_csrEnable) @[RegisterFileInterface.scala 43:43]
    node _io_rf_read1_addr_T = cat(UInt<4>("h8"), io_decode_csrAddress) @[RegisterFileInterface.scala 55:40]
    node _io_rf_read1_addr_T_1 = mux(io_decode_csrEnable, _io_rf_read1_addr_T, io_decode_rs2Address) @[Mux.scala 98:16]
    node _io_rf_read1_addr_T_2 = mux(io_trap_mRet, MepcAddress, _io_rf_read1_addr_T_1) @[Mux.scala 98:16]
    node _io_rf_read1_addr_T_3 = mux(io_trap_doTrap, MtvecAddress, _io_rf_read1_addr_T_2) @[Mux.scala 98:16]
    node _io_csr_readData_T = and(io_rf_read1_data, io_decode_csrEnable) @[RegisterFileInterface.scala 58:41]
    io_rf_write0_addr <= _io_rf_write0_addr_T @[RegisterFileInterface.scala 37:23]
    io_rf_write0_enable <= _io_rf_write0_enable_T @[RegisterFileInterface.scala 38:25]
    io_rf_write0_data <= _io_rf_write0_data_T @[RegisterFileInterface.scala 36:23]
    io_rf_write1_addr <= _io_rf_write1_addr_T @[RegisterFileInterface.scala 42:23]
    io_rf_write1_enable <= _io_rf_write1_enable_T @[RegisterFileInterface.scala 43:25]
    io_rf_write1_data <= _io_rf_write1_data_T @[RegisterFileInterface.scala 41:23]
    io_rf_read0_addr <= io_decode_rs1Address @[RegisterFileInterface.scala 48:22]
    io_rf_read1_addr <= _io_rf_read1_addr_T_3 @[RegisterFileInterface.scala 52:22]
    io_trap_csrPC <= io_rf_read1_data @[RegisterFileInterface.scala 59:19]
    io_csr_readData <= _io_csr_readData_T @[RegisterFileInterface.scala 58:21]
    io_rs1Data <= io_rf_read0_data @[RegisterFileInterface.scala 49:16]
    io_rs2Data <= io_rf_read1_data @[RegisterFileInterface.scala 57:16]

  module MemoryInterface :
    input clock : Clock
    input reset : UInt<1>
    input io_enabled : UInt<1>
    input io_decode_memOp : UInt<1>
    input io_decode_signed : UInt<1>
    input io_decode_word : UInt<1>
    input io_decode_half : UInt<1>
    input io_decode_cmd : UInt<1>
    input io_state_byteCount : UInt<2>
    output io_state_misaligned : UInt<1>
    input io_lsb : UInt<2>
    input io_rs2 : UInt<1>
    output io_rd : UInt<1>
    input io_dbus_rdt : UInt<32>
    input io_dbus_ack : UInt<1>
    output io_dbus_dat : UInt<32>
    output io_dbus_sel : UInt<4>

    node byteCountPlusLsb = add(io_state_byteCount, io_lsb) @[MemoryInterface.scala 17:45]
    reg data : UInt<32>, clock with :
      reset => (UInt<1>("h0"), data) @[MemoryInterface.scala 19:17]
    node _dataEnabled_T = bits(byteCountPlusLsb, 2, 2) @[MemoryInterface.scala 20:52]
    node _dataEnabled_T_1 = eq(_dataEnabled_T, UInt<1>("h0")) @[MemoryInterface.scala 20:35]
    node dataEnabled = and(io_enabled, _dataEnabled_T_1) @[MemoryInterface.scala 20:32]
    node _data_T = bits(data, 31, 1) @[MemoryInterface.scala 21:45]
    node _data_T_1 = cat(io_rs2, _data_T) @[MemoryInterface.scala 21:38]
    node _GEN_0 = mux(dataEnabled, _data_T_1, data) @[MemoryInterface.scala 19:17 21:{21,28}]
    node _GEN_1 = mux(io_dbus_ack, io_dbus_rdt, _GEN_0) @[MemoryInterface.scala 22:{21,28}]
    node _dataCur_T = eq(io_lsb, UInt<2>("h3")) @[MemoryInterface.scala 25:14]
    node _dataCur_T_1 = bits(data, 24, 24) @[MemoryInterface.scala 25:30]
    node _dataCur_T_2 = and(_dataCur_T, _dataCur_T_1) @[MemoryInterface.scala 25:23]
    node _dataCur_T_3 = eq(io_lsb, UInt<2>("h2")) @[MemoryInterface.scala 26:14]
    node _dataCur_T_4 = bits(data, 16, 16) @[MemoryInterface.scala 26:30]
    node _dataCur_T_5 = and(_dataCur_T_3, _dataCur_T_4) @[MemoryInterface.scala 26:23]
    node _dataCur_T_6 = or(_dataCur_T_2, _dataCur_T_5) @[MemoryInterface.scala 25:36]
    node _dataCur_T_7 = eq(io_lsb, UInt<1>("h1")) @[MemoryInterface.scala 27:14]
    node _dataCur_T_8 = bits(data, 8, 8) @[MemoryInterface.scala 27:30]
    node _dataCur_T_9 = and(_dataCur_T_7, _dataCur_T_8) @[MemoryInterface.scala 27:23]
    node _dataCur_T_10 = or(_dataCur_T_6, _dataCur_T_9) @[MemoryInterface.scala 26:36]
    node _dataCur_T_11 = eq(io_lsb, UInt<1>("h0")) @[MemoryInterface.scala 28:14]
    node _dataCur_T_12 = bits(data, 0, 0) @[MemoryInterface.scala 28:30]
    node _dataCur_T_13 = and(_dataCur_T_11, _dataCur_T_12) @[MemoryInterface.scala 28:23]
    node dataCur = or(_dataCur_T_10, _dataCur_T_13) @[MemoryInterface.scala 27:36]
    node _dataValid_T = eq(io_state_byteCount, UInt<1>("h0")) @[MemoryInterface.scala 29:57]
    node _dataValid_T_1 = or(io_decode_word, _dataValid_T) @[MemoryInterface.scala 29:34]
    node _dataValid_T_2 = bits(io_state_byteCount, 1, 1) @[MemoryInterface.scala 29:107]
    node _dataValid_T_3 = eq(_dataValid_T_2, UInt<1>("h0")) @[MemoryInterface.scala 29:88]
    node _dataValid_T_4 = and(io_decode_half, _dataValid_T_3) @[MemoryInterface.scala 29:85]
    node dataValid = or(_dataValid_T_1, _dataValid_T_4) @[MemoryInterface.scala 29:66]
    reg signBit : UInt<1>, clock with :
      reset => (UInt<1>("h0"), signBit) @[Reg.scala 15:16]
    node _GEN_2 = mux(dataValid, dataCur, signBit) @[Reg.scala 15:16 16:{19,23}]
    node _io_rd_T = and(signBit, io_decode_signed) @[MemoryInterface.scala 32:62]
    node _io_rd_T_1 = mux(dataValid, dataCur, _io_rd_T) @[MemoryInterface.scala 32:33]
    node _io_rd_T_2 = and(io_decode_memOp, _io_rd_T_1) @[MemoryInterface.scala 32:28]
    node _sel_3_T = eq(io_lsb, UInt<2>("h3")) @[MemoryInterface.scala 33:23]
    node _sel_3_T_1 = or(_sel_3_T, io_decode_word) @[MemoryInterface.scala 33:32]
    node _sel_3_T_2 = bits(io_lsb, 1, 1) @[MemoryInterface.scala 33:78]
    node _sel_3_T_3 = and(io_decode_half, _sel_3_T_2) @[MemoryInterface.scala 33:69]
    node sel_3 = or(_sel_3_T_1, _sel_3_T_3) @[MemoryInterface.scala 33:50]
    node _sel_2_T = eq(io_lsb, UInt<2>("h2")) @[MemoryInterface.scala 34:23]
    node sel_2 = or(_sel_2_T, io_decode_word) @[MemoryInterface.scala 34:32]
    node _sel_1_T = eq(io_lsb, UInt<1>("h1")) @[MemoryInterface.scala 35:23]
    node _sel_1_T_1 = or(_sel_1_T, io_decode_word) @[MemoryInterface.scala 35:32]
    node _sel_1_T_2 = bits(io_lsb, 1, 1) @[MemoryInterface.scala 35:79]
    node _sel_1_T_3 = eq(_sel_1_T_2, UInt<1>("h0")) @[MemoryInterface.scala 35:72]
    node _sel_1_T_4 = and(io_decode_half, _sel_1_T_3) @[MemoryInterface.scala 35:69]
    node sel_1 = or(_sel_1_T_1, _sel_1_T_4) @[MemoryInterface.scala 35:50]
    node sel_0 = eq(io_lsb, UInt<1>("h0")) @[MemoryInterface.scala 36:23]
    node _io_dbus_sel_T = cat(sel_3, sel_2) @[MemoryInterface.scala 37:24]
    node _io_dbus_sel_T_1 = cat(_io_dbus_sel_T, sel_1) @[MemoryInterface.scala 37:33]
    node _io_dbus_sel_T_2 = cat(_io_dbus_sel_T_1, sel_0) @[MemoryInterface.scala 37:42]
    node _misalign_T = bits(io_lsb, 0, 0) @[MemoryInterface.scala 42:35]
    node _misalign_T_1 = or(io_decode_word, io_decode_half) @[MemoryInterface.scala 42:58]
    node _misalign_T_2 = and(_misalign_T, _misalign_T_1) @[MemoryInterface.scala 42:39]
    node _misalign_T_3 = bits(io_lsb, 1, 1) @[MemoryInterface.scala 42:88]
    node _misalign_T_4 = and(_misalign_T_3, io_decode_word) @[MemoryInterface.scala 42:92]
    node _misalign_T_5 = or(_misalign_T_2, _misalign_T_4) @[MemoryInterface.scala 42:78]
    reg misalign : UInt<1>, clock with :
      reset => (UInt<1>("h0"), misalign) @[MemoryInterface.scala 42:27]
    node _io_state_misaligned_T = and(misalign, io_decode_memOp) @[MemoryInterface.scala 43:37]
    io_state_misaligned <= _io_state_misaligned_T @[MemoryInterface.scala 43:25]
    io_rd <= _io_rd_T_2 @[MemoryInterface.scala 32:9]
    io_dbus_dat <= data @[MemoryInterface.scala 39:15]
    io_dbus_sel <= _io_dbus_sel_T_2 @[MemoryInterface.scala 37:15]
    data <= _GEN_1
    signBit <= _GEN_2
    misalign <= _misalign_T_5 @[MemoryInterface.scala 42:27]

  module Csr :
    input clock : Clock
    input reset : UInt<1>
    input io_count_enabled : UInt<1>
    input io_count_init : UInt<1>
    input io_count_count0 : UInt<1>
    input io_count_count0To3 : UInt<1>
    input io_count_count12To31 : UInt<1>
    input io_count_count1 : UInt<1>
    input io_count_count2 : UInt<1>
    input io_count_count3 : UInt<1>
    input io_count_count7 : UInt<1>
    input io_count_done : UInt<1>
    input io_decode_mStatusEn : UInt<1>
    input io_decode_mieEn : UInt<1>
    input io_decode_mcauseEn : UInt<1>
    input io_decode_source : UInt<2>
    input io_decode_dSel : UInt<1>
    input io_decode_imm : UInt<1>
    input io_decode_eOp : UInt<1>
    input io_decode_eBreak : UInt<1>
    input io_decode_mRet : UInt<1>
    input io_memCmd : UInt<1>
    input io_memMisaligned : UInt<1>
    output io_state_newIrq : UInt<1>
    input io_state_trapTaken : UInt<1>
    input io_state_pendingIrq : UInt<1>
    input io_rf_readData : UInt<1>
    output io_rf_writeData : UInt<1>
    input io_dataIn : UInt<1>
    output io_dataOut : UInt<1>
    input io_timerInterrupt : UInt<1>

    reg mStatusMie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mStatusMie) @[Csr.scala 15:23]
    reg mStatusMpie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mStatusMpie) @[Csr.scala 16:24]
    reg mieMtie : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mieMtie) @[Csr.scala 17:20]
    node _mStatus_T = and(io_count_count2, mStatusMie) @[Csr.scala 18:41]
    reg mStatus : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mStatus) @[Csr.scala 18:24]
    reg mCause3_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), mCause3_0) @[Csr.scala 19:22]
    reg mCause31 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), mCause31) @[Csr.scala 20:21]
    node _mCause_T = bits(mCause3_0, 0, 0) @[Csr.scala 22:49]
    node _mCause_T_1 = mux(io_count_done, mCause31, UInt<1>("h0")) @[Csr.scala 22:57]
    node mCause = mux(io_count_count0To3, _mCause_T, _mCause_T_1) @[Csr.scala 22:19]
    node _csrOut_T = and(io_decode_mStatusEn, io_count_enabled) @[Csr.scala 24:38]
    node _csrOut_T_1 = and(_csrOut_T, mStatus) @[Csr.scala 24:58]
    node _csrOut_T_2 = bits(io_rf_readData, 0, 0) @[Csr.scala 25:26]
    node _csrOut_T_3 = or(_csrOut_T_1, _csrOut_T_2) @[Csr.scala 24:70]
    node _csrOut_T_4 = and(io_decode_mcauseEn, io_count_enabled) @[Csr.scala 26:25]
    node _csrOut_T_5 = bits(mCause, 0, 0) @[Csr.scala 26:61]
    node _csrOut_T_6 = and(_csrOut_T_4, _csrOut_T_5) @[Csr.scala 26:45]
    node csrOut = or(_csrOut_T_3, _csrOut_T_6) @[Csr.scala 25:29]
    node _csrIn_T = or(csrOut, io_dataIn) @[Csr.scala 32:30]
    node _csrIn_T_1 = eq(io_dataIn, UInt<1>("h0")) @[Csr.scala 33:32]
    node _csrIn_T_2 = and(csrOut, _csrIn_T_1) @[Csr.scala 33:30]
    node _csrIn_T_3 = eq(UInt<2>("h2"), io_decode_source) @[Mux.scala 80:60]
    node _csrIn_T_4 = mux(_csrIn_T_3, _csrIn_T, io_dataIn) @[Mux.scala 80:57]
    node _csrIn_T_5 = eq(UInt<2>("h3"), io_decode_source) @[Mux.scala 80:60]
    node _csrIn_T_6 = mux(_csrIn_T_5, _csrIn_T_2, _csrIn_T_4) @[Mux.scala 80:57]
    node _csrIn_T_7 = eq(UInt<2>("h0"), io_decode_source) @[Mux.scala 80:60]
    node csrIn = mux(_csrIn_T_7, csrOut, _csrIn_T_6) @[Mux.scala 80:57]
    node _timerIrq_T = and(io_timerInterrupt, mStatusMie) @[Csr.scala 39:36]
    node timerIrq = and(_timerIrq_T, mieMtie) @[Csr.scala 39:50]
    reg io_state_newIrq_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_state_newIrq_REG) @[Csr.scala 41:30]
    node _io_state_newIrq_T = eq(io_state_newIrq_REG, UInt<1>("h0")) @[Csr.scala 41:22]
    node _io_state_newIrq_T_1 = and(_io_state_newIrq_T, timerIrq) @[Csr.scala 41:41]
    node _T = and(io_decode_mStatusEn, io_count_count3) @[Csr.scala 43:28]
    node _GEN_0 = mux(_T, io_dataIn, mStatusMie) @[Csr.scala 43:48 44:16 15:23]
    node _T_1 = and(io_decode_mieEn, io_count_count7) @[Csr.scala 47:24]
    node _GEN_1 = mux(_T_1, io_dataIn, mieMtie) @[Csr.scala 47:44 48:13 17:20]
    node _GEN_2 = mux(io_decode_mRet, mStatusMpie, _GEN_0) @[Csr.scala 51:24 52:16]
    node _mCause3_0_T = eq(io_decode_eBreak, UInt<1>("h0")) @[Csr.scala 62:21]
    node _mCause3_0_T_1 = cat(_mCause3_0_T, UInt<3>("h3")) @[Csr.scala 62:40]
    node _mCause3_0_T_2 = cat(UInt<2>("h1"), io_memCmd) @[Csr.scala 64:33]
    node _mCause3_0_T_3 = cat(_mCause3_0_T_2, UInt<1>("h0")) @[Csr.scala 64:46]
    node _GEN_3 = mux(io_memMisaligned, _mCause3_0_T_3, UInt<1>("h0")) @[Csr.scala 63:35 64:17 66:17]
    node _GEN_4 = mux(io_decode_eOp, _mCause3_0_T_1, _GEN_3) @[Csr.scala 61:32 62:17]
    node _GEN_5 = mux(io_state_pendingIrq, UInt<3>("h7"), _GEN_4) @[Csr.scala 59:31 60:17]
    node _GEN_6 = mux(io_state_trapTaken, mStatusMie, mStatusMpie) @[Csr.scala 55:28 56:17 16:24]
    node _GEN_7 = mux(io_state_trapTaken, UInt<1>("h0"), _GEN_2) @[Csr.scala 55:28 57:16]
    node _GEN_8 = mux(io_state_trapTaken, io_state_pendingIrq, mCause31) @[Csr.scala 55:28 58:14 20:21]
    node _GEN_9 = mux(io_state_trapTaken, _GEN_5, mCause3_0) @[Csr.scala 19:22 55:28]
    node _T_2 = and(io_decode_mcauseEn, io_count_enabled) @[Csr.scala 70:27]
    node _mCause3_0_T_4 = bits(mCause3_0, 3, 1) @[Csr.scala 72:38]
    node _mCause3_0_T_5 = cat(csrIn, _mCause3_0_T_4) @[Csr.scala 72:26]
    node _GEN_10 = mux(io_count_count0To3, _mCause3_0_T_5, _GEN_9) @[Csr.scala 71:30 72:17]
    node _GEN_11 = mux(io_count_done, csrIn, _GEN_10) @[Csr.scala 74:25 75:17]
    node _GEN_12 = mux(_T_2, _GEN_11, _GEN_9) @[Csr.scala 70:48]
    io_state_newIrq <= _io_state_newIrq_T_1 @[Csr.scala 41:19]
    io_rf_writeData <= csrIn @[Csr.scala 37:19]
    io_dataOut <= csrOut @[Csr.scala 28:14]
    mStatusMie <= _GEN_7
    mStatusMpie <= _GEN_6
    mieMtie <= _GEN_1
    mStatus <= _mStatus_T @[Csr.scala 18:24]
    mCause3_0 <= _GEN_12
    mCause31 <= _GEN_8
    io_state_newIrq_REG <= timerIrq @[Csr.scala 41:30]

  module ServTop :
    input clock : Clock
    input reset : UInt<1>
    input io_timerInterrupt : UInt<1>
    output io_rf_writeRequest : UInt<1>
    output io_rf_readRequest : UInt<1>
    input io_rf_ready : UInt<1>
    output io_rf_ports_write0_addr : UInt<6>
    output io_rf_ports_write0_enable : UInt<1>
    output io_rf_ports_write0_data : UInt<1>
    output io_rf_ports_write1_addr : UInt<6>
    output io_rf_ports_write1_enable : UInt<1>
    output io_rf_ports_write1_data : UInt<1>
    output io_rf_ports_read0_addr : UInt<6>
    input io_rf_ports_read0_data : UInt<1>
    output io_rf_ports_read1_addr : UInt<6>
    input io_rf_ports_read1_data : UInt<1>
    output io_ibus_adr : UInt<32>
    output io_ibus_cyc : UInt<1>
    input io_ibus_rdt : UInt<32>
    input io_ibus_ack : UInt<1>
    output io_dbus_adr : UInt<32>
    output io_dbus_cyc : UInt<1>
    input io_dbus_rdt : UInt<32>
    input io_dbus_ack : UInt<1>
    output io_dbus_dat : UInt<32>
    output io_dbus_sel : UInt<4>
    output io_dbus_we : UInt<1>

    inst state of State @[ServTop.scala 15:21]
    inst decode of Decode @[ServTop.scala 16:22]
    inst bufreg of BufReg @[ServTop.scala 17:22]
    inst control of Control @[ServTop.scala 18:23]
    inst alu of Alu @[ServTop.scala 19:19]
    inst rfInterface of RegisterFileInterface @[ServTop.scala 20:27]
    inst mem of MemoryInterface @[ServTop.scala 21:19]
    inst csr of Csr @[ServTop.scala 108:21]
    node _decode_io_top_wbEn_T = and(io_ibus_cyc, io_ibus_ack) @[ServTop.scala 40:37]
    node _state_io_lsb1_T = bits(bufreg.io_lsb, 1, 1) @[ServTop.scala 61:23]
    node _rfInterface_io_trap_mePC_T = bits(io_ibus_adr, 0, 0) @[ServTop.scala 99:42]
    node _csr_io_dataIn_T = mux(decode.io_csr_dSel, decode.io_csr_imm, rfInterface.io_rs1Data) @[ServTop.scala 111:25]
    io_rf_writeRequest <= state.io_ram_writeRequest @[ServTop.scala 44:22]
    io_rf_readRequest <= state.io_ram_readRequest @[ServTop.scala 45:21]
    io_rf_ports_write0_addr <= rfInterface.io_rf_write0_addr @[ServTop.scala 88:21]
    io_rf_ports_write0_enable <= rfInterface.io_rf_write0_enable @[ServTop.scala 88:21]
    io_rf_ports_write0_data <= rfInterface.io_rf_write0_data @[ServTop.scala 88:21]
    io_rf_ports_write1_addr <= rfInterface.io_rf_write1_addr @[ServTop.scala 88:21]
    io_rf_ports_write1_enable <= rfInterface.io_rf_write1_enable @[ServTop.scala 88:21]
    io_rf_ports_write1_data <= rfInterface.io_rf_write1_data @[ServTop.scala 88:21]
    io_rf_ports_read0_addr <= rfInterface.io_rf_read0_addr @[ServTop.scala 88:21]
    io_rf_ports_read1_addr <= rfInterface.io_rf_read1_addr @[ServTop.scala 88:21]
    io_ibus_adr <= control.io_ibus_address @[ServTop.scala 36:15]
    io_ibus_cyc <= control.io_ibus_cyc @[ServTop.scala 37:15]
    io_dbus_adr <= bufreg.io_dataBusAddress @[ServTop.scala 25:15]
    io_dbus_cyc <= state.io_dbus_cyc @[ServTop.scala 26:15]
    io_dbus_dat <= mem.io_dbus_dat @[ServTop.scala 27:15]
    io_dbus_sel <= mem.io_dbus_sel @[ServTop.scala 28:15]
    io_dbus_we <= decode.io_mem_cmd @[ServTop.scala 29:14]
    state.clock <= clock
    state.reset <= reset
    state.io_csr_newIrq <= csr.io_state_newIrq @[ServTop.scala 114:18]
    state.io_dbus_ack <= io_dbus_ack @[ServTop.scala 31:21]
    state.io_ibus_ack <= io_ibus_ack @[ServTop.scala 38:21]
    state.io_ram_ready <= io_rf_ready @[ServTop.scala 46:22]
    state.io_decode_takeBranch <= decode.io_state_takeBranch @[ServTop.scala 56:19]
    state.io_decode_eOp <= decode.io_state_eOp @[ServTop.scala 56:19]
    state.io_decode_eBreak <= decode.io_state_eBreak @[ServTop.scala 56:19]
    state.io_decode_branchOp <= decode.io_state_branchOp @[ServTop.scala 56:19]
    state.io_decode_memOp <= decode.io_state_memOp @[ServTop.scala 56:19]
    state.io_decode_shiftOp <= decode.io_state_shiftOp @[ServTop.scala 56:19]
    state.io_decode_sltOp <= decode.io_state_sltOp @[ServTop.scala 56:19]
    state.io_decode_rdOp <= decode.io_state_rdOp @[ServTop.scala 56:19]
    state.io_alu_shiftDone <= alu.io_state_shiftDone @[ServTop.scala 59:16]
    state.io_mem_misaligned <= mem.io_state_misaligned @[ServTop.scala 60:16]
    state.io_lsb1 <= _state_io_lsb1_T @[ServTop.scala 61:17]
    decode.clock <= clock
    decode.reset <= reset
    decode.io_count_enabled <= state.io_count_enabled @[ServTop.scala 63:19]
    decode.io_count_init <= state.io_count_init @[ServTop.scala 63:19]
    decode.io_count_count0 <= state.io_count_count0 @[ServTop.scala 63:19]
    decode.io_count_count0To3 <= state.io_count_count0To3 @[ServTop.scala 63:19]
    decode.io_count_count12To31 <= state.io_count_count12To31 @[ServTop.scala 63:19]
    decode.io_count_count1 <= state.io_count_count1 @[ServTop.scala 63:19]
    decode.io_count_count2 <= state.io_count_count2 @[ServTop.scala 63:19]
    decode.io_count_count3 <= state.io_count_count3 @[ServTop.scala 63:19]
    decode.io_count_count7 <= state.io_count_count7 @[ServTop.scala 63:19]
    decode.io_count_done <= state.io_count_done @[ServTop.scala 63:19]
    decode.io_alu_cmpResult <= alu.io_decode_cmpResult @[ServTop.scala 66:17]
    decode.io_top_wbRdt <= io_ibus_rdt @[ServTop.scala 41:23]
    decode.io_top_wbEn <= _decode_io_top_wbEn_T @[ServTop.scala 40:22]
    bufreg.clock <= clock
    bufreg.reset <= reset
    bufreg.io_count_enabled <= state.io_count_enabled @[ServTop.scala 70:19]
    bufreg.io_count_init <= state.io_count_init @[ServTop.scala 70:19]
    bufreg.io_count_count0 <= state.io_count_count0 @[ServTop.scala 70:19]
    bufreg.io_count_count0To3 <= state.io_count_count0To3 @[ServTop.scala 70:19]
    bufreg.io_count_count12To31 <= state.io_count_count12To31 @[ServTop.scala 70:19]
    bufreg.io_count_count1 <= state.io_count_count1 @[ServTop.scala 70:19]
    bufreg.io_count_count2 <= state.io_count_count2 @[ServTop.scala 70:19]
    bufreg.io_count_count3 <= state.io_count_count3 @[ServTop.scala 70:19]
    bufreg.io_count_count7 <= state.io_count_count7 @[ServTop.scala 70:19]
    bufreg.io_count_done <= state.io_count_done @[ServTop.scala 70:19]
    bufreg.io_decode_loop <= decode.io_bufreg_loop @[ServTop.scala 64:20]
    bufreg.io_decode_rs1En <= decode.io_bufreg_rs1En @[ServTop.scala 64:20]
    bufreg.io_decode_immEn <= decode.io_bufreg_immEn @[ServTop.scala 64:20]
    bufreg.io_decode_clearLsb <= decode.io_bufreg_clearLsb @[ServTop.scala 64:20]
    bufreg.io_state_hold <= state.io_bufreg_hold @[ServTop.scala 57:19]
    bufreg.io_imm <= decode.io_top_imm @[ServTop.scala 71:17]
    bufreg.io_rs1 <= rfInterface.io_rs1Data @[ServTop.scala 72:17]
    control.clock <= clock
    control.reset <= reset
    control.io_state_pcEnable <= state.io_control_pcEnable @[ServTop.scala 58:20]
    control.io_state_jump <= state.io_control_jump @[ServTop.scala 58:20]
    control.io_state_trap <= state.io_control_trap @[ServTop.scala 58:20]
    control.io_count_enabled <= state.io_count_enabled @[ServTop.scala 74:20]
    control.io_count_init <= state.io_count_init @[ServTop.scala 74:20]
    control.io_count_count0 <= state.io_count_count0 @[ServTop.scala 74:20]
    control.io_count_count0To3 <= state.io_count_count0To3 @[ServTop.scala 74:20]
    control.io_count_count12To31 <= state.io_count_count12To31 @[ServTop.scala 74:20]
    control.io_count_count1 <= state.io_count_count1 @[ServTop.scala 74:20]
    control.io_count_count2 <= state.io_count_count2 @[ServTop.scala 74:20]
    control.io_count_count3 <= state.io_count_count3 @[ServTop.scala 74:20]
    control.io_count_count7 <= state.io_count_count7 @[ServTop.scala 74:20]
    control.io_count_done <= state.io_count_done @[ServTop.scala 74:20]
    control.io_decode_jalOrJalr <= decode.io_control_jalOrJalr @[ServTop.scala 65:21]
    control.io_decode_uType <= decode.io_control_uType @[ServTop.scala 65:21]
    control.io_decode_pcRel <= decode.io_control_pcRel @[ServTop.scala 65:21]
    control.io_decode_mRet <= decode.io_control_mRet @[ServTop.scala 65:21]
    control.io_ibus_ack <= io_ibus_ack @[ServTop.scala 39:23]
    control.io_data_imm <= decode.io_top_imm @[ServTop.scala 75:23]
    control.io_data_buf <= bufreg.io_dataOut @[ServTop.scala 76:23]
    control.io_data_csrPc <= rfInterface.io_trap_csrPC @[ServTop.scala 78:25]
    alu.clock <= clock
    alu.reset <= reset
    alu.io_decode_doSubtract <= decode.io_alu_doSubtract @[ServTop.scala 66:17]
    alu.io_decode_boolOp <= decode.io_alu_boolOp @[ServTop.scala 66:17]
    alu.io_decode_cmpEqual <= decode.io_alu_cmpEqual @[ServTop.scala 66:17]
    alu.io_decode_cmpUnsigned <= decode.io_alu_cmpUnsigned @[ServTop.scala 66:17]
    alu.io_decode_shiftSigned <= decode.io_alu_shiftSigned @[ServTop.scala 66:17]
    alu.io_decode_shiftRight <= decode.io_alu_shiftRight @[ServTop.scala 66:17]
    alu.io_decode_rdSelect <= decode.io_alu_rdSelect @[ServTop.scala 66:17]
    alu.io_decode_opBIsRS2 <= decode.io_alu_opBIsRS2 @[ServTop.scala 66:17]
    alu.io_data_rs1 <= rfInterface.io_rs1Data @[ServTop.scala 83:19]
    alu.io_data_rs2 <= rfInterface.io_rs2Data @[ServTop.scala 84:19]
    alu.io_data_imm <= decode.io_top_imm @[ServTop.scala 85:19]
    alu.io_data_buffer <= bufreg.io_dataOut @[ServTop.scala 86:22]
    alu.io_count_enabled <= state.io_count_enabled @[ServTop.scala 82:16]
    alu.io_count_init <= state.io_count_init @[ServTop.scala 82:16]
    alu.io_count_count0 <= state.io_count_count0 @[ServTop.scala 82:16]
    alu.io_count_count0To3 <= state.io_count_count0To3 @[ServTop.scala 82:16]
    alu.io_count_count12To31 <= state.io_count_count12To31 @[ServTop.scala 82:16]
    alu.io_count_count1 <= state.io_count_count1 @[ServTop.scala 82:16]
    alu.io_count_count2 <= state.io_count_count2 @[ServTop.scala 82:16]
    alu.io_count_count3 <= state.io_count_count3 @[ServTop.scala 82:16]
    alu.io_count_count7 <= state.io_count_count7 @[ServTop.scala 82:16]
    alu.io_count_done <= state.io_count_done @[ServTop.scala 82:16]
    alu.io_state_shiftAmountEnable <= state.io_alu_shiftAmountEnable @[ServTop.scala 59:16]
    rfInterface.clock <= clock
    rfInterface.reset <= reset
    rfInterface.io_rf_read0_data <= io_rf_ports_read0_data @[ServTop.scala 88:21]
    rfInterface.io_rf_read1_data <= io_rf_ports_read1_data @[ServTop.scala 88:21]
    rfInterface.io_trap_doTrap <= state.io_control_trap @[ServTop.scala 97:30]
    rfInterface.io_trap_mRet <= decode.io_control_mRet @[ServTop.scala 98:28]
    rfInterface.io_trap_mePC <= _rfInterface_io_trap_mePC_T @[ServTop.scala 99:28]
    rfInterface.io_trap_memMisaligned <= mem.io_state_misaligned @[ServTop.scala 100:37]
    rfInterface.io_trap_bufRegData <= bufreg.io_dataOut @[ServTop.scala 101:34]
    rfInterface.io_trap_badPC <= control.io_data_badPc @[ServTop.scala 79:29]
    rfInterface.io_csr_writeData <= csr.io_rf_writeData @[ServTop.scala 117:24]
    rfInterface.io_rd_writeEnable <= state.io_rf_writeEnable @[ServTop.scala 90:33]
    rfInterface.io_rd_controlData <= control.io_data_rd @[ServTop.scala 91:33]
    rfInterface.io_rd_aluData <= alu.io_data_rd @[ServTop.scala 92:29]
    rfInterface.io_rd_aluEnable <= decode.io_top_rdAluEn @[ServTop.scala 93:31]
    rfInterface.io_rd_csrData <= csr.io_dataOut @[ServTop.scala 116:31]
    rfInterface.io_rd_csrEnable <= decode.io_top_rdCsrEn @[ServTop.scala 94:31]
    rfInterface.io_rd_memData <= mem.io_rd @[ServTop.scala 95:29]
    rfInterface.io_decode_rdAddress <= decode.io_rf_rdAddress @[ServTop.scala 67:16]
    rfInterface.io_decode_rs1Address <= decode.io_rf_rs1Address @[ServTop.scala 67:16]
    rfInterface.io_decode_rs2Address <= decode.io_rf_rs2Address @[ServTop.scala 67:16]
    rfInterface.io_decode_csrAddress <= decode.io_rf_csrAddress @[ServTop.scala 67:16]
    rfInterface.io_decode_csrEnable <= decode.io_rf_csrEnable @[ServTop.scala 67:16]
    mem.clock <= clock
    mem.reset <= reset
    mem.io_enabled <= state.io_count_enabled @[ServTop.scala 103:18]
    mem.io_decode_memOp <= decode.io_mem_memOp @[ServTop.scala 68:17]
    mem.io_decode_signed <= decode.io_mem_signed @[ServTop.scala 68:17]
    mem.io_decode_word <= decode.io_mem_word @[ServTop.scala 68:17]
    mem.io_decode_half <= decode.io_mem_half @[ServTop.scala 68:17]
    mem.io_decode_cmd <= decode.io_mem_cmd @[ServTop.scala 68:17]
    mem.io_state_byteCount <= state.io_mem_byteCount @[ServTop.scala 60:16]
    mem.io_lsb <= bufreg.io_lsb @[ServTop.scala 104:14]
    mem.io_rs2 <= rfInterface.io_rs2Data @[ServTop.scala 105:14]
    mem.io_dbus_rdt <= io_dbus_rdt @[ServTop.scala 30:19]
    mem.io_dbus_ack <= io_dbus_ack @[ServTop.scala 32:19]
    csr.clock <= clock
    csr.reset <= reset
    csr.io_count_enabled <= state.io_count_enabled @[ServTop.scala 109:18]
    csr.io_count_init <= state.io_count_init @[ServTop.scala 109:18]
    csr.io_count_count0 <= state.io_count_count0 @[ServTop.scala 109:18]
    csr.io_count_count0To3 <= state.io_count_count0To3 @[ServTop.scala 109:18]
    csr.io_count_count12To31 <= state.io_count_count12To31 @[ServTop.scala 109:18]
    csr.io_count_count1 <= state.io_count_count1 @[ServTop.scala 109:18]
    csr.io_count_count2 <= state.io_count_count2 @[ServTop.scala 109:18]
    csr.io_count_count3 <= state.io_count_count3 @[ServTop.scala 109:18]
    csr.io_count_count7 <= state.io_count_count7 @[ServTop.scala 109:18]
    csr.io_count_done <= state.io_count_done @[ServTop.scala 109:18]
    csr.io_decode_mStatusEn <= decode.io_csr_mStatusEn @[ServTop.scala 115:19]
    csr.io_decode_mieEn <= decode.io_csr_mieEn @[ServTop.scala 115:19]
    csr.io_decode_mcauseEn <= decode.io_csr_mcauseEn @[ServTop.scala 115:19]
    csr.io_decode_source <= decode.io_csr_source @[ServTop.scala 115:19]
    csr.io_decode_dSel <= decode.io_csr_dSel @[ServTop.scala 115:19]
    csr.io_decode_imm <= decode.io_csr_imm @[ServTop.scala 115:19]
    csr.io_decode_eOp <= decode.io_csr_eOp @[ServTop.scala 115:19]
    csr.io_decode_eBreak <= decode.io_csr_eBreak @[ServTop.scala 115:19]
    csr.io_decode_mRet <= decode.io_csr_mRet @[ServTop.scala 115:19]
    csr.io_memCmd <= io_dbus_we @[ServTop.scala 112:19]
    csr.io_memMisaligned <= mem.io_state_misaligned @[ServTop.scala 113:26]
    csr.io_state_trapTaken <= state.io_csr_trapTaken @[ServTop.scala 114:18]
    csr.io_state_pendingIrq <= state.io_csr_pendingIrq @[ServTop.scala 114:18]
    csr.io_rf_readData <= rfInterface.io_csr_readData @[ServTop.scala 117:24]
    csr.io_dataIn <= _csr_io_dataIn_T @[ServTop.scala 111:19]
    csr.io_timerInterrupt <= io_timerInterrupt @[ServTop.scala 110:27]

  module RamInterface :
    input clock : Clock
    input reset : UInt<1>
    output io_ram_writeAddr : UInt<10>
    output io_ram_writeData : UInt<2>
    output io_ram_writeEnable : UInt<1>
    output io_ram_readAddr : UInt<10>
    input io_ram_readData : UInt<2>
    input io_rf_writeRequest : UInt<1>
    input io_rf_readRequest : UInt<1>
    output io_rf_ready : UInt<1>
    input io_rf_ports_write0_addr : UInt<6>
    input io_rf_ports_write0_enable : UInt<1>
    input io_rf_ports_write0_data : UInt<1>
    input io_rf_ports_write1_addr : UInt<6>
    input io_rf_ports_write1_enable : UInt<1>
    input io_rf_ports_write1_data : UInt<1>
    input io_rf_ports_read0_addr : UInt<6>
    output io_rf_ports_read0_data : UInt<1>
    input io_rf_ports_read1_addr : UInt<6>
    output io_rf_ports_read1_data : UInt<1>

    reg readRequestBuffer : UInt<1>, clock with :
      reset => (UInt<1>("h0"), readRequestBuffer) @[Ram.scala 23:34]
    reg rgnt : UInt<1>, clock with :
      reset => (UInt<1>("h0"), rgnt) @[Ram.scala 24:21]
    node _io_rf_ready_T = or(rgnt, io_rf_writeRequest) @[Ram.scala 25:23]
    reg writeCount : UInt<5>, clock with :
      reset => (UInt<1>("h0"), writeCount) @[Ram.scala 28:27]
    reg writeGo : UInt<1>, clock with :
      reset => (UInt<1>("h0"), writeGo) @[Ram.scala 29:20]
    reg writeData0Buffer : UInt<1>, clock with :
      reset => (UInt<1>("h0"), writeData0Buffer) @[Ram.scala 30:29]
    reg writeData1Buffer : UInt<2>, clock with :
      reset => (UInt<1>("h0"), writeData1Buffer) @[Ram.scala 31:29]
    reg writeEnable0Buffer : UInt<1>, clock with :
      reset => (UInt<1>("h0"), writeEnable0Buffer) @[Ram.scala 32:35]
    reg writeEnable1Buffer : UInt<1>, clock with :
      reset => (UInt<1>("h0"), writeEnable1Buffer) @[Ram.scala 33:35]
    node _writeRequestBuffer_T = or(io_rf_writeRequest, rgnt) @[Ram.scala 34:55]
    reg writeRequestBuffer : UInt<1>, clock with :
      reset => (UInt<1>("h0"), writeRequestBuffer) @[Ram.scala 34:35]
    node _T = bits(writeCount, 0, 0) @[Ram.scala 37:17]
    node _T_1 = eq(_T, UInt<1>("h0")) @[Ram.scala 37:6]
    node _T_2 = bits(writeCount, 0, 0) @[Ram.scala 37:32]
    node _io_ram_writeData_T = cat(io_rf_ports_write0_data, writeData0Buffer) @[Ram.scala 42:86]
    node _io_ram_writeData_T_1 = mux(_T_2, writeData1Buffer, _io_ram_writeData_T) @[Ram.scala 42:26]
    node writeAddress = mux(_T_2, io_rf_ports_write1_addr, io_rf_ports_write0_addr) @[Ram.scala 43:25]
    node _io_ram_writeAddr_T = head(writeCount, 4) @[package.scala 18:19]
    node _io_ram_writeAddr_T_1 = tail(writeCount, 4) @[package.scala 18:36]
    node _io_ram_writeAddr_T_2 = cat(writeAddress, _io_ram_writeAddr_T) @[Ram.scala 44:36]
    node _io_ram_writeEnable_T = and(_T_1, writeEnable0Buffer) @[Ram.scala 45:54]
    node _io_ram_writeEnable_T_1 = and(_T_2, writeEnable1Buffer) @[Ram.scala 45:97]
    node _io_ram_writeEnable_T_2 = or(_io_ram_writeEnable_T, _io_ram_writeEnable_T_1) @[Ram.scala 45:77]
    node _io_ram_writeEnable_T_3 = and(writeGo, _io_ram_writeEnable_T_2) @[Ram.scala 45:33]
    node _writeData0Buffer_T_1 = tail(writeData0Buffer, 0) @[package.scala 18:36]
    node _writeData0Buffer_T_3 = tail(_writeData0Buffer_T_1, 0) @[package.scala 18:36]
    node _writeData0Buffer_T_4 = io_rf_ports_write0_data @[Ram.scala 47:47]
    node _writeData1Buffer_T_1 = tail(writeData1Buffer, 0) @[package.scala 18:36]
    node _writeData1Buffer_T_2 = head(_writeData1Buffer_T_1, 1) @[package.scala 18:19]
    node _writeData1Buffer_T_3 = tail(_writeData1Buffer_T_1, 1) @[package.scala 18:36]
    node _writeData1Buffer_T_4 = cat(io_rf_ports_write1_data, _writeData1Buffer_T_2) @[Ram.scala 48:47]
    node _writeCount_T = add(writeCount, UInt<1>("h1")) @[Ram.scala 50:44]
    node _writeCount_T_1 = tail(_writeCount_T, 1) @[Ram.scala 50:44]
    node _GEN_0 = mux(writeGo, _writeCount_T_1, writeCount) @[Ram.scala 50:17 28:27 50:30]
    node _GEN_1 = mux(writeRequestBuffer, UInt<1>("h1"), writeGo) @[Ram.scala 29:20 51:{28,38}]
    node _T_3 = eq(writeCount, UInt<5>("h1f")) @[Ram.scala 52:19]
    node _GEN_2 = mux(_T_3, UInt<1>("h0"), _GEN_1) @[Ram.scala 52:{35,45}]
    reg readCount : UInt<5>, clock with :
      reset => (UInt<1>("h0"), readCount) @[Ram.scala 55:22]
    node _readTrigger0_T = head(readCount, 4) @[package.scala 18:19]
    node _readTrigger0_T_1 = tail(readCount, 4) @[package.scala 18:36]
    node readTrigger0 = eq(_readTrigger0_T_1, UInt<1>("h1")) @[Ram.scala 56:53]
    reg readTrigger1 : UInt<1>, clock with :
      reset => (UInt<1>("h0"), readTrigger1) @[Ram.scala 57:29]
    node readAddress = mux(readTrigger0, io_rf_ports_read1_addr, io_rf_ports_read0_addr) @[Ram.scala 58:24]
    node _io_ram_readAddr_T = head(readCount, 4) @[package.scala 18:19]
    node _io_ram_readAddr_T_1 = tail(readCount, 4) @[package.scala 18:36]
    node _io_ram_readAddr_T_2 = cat(readAddress, _io_ram_readAddr_T) @[Ram.scala 60:34]
    reg readData0Buffer : UInt<2>, clock with :
      reset => (UInt<1>("h0"), readData0Buffer) @[Ram.scala 62:28]
    node _readData0Buffer_T = head(readData0Buffer, 1) @[package.scala 18:19]
    node _readData0Buffer_T_1 = tail(readData0Buffer, 1) @[package.scala 18:36]
    node _GEN_3 = mux(readTrigger0, io_ram_readData, _readData0Buffer_T) @[Ram.scala 63:19 64:{22,40}]
    reg readData1Buffer : UInt<1>, clock with :
      reset => (UInt<1>("h0"), readData1Buffer) @[Ram.scala 65:28]
    node _readData1Buffer_T_1 = tail(readData1Buffer, 0) @[package.scala 18:36]
    node _readData1Buffer_T_2 = head(io_ram_readData, 1) @[package.scala 18:19]
    node _readData1Buffer_T_3 = tail(io_ram_readData, 1) @[package.scala 18:36]
    node _GEN_4 = mux(readTrigger1, _readData1Buffer_T_2, UInt<1>("h0")) @[Ram.scala 66:19 67:{22,40}]
    node _io_rf_ports_read0_data_T = bits(readData0Buffer, 0, 0) @[Ram.scala 69:44]
    node _io_rf_ports_read1_data_T = bits(io_ram_readData, 0, 0) @[Ram.scala 70:62]
    node _io_rf_ports_read1_data_T_1 = bits(readData1Buffer, 0, 0) @[Ram.scala 70:82]
    node _io_rf_ports_read1_data_T_2 = mux(readTrigger1, _io_rf_ports_read1_data_T, _io_rf_ports_read1_data_T_1) @[Ram.scala 70:32]
    node _readCount_T = add(readCount, UInt<1>("h1")) @[Ram.scala 72:26]
    node _readCount_T_1 = tail(_readCount_T, 1) @[Ram.scala 72:26]
    node _GEN_5 = mux(io_rf_readRequest, UInt<1>("h0"), _readCount_T_1) @[Ram.scala 72:13 73:{27,39}]
    io_ram_writeAddr <= _io_ram_writeAddr_T_2 @[Ram.scala 44:20]
    io_ram_writeData <= _io_ram_writeData_T_1 @[Ram.scala 42:20]
    io_ram_writeEnable <= _io_ram_writeEnable_T_3 @[Ram.scala 45:22]
    io_ram_readAddr <= _io_ram_readAddr_T_2 @[Ram.scala 60:19]
    io_rf_ready <= _io_rf_ready_T @[Ram.scala 25:15]
    io_rf_ports_read0_data <= _io_rf_ports_read0_data_T @[Ram.scala 69:26]
    io_rf_ports_read1_data <= _io_rf_ports_read1_data_T_2 @[Ram.scala 70:26]
    readRequestBuffer <= mux(reset, UInt<1>("h0"), io_rf_readRequest) @[Ram.scala 23:{34,34,34}]
    rgnt <= mux(reset, UInt<1>("h0"), readRequestBuffer) @[Ram.scala 24:{21,21,21}]
    writeCount <= mux(reset, UInt<5>("h0"), _GEN_0) @[Ram.scala 28:{27,27}]
    writeGo <= _GEN_2
    writeData0Buffer <= _writeData0Buffer_T_4 @[Ram.scala 47:20]
    writeData1Buffer <= _writeData1Buffer_T_4 @[Ram.scala 48:20]
    writeEnable0Buffer <= io_rf_ports_write0_enable @[Ram.scala 32:35]
    writeEnable1Buffer <= io_rf_ports_write1_enable @[Ram.scala 33:35]
    writeRequestBuffer <= _writeRequestBuffer_T @[Ram.scala 34:35]
    readCount <= _GEN_5
    readTrigger1 <= readTrigger0 @[Ram.scala 57:29]
    readData0Buffer <= _GEN_3
    readData1Buffer <= _GEN_4

  module Ram :
    input clock : Clock
    input reset : UInt<1>
    input io_writeAddr : UInt<10>
    input io_writeData : UInt<2>
    input io_writeEnable : UInt<1>
    input io_readAddr : UInt<10>
    output io_readData : UInt<2>

    mem memory : @[Ram.scala 79:27]
      data-type => UInt<2>
      depth => 576
      read-latency => 1
      write-latency => 1
      reader => io_readData_MPORT
      writer => MPORT
      read-under-write => undefined
    node _GEN_0 = validif(io_writeEnable, io_writeAddr) @[Ram.scala 80:24]
    node _GEN_1 = validif(io_writeEnable, clock) @[Ram.scala 80:24]
    node _GEN_2 = mux(io_writeEnable, UInt<1>("h1"), UInt<1>("h0")) @[Ram.scala 80:24 79:27]
    node _GEN_3 = validif(io_writeEnable, UInt<1>("h1")) @[Ram.scala 80:24]
    node _GEN_4 = validif(io_writeEnable, io_writeData) @[Ram.scala 80:24]
    io_readData <= memory.io_readData_MPORT.data @[Ram.scala 81:15]
    memory.io_readData_MPORT.addr <= io_readAddr @[Ram.scala 81:29]
    memory.io_readData_MPORT.en <= UInt<1>("h1") @[Ram.scala 81:29]
    memory.io_readData_MPORT.clk <= clock @[Ram.scala 81:29]
    memory.MPORT.addr <= _GEN_0
    memory.MPORT.en <= _GEN_2
    memory.MPORT.clk <= _GEN_1
    memory.MPORT.data <= _GEN_4
    memory.MPORT.mask <= _GEN_3

  module ServTopWithRam :
    input clock : Clock
    input reset : UInt<1>
    input io_timerInterrupt : UInt<1>
    output io_ibus_adr : UInt<32>
    output io_ibus_cyc : UInt<1>
    input io_ibus_rdt : UInt<32>
    input io_ibus_ack : UInt<1>
    output io_dbus_adr : UInt<32>
    output io_dbus_cyc : UInt<1>
    input io_dbus_rdt : UInt<32>
    input io_dbus_ack : UInt<1>
    output io_dbus_dat : UInt<32>
    output io_dbus_sel : UInt<4>
    output io_dbus_we : UInt<1>

    inst top of ServTop @[ServTopWithRam.scala 21:19]
    inst ramInterface of RamInterface @[ServTopWithRam.scala 22:28]
    inst ram of Ram @[ServTopWithRam.scala 24:19]
    io_ibus_adr <= top.io_ibus_adr @[ServTopWithRam.scala 27:15]
    io_ibus_cyc <= top.io_ibus_cyc @[ServTopWithRam.scala 27:15]
    io_dbus_adr <= top.io_dbus_adr @[ServTopWithRam.scala 28:15]
    io_dbus_cyc <= top.io_dbus_cyc @[ServTopWithRam.scala 28:15]
    io_dbus_dat <= top.io_dbus_dat @[ServTopWithRam.scala 28:15]
    io_dbus_sel <= top.io_dbus_sel @[ServTopWithRam.scala 28:15]
    io_dbus_we <= top.io_dbus_we @[ServTopWithRam.scala 28:15]
    top.clock <= clock
    top.reset <= reset
    top.io_timerInterrupt <= io_timerInterrupt @[ServTopWithRam.scala 26:25]
    top.io_rf_ready <= ramInterface.io_rf_ready @[ServTopWithRam.scala 30:13]
    top.io_rf_ports_read0_data <= ramInterface.io_rf_ports_read0_data @[ServTopWithRam.scala 30:13]
    top.io_rf_ports_read1_data <= ramInterface.io_rf_ports_read1_data @[ServTopWithRam.scala 30:13]
    top.io_ibus_rdt <= io_ibus_rdt @[ServTopWithRam.scala 27:15]
    top.io_ibus_ack <= io_ibus_ack @[ServTopWithRam.scala 27:15]
    top.io_dbus_rdt <= io_dbus_rdt @[ServTopWithRam.scala 28:15]
    top.io_dbus_ack <= io_dbus_ack @[ServTopWithRam.scala 28:15]
    ramInterface.clock <= clock
    ramInterface.reset <= reset
    ramInterface.io_ram_readData <= ram.io_readData @[ServTopWithRam.scala 31:10]
    ramInterface.io_rf_writeRequest <= top.io_rf_writeRequest @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_readRequest <= top.io_rf_readRequest @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_ports_write0_addr <= top.io_rf_ports_write0_addr @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_ports_write0_enable <= top.io_rf_ports_write0_enable @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_ports_write0_data <= top.io_rf_ports_write0_data @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_ports_write1_addr <= top.io_rf_ports_write1_addr @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_ports_write1_enable <= top.io_rf_ports_write1_enable @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_ports_write1_data <= top.io_rf_ports_write1_data @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_ports_read0_addr <= top.io_rf_ports_read0_addr @[ServTopWithRam.scala 30:13]
    ramInterface.io_rf_ports_read1_addr <= top.io_rf_ports_read1_addr @[ServTopWithRam.scala 30:13]
    ram.clock <= clock
    ram.reset <= reset
    ram.io_writeAddr <= ramInterface.io_ram_writeAddr @[ServTopWithRam.scala 31:10]
    ram.io_writeData <= ramInterface.io_ram_writeData @[ServTopWithRam.scala 31:10]
    ram.io_writeEnable <= ramInterface.io_ram_writeEnable @[ServTopWithRam.scala 31:10]
    ram.io_readAddr <= ramInterface.io_ram_readAddr @[ServTopWithRam.scala 31:10]
