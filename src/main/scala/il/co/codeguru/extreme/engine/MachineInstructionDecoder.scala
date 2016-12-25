// Copyright (C) 2014-2017 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package il.co.codeguru.extreme.engine

import il.co.codeguru.extreme.engine.MachineInstructionOpcode._
import il.co.codeguru.extreme.engine.Register._

/**
  * Translate byte stream into next instruction
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class MachineInstructionDecoder(val cpu: Cpu, val opcodeFetcher: OpcodeFetcher) {
  def decode(): MachineInstructionOpcode.OperationCode = {
    decode(new IndirectAddressingDecoder(cpu))
  }

  def decode(indirect: IndirectAddressingDecoder): MachineInstructionOpcode.OperationCode = {
    val opcode = Unsigned.unsignedByte(readByte)

    opcode match {
      case 0x00 => indirect.reset(); ADD(indirect.getRegOrMem8, indirect.getReg8)
      case 0x01 => indirect.reset(); ADD(indirect.getRegOrMem16, indirect.getReg16)
      case 0x02 => indirect.reset(); ADD(indirect.getReg8, indirect.getRegOrMem8)
      case 0x03 => indirect.reset(); ADD(indirect.getReg16, indirect.getRegOrMem16)
      case 0x04 => ADD(Reg8Operand(AL), Immed8Operand(readByte))
      case 0x05 => ADD(Reg16Operand(AX), Immed16Operand(readWord))
      case 0x06 => PUSH(Reg16Operand(ES))
      case 0x07 => POP(Reg16Operand(ES))
      case 0x08 => indirect.reset(); OR(indirect.getRegOrMem8, indirect.getReg8)
      case 0x09 => indirect.reset(); OR(indirect.getRegOrMem16, indirect.getReg16)
      case 0x0A => indirect.reset(); OR(indirect.getReg8, indirect.getRegOrMem8)
      case 0x0B => indirect.reset(); OR(indirect.getReg16, indirect.getRegOrMem16)
      case 0x0C => OR(Reg8Operand(AL), Immed8Operand(readByte))
      case 0x0D => OR(Reg16Operand(AX), Immed16Operand(readWord))
      case 0x0E => PUSH(Reg16Operand(CS))
      case 0x0F => NotUsed /* Forbidden: POP(RegisterOperand(CS)) */

      case 0x10 => indirect.reset(); ADC(indirect.getRegOrMem8, indirect.getReg8)
      case 0x11 => indirect.reset(); ADC(indirect.getRegOrMem16, indirect.getReg16)
      case 0x12 => indirect.reset(); ADC(indirect.getReg8, indirect.getRegOrMem8)
      case 0x13 => indirect.reset(); ADC(indirect.getReg16, indirect.getRegOrMem16)
      case 0x14 => ADC(Reg8Operand(AL), Immed8Operand(readByte))
      case 0x15 => ADC(Reg16Operand(AX), Immed16Operand(readWord))
      case 0x16 => PUSH(Reg16Operand(SS))
      case 0x17 => POP(Reg16Operand(SS))
      case 0x18 => indirect.reset(); SBB(indirect.getRegOrMem8, indirect.getReg8)
      case 0x19 => indirect.reset(); SBB(indirect.getRegOrMem16, indirect.getReg16)
      case 0x1A => indirect.reset(); SBB(indirect.getReg8, indirect.getRegOrMem8)
      case 0x1B => indirect.reset(); SBB(indirect.getReg16, indirect.getRegOrMem16)
      case 0x1C => SBB(Reg8Operand(AL), Immed8Operand(readByte))
      case 0x1D => SBB(Reg16Operand(AX), Immed16Operand(readWord))
      case 0x1E => PUSH(Reg16Operand(DS))
      case 0x1F => POP(Reg16Operand(DS))

      case 0x20 => indirect.reset(); AND(indirect.getRegOrMem8, indirect.getReg8)
      case 0x21 => indirect.reset(); AND(indirect.getRegOrMem16, indirect.getReg16)
      case 0x22 => indirect.reset(); AND(indirect.getReg8, indirect.getRegOrMem8)
      case 0x23 => indirect.reset(); AND(indirect.getReg16, indirect.getRegOrMem16)
      case 0x24 => AND(Reg8Operand(AL), Immed8Operand(readByte))
      case 0x25 => AND(Reg16Operand(AX), Immed16Operand(readWord))
      case 0x26 => indirect.forceSegReg(ES); decode(indirect)
      case 0x27 => DAA()
      case 0x28 => indirect.reset(); SUB(indirect.getRegOrMem8, indirect.getReg8)
      case 0x29 => indirect.reset(); SUB(indirect.getRegOrMem16, indirect.getReg16)
      case 0x2A => indirect.reset(); SUB(indirect.getReg8, indirect.getRegOrMem8)
      case 0x2B => indirect.reset(); SUB(indirect.getReg16, indirect.getRegOrMem16)
      case 0x2C => SUB(Reg8Operand(AL), Immed8Operand(readByte))
      case 0x2D => SUB(Reg16Operand(AX), Immed16Operand(readWord))
      case 0x2E => indirect.forceSegReg(CS); decode(indirect)
      case 0x2F => DAS()

      case 0x30 => indirect.reset(); XOR(indirect.getRegOrMem8, indirect.getReg8)
      case 0x31 => indirect.reset(); XOR(indirect.getRegOrMem16, indirect.getReg16)
      case 0x32 => indirect.reset(); XOR(indirect.getReg8, indirect.getRegOrMem8)
      case 0x33 => indirect.reset(); XOR(indirect.getReg16, indirect.getRegOrMem16)
      case 0x34 => XOR(Reg8Operand(AL), Immed8Operand(readByte))
      case 0x35 => XOR(Reg16Operand(AX), Immed16Operand(readWord))
      case 0x36 => indirect.forceSegReg(SS); decode(indirect)
      case 0x37 => AAA()
      case 0x38 => indirect.reset(); CMP(indirect.getRegOrMem8, indirect.getReg8)
      case 0x39 => indirect.reset(); CMP(indirect.getRegOrMem16, indirect.getReg16)
      case 0x3A => indirect.reset(); CMP(indirect.getReg8, indirect.getRegOrMem8)
      case 0x3B => indirect.reset(); CMP(indirect.getReg16, indirect.getRegOrMem16)
      case 0x3C => CMP(Reg8Operand(AL), Immed8Operand(readByte))
      case 0x3D => CMP(Reg16Operand(AX), Immed16Operand(readWord))
      case 0x3E => indirect.forceSegReg(SS); decode(indirect)
      case 0x3F => AAS()

      case 0x40 => INC(Reg16Operand(AX))
      case 0x41 => INC(Reg16Operand(CX))
      case 0x42 => INC(Reg16Operand(DX))
      case 0x43 => INC(Reg16Operand(BX))
      case 0x44 => INC(Reg16Operand(SP))
      case 0x45 => INC(Reg16Operand(BP))
      case 0x46 => INC(Reg16Operand(SI))
      case 0x47 => INC(Reg16Operand(DI))
      case 0x48 => DEC(Reg16Operand(AX))
      case 0x49 => DEC(Reg16Operand(CX))
      case 0x4A => DEC(Reg16Operand(DX))
      case 0x4B => DEC(Reg16Operand(BX))
      case 0x4C => DEC(Reg16Operand(SP))
      case 0x4D => DEC(Reg16Operand(BP))
      case 0x4E => DEC(Reg16Operand(SI))
      case 0x4F => DEC(Reg16Operand(DI))

      case 0x50 => PUSH(Reg16Operand(AX))
      case 0x51 => PUSH(Reg16Operand(CX))
      case 0x52 => PUSH(Reg16Operand(DX))
      case 0x53 => PUSH(Reg16Operand(BX))
      case 0x54 => PUSH(Reg16Operand(SP))
      case 0x55 => PUSH(Reg16Operand(BP))
      case 0x56 => PUSH(Reg16Operand(SI))
      case 0x57 => PUSH(Reg16Operand(DI))
      case 0x58 => POP(Reg16Operand(AX))
      case 0x59 => POP(Reg16Operand(CX))
      case 0x5A => POP(Reg16Operand(DX))
      case 0x5B => POP(Reg16Operand(BX))
      case 0x5C => POP(Reg16Operand(SP))
      case 0x5D => POP(Reg16Operand(BP))
      case 0x5E => POP(Reg16Operand(SI))
      case 0x5F => POP(Reg16Operand(DI))

      case 0x60 => PUSHA() // 80186
      case 0x61 => POPA()
      case 0x62 => NotUsed
      case 0x63 => NotUsed
      case 0x64 => NotUsed
      case 0x65 => NotUsed
      case 0x66 => NotUsed
      case 0x67 => NotUsed
      case 0x68 => PUSH(Immed16Operand(readWord))
      case 0x69 => NotUsed // IMUL reg,reg,word
      case 0x6A => PUSH(Immed8Operand(readByte))
      case 0x6B => NotUsed // IMUL reg,reg,byte
      case 0x6C => NotUsed
      case 0x6D => NotUsed
      case 0x6E => NotUsed
      case 0x6F => NotUsed

      case 0x70 => JO(ShortLabelOperand(readByte))
      case 0x71 => JNO(ShortLabelOperand(readByte))
      case 0x72 => JB_JBAE_JC(ShortLabelOperand(readByte))
      case 0x73 => JNB_JAE_JNC(ShortLabelOperand(readByte))
      case 0x74 => JE_JZ(ShortLabelOperand(readByte))
      case 0x75 => JNE_JNZ(ShortLabelOperand(readByte))
      case 0x76 => JBE_JNA(ShortLabelOperand(readByte))
      case 0x77 => JNBE_JA(ShortLabelOperand(readByte))
      case 0x78 => JS(ShortLabelOperand(readByte))
      case 0x79 => JNS(ShortLabelOperand(readByte))
      case 0x7A => JP_JPE(ShortLabelOperand(readByte))
      case 0x7B => JNP_JPO(ShortLabelOperand(readByte))
      case 0x7C => JL_JNGE(ShortLabelOperand(readByte))
      case 0x7D => JNL_JGE(ShortLabelOperand(readByte))
      case 0x7E => JLE_JNG(ShortLabelOperand(readByte))
      case 0x7F => JNLE_JG(ShortLabelOperand(readByte))

      case 0x80 => indirect.reset()
        val immed8 = Immed8Operand(readByte)
        val regOrMem8 = indirect.getRegOrMem8
        indirect.getRegIndex match {
          case 0 => ADD(regOrMem8, immed8)
          case 1 => OR(regOrMem8, immed8)
          case 2 => ADC(regOrMem8, immed8)
          case 3 => SBB(regOrMem8, immed8)
          case 4 => AND(regOrMem8, immed8)
          case 5 => SUB(regOrMem8, immed8)
          case 6 => XOR(regOrMem8, immed8)
          case 7 => CMP(regOrMem8, immed8)
        }
      case 0x81 => indirect.reset()
        val immed16 = Immed16Operand(readWord)
        val regOrMem16 = indirect.getRegOrMem16
        indirect.getRegIndex match {
          case 0 => ADD(regOrMem16, immed16)
          case 1 => OR(regOrMem16, immed16)
          case 2 => ADC(regOrMem16, immed16)
          case 3 => SBB(regOrMem16, immed16)
          case 4 => AND(regOrMem16, immed16)
          case 5 => SUB(regOrMem16, immed16)
          case 6 => XOR(regOrMem16, immed16)
          case 7 => CMP(regOrMem16, immed16)
        }

      case 0x82 => indirect.reset()
        val immed8 = Immed8Operand(readByte)
        val regOrMem8 = indirect.getRegOrMem8
        indirect.getRegIndex match {
          case 0 => ADD(regOrMem8, immed8)
          case 1 => NotUsed
          case 2 => ADC(regOrMem8, immed8)
          case 3 => SBB(regOrMem8, immed8)
          case 4 => NotUsed
          case 5 => SUB(regOrMem8, immed8)
          case 6 => NotUsed
          case 7 => CMP(regOrMem8, immed8)
        }

      case 0x83 => indirect.reset()
        indirect.getRegIndex match {
          case 0 => ADD(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 1 => NotUsed
          case 2 => ADC(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 3 => SBB(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 4 => NotUsed
          case 5 => SUB(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 6 => NotUsed
          case 7 => CMP(indirect.getRegOrMem16, Immed8Operand(readByte))
        }
      case 0x84 => indirect.reset(); TEST(indirect.getRegOrMem8, indirect.getReg8)
      case 0x85 => indirect.reset(); TEST(indirect.getRegOrMem16, indirect.getReg16)
      case 0x86 => indirect.reset(); XCHG(indirect.getRegOrMem8, indirect.getReg8)
      case 0x87 => indirect.reset(); XCHG(indirect.getRegOrMem16, indirect.getReg16)
      case 0x88 => indirect.reset(); MOV(indirect.getRegOrMem8, indirect.getReg8)
      case 0x89 => indirect.reset(); MOV(indirect.getRegOrMem16, indirect.getReg16)
      case 0x8A => indirect.reset(); MOV(indirect.getReg8, indirect.getRegOrMem8)
      case 0x8B => indirect.reset(); MOV(indirect.getReg16, indirect.getRegOrMem16)
      case 0x8C => indirect.reset()
        if (indirect.getRegIndex > 7) NotUsed
        else MOV(indirect.getRegOrMem16, indirect.getSeg)
      case 0x8D => indirect.reset()
        indirect.getRegOrMem16 match {
          case mem16: Mem8Operand => LEA(indirect.getReg16, mem16)
          case reg16: Reg8Operand => NotUsed
        }

      case 0x8E => indirect.reset()
        indirect.reset()
        if (indirect.getRegIndex > 7) NotUsed
        else MOV(indirect.getSeg, indirect.getRegOrMem16)
      case 0x8F => indirect.reset()
        indirect.getRegIndex match {
          case 0 => POP(indirect.getRegOrMem16)
          case 1 => NotUsed
          case 2 => NotUsed
          case 3 => NotUsed
          case 4 => NotUsed
          case 5 => NotUsed
          case 6 => NotUsed
          case 7 => NotUsed
        }

      case 0x90 => XCHG(Reg16Operand(AX), Reg16Operand(AX)) /* aka NOP - No Operation */
      case 0x91 => XCHG(Reg16Operand(AX), Reg16Operand(CX))
      case 0x92 => XCHG(Reg16Operand(AX), Reg16Operand(DX))
      case 0x93 => XCHG(Reg16Operand(AX), Reg16Operand(BX))
      case 0x94 => XCHG(Reg16Operand(AX), Reg16Operand(SP))
      case 0x95 => XCHG(Reg16Operand(AX), Reg16Operand(BP))
      case 0x96 => XCHG(Reg16Operand(AX), Reg16Operand(SI))
      case 0x97 => XCHG(Reg16Operand(AX), Reg16Operand(DI))
      case 0x98 => CBW()
      case 0x99 => CWD()
      case 0x9A => CALLF(FarProcOperand(Immed16Operand(readWord), Immed16Operand(readWord)))
      case 0x9B => WAIT()
      case 0x9C => PUSHF()
      case 0x9D => POPF()
      case 0x9E => SAHF()
      case 0x9F => LAHF()

      case 0xA0 => val addr = indirect.newAddress(DS, readWord); MOV(Reg8Operand(AL), Mem8Operand(addr))
      case 0xA1 => val addr = indirect.newAddress(DS, readWord); MOV(Reg16Operand(AX), Mem16Operand(addr))
      case 0xA2 => val addr = indirect.newAddress(DS, readWord); MOV(Mem8Operand(addr), Reg8Operand(AL))
      case 0xA3 => val addr = indirect.newAddress(DS, readWord); MOV(Mem16Operand(addr), Reg16Operand(AX))
      case 0xA4 => MOVSB()
      case 0xA5 => MOVSW()
      case 0xA6 => CMPSB()
      case 0xA7 => CMPSW()
      case 0xA8 => TEST(Reg8Operand(AL), Immed8Operand(readByte))
      case 0xA9 => TEST(Reg16Operand(AX), Immed16Operand(readWord))
      case 0xAA => STOSB()
      case 0xAB => STOSW()
      case 0xAC => LODSB()
      case 0xAD => LODSW()
      case 0xAE => SCASB()
      case 0xAF => SCASW()

      case 0xB0 => MOV(Reg8Operand(AL), Immed8Operand(readByte))
      case 0xB1 => MOV(Reg8Operand(CL), Immed8Operand(readByte))
      case 0xB2 => MOV(Reg8Operand(DL), Immed8Operand(readByte))
      case 0xB3 => MOV(Reg8Operand(BL), Immed8Operand(readByte))
      case 0xB4 => MOV(Reg8Operand(AH), Immed8Operand(readByte))
      case 0xB5 => MOV(Reg8Operand(CH), Immed8Operand(readByte))
      case 0xB6 => MOV(Reg8Operand(DH), Immed8Operand(readByte))
      case 0xB7 => MOV(Reg8Operand(BH), Immed8Operand(readByte))
      case 0xB8 => MOV(Reg16Operand(AX), Immed16Operand(readWord))
      case 0xB9 => MOV(Reg16Operand(CX), Immed16Operand(readWord))
      case 0xBA => MOV(Reg16Operand(DX), Immed16Operand(readWord))
      case 0xBB => MOV(Reg16Operand(BX), Immed16Operand(readWord))
      case 0xBC => MOV(Reg16Operand(SP), Immed16Operand(readWord))
      case 0xBD => MOV(Reg16Operand(BP), Immed16Operand(readWord))
      case 0xBE => MOV(Reg16Operand(SI), Immed16Operand(readWord))
      case 0xBF => MOV(Reg16Operand(DI), Immed16Operand(readWord))

      case 0xC0 => indirect.reset()
        indirect.getRegIndex match {
          case 0 => ROL(indirect.getRegOrMem8, Immed8Operand(readByte))
          case 1 => ROR(indirect.getRegOrMem8, Immed8Operand(readByte))
          case 2 => RCL(indirect.getRegOrMem8, Immed8Operand(readByte))
          case 3 => RCR(indirect.getRegOrMem8, Immed8Operand(readByte))
          case 4 => SHL_SAL(indirect.getRegOrMem8, Immed8Operand(readByte))
          case 5 => SHR(indirect.getRegOrMem8, Immed8Operand(readByte))
          case 6 => NotUsed
          //case 7 => SAR(indirect.getRegOrMem8, Immed8Operand(readByte))
        }
      case 0xC1 => indirect.reset()
        indirect.getRegIndex match {
          case 0 => ROL(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 1 => ROR(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 2 => RCL(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 3 => RCR(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 4 => SHL_SAL(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 5 => SHR(indirect.getRegOrMem16, Immed8Operand(readByte))
          case 6 => NotUsed
          //case 7 => SAR(indirect.getRegOrMem16, Immed8Operand(readByte))
        }
      case 0xC2 => RETN(Immed16Operand(readWord))
      case 0xC3 => RETN()
      case 0xC4 => indirect.reset(); if (indirect.isMemDefined) LES(indirect.getReg16, indirect.getMem16) else InvalidOpcode
      case 0xC5 => indirect.reset(); if (indirect.isMemDefined) LDS(indirect.getReg16, indirect.getMem16) else InvalidOpcode
      case 0xC6 => indirect.reset()
        indirect.getRegIndex match {
          //case 0 => MOV(indirect.getMem8, Immed8Operand(readByte))
          case 1 => NotUsed
          case 2 => NotUsed
          case 3 => NotUsed
          case 4 => NotUsed
          case 5 => NotUsed
          case 6 => NotUsed
          case 7 => NotUsed
        }
      case 0xC7 => indirect.reset()
        indirect.getRegIndex match {
          case 0 => if (indirect.isMemDefined) MOV(indirect.getMem16, Immed16Operand(readWord)) else InvalidOpcode
          case 1 => NotUsed
          case 2 => NotUsed
          case 3 => NotUsed
          case 4 => NotUsed
          case 5 => NotUsed
          case 6 => NotUsed
          case 7 => NotUsed
        }
      case 0xC8 => ENTER()
      case 0xC9 => LEAVE()
      case 0xCA => RETF(Immed16Operand(readWord))
      case 0xCB => RETF()
      case 0xCC => INT(3)
      //case 0xCD => INT(Immed8Operand(readByte))
      case 0xCE => INTO()
      case 0xCF => IRET()

      case 0xD0 => indirect.reset()
        indirect.getRegIndex match {
          //case 0 => ROL(indirect.getRegOrMem8, 1)
          //case 1 => ROR(indirect.getRegOrMem8, 1)
          //case 2 => RCL(indirect.getRegOrMem8, 1)
          //case 3 => RCR(indirect.getRegOrMem8, 1)
          //case 4 => SHL_SAL(indirect.getRegOrMem8, 1)
          //case 5 => SHR(indirect.getRegOrMem8, 1)
          case 6 => NotUsed
          //case 7 => SAR(indirect.getRegOrMem8, 1)
        }
      case 0xD1 => indirect.reset()
        indirect.getRegIndex match {
          //case 0 => ROL(indirect.getRegOrMem16, 1)
          //case 1 => ROR(indirect.getRegOrMem16, 1)
          //case 2 => RCL(indirect.getRegOrMem16, 1)
          //case 3 => RCR(indirect.getRegOrMem16, 1)
          //case 4 => SHL_SAL(indirect.getRegOrMem16, 1)
          //case 5 => SHR(indirect.getRegOrMem16, 1)
          case 6 => NotUsed
          //case 7 => SAR(indirect.getRegOrMem16, 1)
        }
      case 0xD2 => indirect.reset()
        indirect.getRegIndex match {
          case 0 => ROL(indirect.getRegOrMem8, Reg8Operand(CL))
          case 1 => ROR(indirect.getRegOrMem8, Reg8Operand(CL))
          case 2 => RCL(indirect.getRegOrMem8, Reg8Operand(CL))
          case 3 => RCR(indirect.getRegOrMem8, Reg8Operand(CL))
          case 4 => SHL_SAL(indirect.getRegOrMem8, Reg8Operand(CL))
          case 5 => SHR(indirect.getRegOrMem8, Reg8Operand(CL))
          case 6 => NotUsed
          //case 7 => SAR(indirect.getRegOrMem8, Reg8Operand(CL))
        }
      case 0xD3 => indirect.reset()
        indirect.getRegIndex match {
          case 0 => ROL(indirect.getRegOrMem16, Reg8Operand(CL))
          case 1 => ROR(indirect.getRegOrMem16, Reg8Operand(CL))
          case 2 => RCL(indirect.getRegOrMem16, Reg8Operand(CL))
          case 3 => RCR(indirect.getRegOrMem16, Reg8Operand(CL))
          case 4 => SHL_SAL(indirect.getRegOrMem16, Reg8Operand(CL))
          case 5 => SHR(indirect.getRegOrMem16, Reg8Operand(CL))
          case 6 => NotUsed
          //case 7 => SAR(indirect.getRegOrMem16, Reg8Operand(CL))
        }
      case 0xD4 => AAM()
      case 0xD5 => AAD()
      case 0xD6 => NotUsed
      case 0xD7 => XLAT(Mem16Operand(indirect.newAddress(DS, BX, AL)))
      case 0xD8 => NotUsed // ESC???
      case 0xD9 => NotUsed
      case 0xDA => NotUsed
      case 0xDB => NotUsed
      case 0xDC => NotUsed
      case 0xDD => NotUsed
      case 0xDE => NotUsed
      case 0xDF => NotUsed

      case 0xE0 => LOOPNZ_LOOPNE(ShortLabelOperand(readByte))
      case 0xE1 => LOOPZ_LOOPE(ShortLabelOperand(readByte))
      case 0xE2 => LOOP(ShortLabelOperand(readByte))
      case 0xE3 => JCXZ(ShortLabelOperand(readByte))
      case 0xE4 => IN(AccumulatorOperand(AL), Immed8Operand(readByte))
      case 0xE5 => IN(AccumulatorOperand(AX), Immed8Operand(readByte))
      case 0xE6 => OUT(AccumulatorOperand(AL), Immed8Operand(readByte))
      case 0xE7 => OUT(AccumulatorOperand(AX), Immed8Operand(readByte))
      case 0xE8 => CALLN(NearProcOperand(Immed16Operand(readWord)))
      case 0xE9 => JMP(NearLabelOperand(Immed16Operand(readWord)))
      case 0xEA => JMP(FarLabelOperand(Immed16Operand(readWord), Immed16Operand(readWord)))
      case 0xEB => JMP(ShortLabelOperand(readByte))
      case 0xEC => IN(AccumulatorOperand(AL), Reg16Operand(DX))
      case 0xED => IN(AccumulatorOperand(AX), Reg16Operand(DX))
      case 0xEE => OUT(AccumulatorOperand(AL), Reg16Operand(DX))
      case 0xEF => OUT(AccumulatorOperand(AX), Reg16Operand(DX))

      case 0xF0 => NotUsed // LOCK
      case 0xF1 => NotUsed
      case 0xF2 => indirect.setRepeatWhileNotEqual(); decode(indirect) // REPNE/REPNZ
      case 0xF3 => indirect.setRepeatWhileEqual(); decode(indirect) // REP/REPE/REPZ
      case 0xF4 => HLT()
      case 0xF5 => CMC()
      case 0xF6 => indirect.reset()
        indirect.getRegIndex match {
          case 0 => TEST(indirect.getRegOrMem8, Immed8Operand(readByte))
          case 1 => NotUsed
          case 2 => NOT(indirect.getRegOrMem8)
          case 3 => NEG(indirect.getRegOrMem8)
          case 4 => MUL(indirect.getRegOrMem8)
          case 5 => IMUL(indirect.getRegOrMem8)
          case 6 => DIV(indirect.getRegOrMem8)
          case 7 => IDIV(indirect.getRegOrMem8)
        }
      case 0xF7 => indirect.reset()
        indirect.getRegIndex match {
          case 0 => TEST(indirect.getRegOrMem16, Immed16Operand(readWord))
          case 1 => NotUsed
          case 2 => NOT(indirect.getRegOrMem16)
          case 3 => NEG(indirect.getRegOrMem16)
          case 4 => MUL(indirect.getRegOrMem16)
          case 5 => IMUL(indirect.getRegOrMem16)
          case 6 => DIV(indirect.getRegOrMem16)
          case 7 => IDIV(indirect.getRegOrMem16)
        }
      case 0xF8 => CLC()
      case 0xF9 => STC()
      case 0xFA => CLI()
      case 0xFB => STI()
      case 0xFC => CLD()
      case 0xFD => STD()
      case 0xFE => indirect.reset()
        indirect.getRegIndex match {
          case 0 => INC(indirect.getRegOrMem8)
          case 1 => DEC(indirect.getRegOrMem8)
          case 2 => NotUsed
          case 3 => NotUsed
          case 4 => NotUsed
          case 5 => NotUsed
          case 6 => NotUsed
          case 7 => NotUsed
        }
      case 0xFF => indirect.reset()
        indirect.getRegIndex match {
          case 0 => if (indirect.isMemDefined) INC(indirect.getMem16) else InvalidOpcode
          case 1 => if (indirect.isMemDefined) DEC(indirect.getMem16) else InvalidOpcode
          case 2 => CALLN(NearProcOperand(indirect.getRegOrMem16))
          case 3 => if (indirect.isMemDefined) CALLF(FarProcOperand(indirect.getMem16, indirect.getMem16Next)) else InvalidOpcode
          case 4 => JMP(NearLabelOperand(indirect.getRegOrMem16))
          //case 5 => if (indirect.isMemDefined) JMP(FarLabelOperand(indirect.getMem16)) else InvalidOpcode
          case 6 => if (indirect.isMemDefined) PUSH(indirect.getMem16) else InvalidOpcode
          case 7 => NotUsed
        }

    }
  }

  private def readByte = opcodeFetcher.nextByte

  private def readWord = opcodeFetcher.nextWord
}
