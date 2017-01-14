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
import il.co.codeguru.extreme.engine.datatypes.{M86Byte, M86Word}

/**
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class Cpu(var state: CpuState, var machine: Machine) {
  val opcodeFetcher = new OpcodeFetcher(this)

  val opcodeDecoder = new MachineInstructionDecoder(this, opcodeFetcher)

  var cpuTime = 0

  def nextOpcode(): Unit = {
    val opCode = opcodeDecoder.decode()

    val opTime = runOperation(opCode)

    cpuTime += opTime
  }

  def runOperation(opcode: OperationCode): Int = opcode match {
    case AAA() => ???
    case AAD() => ???
    case AAM() => ???
    case AAS() => ???
    case ADC(destination, source) => ???
    case ADD(destination, source) => ???
    case AND(destination, source) => ???
    case CALLF(proc) => ???
    case CALLN(proc) => ???
    case CBW() => ???
    case CLC() => ???
    case CLD() => ???
    case CLI() => ???
    case CMC() => ???
    case CMP(destination, source) => ???
    case CMPSB(destination, source) => ???
    case CMPSW(destination, source) => ???
    case CWD() => ???
    case DAA() => ???
    case DAS() => ???
    case DEC(destination) => ???
    case DIV(source) => ???
    case ENTER() => ???
    case ESC() => ???
    case HLT() => ???
    case IDIV(source) => ???
    case IMUL(source) => ???
    case IN(accumulator, port) => ???
    case INC(destination) => ???
    case INT(interruptType) => ???
    case INTO() => ???
    case IRET() => ???
    case JBE_JNA(shortLabel) => ???
    case JB_JBAE_JC(shortLabel) => ???
    case JCXZ(shortLabel) => ???
    case JE_JZ(shortLabel) => ???
    case JLE_JNG(shortLabel) => ???
    case JL_JNGE(shortLabel) => ???

    case JMP(target) => target match {
      case ShortLabelOperand(offset) =>
        state = state.setRegister16(IP, state.ip + M86Word(offset))
        15
      case NearLabelOperand(offset) => offset match {
        case offset: Immed16Operand =>
          state = state.setRegister16(IP, state.ip + offset.value)
          15
        case offset: Reg16Operand =>
          state = state.setRegister16(IP, state.getRegister16(offset.register))
          11
        case offset: Mem16Operand =>
          state = state.setRegister16(IP, machine.memory.readWord(offset.address, execute = false))
          18 + machine.memory.getTransferCost(offset.address) // + EA
      }
      case FarLabelOperand(offset, segment) => (offset, segment) match {
        case (offset: Immed16Operand, segment: Immed16Operand) =>
          state = state.setRegister16(IP, offset.value)
          state = state.setRegister16(CS, segment.value)
          15
        case (offset: Mem16Operand, segment: Mem16Operand) =>
          val offsetVal = machine.memory.readWord(offset.address, execute = false)
          val segmentVal = machine.memory.readWord(segment.address, execute = false)
          state = state.setRegister16(IP, offsetVal)
          state = state.setRegister16(CS, segmentVal)
          24 + machine.memory.getTransferCost(offset.address) + machine.memory.getTransferCost(segment.address) // + EA
      }
    }

    case JNBE_JA(shortLabel) => ???
    case JNB_JAE_JNC(shortLabel) => ???
    case JNE_JNZ(shortLabel) => ???
    case JNLE_JG(shortLabel) => ???
    case JNL_JGE(shortLabel) => ???
    case JNO(shortLabel) => ???
    case JNP_JPO(shortLabel) => ???
    case JNS(shortLabel) => ???
    case JO(shortLabel) => ???
    case JP_JPE(shortLabel) => ???
    case JS(shortLabel) => ???
    case LAHF() => ???
    case LDS(destination, source) => ???
    case LEA(destination, source) => ???
    case LEAVE() => ???
    case LES(destination, source) => ???
    case LOCK() => ???
    case LODSB(source) => ???
    case LODSW(source) => ???
    case LOOP(shortLabel) => ???
    case LOOPNZ_LOOPNE(shortLabel) => ???
    case LOOPZ_LOOPE(shortLabel) => ???
    //

    case MOV(destination, source) => (destination, source) match {
      //case (dest: Mem8Operand, src: AccumulatorOperand) => MOV(dest, src, 10)
      //case (dest: AccumulatorOperand, src: Mem8Operand) => MOV(dest, src, 10)
      //case (dest: Reg8Operand, src: Reg8Operand) => MOV(dest, src, 2)
      //case (dest: Reg8Operand, src: Mem8Operand) => MOV(dest, src, 8)
      //case (dest: Mem8Operand, src: Reg8Operand) => MOV(dest, src, 9)
      //case (dest: Reg8Operand, src: ImmediateOperand) => MOV(dest, src, 4)
      //case (dest: Mem8Operand, src: ImmediateOperand) => MOV(dest, src, 4)
      //case (dest: SegRegOperand, src: Reg16Operand) => MOV(dest, src, 2)
      //case (dest: SegRegOperand, src: Mem16Operand) => MOV(dest, src, 8)
      //case (dest: Reg16Operand, src: SegRegOperand) => MOV(dest, src, 2)
      //case (dest: Mem8Operand, src: SegRegOperand) => MOV(dest, src, 9)
      case (dest: Reg16Operand, src: Immed16Operand) => state = state.setRegister16(dest.register, src.value); 8
      case (dest: Reg16Operand, src: Reg16Operand) => state = state.setRegister16(dest.register, state.getRegister16(src.register)); 8
    }

    case MOVSB(destination, source) => ???
    case MOVSW(destination, source) => ???
    case MUL(source) => ???
    case NEG(destination) => ???
    case NOT(destination) => ???
    case OR(destination, source) => ???
    case OUT(accumulator, port) => ???
    case POP(destination) => ???
    case POPA() => ???
    case POPF() => ???
    case PUSH(source) => ???
    case PUSHA() => ???
    case PUSHF() => ???
    case RCL(destination, count) => ???
    case RCR(destination, count) => ???
    case RETF(popValue) => ???
    case RETN(popValue) => ???
    case ROL(destination, count) => ???
    case ROR(destination, count) => ???
    case SAAR(destination, count) => ???
    case SAHF() => ???
    case SAR() => ???
    case SBB(destination, source) => ???
    case SCASB(destination) => ???
    case SCASW(destination) => ???
    case SEGMENT() => ???
    case SHL_SAL(destination, count) => ???
    case SHR(destination, source) => ???
    case STC() => ???
    case STD() => ???
    case STI() => ???
    case STOSB(destination) => ???
    case STOSW(destination) => ???
    case SUB(destination, source) => ???
    case TEST(destination, source) => ???
    case WAIT() => ???
    case XCHG(destination, source) => ???
    case XLAT(translateTable) => ???
    case XOR(destination, source) => ???
  }

  private def calculateEffectiveAddress(memoryAddressing: MemoryAddressing, defaultSegment: M86Word): (RealModeAddress, Int) = {
    def effectiveAddress(baseRegister: AddressBaseRegister = null,
                         indexRegister: IndexRegister = null,
                         displacement: M86Word = M86Word(0)): RealModeAddress = {
      val segment: M86Word = baseRegister match {
        case BP => state.ss
        case _ => defaultSegment
      }
      val offset = (baseRegister match {
        case BX => state.bx
        case BP => state.bp
        case _ => M86Word(0)
      }) + (indexRegister match {
        case DI => state.di
        case SI => state.si
        case _ => M86Word(0)
      }) + displacement
      new RealModeAddress(segment, offset)
    }
    // TODO add support for segment override
    memoryAddressing match {
      case MemoryDirectAddressing(offset) =>
        (effectiveAddress(displacement = offset.value), 6)
      case MemoryBaseAddressing(baseRegister) =>
        (effectiveAddress(baseRegister = baseRegister), 5)
      case MemoryBaseDisp8Addressing(baseRegister, displacement) =>
        (effectiveAddress(baseRegister = baseRegister, displacement = M86Word(displacement.value)), 9)
      case MemoryBaseDisp16Addressing(baseRegister, displacement) =>
        (effectiveAddress(baseRegister = baseRegister, displacement = displacement.value), 9)
      case MemoryIndexAddressing(indexRegister) =>
        (effectiveAddress(indexRegister = indexRegister), 5)
      case MemoryIndexDisp8Addressing(indexRegister, displacement) =>
        (effectiveAddress(indexRegister = indexRegister, displacement = M86Word(displacement.value)), 9)
      case MemoryIndexDisp16Addressing(indexRegister, displacement) =>
        (effectiveAddress(indexRegister = indexRegister, displacement = displacement.value), 9)
      case MemoryBaseIndexAddressing(baseRegister, indexRegister) =>
        val address = effectiveAddress(baseRegister = baseRegister, indexRegister = indexRegister)
        (baseRegister, indexRegister) match {
          case (BP, DI) | (BX, SI) => (address, 7)
          case (BP, SI) | (BX, DI) => (address, 8)
        }
      case MemoryBaseIndexDisp8Addressing(baseRegister, indexRegister, displacement) =>
        val address = effectiveAddress(baseRegister, indexRegister, M86Word(displacement.value))
        (baseRegister, indexRegister) match {
          case (BP, DI) | (BX, SI) => (address, 11)
          case (BP, SI) | (BX, DI) => (address, 12)
        }
      case MemoryBaseIndexDisp16Addressing(baseRegister, indexRegister, displacement) =>
        val address = effectiveAddress(baseRegister, indexRegister, displacement.value)
        (baseRegister, indexRegister) match {
          case (BP, DI) | (BX, SI) => (address, 11)
          case (BP, SI) | (BX, DI) => (address, 12)
        }
      case MemoryBaseReg8Addressing(baseRegister, displacement) =>
        (effectiveAddress(baseRegister = baseRegister, displacement = M86Word(state.getRegister8(displacement))), 9)
    }
  }
}

class OpcodeFetcher(val cpu: Cpu) {
  def nextByte(): M86Byte = {
    val address = cpu.state.csip
    cpu.state = cpu.state.setRegister16(IP, cpu.state.ip + M86Word(1))
    cpu.machine.memory.readByte(address, execute = true)
  }

  def nextWord(): M86Word = {
    val address = cpu.state.csip
    cpu.state = cpu.state.setRegister16(IP, cpu.state.ip + M86Word(2))
    cpu.machine.memory.readWord(address, execute = true)
  }
}
