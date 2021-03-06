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
  * Decoder for MOD + REG + REG/MEM encoding in Cpu instructions
  *
  * @author Romi Kuntsman <romik.code@gmail.com>
  * @since 2016-12-25
  */

class IndirectAddressingDecoder(val cpu: Cpu) {

  private val m_state = cpu.state
  private val m_fetcher: OpcodeFetcher = cpu.opcodeFetcher
  private var regIndex: M86Byte = M86Byte(0)
  private var regOrMemIndex: M86Byte = M86Byte(0)
  private var memAddress: Option[RealModeAddress] = None
  private var regOperand: Operand = _
  private var regOrMemOperand: Operand = _
  private var forcedSegReg: Option[SegmentRegister] = None
  private var repeatWhileEqual: Option[Boolean] = None // true = REP/REPE/REPZ; false = REPNE/REPNZ

  // def getMemAddress: Address = m_memAddress

  def reset(): Unit = {
    val modeByte = m_fetcher.nextByte

    val mode: M86Byte = (modeByte >> 6) & 0x03
    regIndex = (modeByte >> 3) & 0x07
    regOrMemIndex = modeByte & 0x07

    if (mode == 3) {
      memAddress = None
    }
    else {
      memAddress = Some(getModeAddress(mode))
    }
  }

  private def getModeAddress(mode: M86Byte): RealModeAddress = {
    val displacement: M86Word = mode match {
      case M86Byte(0) => M86Word(0)
      case M86Byte(1) => M86Word(m_fetcher.nextByte)
      case M86Byte(2) => m_fetcher.nextWord
    }

    regOrMemIndex match {
      case M86Byte(0) => newAddress(DS, m_state.bx + m_state.si + displacement)
      case M86Byte(1) => newAddress(DS, m_state.bx + m_state.di + displacement)
      case M86Byte(2) => newAddress(SS, m_state.bp + m_state.si + displacement)
      case M86Byte(3) => newAddress(SS, m_state.bp + m_state.di + displacement)
      case M86Byte(4) => newAddress(DS, m_state.si + displacement)
      case M86Byte(5) => newAddress(DS, m_state.di + displacement)
      case M86Byte(6) => if (mode == 0) {
        newAddress(DS, m_fetcher.nextWord)
      }
      else {
        newAddress(SS, m_state.bp + displacement)
      }
      case M86Byte(7) => newAddress(DS, m_state.bx + displacement)
    }
  }

  def newAddress(segIndex: SegmentRegister, offset: M86Word): RealModeAddress = {
    val segmentRegister = forcedSegReg.getOrElse(segIndex)
    val segment: M86Word = m_state.getRegister16(segmentRegister)
    new RealModeAddress(segment, offset)
  }

  def getRegIndex: M86Byte = regIndex

  def getReg8: Reg8Operand = Reg8Operand(Register.getReg8(regIndex))

  def getMem8: Option[Mem8Operand] = if (isMemDefined) Some(Mem8Operand(memAddress.get)) else None

  def getRegOrMem8: RegisterOrMemoryOperand = {
    if (isMemDefined) {
      Mem8Operand(memAddress.get)
    }
    else {
      Reg8Operand(Register.getReg8(regOrMemIndex))
    }
  }

  def getReg16: Reg16Operand = Reg16Operand(Register.getReg16(regIndex))

  def getSeg: Reg16Operand = Reg16Operand(Register.getSeg(regIndex))

  def getMem16: Mem16Operand = if (isMemDefined) Mem16Operand(memAddress.get) else throw new IllegalArgumentException("Memory Undefined")

  def isMemDefined: Boolean = memAddress.isDefined

  def getMem16Next: Mem16Operand = if (isMemDefined) Mem16Operand(memAddress.get.addOffset(M86Word(2))) else throw new IllegalArgumentException("Memory Undefined")

  def getRegOrMem16: RegisterOrMemoryOperand = {
    if (isMemDefined) {
      Mem16Operand(memAddress.get)
    }
    else {
      Reg16Operand(Register.getReg16(regOrMemIndex))
    }
  }

  def forceSegReg(reg: SegmentRegister) {
    forcedSegReg = Option(reg)
  }

  def setRepeatWhileEqual(): Unit = {
    repeatWhileEqual = Some(true)
  }

  def setRepeatWhileNotEqual(): Unit = {
    repeatWhileEqual = Some(false)
  }

  def newAddress(segIndex: SegmentRegister, baseReg: WordRegister, indexReg: ByteRegister): RealModeAddress = {
    val segment: M86Word = m_state.getRegister16(forcedSegReg.getOrElse(segIndex))
    val offsetBase: M86Word = m_state.getRegister16(baseReg)
    val offsetIndex: M86Byte = m_state.getRegister8(indexReg)
    new RealModeAddress(segment, offsetBase + offsetIndex)
  }

  def newAddress(segIndex: SegmentRegister, baseReg: WordRegister, indexReg: WordRegister): RealModeAddress = {
    val segment: M86Word = m_state.getRegister16(forcedSegReg.getOrElse(segIndex))
    val offsetBase: M86Word = m_state.getRegister16(baseReg)
    val offsetIndex: M86Word = m_state.getRegister16(indexReg)
    new RealModeAddress(segment, offsetBase + offsetIndex)
  }
}
