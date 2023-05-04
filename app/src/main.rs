use std::env::args;
use std::fs::read;
use std::collections::{BTreeSet, HashMap};

const OFFSET_STR: &str = "#OFFSET#";

#[derive(PartialEq, Clone, Copy)]
enum BitValue{
    BV0 = 0,
    BV1 = 1
}

#[derive(PartialEq, Clone, Copy)]
enum TwoBitValue{
    DBV00 = 0b00,
    DBV01 = 0b01,
    DBV10 = 0b10,
    DBV11 = 0b11
}

#[derive(PartialEq, Clone, Copy)]
enum ThreeBitValue{
    TBV000 = 0b000,
    TBV001 = 0b001,
    TBV010 = 0b010,
    TBV011 = 0b011,
    TBV100 = 0b100,
    TBV101 = 0b101,
    TBV110 = 0b110,
    TBV111 = 0b111
}

#[derive (Clone, Copy)]
enum OpcodeParseType
{
    RegRmWithDisp,
    RegRmWithDispD1,
    RegRmWithDispD1W1,
    ImmReg,
    ImmRm,
    AccMem,
    ImmAcc,
    Jump,
    RmWithDisp,
    ShiftRot,
    SingleByteWithReg,
    SingleByteWithSr,
    InOut,
    AsciiAdjust,
    Repeat,
    Return,
    Direct,
    Nop
}

const D_MASK: u8 = 0x02;
const D_SHFT: u8 = 1;

const S_MASK: u8 = 0x02;
const S_SHFT: u8 = 1;

const V_MASK: u8 = 0x02;
const V_SHFT: u8 = 1;

const W_MASK: u8 = 0x01;
const W_SHFT: u8 = 0;

const IMMW_MASK: u8 = 0x08;
const IMMW_SHFT: u8 = 3;

const MOD_MASK: u8 = 0xC0;
const MOD_SHFT: u8 = 6;

const REG_MASK: u8 = 0x38;
const REG_SHFT: u8 = 3;

const IMMREG_MASK: u8 = 0x07;
const IMMREG_SHFT: u8 = 0;

const RM_MASK: u8 = 0x07;
const RM_SHFT: u8 = 0;

const SUB_CODE_MASK: u8 = REG_MASK;
const SUB_CODE_SHFT: u8 = REG_SHFT;


#[derive (Clone, Copy)]
struct OpcodeTableEntry {
    mnemonic: &'static str,
    opt: OpcodeParseType
}

const OPCODE_TABLE: [OpcodeTableEntry; 256] =
    [
        OpcodeTableEntry { mnemonic: "add", opt: OpcodeParseType::RegRmWithDisp}, //0x00
        OpcodeTableEntry { mnemonic: "add", opt: OpcodeParseType::RegRmWithDisp}, //0x01
        OpcodeTableEntry { mnemonic: "add", opt: OpcodeParseType::RegRmWithDisp}, //0x02
        OpcodeTableEntry { mnemonic: "add", opt: OpcodeParseType::RegRmWithDisp}, //0x03
        OpcodeTableEntry { mnemonic: "add", opt: OpcodeParseType::ImmAcc}, //0x04
        OpcodeTableEntry { mnemonic: "add", opt: OpcodeParseType::ImmAcc}, //0x05
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithSr}, //0x06
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithSr}, //0x07
        OpcodeTableEntry { mnemonic: "or", opt: OpcodeParseType::RegRmWithDisp}, //0x08
        OpcodeTableEntry { mnemonic: "or", opt: OpcodeParseType::RegRmWithDisp}, //0x09
        OpcodeTableEntry { mnemonic: "or", opt: OpcodeParseType::RegRmWithDisp}, //0x0A
        OpcodeTableEntry { mnemonic: "or", opt: OpcodeParseType::RegRmWithDisp}, //0x0B
        OpcodeTableEntry { mnemonic: "or", opt: OpcodeParseType::ImmAcc}, //0x0C
        OpcodeTableEntry { mnemonic: "or", opt: OpcodeParseType::ImmAcc}, //0x0D
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithSr}, //0x0E
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x0F
        OpcodeTableEntry { mnemonic: "adc", opt: OpcodeParseType::RegRmWithDisp}, //0x10
        OpcodeTableEntry { mnemonic: "adc", opt: OpcodeParseType::RegRmWithDisp}, //0x11
        OpcodeTableEntry { mnemonic: "adc", opt: OpcodeParseType::RegRmWithDisp}, //0x12
        OpcodeTableEntry { mnemonic: "adc", opt: OpcodeParseType::RegRmWithDisp}, //0x13
        OpcodeTableEntry { mnemonic: "adc", opt: OpcodeParseType::ImmAcc}, //0x14
        OpcodeTableEntry { mnemonic: "adc", opt: OpcodeParseType::ImmAcc}, //0x15
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithSr}, //0x16
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithSr}, //0x17
        OpcodeTableEntry { mnemonic: "sbb", opt: OpcodeParseType::RegRmWithDisp}, //0x18
        OpcodeTableEntry { mnemonic: "sbb", opt: OpcodeParseType::RegRmWithDisp}, //0x19
        OpcodeTableEntry { mnemonic: "sbb", opt: OpcodeParseType::RegRmWithDisp}, //0x1A
        OpcodeTableEntry { mnemonic: "sbb", opt: OpcodeParseType::RegRmWithDisp}, //0x1B
        OpcodeTableEntry { mnemonic: "sbb", opt: OpcodeParseType::ImmAcc}, //0x1C
        OpcodeTableEntry { mnemonic: "sbb", opt: OpcodeParseType::ImmAcc}, //0x1D
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithSr}, //0x1E
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithSr}, //0x1F
        OpcodeTableEntry { mnemonic: "and", opt: OpcodeParseType::RegRmWithDisp}, //0x20
        OpcodeTableEntry { mnemonic: "and", opt: OpcodeParseType::RegRmWithDisp}, //0x21
        OpcodeTableEntry { mnemonic: "and", opt: OpcodeParseType::RegRmWithDisp}, //0x22
        OpcodeTableEntry { mnemonic: "and", opt: OpcodeParseType::RegRmWithDisp}, //0x23
        OpcodeTableEntry { mnemonic: "and", opt: OpcodeParseType::ImmAcc}, //0x24
        OpcodeTableEntry { mnemonic: "and", opt: OpcodeParseType::ImmAcc}, //0x25
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x26
        OpcodeTableEntry { mnemonic: "daa", opt: OpcodeParseType::Direct}, //0x27
        OpcodeTableEntry { mnemonic: "sub", opt: OpcodeParseType::RegRmWithDisp}, //0x28
        OpcodeTableEntry { mnemonic: "sub", opt: OpcodeParseType::RegRmWithDisp}, //0x29
        OpcodeTableEntry { mnemonic: "sub", opt: OpcodeParseType::RegRmWithDisp}, //0x2A
        OpcodeTableEntry { mnemonic: "sub", opt: OpcodeParseType::RegRmWithDisp}, //0x2B
        OpcodeTableEntry { mnemonic: "sub", opt: OpcodeParseType::ImmAcc}, //0x2C
        OpcodeTableEntry { mnemonic: "sub", opt: OpcodeParseType::ImmAcc}, //0x2D
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x2E
        OpcodeTableEntry { mnemonic: "das", opt: OpcodeParseType::Direct}, //0x2F
        OpcodeTableEntry { mnemonic: "xor", opt: OpcodeParseType::RegRmWithDisp}, //0x30
        OpcodeTableEntry { mnemonic: "xor", opt: OpcodeParseType::RegRmWithDisp}, //0x31
        OpcodeTableEntry { mnemonic: "xor", opt: OpcodeParseType::RegRmWithDisp}, //0x32
        OpcodeTableEntry { mnemonic: "xor", opt: OpcodeParseType::RegRmWithDisp}, //0x33
        OpcodeTableEntry { mnemonic: "xor", opt: OpcodeParseType::ImmAcc}, //0x34
        OpcodeTableEntry { mnemonic: "xor", opt: OpcodeParseType::ImmAcc}, //0x35
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x36
        OpcodeTableEntry { mnemonic: "aaa", opt: OpcodeParseType::Direct}, //0x37
        OpcodeTableEntry { mnemonic: "cmp", opt: OpcodeParseType::RegRmWithDisp}, //0x38
        OpcodeTableEntry { mnemonic: "cmp", opt: OpcodeParseType::RegRmWithDisp}, //0x39
        OpcodeTableEntry { mnemonic: "cmp", opt: OpcodeParseType::RegRmWithDisp}, //0x3A
        OpcodeTableEntry { mnemonic: "cmp", opt: OpcodeParseType::RegRmWithDisp}, //0x3B
        OpcodeTableEntry { mnemonic: "cmp", opt: OpcodeParseType::ImmAcc}, //0x3C
        OpcodeTableEntry { mnemonic: "cmp", opt: OpcodeParseType::ImmAcc}, //0x3D
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x3E
        OpcodeTableEntry { mnemonic: "aas", opt: OpcodeParseType::Direct}, //0x3F
        OpcodeTableEntry { mnemonic: "inc ax", opt: OpcodeParseType::Direct}, //0x40
        OpcodeTableEntry { mnemonic: "inc cx", opt: OpcodeParseType::Direct}, //0x41
        OpcodeTableEntry { mnemonic: "inc dx", opt: OpcodeParseType::Direct}, //0x42
        OpcodeTableEntry { mnemonic: "inc bx", opt: OpcodeParseType::Direct}, //0x43
        OpcodeTableEntry { mnemonic: "inc sp", opt: OpcodeParseType::Direct}, //0x44
        OpcodeTableEntry { mnemonic: "inc bp", opt: OpcodeParseType::Direct}, //0x45
        OpcodeTableEntry { mnemonic: "inc si", opt: OpcodeParseType::Direct}, //0x46
        OpcodeTableEntry { mnemonic: "inc di", opt: OpcodeParseType::Direct}, //0x47
        OpcodeTableEntry { mnemonic: "dec ax", opt: OpcodeParseType::Direct}, //0x48
        OpcodeTableEntry { mnemonic: "dec cx", opt: OpcodeParseType::Direct}, //0x49
        OpcodeTableEntry { mnemonic: "dec dx", opt: OpcodeParseType::Direct}, //0x4A
        OpcodeTableEntry { mnemonic: "dec bx", opt: OpcodeParseType::Direct}, //0x4B
        OpcodeTableEntry { mnemonic: "dec sp", opt: OpcodeParseType::Direct}, //0x4C
        OpcodeTableEntry { mnemonic: "dec bp", opt: OpcodeParseType::Direct}, //0x4D
        OpcodeTableEntry { mnemonic: "dec si", opt: OpcodeParseType::Direct}, //0x4E
        OpcodeTableEntry { mnemonic: "dec di", opt: OpcodeParseType::Direct}, //0x4F
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithReg}, //0x50
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithReg}, //0x51
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithReg}, //0x52
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithReg}, //0x53
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithReg}, //0x54
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithReg}, //0x55
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithReg}, //0x56
        OpcodeTableEntry { mnemonic: "push", opt: OpcodeParseType::SingleByteWithReg}, //0x57
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithReg}, //0x58
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithReg}, //0x59
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithReg}, //0x5A
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithReg}, //0x5B
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithReg}, //0x5C
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithReg}, //0x5D
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithReg}, //0x5E
        OpcodeTableEntry { mnemonic: "pop", opt: OpcodeParseType::SingleByteWithReg}, //0x5F
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x60
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x61
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x62
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x63
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x64
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x65
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x66
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x67
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x68
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x69
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x6A
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x6B
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x6C
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x6D
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x6E
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x6F
        OpcodeTableEntry { mnemonic: "jo", opt: OpcodeParseType::Jump}, //0x70
        OpcodeTableEntry { mnemonic: "jno", opt: OpcodeParseType::Jump}, //0x71
        OpcodeTableEntry { mnemonic: "jb", opt: OpcodeParseType::Jump}, //0x72
        OpcodeTableEntry { mnemonic: "jnb", opt: OpcodeParseType::Jump}, //0x73
        OpcodeTableEntry { mnemonic: "je", opt: OpcodeParseType::Jump}, //0x74
        OpcodeTableEntry { mnemonic: "jnz", opt: OpcodeParseType::Jump}, //0x75
        OpcodeTableEntry { mnemonic: "jbe", opt: OpcodeParseType::Jump}, //0x76
        OpcodeTableEntry { mnemonic: "ja", opt: OpcodeParseType::Jump}, //0x77
        OpcodeTableEntry { mnemonic: "js", opt: OpcodeParseType::Jump}, //0x78
        OpcodeTableEntry { mnemonic: "jns", opt: OpcodeParseType::Jump}, //0x79
        OpcodeTableEntry { mnemonic: "jp", opt: OpcodeParseType::Jump}, //0x7A
        OpcodeTableEntry { mnemonic: "jnp", opt: OpcodeParseType::Jump}, //0x7B
        OpcodeTableEntry { mnemonic: "jl", opt: OpcodeParseType::Jump}, //0x7C
        OpcodeTableEntry { mnemonic: "jnl", opt: OpcodeParseType::Jump}, //0x7D
        OpcodeTableEntry { mnemonic: "jle", opt: OpcodeParseType::Jump}, //0x7E
        OpcodeTableEntry { mnemonic: "jg", opt: OpcodeParseType::Jump}, //0x7F
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::ImmRm}, //0x80
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::ImmRm}, //0x81
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::ImmRm}, //0x82
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::ImmRm}, //0x83
        OpcodeTableEntry { mnemonic: "test", opt: OpcodeParseType::RegRmWithDisp}, //0x84
        OpcodeTableEntry { mnemonic: "test", opt: OpcodeParseType::RegRmWithDisp}, //0x85
        OpcodeTableEntry { mnemonic: "xchg", opt: OpcodeParseType::RegRmWithDispD1}, //0x86
        OpcodeTableEntry { mnemonic: "xchg", opt: OpcodeParseType::RegRmWithDispD1}, //0x87
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::RegRmWithDisp}, //0x88
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::RegRmWithDisp}, //0x89
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::RegRmWithDisp}, //0x8A
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::RegRmWithDisp}, //0x8B
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x8C
        OpcodeTableEntry { mnemonic: "lea", opt: OpcodeParseType::RegRmWithDispD1}, //0x8D
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x8E
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::RmWithDisp}, //0x8F
        OpcodeTableEntry { mnemonic: "xchg ax,", opt: OpcodeParseType::SingleByteWithReg}, //0x90
        OpcodeTableEntry { mnemonic: "xchg ax,", opt: OpcodeParseType::SingleByteWithReg}, //0x91
        OpcodeTableEntry { mnemonic: "xchg ax,", opt: OpcodeParseType::SingleByteWithReg}, //0x92
        OpcodeTableEntry { mnemonic: "xchg ax,", opt: OpcodeParseType::SingleByteWithReg}, //0x93
        OpcodeTableEntry { mnemonic: "xchg ax,", opt: OpcodeParseType::SingleByteWithReg}, //0x94
        OpcodeTableEntry { mnemonic: "xchg ax,", opt: OpcodeParseType::SingleByteWithReg}, //0x95
        OpcodeTableEntry { mnemonic: "xchg ax,", opt: OpcodeParseType::SingleByteWithReg}, //0x96
        OpcodeTableEntry { mnemonic: "xchg ax,", opt: OpcodeParseType::SingleByteWithReg}, //0x97
        OpcodeTableEntry { mnemonic: "cbw", opt: OpcodeParseType::Direct}, //0x98
        OpcodeTableEntry { mnemonic: "cwd", opt: OpcodeParseType::Direct}, //0x99
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x9A
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0x9B
        OpcodeTableEntry { mnemonic: "pushf", opt: OpcodeParseType::Direct}, //0x9C
        OpcodeTableEntry { mnemonic: "popf", opt: OpcodeParseType::Direct}, //0x9D
        OpcodeTableEntry { mnemonic: "sahf", opt: OpcodeParseType::Direct}, //0x9E
        OpcodeTableEntry { mnemonic: "lahf", opt: OpcodeParseType::Direct}, //0x9F
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xA0
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::AccMem}, //0xA1
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::AccMem}, //0xA2
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::AccMem}, //0xA3
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::AccMem}, //0xA4
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xA5
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xA6
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xA7
        OpcodeTableEntry { mnemonic: "test", opt: OpcodeParseType::ImmAcc}, //0xA8
        OpcodeTableEntry { mnemonic: "test", opt: OpcodeParseType::ImmAcc}, //0xA9
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xAA
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xAB
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xAC
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xAD
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xAE
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xAF
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB0
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB1
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB2
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB3
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB4
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB5
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB6
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB7
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB8
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xB9
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xBA
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xBB
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xBC
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xBD
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xBE
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmReg}, //0xBF
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xC0
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xC1
        OpcodeTableEntry { mnemonic: "ret", opt: OpcodeParseType::Return}, //0xC2
        OpcodeTableEntry { mnemonic: "ret", opt: OpcodeParseType::Return}, //0xC3
        OpcodeTableEntry { mnemonic: "les", opt: OpcodeParseType::RegRmWithDispD1W1}, //0xC4
        OpcodeTableEntry { mnemonic: "lds", opt: OpcodeParseType::RegRmWithDispD1}, //0xC5
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmRm}, //0xC6
        OpcodeTableEntry { mnemonic: "mov", opt: OpcodeParseType::ImmRm}, //0xC7
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xC8
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xC9
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xCA
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xCB
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xCC
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xCD
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xCE
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xCF
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::ShiftRot}, //0xD0
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::ShiftRot}, //0xD1
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::ShiftRot}, //0xD2
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::ShiftRot}, //0xD3
        OpcodeTableEntry { mnemonic: "aam", opt: OpcodeParseType::AsciiAdjust}, //0xD4
        OpcodeTableEntry { mnemonic: "aad", opt: OpcodeParseType::AsciiAdjust}, //0xD5
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xD6
        OpcodeTableEntry { mnemonic: "xlat", opt: OpcodeParseType::Direct}, //0xD7
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xD8
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xD9
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xDA
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xDB
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xDC
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xDD
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xDE
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xDF
        OpcodeTableEntry { mnemonic: "loopnz", opt: OpcodeParseType::Jump}, //0xE0
        OpcodeTableEntry { mnemonic: "loopz", opt: OpcodeParseType::Jump}, //0xE1
        OpcodeTableEntry { mnemonic: "loop", opt: OpcodeParseType::Jump}, //0xE2
        OpcodeTableEntry { mnemonic: "jcxz", opt: OpcodeParseType::Jump}, //0xE3
        OpcodeTableEntry { mnemonic: "in", opt: OpcodeParseType::InOut}, //0xE4
        OpcodeTableEntry { mnemonic: "in", opt: OpcodeParseType::InOut}, //0xE5
        OpcodeTableEntry { mnemonic: "out", opt: OpcodeParseType::InOut}, //0xE6
        OpcodeTableEntry { mnemonic: "out", opt: OpcodeParseType::InOut}, //0xE7
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xE8
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xE9
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xEA
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xEB
        OpcodeTableEntry { mnemonic: "in al, dx", opt: OpcodeParseType::Direct}, //0xEC
        OpcodeTableEntry { mnemonic: "in ax, dx", opt: OpcodeParseType::Direct}, //0xED
        OpcodeTableEntry { mnemonic: "out dx, al", opt: OpcodeParseType::Direct}, //0xEE
        OpcodeTableEntry { mnemonic: "out dx, ax", opt: OpcodeParseType::Direct}, //0xEF
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xF0
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xF1
        OpcodeTableEntry { mnemonic: "repnz", opt: OpcodeParseType::Repeat}, //0xF2
        OpcodeTableEntry { mnemonic: "rep", opt: OpcodeParseType::Repeat}, //0xF3
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xF4
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xF5
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::RmWithDisp}, //0xF6
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::RmWithDisp}, //0xF7
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xF8
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xF9
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xFA
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xFB
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xFC
        OpcodeTableEntry { mnemonic: "", opt: OpcodeParseType::Nop}, //0xFD
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::RmWithDisp}, //0xFE
        OpcodeTableEntry { mnemonic: "---", opt: OpcodeParseType::RmWithDisp}, //0xFF
    ];


fn get_opcode_info(opcode: u8) -> OpcodeTableEntry {
    OPCODE_TABLE[opcode as usize]
}

fn get_bit_value(b: u8) -> BitValue {
    if (b & 0x1) == 0 {
        BitValue::BV0
    } else {
        BitValue::BV1
    }
}

fn get_two_bit_value(b: u8) -> TwoBitValue {
    let tb = b & 0x3;
    if tb == 0 {
        TwoBitValue::DBV00
    } else if tb == 1 {
        TwoBitValue::DBV01
    } else if tb == 2 {
        TwoBitValue::DBV10
    } else {
        TwoBitValue::DBV11
    }
}

fn get_three_bit_value(b: u8) -> ThreeBitValue {
    let tb = b & 0x7;
    if tb == 0 {
        ThreeBitValue::TBV000
    } else if tb == 1 {
        ThreeBitValue::TBV001
    } else if tb == 2 {
        ThreeBitValue::TBV010
    } else if tb == 3 {
        ThreeBitValue::TBV011
    } else if tb == 4 {
        ThreeBitValue::TBV100
    } else if tb == 5 {
        ThreeBitValue::TBV101
    } else if tb == 6 {
        ThreeBitValue::TBV110
    } else {
        ThreeBitValue::TBV111
    }
}

fn get_seg_reg_str(code: TwoBitValue) -> String
{
    use TwoBitValue::*;
    String::from(match code {
        DBV00 => "es",
        DBV01 => "cs",
        DBV10 => "ss",
        DBV11 => "ds"
    })
}

fn get_reg_str(flag: BitValue,
               code: ThreeBitValue) -> String
{
    const REGS_BYTE: [&str; 8] = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
    const REGS_WORD: [&str; 8] = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];

    String::from(
        match flag {
            BitValue::BV0 => REGS_BYTE[code as usize],
            BitValue::BV1 => REGS_WORD[code as usize]
        }
    )
}

fn read_i16_val(data: &[u8]) -> i16
{
    data [0] as i16 | ((data[1] as i16) << 8)
}

fn read_u16_val(data: &[u8]) -> u16
{
    data [0] as u16 | ((data[1] as u16) << 8)
}

fn get_mem_ptr_and_displacement(data: &[u8],
                                rm_code: ThreeBitValue,
                                mod_code: TwoBitValue) -> (usize, String)
{
    use TwoBitValue::*;
    use ThreeBitValue::*;
    let mut data_offset: usize = 0;

    let mut ret: String = "[".to_owned();
    if (mod_code == DBV00) && (rm_code == TBV110) {
        let disp = read_u16_val(&data[2..4]);
        ret.push_str(&format!("{}]", disp));
        (2, ret)
    } else {
        ret.push_str(
            match rm_code {
                TBV000 => "bx + si",
                TBV001 => "bx + di",
                TBV010 => "bp + si",
                TBV011 => "bp + di",
                TBV100 => "si",
                TBV101 => "di",
                TBV110 => "bp",
                TBV111 => "bx",
            }
        );
        let suffix: String =
            match mod_code {
                DBV00 => String::from("]"),
                DBV01 => {
                    data_offset = 1;
                    if data[2] == 0 {
                        String::from(format!("]"))
                    } else {
                        let byte_val: i8 = data[2] as i8;
                        if byte_val == 0 {
                            String::from(format!("]"))
                        }else if byte_val < 0 {
                            String::from(format!(" - {}]", -byte_val))
                        } else {
                            String::from(format!(" + {}]", byte_val))
                        }
                    }
                },
                DBV10 => {
                    data_offset = 2;
                    let data_val = read_i16_val(&data[2..4]);
                    if data_val == 0 {
                        String::from(format!("]"))
                    } else if data_val > 0 {
                        String::from(format!(" + {}]", data_val))
                    } else {
                        String::from(format!(" - {}]", -data_val))
                    }
                },
                DBV11 => {
                    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
                    return (0, get_reg_str(w_flag, rm_code));
                }
            };
        ret.push_str(&suffix);
        (data_offset, ret)
    }
}

fn parse_imm_reg_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    let oc_mnmnc = opcode.mnemonic;
    let immw_flag = get_bit_value((data[0] & IMMW_MASK) >> IMMW_SHFT);
    let immreg_code = get_three_bit_value((data[0] & IMMREG_MASK) >> IMMREG_SHFT);
    let immreg_string = get_reg_str(immw_flag, immreg_code);

    match immw_flag {
        BitValue::BV0 => (2, String::from(format!("{} {}, {}", oc_mnmnc, immreg_string, data[1] as i8))),
        BitValue::BV1 => {
            let val: i16 = read_i16_val(&data[1..3]);
            (3, String::from(format!("{} {}, {}", oc_mnmnc, immreg_string, val)))
        }
    }
}

fn parse_imm_rm_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    use BitValue::*;
    use TwoBitValue::*;
    use ThreeBitValue::*;
    let (use_s_flag, oc_mnmnc) = if (data[0] & 0xFC) == 0x80 {
        let sub_opcode = get_three_bit_value((data[1] & SUB_CODE_MASK) >> SUB_CODE_SHFT);
        match sub_opcode {
            TBV000 => (true, "add"),
            TBV001 => (false, "or"),
            TBV010 => (true, "adc"),
            TBV011 => (true, "sbb"),
            TBV100 => (false, "and"),
            TBV101 => (true, "sub"),
            TBV110 => (false, "xor"),
            TBV111 => (true, "cmp"),
        }
    } else {
        (false, opcode.mnemonic)
    };
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);

    if mod_code != DBV11 {
        let (data_offset, reg_string) = get_mem_ptr_and_displacement(data, rm_code, mod_code);
        let data_idx: usize = if (mod_code == DBV10) || ((mod_code == DBV00) & (rm_code == TBV110)) {
            4
        } else if mod_code == DBV01 {
            3
        } else {
            2
        };

        if (data[0] & 0xFE) == 0xC6 {
            return match w_flag {
                BV0 =>  (data_offset + 3, String::from(format!("{} {}, byte {}", oc_mnmnc, reg_string, data[data_idx]))),
                BV1 =>  (data_offset + 4, String::from(format!("{} {}, word {}", oc_mnmnc, reg_string,
                                                                      read_u16_val(&data[data_idx..data_idx+2])))),
            }
        } else {
            return match w_flag {
                BV0 =>  (data_offset + 3, String::from(format!("{} byte {}, {}", oc_mnmnc, reg_string, data[data_idx]))),
                BV1 =>  {
                    if use_s_flag {
                        let s_flag = (data[0] & S_MASK) >> S_SHFT;
                        if s_flag == 0 {
                            (data_offset + 3, String::from(format!("{} word {}, {}", oc_mnmnc, reg_string,
                                                                   read_u16_val(&data[data_idx..data_idx+2]))))
                        } else {
                            (data_offset + 3, String::from(format!("{} word {}, {}", oc_mnmnc, reg_string,
                                                                   (data[data_idx] as i8) as i16)))
                        }
                    } else {
                        (data_offset + 4, String::from(format!("{} word {}, {}", oc_mnmnc, reg_string,
                                                               read_i16_val(&data[data_idx..data_idx+2]))))
                    }
                }
            }
        }
    } else {
        let rm_string = get_reg_str(w_flag, rm_code);
        let data_idx: usize = 2;
        if oc_mnmnc == "mov" {
            (3, String::from(format!("{} {}, {}", oc_mnmnc, rm_string, data[data_idx])))
        } else {
            let sw_flag = get_two_bit_value(data[0] & 0x3);
            match sw_flag {
                DBV01 => (4, String::from(format!("{} {}, {}", oc_mnmnc, rm_string, read_u16_val(&data[data_idx..data_idx+2])))),
                _ => (3, String::from(format!("{} {}, {}", oc_mnmnc, rm_string, data[data_idx]))),
            }
        }
    }
}

fn parse_acc_mem_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    use BitValue::*;
    use ThreeBitValue::*;

    let oc_mnmnc = opcode.mnemonic;
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_string = get_reg_str(w_flag, TBV000);
    let val = read_u16_val(&data[1..2+w_flag as usize]);
    let d_flag = get_bit_value((data[0] & D_MASK) >> D_SHFT);
    match d_flag {
        BV0 => (3, String::from(format!("{} {}, [{}]", oc_mnmnc, reg_string, val))),
        BV1 => (3, String::from(format!("{} [{}], {}", oc_mnmnc, val, reg_string)))
    }
}

fn parse_imm_acc_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    use BitValue::*;
    use ThreeBitValue::*;

    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_string = get_reg_str(w_flag, TBV000);
    let oc_mnmnc = opcode.mnemonic;
    match w_flag {
        BV0 => (2, String::from(format!("{} {}, {}", oc_mnmnc,  reg_string, data[1] as i8))),
        BV1 => {
            let val = read_u16_val(&data[1..2+w_flag as usize]);
            (3, String::from(format!("{} {}, {}", oc_mnmnc,  reg_string, val)))
        }
    }
}

fn parse_reg_rm_strings_with_indicated_dw(opcode: OpcodeTableEntry,
                                          data: &[u8],
                                          w_flag: BitValue,
                                          d_flag: BitValue) -> (usize, String)
{
    use BitValue::*;
    use TwoBitValue::*;

    let oc_mnmc = opcode.mnemonic;
    let reg_code = get_three_bit_value((data[1] & REG_MASK) >> REG_SHFT);
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let reg_string = get_reg_str(w_flag, reg_code);
    let mut offset: usize = 2;
    let rm_string = match mod_code {
        DBV11 => get_reg_str(w_flag, rm_code),
        _ => {
            let t = get_mem_ptr_and_displacement(data, rm_code, mod_code);
            offset += t.0;
            t.1
        }
    };

    let (left, right) = if d_flag == BV0 {
        (rm_string, reg_string)
    } else {
        (reg_string, rm_string)
    };

    (offset, String::from(format!("{} {}, {}", oc_mnmc, left, right)))
}

fn parse_reg_rm_with_disp_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let d_flag = get_bit_value((data[0] & D_MASK) >> D_SHFT);
    parse_reg_rm_strings_with_indicated_dw(opcode, data, w_flag, d_flag)
}

fn parse_reg_rm_with_disp_instruction_d1(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    parse_reg_rm_strings_with_indicated_dw(opcode, data, w_flag, BitValue::BV1)
}

fn parse_reg_rm_with_disp_instruction_d1w1(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    parse_reg_rm_strings_with_indicated_dw(opcode, data, BitValue::BV1, BitValue::BV1)
}

fn parse_jmp_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    let jump_offset: i8 = data[1] as i8;
    let oc_mnmc = opcode.mnemonic;
    (2, String::from(format!("{} {} {}", oc_mnmc, OFFSET_STR, jump_offset)))
}

fn parse_shift_rot_instruction(_opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    use BitValue::*;
    use TwoBitValue::*;
    use ThreeBitValue::*;
    let oc_mnmc = match get_three_bit_value((data[1] & SUB_CODE_MASK) >> SUB_CODE_SHFT) {
        TBV000 => "rol",
        TBV001 => "ror",
        TBV010 => "rcl",
        TBV011 => "rcr",
        TBV100 => "shl",
        TBV101 => "shr",
        TBV111 => "sar",
        _ => panic!("Unsupported subcode")
    };
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let v_flag = get_bit_value((data[0] & V_MASK) >> V_SHFT);
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let t = get_mem_ptr_and_displacement(data, rm_code, mod_code);

    match mod_code {
        DBV11 => {
            match v_flag {
                BV0 => (2 + t.0, String::from(format!("{} {}, 1", oc_mnmc, t.1))),
                BV1 => (2 + t.0, String::from(format!("{} {}, cl", oc_mnmc, t.1))),
            }
        },
        _ => {
            let size_str = match w_flag { BV0 => "byte", BV1 => "word"};
            match v_flag {
                BV0 => (2 + t.0, String::from(format!("{} {} {}, 1", oc_mnmc, size_str, t.1))),
                BV1 => (2 + t.0, String::from(format!("{} {} {}, cl", oc_mnmc, size_str, t.1))),
            }
        }
    }
}

fn parse_rm_with_disp_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    use BitValue::*;
    use TwoBitValue::*;
    use ThreeBitValue::*;
    let (use_w_flag, oc_mnmc) = {
        match get_three_bit_value((data[1] & SUB_CODE_MASK) >> SUB_CODE_SHFT) {
            TBV000 => {
                if data[0] == 0x8F {
                    (true, "pop")
                } else if (data[0] &  0xFE) == 0xFE {
                    (true, "inc")
                } else if (data[0] & 0xFE) == 0xF6 {
                    let mod_opcode: OpcodeTableEntry = OpcodeTableEntry {mnemonic: "test", opt: opcode.opt};
                    return parse_imm_rm_instruction(mod_opcode, data);
                } else {
                    panic!("unknown opcode/subcode")
                }
            },
            TBV001 => (true, "dec"),
            TBV010 => {
                if data[0] == 0xFF { (false, "call") } else { (true, "not")}
            },
            TBV011 => (true, "neg"),
            TBV100 => {if  data[0] == 0xFF {(false, "jmp")} else {(true, "mul")}},
            TBV101 => (true, "imul"),
            TBV110 => {
                if data[0] == 0xFF {
                    (true, "push")
                } else if (data[0] & 0xFE) == 0xF6 {
                    (true, "div")
                } else {
                    panic!("unknwon opcode/subcode")
                }
            }
            TBV111 => (true, "idiv"),
        }
    };

    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let t = get_mem_ptr_and_displacement(data, rm_code, mod_code);
    match mod_code {
        DBV11 => (2 + t.0, String::from(format!("{} {}", oc_mnmc, t.1))),
        _ => {
            if use_w_flag {
                let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
                match w_flag {
                    BV0 => (2 + t.0, String::from(format!("{} byte {}", oc_mnmc, t.1))),
                    BV1 => (2 + t.0, String::from(format!("{} word {}", oc_mnmc, t.1)))
                }
            }else {
                (2 + t.0, String::from(format!("{} {}", oc_mnmc, t.1)))
            }

        }
    }

}

fn parse_single_byte_instruction_with_sr(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    let oc_mnmc = opcode.mnemonic;
    let reg_string = get_seg_reg_str(get_two_bit_value((data[0] & 0x18) >> 3));
    (1, String::from(format!{"{} {}", oc_mnmc, reg_string}))
}

fn parse_single_byte_instruction_with_reg(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    let oc_mnmc = opcode.mnemonic;
    let reg_string = get_reg_str(BitValue::BV1, get_three_bit_value(data[0] & 0x7));
    (1, String::from(format!("{} {}", oc_mnmc, reg_string)))
}

fn parse_inout_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    use BitValue::*;
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_mnmc = match w_flag { BV0 => "al", BV1 => "ax" };
    if opcode.mnemonic == "in" {
        (2, String::from(format!("in {}, {}", reg_mnmc, data[1])))
    } else {
        (2, String::from(format!("out {}, {}", data[1], reg_mnmc)))
    }
}

fn parse_ascii_adjust_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    if data[1] != 0x0A {
        panic!("Invalid second byte for ascii adjust for mul/div instruction")
    }
    (2, String::from(format!("{}", opcode.mnemonic)))
}

fn parse_repeat_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    use BitValue::*;
    let w_char = match get_bit_value((data[1] & W_MASK) >> W_SHFT) { BV0 => 'b', BV1 => 'w' };
    let operand = match data[1] & 0xFE {
        0xA4 => "movs",
        0xA6 => "cmps",
        0xAE => "scas",
        0xAC => "lods",
        0xAA => "stos",
        _ => panic!("Unknown operand for repeat instruction")
    };
    (2, String::from(format!("{} {}{}", opcode.mnemonic, operand, w_char)))
}

fn parse_return_instruction(opcode: OpcodeTableEntry, data: &[u8]) -> (usize, String) {
    use BitValue::*;
    let oc_mnmc = opcode.mnemonic;
    match get_bit_value(data[0] & 0x1) {
        BV0 => (3, String::from(format!("{} {}", oc_mnmc, read_i16_val(&data[1..3])))),
        BV1 => (1, String::from("ret"))
    }
}

fn parse_direct_instruction(opcode: OpcodeTableEntry, _data: &[u8]) -> (usize, String) {
    (1, String::from(opcode.mnemonic))
}

fn parse_instruction(data: &[u8]) -> (usize, String)
{
    let opcode = get_opcode_info(data[0]);
    match opcode.opt {
        OpcodeParseType::RegRmWithDisp => parse_reg_rm_with_disp_instruction(opcode, data),
        OpcodeParseType::RegRmWithDispD1 => parse_reg_rm_with_disp_instruction_d1(opcode, data),
        OpcodeParseType::RegRmWithDispD1W1 => parse_reg_rm_with_disp_instruction_d1w1(opcode, data),
        OpcodeParseType::ImmReg => parse_imm_reg_instruction(opcode, data),
        OpcodeParseType::ImmRm => parse_imm_rm_instruction(opcode, data),
        OpcodeParseType::AccMem =>  parse_acc_mem_instruction(opcode, data),
        OpcodeParseType::ImmAcc =>  parse_imm_acc_instruction(opcode, data),
        OpcodeParseType::RmWithDisp => parse_rm_with_disp_instruction(opcode, data),
        OpcodeParseType::ShiftRot => parse_shift_rot_instruction(opcode, data),
        OpcodeParseType::Jump => parse_jmp_instruction(opcode, data),
        OpcodeParseType::SingleByteWithSr  => parse_single_byte_instruction_with_sr(opcode, data),
        OpcodeParseType::SingleByteWithReg => parse_single_byte_instruction_with_reg(opcode, data),
        OpcodeParseType::InOut => parse_inout_instruction(opcode, data),
        OpcodeParseType::AsciiAdjust => parse_ascii_adjust_instruction(opcode, data),
        OpcodeParseType::Repeat => parse_repeat_instruction(opcode, data),
        OpcodeParseType::Return => parse_return_instruction(opcode, data),
        OpcodeParseType::Direct => parse_direct_instruction(opcode, data),
        OpcodeParseType::Nop => panic!("Invalid opcode 0x{:x}",data[0])
    }
}

fn decode_from_file(filepath: &String) -> String {
    let data = read(filepath).expect(
        &format!("Failure reading {}", filepath));
    decode_from_data(&data)
}

fn decode_from_data(data: &[u8]) -> String {
    let n = data.len();
    let mut i = 0;
    let mut lines: Vec<(String, Option<usize>, Option<usize>)> = Vec::new();
    let mut labels: BTreeSet<usize> = BTreeSet::new();

    lines.push((String::from("bits 16"), None, None));
    lines.push((String::from(""), None, None));

    while i < n {
        let (offset, code): (usize, String) = {
            parse_instruction(&data[i..n])
        };

        let (codestr, dst) = match code.find(OFFSET_STR) {
            Some(idx) => {
                let mut modified_line = String::from(&code[0..idx]);
                let jump_offset: i8 = code[idx+9..].parse::<i8>().unwrap();
                let jump_address: usize = (i as isize + offset as isize + jump_offset as isize) as usize;
                labels.insert(jump_address);
                (modified_line, Some(jump_address))
            }
            None => (code, None)
        };
        lines.push((codestr, Some(i), dst));
        i += offset;

    }
    let mut label_map = HashMap::<usize, String>::new();

    for (label_no, label_address) in labels.iter().enumerate() {
        let label_string = String::from(format!("label_{}", label_no));
        label_map.insert(*label_address, label_string);
    }

    let mut ret: String = "".to_owned();
    for line in lines.iter() {
        let mut code = String::from(line.0.clone());
        match line.1 {
            Some(adr) => {
                if label_map.contains_key(&adr) {
                    ret.push_str(&format!("{}:\n", label_map[&adr]));
                }
            }
            _ => {}
        };
        code.push_str(
            &match line.2 {
                None => {String::from("\n")},
                Some(adr) => {
                    format!("{}\n", label_map[&adr])
                }
            });

        ret.push_str(&code);
    }
    ret
}

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        println!("Usage: args[0] binary_file");
        return;
    }
    println!("{}", decode_from_file(&args[1]));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_instruction() {
        let test_data: [u8; 2] = [0x89, 0xd9];
        assert_eq!(parse_instruction(&test_data), (2, String::from("mov cx, bx")));
    }

    #[test]
    fn test_decode_data_1() {
        let test_data: [u8; 22] = [
            0x89, 0xd9, 0x88, 0xe5, 0x89, 0xda, 0x89, 0xde,
            0x89, 0xfb, 0x88, 0xc8, 0x88, 0xed, 0x89, 0xc3,
            0x89, 0xf3, 0x89, 0xfc, 0x89, 0xc5];
        let expected =
            "bits 16\n\n\
            mov cx, bx\n\
            mov ch, ah\n\
            mov dx, bx\n\
            mov si, bx\n\
            mov bx, di\n\
            mov al, cl\n\
            mov ch, ch\n\
            mov bx, ax\n\
            mov bx, si\n\
            mov sp, di\n\
            mov bp, ax\n";

        assert_eq!(decode_from_data(&test_data), expected);
    }

    #[test]
    fn test_decode_data_2() {
        let test_data: [u8; 41] = [
            0x89, 0xde, 0x88, 0xc6, 0xb1, 0x0c, 0xb5, 0xf4,
            0xb9, 0x0c, 0x00, 0xb9, 0xf4, 0xff, 0xba, 0x6c,
            0x0f, 0xba, 0x94, 0xf0, 0x8a, 0x00, 0x8b, 0x1b,
            0x8b, 0x56, 0x00, 0x8a, 0x60, 0x04, 0x8a, 0x80,
            0x87, 0x13, 0x89, 0x09, 0x88, 0x0a, 0x88, 0x6e,
            0x00];
        let expected =
            "bits 16\n\n\
             mov si, bx\n\
             mov dh, al\n\
             mov cl, 12\n\
             mov ch, -12\n\
             mov cx, 12\n\
             mov cx, -12\n\
             mov dx, 3948\n\
             mov dx, -3948\n\
             mov al, [bx + si]\n\
             mov bx, [bp + di]\n\
             mov dx, [bp]\n\
             mov ah, [bx + si + 4]\n\
             mov al, [bx + si + 4999]\n\
             mov [bx + di], cx\n\
             mov [bp + si], cl\n\
             mov [bp], ch\n";

        assert_eq!(decode_from_data(&test_data), expected);
    }

    #[test]
    fn test_decode_data_3() {
        let test_data: [u8; 39] = [
            0x8b, 0x41, 0xdb, 0x89, 0x8c, 0xd4, 0xfe, 0x8b,
            0x57, 0xe0, 0xc6, 0x03, 0x07, 0xc7, 0x85, 0x85,
            0x03, 0x5b, 0x01, 0x8b, 0x2e, 0x05, 0x00, 0x8b,
            0x1e, 0x82, 0x0d, 0xa1, 0xfb, 0x09, 0xa1, 0x10,
            0x00, 0xa3, 0xfa, 0x09, 0xa3, 0x0f, 0x00];

        let expected =
            "bits 16\n\n\
             mov ax, [bx + di - 37]\n\
             mov [si - 300], cx\n\
             mov dx, [bx - 32]\n\
             mov [bp + di], byte 7\n\
             mov [di + 901], word 347\n\
             mov bp, [5]\n\
             mov bx, [3458]\n\
             mov ax, [2555]\n\
             mov ax, [16]\n\
             mov [2554], ax\n\
             mov [15], ax\n";

        assert_eq!(decode_from_data(&test_data), expected);
    }

    #[test]
    fn test_decode_with_labels() {
        let test_data: [u8; 48] = [
            0x75, 0x02, 0x75, 0xfc, 0x75, 0xfa, 0x75, 0xfc,
            0x74, 0xfe, 0x7c, 0xfc, 0x7e, 0xfa, 0x72, 0xf8,
            0x76, 0xf6, 0x7a, 0xf4, 0x70, 0xf2, 0x78, 0xf0,
            0x75, 0xee, 0x7d, 0xec, 0x7f, 0xea, 0x73, 0xe8,
            0x77, 0xe6, 0x7b, 0xe4, 0x71, 0xe2, 0x79, 0xe0,
            0xe2, 0xde, 0xe1, 0xdc, 0xe0, 0xda, 0xe3, 0xd8];

        let expected =
            "bits 16\n\n\
             label_0:\n\
             jnz label_1\n\
             jnz label_0\n\
             label_1:\n\
             jnz label_0\n\
             jnz label_1\n\
             label_2:\n\
             je label_2\n\
             jl label_2\n\
             jle label_2\n\
             jb label_2\n\
             jbe label_2\n\
             jp label_2\n\
             jo label_2\n\
             js label_2\n\
             jnz label_2\n\
             jnl label_2\n\
             jg label_2\n\
             jnb label_2\n\
             ja label_2\n\
             jnp label_2\n\
             jno label_2\n\
             jns label_2\n\
             loop label_2\n\
             loopz label_2\n\
             loopnz label_2\n\
             jcxz label_2\n";

        assert_eq!(decode_from_data(&test_data), expected);
    }

    #[test]
    fn test_parse_8bit_imm_mov() {
        {
            let test_data: [u8; 2] = [0xb1, 0x0c];
            assert_eq!(parse_instruction(&test_data), (2, String::from("mov cl, 12")));
        }
        {
            let test_data: [u8; 2] = [0xb5, 0xf4];
            assert_eq!(parse_instruction(&test_data), (2, String::from("mov ch, -12")));
        }
    }

    #[test]
    fn test_parse_16bit_imm_mov() {
        let test_data: [[u8; 3]; 4] = [[0xb9, 0x0c, 0x00],
                                       [0xb9, 0xf4, 0xff],
                                       [0xba, 0x6c, 0x0f],
                                       [0xba, 0x94, 0xf0]];

        assert_eq!(parse_instruction(&test_data[0]), (3, String::from("mov cx, 12")));
        assert_eq!(parse_instruction(&test_data[1]), (3, String::from("mov cx, -12")));
        assert_eq!(parse_instruction(&test_data[2]), (3, String::from("mov dx, 3948")));
        assert_eq!(parse_instruction(&test_data[3]), (3, String::from("mov dx, -3948")));
    }

    #[test]
    fn test_src_address_calcualtion() {
        let test_data_w2: [[u8; 2]; 2] = [[0x8a, 0x00],
                                          [0x8b, 0x1b]];
        let test_data_w3: [[u8; 3]; 2] = [[0x8b, 0x56, 0x00],
                                          [0x8a, 0x60, 0x04]];
        let test_data_w4: [[u8; 4]; 1] = [[0x8a, 0x80, 0x87, 0x13]];

        assert_eq!(parse_instruction(&test_data_w2[0]), (2, String::from("mov al, [bx + si]")));
        assert_eq!(parse_instruction(&test_data_w2[1]), (2, String::from("mov bx, [bp + di]")));
        assert_eq!(parse_instruction(&test_data_w3[0]), (3, String::from("mov dx, [bp]")));
        assert_eq!(parse_instruction(&test_data_w3[1]), (3, String::from("mov ah, [bx + si + 4]")));
        assert_eq!(parse_instruction(&test_data_w4[0]), (4, String::from("mov al, [bx + si + 4999]")));
    }

    #[test]
    fn test_dst_address_calcualtion() {
        let test_data_w2: [[u8; 2]; 2] = [[0x89, 0x09],
                                          [0x88, 0x0a]];
        let test_data_w3: [[u8; 3]; 1] = [[0x88, 0x6e, 0x00]];

        assert_eq!(parse_instruction(&test_data_w2[0]), (2, String::from("mov [bx + di], cx")));
        assert_eq!(parse_instruction(&test_data_w2[1]), (2, String::from("mov [bp + si], cl")));
        assert_eq!(parse_instruction(&test_data_w3[0]), (3, String::from("mov [bp], ch")));

    }

    #[test]
    fn test_signed_displacements() {
        let test_data_w3: [[u8; 3]; 2] = [[0x8b, 0x41, 0xdb],
                                          [0x8b, 0x57, 0xe0]];
        let test_data_w4: [[u8; 4]; 1] = [[0x89, 0x8c, 0xd4, 0xfe]];

        assert_eq!(parse_instruction(&test_data_w3[0]), (3, String::from("mov ax, [bx + di - 37]")));
        assert_eq!(parse_instruction(&test_data_w3[1]), (3, String::from("mov dx, [bx - 32]")));
        assert_eq!(parse_instruction(&test_data_w4[0]), (4, String::from("mov [si - 300], cx")));
    }

    #[test]
    fn test_explicit_sizes() {
        let test_data_w3: [[u8; 3]; 1] = [[0xc6, 0x03, 0x07]];
        let test_data_w4: [[u8; 4]; 1] = [[0xc6, 0x46, 0xd9, 0xef]];
        let test_data_w6: [[u8; 6]; 1] = [[0xc7, 0x85, 0x85, 0x03, 0x5b, 0x01]];

        assert_eq!(parse_instruction(&test_data_w3[0]), (3, String::from("mov [bp + di], byte 7")));
        assert_eq!(parse_instruction(&test_data_w4[0]), (4, String::from("mov [bp - 39], byte 239")));
        assert_eq!(parse_instruction(&test_data_w6[0]), (6, String::from("mov [di + 901], word 347")));
    }

    #[test]
    fn test_direct_addresses() {
        let test_data_w4: [[u8; 4]; 2] = [[0x8b, 0x2e, 0x05, 0x00],
                                          [0x8b, 0x1e, 0x82, 0x0d]];

        assert_eq!(parse_instruction(&test_data_w4[0]),
                   (4, String::from("mov bp, [5]")));
        assert_eq!(parse_instruction(&test_data_w4[1]),
                   (4, String::from("mov bx, [3458]")));
    }

    #[test]
    fn test_mem_to_acc() {
        let test_data_w3: [[u8; 3]; 2] = [[0xa1, 0xfb, 0x09],
                                          [0xa1, 0x10, 0x00]];

        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("mov ax, [2555]")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("mov ax, [16]")));
    }

    #[test]
    fn test_acc_to_mem() {
        let test_data_w3: [[u8; 3]; 2] = [[0xa3, 0xfa, 0x09],
                                          [0xa3, 0x0f, 0x00]];

        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("mov [2554], ax")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("mov [15], ax")));
    }

    #[test]
    fn test_add_instructions_1 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x03, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x03, 0x5e, 0x00],
                                          [0x83, 0xc6, 0x02],
                                          [0x83, 0xc5, 0x02],
                                          [0x83, 0xc1, 0x08]];
        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("add bx, [bx + si]")));
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("add bx, [bp]")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("add si, 2")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("add bp, 2")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("add cx, 8")));
    }

    #[test]
    fn test_add_instructions_2 () {
        let test_data_w3: [[u8; 3]; 4] = [[0x03, 0x5e, 0x00],
                                          [0x03, 0x4f, 0x02],
                                          [0x02, 0x7a, 0x04],
                                          [0x03, 0x7b, 0x06]];
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("add bx, [bp]")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("add cx, [bx + 2]")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("add bh, [bp + si + 4]")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("add di, [bp + di + 6]")));
    }

    #[test]
    fn test_add_instructions_3 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x01, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x01, 0x5e, 0x00],
                                          [0x01, 0x4f, 0x02],
                                          [0x00, 0x7a, 0x04],
                                          [0x01, 0x7b, 0x06]];

        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("add [bx + si], bx")));
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("add [bp], bx")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("add [bx + 2], cx")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("add [bp + si + 4], bh")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("add [bp + di + 6], di")));
    }

    #[test]
    fn test_add_instructions_4 () {
        let test_data_w2: [[u8; 2]; 5] = [[0x02, 0x00],
                                          [0x01, 0xd8],
                                          [0x00, 0xe0],
                                          [0x04, 0xe2],
                                          [0x04, 0x09]];

        let test_data_w3: [[u8; 3]; 3] = [[0x80, 0x07, 0x22],
                                          [0x03, 0x46, 0x00],
                                          [0x05, 0xe8, 0x03]];
        let test_data_w5: [[u8; 5]; 1] = [[0x83, 0x82, 0xe8, 0x03, 0x1d]];

        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("add al, [bx + si]")));
        assert_eq!(parse_instruction(&test_data_w2[1]),
                   (2, String::from("add ax, bx")));
        assert_eq!(parse_instruction(&test_data_w2[2]),
                   (2, String::from("add al, ah")));
        assert_eq!(parse_instruction(&test_data_w2[3]),
                   (2, String::from("add al, -30")));
        assert_eq!(parse_instruction(&test_data_w2[4]),
                   (2, String::from("add al, 9")));
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("add byte [bx], 34")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("add ax, [bp]")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("add ax, 1000")));
        assert_eq!(parse_instruction(&test_data_w5[0]),
                   (5, String::from("add word [bp + si + 1000], 29")));
    }

    #[test]
    fn test_sub_instructions_1 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x2b, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x2b, 0x5e, 0x00],
                                          [0x83, 0xee, 0x02],
                                          [0x83, 0xed, 0x02],
                                          [0x83, 0xe9, 0x08]];
        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("sub bx, [bx + si]")));
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("sub bx, [bp]")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("sub si, 2")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("sub bp, 2")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("sub cx, 8")));
    }

    #[test]
    fn test_sub_instructions_2 () {
        let test_data_w3: [[u8; 3]; 4] = [[0x2b, 0x5e, 0x00],
                                          [0x2b, 0x4f, 0x02],
                                          [0x2a, 0x7a, 0x04],
                                          [0x2b, 0x7b, 0x06]];
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("sub bx, [bp]")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("sub cx, [bx + 2]")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("sub bh, [bp + si + 4]")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("sub di, [bp + di + 6]")));
    }

    #[test]
    fn test_sub_instructions_3 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x29, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x29, 0x5e, 0x00],
                                          [0x29, 0x4f, 0x02],
                                          [0x28, 0x7a, 0x04],
                                          [0x29, 0x7b, 0x06]];

        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("sub [bx + si], bx")));
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("sub [bp], bx")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("sub [bx + 2], cx")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("sub [bp + si + 4], bh")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("sub [bp + di + 6], di")));
    }

    #[test]
    fn test_sub_instructions_4 () {
        let test_data_w2: [[u8; 2]; 5] = [[0x2a, 0x00],
                                          [0x29, 0xd8],
                                          [0x28, 0xe0],
                                          [0x2c, 0xe2],
                                          [0x2c, 0x09]];

        let test_data_w3: [[u8; 3]; 4] = [[0x80, 0x2f, 0x22],
                                          [0x83, 0x29, 0x1d],
                                          [0x2b, 0x46, 0x00],
                                          [0x2d, 0xe8, 0x03]];

        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("sub byte [bx], 34")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("sub word [bx + di], 29")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("sub ax, [bp]")));

        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("sub al, [bx + si]")));
        assert_eq!(parse_instruction(&test_data_w2[1]),
                   (2, String::from("sub ax, bx")));
        assert_eq!(parse_instruction(&test_data_w2[2]),
                   (2, String::from("sub al, ah")));

        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("sub ax, 1000")));

        assert_eq!(parse_instruction(&test_data_w2[3]),
                   (2, String::from("sub al, -30")));
        assert_eq!(parse_instruction(&test_data_w2[4]),
                   (2, String::from("sub al, 9")));
    }

    #[test]
    fn test_cmp_instructions_1 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x3b, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x3b, 0x5e, 0x00],
                                          [0x83, 0xfe, 0x02],
                                          [0x83, 0xfd, 0x02],
                                          [0x83, 0xf9, 0x08]];
        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("cmp bx, [bx + si]")));
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("cmp bx, [bp]")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("cmp si, 2")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("cmp bp, 2")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("cmp cx, 8")));
    }

    #[test]
    fn test_cmp_instructions_2 () {
        let test_data_w3: [[u8; 3]; 4] = [[0x3b, 0x5e, 0x00],
                                          [0x3b, 0x4f, 0x02],
                                          [0x3a, 0x7a, 0x04],
                                          [0x3b, 0x7b, 0x06]];
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("cmp bx, [bp]")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("cmp cx, [bx + 2]")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("cmp bh, [bp + si + 4]")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("cmp di, [bp + di + 6]")));
    }

    #[test]
    fn test_cmp_instructions_3 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x39, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x39, 0x5e, 0x00],
                                          [0x39, 0x4f, 0x02],
                                          [0x38, 0x7a, 0x04],
                                          [0x39, 0x7b, 0x06]];

        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("cmp [bx + si], bx")));
        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("cmp [bp], bx")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("cmp [bx + 2], cx")));
        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("cmp [bp + si + 4], bh")));
        assert_eq!(parse_instruction(&test_data_w3[3]),
                   (3, String::from("cmp [bp + di + 6], di")));
    }

    #[test]
    fn test_cmp_instructions_4 () {
        let test_data_w2: [[u8; 2]; 5] = [[0x3a, 0x00],
                                          [0x39, 0xd8],
                                          [0x38, 0xe0],
                                          [0x3c, 0xe2],
                                          [0x3c, 0x09]];

        let test_data_w3: [[u8; 3]; 3] = [[0x80, 0x3f, 0x22],
                                          [0x3b, 0x46, 0x00],
                                          [0x3d, 0xe8, 0x03]];
        let test_data_w5: [[u8; 5]; 1] = [[0x83, 0x3e, 0xe2, 0x12, 0x1d]];

        assert_eq!(parse_instruction(&test_data_w3[0]),
                   (3, String::from("cmp byte [bx], 34")));
        assert_eq!(parse_instruction(&test_data_w5[0]),
                   (5, String::from("cmp word [4834], 29")));
        assert_eq!(parse_instruction(&test_data_w3[1]),
                   (3, String::from("cmp ax, [bp]")));

        assert_eq!(parse_instruction(&test_data_w2[0]),
                   (2, String::from("cmp al, [bx + si]")));
        assert_eq!(parse_instruction(&test_data_w2[1]),
                   (2, String::from("cmp ax, bx")));
        assert_eq!(parse_instruction(&test_data_w2[2]),
                   (2, String::from("cmp al, ah")));

        assert_eq!(parse_instruction(&test_data_w3[2]),
                   (3, String::from("cmp ax, 1000")));

        assert_eq!(parse_instruction(&test_data_w2[3]),
                   (2, String::from("cmp al, -30")));
        assert_eq!(parse_instruction(&test_data_w2[4]),
                   (2, String::from("cmp al, 9")));
    }

    #[test]
    fn test_jnz_instructions() {
        let test_data: [[u8; 2]; 24] = [[0x75, 0x02],
                                        [0x75, 0xfc],
                                        [0x75, 0xfa],
                                        [0x75, 0xfc],
                                        [0x74, 0xfe],
                                        [0x7c, 0xfc],
                                        [0x7e, 0xfa],
                                        [0x72, 0xf8],
                                        [0x76, 0xf6],
                                        [0x7a, 0xf4],
                                        [0x70, 0xf2],
                                        [0x78, 0xf0],
                                        [0x75, 0xee],
                                        [0x7d, 0xec],
                                        [0x7f, 0xea],
                                        [0x73, 0xe8],
                                        [0x77, 0xe6],
                                        [0x7b, 0xe4],
                                        [0x71, 0xe2],
                                        [0x79, 0xe0],
                                        [0xe2, 0xde],
                                        [0xe1, 0xdc],
                                        [0xe0, 0xda],
                                        [0xe3, 0xd8]];

        assert_eq!(parse_instruction(&test_data[0]),
                   (2, String::from("jnz #OFFSET# 2")));
        assert_eq!(parse_instruction(&test_data[1]),
                   (2, String::from("jnz #OFFSET# -4")));
        assert_eq!(parse_instruction(&test_data[2]),
                   (2, String::from("jnz #OFFSET# -6")));
        assert_eq!(parse_instruction(&test_data[3]),
                   (2, String::from("jnz #OFFSET# -4")));
        assert_eq!(parse_instruction(&test_data[4]),
                   (2, String::from("je #OFFSET# -2")));
        assert_eq!(parse_instruction(&test_data[5]),
                   (2, String::from("jl #OFFSET# -4")));
        assert_eq!(parse_instruction(&test_data[6]),
                   (2, String::from("jle #OFFSET# -6")));
        assert_eq!(parse_instruction(&test_data[7]),
                   (2, String::from("jb #OFFSET# -8")));
        assert_eq!(parse_instruction(&test_data[8]),
                   (2, String::from("jbe #OFFSET# -10")));
        assert_eq!(parse_instruction(&test_data[9]),
                   (2, String::from("jp #OFFSET# -12")));
        assert_eq!(parse_instruction(&test_data[10]),
                   (2, String::from("jo #OFFSET# -14")));
        assert_eq!(parse_instruction(&test_data[11]),
                   (2, String::from("js #OFFSET# -16")));
        assert_eq!(parse_instruction(&test_data[12]),
                   (2, String::from("jnz #OFFSET# -18")));
        assert_eq!(parse_instruction(&test_data[13]),
                   (2, String::from("jnl #OFFSET# -20")));
        assert_eq!(parse_instruction(&test_data[14]),
                   (2, String::from("jg #OFFSET# -22")));
        assert_eq!(parse_instruction(&test_data[15]),
                   (2, String::from("jnb #OFFSET# -24")));
        assert_eq!(parse_instruction(&test_data[16]),
                   (2, String::from("ja #OFFSET# -26")));
        assert_eq!(parse_instruction(&test_data[17]),
                   (2, String::from("jnp #OFFSET# -28")));
        assert_eq!(parse_instruction(&test_data[18]),
                   (2, String::from("jno #OFFSET# -30")));
        assert_eq!(parse_instruction(&test_data[19]),
                   (2, String::from("jns #OFFSET# -32")));
        assert_eq!(parse_instruction(&test_data[20]),
                   (2, String::from("loop #OFFSET# -34")));
        assert_eq!(parse_instruction(&test_data[21]),
                   (2, String::from("loopz #OFFSET# -36")));
        assert_eq!(parse_instruction(&test_data[22]),
                   (2, String::from("loopnz #OFFSET# -38")));
        assert_eq!(parse_instruction(&test_data[23]),
                   (2, String::from("jcxz #OFFSET# -40")));
    }

    #[test]
    fn test_push_pop_instructions() {
        assert_eq!(parse_instruction(&[0xff, 0x32]),
                   (2, String::from("push word [bp + si]")));
        assert_eq!(parse_instruction(&[0xff, 0x36, 0xb8, 0x0b]),
                   (4, String::from("push word [3000]")));
        assert_eq!(parse_instruction(&[0xff, 0x71, 0xe2]),
                   (3, String::from("push word [bx + di - 30]")));
        assert_eq!(parse_instruction(&[0x51]),
                   (1, String::from("push cx")));
        assert_eq!(parse_instruction(&[0x50]),
                   (1, String::from("push ax")));
        assert_eq!(parse_instruction(&[0x52]),
                   (1, String::from("push dx")));
        assert_eq!(parse_instruction(&[0x0e]),
                   (1, String::from("push cs")));
        assert_eq!(parse_instruction(&[0x8f, 0x02]),
                   (2, String::from("pop word [bp + si]")));
        assert_eq!(parse_instruction(&[0x8f, 0x06, 0x03, 0x00]),
                   (4, String::from("pop word [3]")));
        assert_eq!(parse_instruction(&[0x8f, 0x81, 0x48, 0xf4]),
                   (4, String::from("pop word [bx + di - 3000]")));
        assert_eq!(parse_instruction(&[0x5c]),
                   (1, String::from("pop sp")));
        assert_eq!(parse_instruction(&[0x5f]),
                   (1, String::from("pop di")));
        assert_eq!(parse_instruction(&[0x5e]),
                   (1, String::from("pop si")));
        assert_eq!(parse_instruction(&[0x1f]),
                   (1, String::from("pop ds")));
    }

    #[test]
    fn test_xchg_instructions() {
        assert_eq!(parse_instruction(&[0x87, 0x86, 0x18, 0xfc]),
                   (4, String::from("xchg ax, [bp - 1000]")));
        // assert_eq!(parse_instruction(&[0x87, 0x6f, 0x32]),
        //            (3, String::from("xchg [bx + 50], bp")));
        // The above fails but below passes (should be equivalent anyway)
        // I checked using nasm, and both instructions assemble to the same machine code
        assert_eq!(parse_instruction(&[0x87, 0x6f, 0x32]),
                   (3, String::from("xchg bp, [bx + 50]")));
        assert_eq!(parse_instruction(&[0x90]),
                   (1, String::from("xchg ax, ax")));
        assert_eq!(parse_instruction(&[0x92]),
                   (1, String::from("xchg ax, dx")));
        assert_eq!(parse_instruction(&[0x94]),
                   (1, String::from("xchg ax, sp")));
        assert_eq!(parse_instruction(&[0x96]),
                   (1, String::from("xchg ax, si")));
        assert_eq!(parse_instruction(&[0x97]),
                   (1, String::from("xchg ax, di")));
        assert_eq!(parse_instruction(&[0x87, 0xca]),
                   (2, String::from("xchg cx, dx")));
        assert_eq!(parse_instruction(&[0x87, 0xf1]),
                   (2, String::from("xchg si, cx")));
        assert_eq!(parse_instruction(&[0x86, 0xcc]),
                   (2, String::from("xchg cl, ah")));
    }

    #[test]
    fn test_in_instructions() {
        assert_eq!(parse_instruction(&[0xe4, 0xc8]),
                   (2, String::from("in al, 200")));
        assert_eq!(parse_instruction(&[0xec]),
                   (1, String::from("in al, dx")));
        assert_eq!(parse_instruction(&[0xed]),
                   (1, String::from("in ax, dx")));
    }

    #[test]
    fn test_out_instructions() {
        assert_eq!(parse_instruction(&[0xe7, 0x2c]),
                   (2, String::from("out 44, ax")));
        assert_eq!(parse_instruction(&[0xee]),
                   (1, String::from("out dx, al")));
        assert_eq!(parse_instruction(&[0xef]),
                   (1, String::from("out dx, ax")));
    }

    #[test]
    fn test_direct_instructions() {
        assert_eq!(parse_instruction(&[0xd7]),
                   (1, String::from("xlat")));
        assert_eq!(parse_instruction(&[0x9f]),
                   (1, String::from("lahf")));
        assert_eq!(parse_instruction(&[0x9e]),
                   (1, String::from("sahf")));
        assert_eq!(parse_instruction(&[0x9c]),
                   (1, String::from("pushf")));
        assert_eq!(parse_instruction(&[0x9d]),
                   (1, String::from("popf")));
        assert_eq!(parse_instruction(&[0x37]),
                   (1, String::from("aaa")));
        assert_eq!(parse_instruction(&[0x27]),
                   (1, String::from("daa")));
        assert_eq!(parse_instruction(&[0x3f]),
                   (1, String::from("aas")));
        assert_eq!(parse_instruction(&[0x2f]),
                   (1, String::from("das")));
        assert_eq!(parse_instruction(&[0x98]),
                   (1, String::from("cbw")));
        assert_eq!(parse_instruction(&[0x99]),
                   (1, String::from("cwd")));
        assert_eq!(parse_instruction(&[0xcc]),
                   (1, String::from("int3")));
        assert_eq!(parse_instruction(&[0xce]),
                   (1, String::from("into")));
        assert_eq!(parse_instruction(&[0xcf]),
                   (1, String::from("iret")));
        assert_eq!(parse_instruction(&[0xf8]),
                   (1, String::from("clc")));
        assert_eq!(parse_instruction(&[0xf5]),
                   (1, String::from("cmc")));
        assert_eq!(parse_instruction(&[0xf9]),
                   (1, String::from("stc")));
        assert_eq!(parse_instruction(&[0xfc]),
                   (1, String::from("cld")));
        assert_eq!(parse_instruction(&[0xfd]),
                   (1, String::from("std")));
        assert_eq!(parse_instruction(&[0xfa]),
                   (1, String::from("cli")));
        assert_eq!(parse_instruction(&[0xfb]),
                   (1, String::from("sti")));
        assert_eq!(parse_instruction(&[0xf4]),
                   (1, String::from("hlt")));
        assert_eq!(parse_instruction(&[0x9b]),
                   (1, String::from("wait")));
    }

    #[test]
    fn test_lea_instruction() {
        assert_eq!(parse_instruction(&[0x8d, 0x81, 0x8c, 0x05]),
                   (4, String::from("lea ax, [bx + di + 1420]")));
        assert_eq!(parse_instruction(&[0x8d, 0x5e, 0xce]),
                   (3, String::from("lea bx, [bp - 50]")));
        assert_eq!(parse_instruction(&[0x8d, 0xa6, 0x15, 0xfc]),
                   (4, String::from("lea sp, [bp - 1003]")));
        assert_eq!(parse_instruction(&[0x8d, 0x78, 0xf9]),
                   (3, String::from("lea di, [bx + si - 7]")));
    }

    #[test]
    fn test_lds_instruction() {
        assert_eq!(parse_instruction(&[0xc5, 0x81, 0x8c, 0x05]),
                   (4, String::from("lds ax, [bx + di + 1420]")));
        assert_eq!(parse_instruction(&[0xc5, 0x5e, 0xce]),
                   (3, String::from("lds bx, [bp - 50]")));
        assert_eq!(parse_instruction(&[0xc5, 0xa6, 0x15, 0xfc]),
                   (4, String::from("lds sp, [bp - 1003]")));
        assert_eq!(parse_instruction(&[0xc5, 0x78, 0xf9]),
                   (3, String::from("lds di, [bx + si - 7]")));
    }

    #[test]
    fn test_les_instruction() {
        assert_eq!(parse_instruction(&[0xc4, 0x81, 0x8c, 0x05]),
                   (4, String::from("les ax, [bx + di + 1420]")));
        assert_eq!(parse_instruction(&[0xc4, 0x5e, 0xce]),
                   (3, String::from("les bx, [bp - 50]")));
        assert_eq!(parse_instruction(&[0xc4, 0xa6, 0x15, 0xfc]),
                   (4, String::from("les sp, [bp - 1003]")));
        assert_eq!(parse_instruction(&[0xc4, 0x78, 0xf9]),
                   (3, String::from("les di, [bx + si - 7]")));
    }

    #[test]
    fn test_add_instructions_5() {
        assert_eq!(parse_instruction(&[0x81, 0xc4, 0x88, 0x01]),
                   (4, String::from("add sp, 392")));
        assert_eq!(parse_instruction(&[0x83, 0xc6, 0x05]),
                   (3, String::from("add si, 5")));
        assert_eq!(parse_instruction(&[0x05, 0xe8, 0x03]),
                   (3, String::from("add ax, 1000")));
        assert_eq!(parse_instruction(&[0x80, 0xc4, 0x1e]),
                   (3, String::from("add ah, 30")));
        assert_eq!(parse_instruction(&[0x04, 0x09]),
                   (2, String::from("add al, 9")));
        assert_eq!(parse_instruction(&[0x01, 0xd9]),
                   (2, String::from("add cx, bx")));
        assert_eq!(parse_instruction(&[0x00, 0xc5]),
                   (2, String::from("add ch, al")));
    }

    #[test]
    fn test_adc_instructions() {
        assert_eq!(parse_instruction(&[0x13, 0x4e, 0x00]),
                   (3, String::from("adc cx, [bp]")));
        assert_eq!(parse_instruction(&[0x13, 0x10]),
                   (2, String::from("adc dx, [bx + si]")));
        assert_eq!(parse_instruction(&[0x10, 0xa3, 0x88, 0x13]),
                   (4, String::from("adc [bp + di + 5000], ah")));
        assert_eq!(parse_instruction(&[0x10, 0x07]),
                   (2, String::from("adc [bx], al")));
        assert_eq!(parse_instruction(&[0x81, 0xd4, 0x88, 0x01]),
                   (4, String::from("adc sp, 392")));
        assert_eq!(parse_instruction(&[0x83, 0xd6, 0x05]),
                   (3, String::from("adc si, 5")));
        assert_eq!(parse_instruction(&[0x15, 0xe8, 0x03]),
                   (3, String::from("adc ax, 1000")));
        assert_eq!(parse_instruction(&[0x80, 0xd4, 0x1e]),
                   (3, String::from("adc ah, 30")));
        assert_eq!(parse_instruction(&[0x14, 0x09]),
                   (2, String::from("adc al, 9")));
        assert_eq!(parse_instruction(&[0x11, 0xd9]),
                   (2, String::from("adc cx, bx")));
        assert_eq!(parse_instruction(&[0x10, 0xc5]),
                   (2, String::from("adc ch, al")));
    }

    #[test]
    fn test_inc_instructions() {
        assert_eq!(parse_instruction(&[0x40]),
                   (1, String::from("inc ax")));
        assert_eq!(parse_instruction(&[0x41]),
                   (1, String::from("inc cx")));
        assert_eq!(parse_instruction(&[0xfe, 0xc6]),
                   (2, String::from("inc dh")));
        assert_eq!(parse_instruction(&[0xfe, 0xc0]),
                   (2, String::from("inc al")));
        assert_eq!(parse_instruction(&[0xfe, 0xc4]),
                   (2, String::from("inc ah")));
        assert_eq!(parse_instruction(&[0x44]),
                   (1, String::from("inc sp")));
        assert_eq!(parse_instruction(&[0x47]),
                   (1, String::from("inc di")));
        assert_eq!(parse_instruction(&[0xfe, 0x86, 0xea, 0x03]),
                   (4, String::from("inc byte [bp + 1002]")));
        assert_eq!(parse_instruction(&[0xff, 0x47,  0x27]),
                   (3, String::from("inc word [bx + 39]")));
        assert_eq!(parse_instruction(&[0xfe, 0x40, 0x05]),
                   (3, String::from("inc byte [bx + si + 5]")));
        assert_eq!(parse_instruction(&[0xff, 0x83, 0xc4, 0xd8]),
                   (4, String::from("inc word [bp + di - 10044]")));
        assert_eq!(parse_instruction(&[0xff, 0x06, 0x85, 0x24]),
                   (4, String::from("inc word [9349]")));
        assert_eq!(parse_instruction(&[0xfe, 0x46, 0x00]),
                   (3, String::from("inc byte [bp]")));
    }

    #[test]
    fn test_sbb_instructions() {
        assert_eq!(parse_instruction(&[0x1b, 0x4e, 0x00]),
                   (3, String::from("sbb cx, [bp]")));
        assert_eq!(parse_instruction(&[0x1b, 0x10]),
                   (2, String::from("sbb dx, [bx + si]")));
        assert_eq!(parse_instruction(&[0x18, 0xa3, 0x88, 0x13]),
                   (4, String::from("sbb [bp + di + 5000], ah")));
        assert_eq!(parse_instruction(&[0x18, 0x07]),
                   (2, String::from("sbb [bx], al")));
        assert_eq!(parse_instruction(&[0x81, 0xdc, 0x88, 0x01]),
                   (4, String::from("sbb sp, 392")));
        assert_eq!(parse_instruction(&[0x83, 0xde, 0x05]),
                   (3, String::from("sbb si, 5")));
        assert_eq!(parse_instruction(&[0x1d, 0xe8, 0x03]),
                   (3, String::from("sbb ax, 1000")));
        assert_eq!(parse_instruction(&[0x80, 0xdc, 0x1e]),
                   (3, String::from("sbb ah, 30")));
        assert_eq!(parse_instruction(&[0x1c, 0x09]),
                   (2, String::from("sbb al, 9")));
        assert_eq!(parse_instruction(&[0x19, 0xd9]),
                   (2, String::from("sbb cx, bx")));
        assert_eq!(parse_instruction(&[0x18, 0xc5]),
                   (2, String::from("sbb ch, al")));
    }

    #[test]
    fn test_dec_instructions() {
        assert_eq!(parse_instruction(&[0x48]),
                   (1, String::from("dec ax")));
        assert_eq!(parse_instruction(&[0x49]),
                   (1, String::from("dec cx")));
        assert_eq!(parse_instruction(&[0xfe, 0xce]),
                   (2, String::from("dec dh")));
        assert_eq!(parse_instruction(&[0xfe, 0xc8]),
                   (2, String::from("dec al")));
        assert_eq!(parse_instruction(&[0xfe, 0xcc]),
                   (2, String::from("dec ah")));
        assert_eq!(parse_instruction(&[0x4c]),
                   (1, String::from("dec sp")));
        assert_eq!(parse_instruction(&[0x4f]),
                   (1, String::from("dec di")));
        assert_eq!(parse_instruction(&[0xfe, 0x8e, 0xea, 0x03]),
                   (4, String::from("dec byte [bp + 1002]")));
        assert_eq!(parse_instruction(&[0xff, 0x4f, 0x27]),
                   (3, String::from("dec word [bx + 39]")));
        assert_eq!(parse_instruction(&[0xfe, 0x48, 0x05]),
                   (3, String::from("dec byte [bx + si + 5]")));
        assert_eq!(parse_instruction(&[0xff, 0x8b, 0xc4, 0xd8]),
                   (4, String::from("dec word [bp + di - 10044]")));
        assert_eq!(parse_instruction(&[0xff, 0x0e, 0x85, 0x24]),
                   (4, String::from("dec word [9349]")));
        assert_eq!(parse_instruction(&[0xfe, 0x4e, 0x00]),
                   (3, String::from("dec byte [bp]")));
    }

    #[test]
    fn test_neg_instructions() {
        assert_eq!(parse_instruction(&[0xf7, 0xd8]),
                   (2, String::from("neg ax")));
        assert_eq!(parse_instruction(&[0xf7, 0xd9]),
                   (2, String::from("neg cx")));
        assert_eq!(parse_instruction(&[0xf6, 0xde]),
                   (2, String::from("neg dh")));
        assert_eq!(parse_instruction(&[0xf6, 0xd8]),
                   (2, String::from("neg al")));
        assert_eq!(parse_instruction(&[0xf6, 0xdc]),
                   (2, String::from("neg ah")));
        assert_eq!(parse_instruction(&[0xf7, 0xdc]),
                   (2, String::from("neg sp")));
        assert_eq!(parse_instruction(&[0xf7, 0xdf]),
                   (2, String::from("neg di")));
        assert_eq!(parse_instruction(&[0xf6, 0x9e, 0xea, 0x03]),
                   (4, String::from("neg byte [bp + 1002]")));
        assert_eq!(parse_instruction(&[0xf7, 0x5f, 0x27]),
                   (3, String::from("neg word [bx + 39]")));
        assert_eq!(parse_instruction(&[0xf6, 0x58, 0x05]),
                   (3, String::from("neg byte [bx + si + 5]")));
        assert_eq!(parse_instruction(&[0xf7, 0x9b, 0xc4, 0xd8]),
                   (4, String::from("neg word [bp + di - 10044]")));
        assert_eq!(parse_instruction(&[0xf7, 0x1e, 0x85, 0x24,]),
                   (4, String::from("neg word [9349]")));
        assert_eq!(parse_instruction(&[0xf6, 0x5e, 0x00]),
                   (3, String::from("neg byte [bp]")));
    }

    #[test]
    fn test_mul_div_instructions() {
        assert_eq!(parse_instruction(&[0xf6, 0xe0]),
                   (2, String::from("mul al")));
        assert_eq!(parse_instruction(&[0xf7, 0xe1]),
                   (2, String::from("mul cx")));
        assert_eq!(parse_instruction(&[0xf7, 0x66, 0x00]),
                   (3, String::from("mul word [bp]")));
        assert_eq!(parse_instruction(&[0xf6, 0xa1, 0xf4, 0x01]),
                   (4, String::from("mul byte [bx + di + 500]")));
        assert_eq!(parse_instruction(&[0xf6, 0xed]),
                   (2, String::from("imul ch")));
        assert_eq!(parse_instruction(&[0xf7, 0xea]),
                   (2, String::from("imul dx")));
        assert_eq!(parse_instruction(&[0xf6, 0x2f]),
                   (2, String::from("imul byte [bx]")));
        assert_eq!(parse_instruction(&[0xf7, 0x2e, 0x0b, 0x25]),
                   (4, String::from("imul word [9483]")));
        assert_eq!(parse_instruction(&[0xf6, 0xf3,]),
                   (2, String::from("div bl")));
        assert_eq!(parse_instruction(&[0xf7, 0xf4,]),
                   (2, String::from("div sp")));
        assert_eq!(parse_instruction(&[0xf6, 0xb0, 0xae, 0x0b]),
                   (4, String::from("div byte [bx + si + 2990]")));
        assert_eq!(parse_instruction(&[0xf7, 0xb3, 0xe8, 0x03]),
                   (4, String::from("div word [bp + di + 1000]")));
        assert_eq!(parse_instruction(&[0xf7, 0xf8]),
                   (2, String::from("idiv ax")));
        assert_eq!(parse_instruction(&[0xf7, 0xfe]),
                   (2, String::from("idiv si")));
        assert_eq!(parse_instruction(&[0xf6, 0x3a]),
                   (2, String::from("idiv byte [bp + si]")));
        assert_eq!(parse_instruction(&[0xf7, 0xbf, 0xed, 0x01]),
                   (4, String::from("idiv word [bx + 493]")));
    }

    #[test]
    fn test_ascii_adjust_instructions() {
        assert_eq!(parse_instruction(&[0xd4, 0x0a]),
                   (2, String::from("aam")));
        assert_eq!(parse_instruction(&[0xd5, 0x0a]),
                   (2, String::from("aad")));
    }

    #[test]
    fn test_not_instructions() {
        assert_eq!(parse_instruction(&[0xf6, 0xd4]),
                   (2, String::from("not ah")));
        assert_eq!(parse_instruction(&[0xf6, 0xd3]),
                   (2, String::from("not bl")));
        assert_eq!(parse_instruction(&[0xf7, 0xd4]),
                   (2, String::from("not sp")));
        assert_eq!(parse_instruction(&[0xf7, 0xd6]),
                   (2, String::from("not si")));
        assert_eq!(parse_instruction(&[0xf7, 0x56, 0x00]),
                   (3, String::from("not word [bp]")));
        assert_eq!(parse_instruction(&[0xf6, 0x96, 0xb1, 0x26]),
                   (4, String::from("not byte [bp + 9905]")));
    }

    #[test]
    fn test_shift_rot_instructions() {
        assert_eq!(parse_instruction(&[0xd0, 0xe4]),
                   (2, String::from("shl ah, 1")));
        assert_eq!(parse_instruction(&[0xd1, 0xe8]),
                   (2, String::from("shr ax, 1")));
        assert_eq!(parse_instruction(&[0xd1, 0xfb]),
                   (2, String::from("sar bx, 1")));
        assert_eq!(parse_instruction(&[0xd1, 0xc1]),
                   (2, String::from("rol cx, 1")));
        assert_eq!(parse_instruction(&[0xd0, 0xce]),
                   (2, String::from("ror dh, 1")));
        assert_eq!(parse_instruction(&[0xd1, 0xd4]),
                   (2, String::from("rcl sp, 1")));
        assert_eq!(parse_instruction(&[0xd1, 0xdd]),
                   (2, String::from("rcr bp, 1")));
        assert_eq!(parse_instruction(&[0xd1, 0x66, 0x05]),
                   (3, String::from("shl word [bp + 5], 1")));
        assert_eq!(parse_instruction(&[0xd0, 0xa8, 0x39, 0xff]),
                   (4, String::from("shr byte [bx + si - 199], 1")));
        assert_eq!(parse_instruction(&[0xd0, 0xb9, 0xd4, 0xfe]),
                   (4, String::from("sar byte [bx + di - 300], 1")));
        assert_eq!(parse_instruction(&[0xd1, 0x46, 0x00]),
                   (3, String::from("rol word [bp], 1")));
        assert_eq!(parse_instruction(&[0xd1, 0x0e, 0x4a, 0x13]),
                   (4, String::from("ror word [4938], 1")));
        assert_eq!(parse_instruction(&[0xd0, 0x16, 0x03, 0x00]),
                   (4, String::from("rcl byte [3], 1")));
        assert_eq!(parse_instruction(&[0xd1, 0x1f]),
                   (2, String::from("rcr word [bx], 1")));
        assert_eq!(parse_instruction(&[0xd2, 0xe4]),
                   (2, String::from("shl ah, cl")));
        assert_eq!(parse_instruction(&[0xd3, 0xe8]),
                   (2, String::from("shr ax, cl")));
        assert_eq!(parse_instruction(&[0xd3, 0xfb]),
                   (2, String::from("sar bx, cl")));
        assert_eq!(parse_instruction(&[0xd3, 0xc1]),
                   (2, String::from("rol cx, cl")));
        assert_eq!(parse_instruction(&[0xd2, 0xce]),
                   (2, String::from("ror dh, cl")));
        assert_eq!(parse_instruction(&[0xd3, 0xd4]),
                   (2, String::from("rcl sp, cl")));
        assert_eq!(parse_instruction(&[0xd3, 0xdd]),
                   (2, String::from("rcr bp, cl")));
        assert_eq!(parse_instruction(&[0xd3, 0x66, 0x05]),
                   (3, String::from("shl word [bp + 5], cl")));
        assert_eq!(parse_instruction(&[0xd3, 0xa8, 0x39, 0xff]),
                   (4, String::from("shr word [bx + si - 199], cl")));
        assert_eq!(parse_instruction(&[0xd2, 0xb9, 0xd4, 0xfe]),
                   (4, String::from("sar byte [bx + di - 300], cl")));
        assert_eq!(parse_instruction(&[0xd2, 0x46, 0x00]),
                   (3, String::from("rol byte [bp], cl")));
        assert_eq!(parse_instruction(&[0xd2, 0x0e, 0x4a, 0x13]),
                   (4, String::from("ror byte [4938], cl")));
        assert_eq!(parse_instruction(&[0xd2, 0x16, 0x03, 0x00]),
                   (4, String::from("rcl byte [3], cl")));
        assert_eq!(parse_instruction(&[0xd3, 0x1f]),
                   (2, String::from("rcr word [bx], cl")));
    }

    #[test]
    fn test_and_instructions(){
        assert_eq!(parse_instruction(&[0x20, 0xe0]),
                   (2, String::from("and al, ah")));
        assert_eq!(parse_instruction(&[0x20, 0xcd]),
                   (2, String::from("and ch, cl")));
        assert_eq!(parse_instruction(&[0x21, 0xf5]),
                   (2, String::from("and bp, si")));
        assert_eq!(parse_instruction(&[0x21, 0xe7]),
                   (2, String::from("and di, sp")));
        assert_eq!(parse_instruction(&[0x24, 0x5d]),
                   (2, String::from("and al, 93")));
        assert_eq!(parse_instruction(&[0x25, 0xa8, 0x4f]),
                   (3, String::from("and ax, 20392")));
        assert_eq!(parse_instruction(&[0x20, 0x6a, 0x0a]),
                   (3, String::from("and [bp + si + 10], ch")));
        assert_eq!(parse_instruction(&[0x21, 0x91, 0xe8, 0x03]),
                   (4, String::from("and [bx + di + 1000], dx")));
        assert_eq!(parse_instruction(&[0x23, 0x5e, 0x00]),
                   (3, String::from("and bx, [bp]")));
        assert_eq!(parse_instruction(&[0x23, 0x0e, 0x20, 0x11]),
                   (4, String::from("and cx, [4384]")));
        assert_eq!(parse_instruction(&[0x80, 0x66, 0xd9, 0xef]),
                   (4, String::from("and byte [bp - 39], 239")));
        assert_eq!(parse_instruction(&[0x81, 0xa0, 0x14, 0xef, 0x58, 0x28]),
                   (6, String::from("and word [bx + si - 4332], 10328")));
    }

    #[test]
    fn test_test_instructions() {
        assert_eq!(parse_instruction(&[0x85, 0xcb]),
                   (2, String::from("test bx, cx")));
        assert_eq!(parse_instruction(&[0x84, 0xb6, 0x86, 0x01]),
                   (4, String::from("test [bp + 390], dh")));
        assert_eq!(parse_instruction(&[0x85, 0x76, 0x02]),
                   (3, String::from("test [bp + 2], si")));
        assert_eq!(parse_instruction(&[0xf6, 0xc3, 0x14]),
                  (3, String::from("test bl, 20")));
        assert_eq!(parse_instruction(&[0xf6, 0x07, 0x22]),
                   (3, String::from("test byte [bx], 34")));
        assert_eq!(parse_instruction(&[0xa9, 0x65, 0x5d]),
                   (3, String::from("test ax, 23909")));
    }

    #[test]
    fn test_or_instructions() {
        assert_eq!(parse_instruction(&[0x08, 0xe0]),
                   (2, String::from("or al, ah")));
        assert_eq!(parse_instruction(&[0x08, 0xcd]),
                   (2, String::from("or ch, cl")));
        assert_eq!(parse_instruction(&[0x09, 0xf5]),
                   (2, String::from("or bp, si")));
        assert_eq!(parse_instruction(&[0x09, 0xe7]),
                   (2, String::from("or di, sp")));
        assert_eq!(parse_instruction(&[0x0c, 0x5d]),
                   (2, String::from("or al, 93")));
        assert_eq!(parse_instruction(&[0x0d, 0xa8, 0x4f]),
                   (3, String::from("or ax, 20392")));
        assert_eq!(parse_instruction(&[0x08, 0x6a, 0x0a]),
                   (3, String::from("or [bp + si + 10], ch")));
        assert_eq!(parse_instruction(&[0x09, 0x91, 0xe8, 0x03]),
                   (4, String::from("or [bx + di + 1000], dx")));
        assert_eq!(parse_instruction(&[0x0b, 0x5e, 0x00]),
                   (3, String::from("or bx, [bp]")));
        assert_eq!(parse_instruction(&[0x0b, 0x0e, 0x20, 0x11]),
                   (4, String::from("or cx, [4384]")));
        assert_eq!(parse_instruction(&[0x80, 0x4e, 0xd9, 0xef]),
                   (4, String::from("or byte [bp - 39], 239")));
        assert_eq!(parse_instruction(&[0x81, 0x88, 0x14, 0xef, 0x58, 0x28]),
                   (6, String::from("or word [bx + si - 4332], 10328")));
    }

    #[test]
    fn test_xor_instructions() {
        assert_eq!(parse_instruction(&[0x30, 0xe0]),
                   (2, String::from("xor al, ah")));
        assert_eq!(parse_instruction(&[0x30, 0xcd]),
                   (2, String::from("xor ch, cl")));
        assert_eq!(parse_instruction(&[0x31, 0xf5]),
                   (2, String::from("xor bp, si")));
        assert_eq!(parse_instruction(&[0x31, 0xe7]),
                   (2, String::from("xor di, sp")));
        assert_eq!(parse_instruction(&[0x34, 0x5d]),
                   (2, String::from("xor al, 93")));
        assert_eq!(parse_instruction(&[0x35, 0xa8, 0x4f]),
                   (3, String::from("xor ax, 20392")));
        assert_eq!(parse_instruction(&[0x30, 0x6a, 0x0a]),
                   (3, String::from("xor [bp + si + 10], ch")));
        assert_eq!(parse_instruction(&[0x31, 0x91, 0xe8, 0x03]),
                   (4, String::from("xor [bx + di + 1000], dx")));
        assert_eq!(parse_instruction(&[0x33, 0x5e, 0x00]),
                   (3, String::from("xor bx, [bp]")));
        assert_eq!(parse_instruction(&[0x33, 0x0e, 0x20, 0x11]),
                   (4, String::from("xor cx, [4384]")));
        assert_eq!(parse_instruction(&[0x80, 0x76, 0xd9, 0xef]),
                   (4, String::from("xor byte [bp - 39], 239")));
        assert_eq!(parse_instruction(&[0x81, 0xb0, 0x14, 0xef, 0x58, 0x28]),
                   (6, String::from("xor word [bx + si - 4332], 10328")));
    }

    #[test]
    fn test_rep_instructions() {
        assert_eq!(parse_instruction(&[0xf3, 0xa4]),
                   (2, String::from("rep movsb")));
        assert_eq!(parse_instruction(&[0xf3, 0xa6]),
                   (2, String::from("rep cmpsb")));
        assert_eq!(parse_instruction(&[0xf3, 0xae]),
                   (2, String::from("rep scasb")));
        assert_eq!(parse_instruction(&[0xf3, 0xac]),
                   (2, String::from("rep lodsb")));
        assert_eq!(parse_instruction(&[0xf3, 0xa5]),
                   (2, String::from("rep movsw")));
        assert_eq!(parse_instruction(&[0xf3, 0xa7]),
                   (2, String::from("rep cmpsw")));
        assert_eq!(parse_instruction(&[0xf3, 0xaf]),
                   (2, String::from("rep scasw")));
        assert_eq!(parse_instruction(&[0xf3, 0xad]),
                   (2, String::from("rep lodsw")));
        assert_eq!(parse_instruction(&[0xf3, 0xaa]),
                   (2, String::from("rep stosb")));
        assert_eq!(parse_instruction(&[0xf3, 0xab]),
                   (2, String::from("rep stosw")));
        assert_eq!(parse_instruction(&[0xf2, 0xa4]),
                   (2, String::from("repnz movsb")));
        assert_eq!(parse_instruction(&[0xf2, 0xa6]),
                   (2, String::from("repnz cmpsb")));
        assert_eq!(parse_instruction(&[0xf2, 0xae]),
                   (2, String::from("repnz scasb")));
        assert_eq!(parse_instruction(&[0xf2, 0xac]),
                   (2, String::from("repnz lodsb")));
        assert_eq!(parse_instruction(&[0xf2, 0xa5]),
                   (2, String::from("repnz movsw")));
        assert_eq!(parse_instruction(&[0xf2, 0xa7]),
                   (2, String::from("repnz cmpsw")));
        assert_eq!(parse_instruction(&[0xf2, 0xaf]),
                   (2, String::from("repnz scasw")));
        assert_eq!(parse_instruction(&[0xf2, 0xad]),
                   (2, String::from("repnz lodsw")));
        assert_eq!(parse_instruction(&[0xf2, 0xaa]),
                   (2, String::from("repnz stosb")));
        assert_eq!(parse_instruction(&[0xf2, 0xab]),
                   (2, String::from("repnz stosw")));
    }

    #[test]
    fn test_call_ret() {
        assert_eq!(parse_instruction(&[0xff, 0x16, 0x21, 0x99]),
                   (4, String::from("call [39201]")));
        assert_eq!(parse_instruction(&[0xff, 0x56, 0x9c]),
                   (3, String::from("call [bp - 100]")));
        assert_eq!(parse_instruction(&[0xff, 0xd4]),
                   (2, String::from("call sp")));
        assert_eq!(parse_instruction(&[0xff, 0xd0]),
                   (2, String::from("call ax")));
        assert_eq!(parse_instruction(&[0xff, 0xe0]),
                   (2, String::from("jmp ax")));
        assert_eq!(parse_instruction(&[0xff, 0xe7]),
                   (2, String::from("jmp di")));
        assert_eq!(parse_instruction(&[0xff, 0x26, 0x0c, 0x00]),
                   (4, String::from("jmp [12]")));
        assert_eq!(parse_instruction(&[0xff, 0x26, 0x2b, 0x11]),
                   (4, String::from("jmp [4395]")));
        assert_eq!(parse_instruction(&[0xc2, 0xf9, 0xff]),
                   (3, String::from("ret -7")));
        assert_eq!(parse_instruction(&[0xc2, 0xf4, 0x01]),
                   (3, String::from("ret 500")));
        assert_eq!(parse_instruction(&[0xc3]),
                   (1, String::from("ret")));
    }

    fn test_int_instructions() {
        assert_eq!(parse_instruction(&[0xcd, 0x0d]),
                   (3, String::from("int 13")));
    }
}
