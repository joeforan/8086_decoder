use std::env::args;
use std::fs::read;
use std::collections::{BTreeSet, HashMap};


const OFFSET_STR: &str = "#OFFSET#";

enum OpcodeType
{
    RegMov,
    Rm,
    Acc,
    Standard,
    Jump,
    PushPop,
    Xchg,
    InOut
}

#[derive(PartialEq, Clone, Copy)]
enum Opcode {
    MovStd,
    MovReg,
    MovRm,
    MovMem2Acc,
    MovAcc2Mem,
    AddStd,
    AddRm,
    AddAcc,
    SubStd,
    SubRm,
    SubAcc,
    CmpStd,
    CmpRm,
    CmpAcc,
    Jnz,
    Je,
    Jl,
    Jle,
    Jb,
    Jbe,
    Jp,
    Jo,
    Js,
    Jnl,
    Jg,
    Jnb,
    Ja,
    Jnp,
    Jno,
    Jns,
    Loop,
    Loopz,
    Loopnz,
    Jcxz,
    PushRm,
    PushReg,
    PushSr,
    PopRm,
    PopReg,
    PopSr,
    XchgReg,
    XchgAcc,
    InData,
    InReg,
    OutData,
    OutReg
}

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

const NO_OPCODES: usize = 46;

const D_MASK: u8 = 0x02;
const D_SHFT: u8 = 1;

const S_MASK: u8 = 0x02;
const S_SHFT: u8 = 1;

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


fn get_opcode_mnemonic(opcode: Opcode) -> String
{
    use Opcode::*;
    String::from (
        match opcode {
            AddStd | AddRm | AddAcc => "add",

            SubStd |
            SubRm |
            SubAcc => "sub",

            MovStd |
            MovReg |
            MovRm |
            MovMem2Acc |
            MovAcc2Mem  => "mov",

            CmpStd |
            CmpRm |
            CmpAcc => "cmp",

            Jnz => "jnz",
            Je => "je",
            Jl => "jl",
            Jle => "jle",
            Jb => "jb",
            Jbe => "jbe",
            Jp => "jp",
            Jo => "jo",
            Js => "js",
            Jnl => "jnl",
            Jg => "jg",
            Jnb => "jnb",
            Ja => "ja",
            Jnp => "jnp",
            Jno => "jno",
            Jns => "jns",
            Loop => "loop",
            Loopz => "loopz",
            Loopnz => "loopnz",
            Jcxz => "jcxz",

            PushRm |
            PushReg |
            PushSr => "push",

            PopRm |
            PopReg |
            PopSr => "pop",

            XchgReg |
            XchgAcc => "xchg",

            InData |
            InReg => "in",

            OutData |
            OutReg => "out"

        }
    )
}

fn get_opcode_type(opcode: Opcode) -> OpcodeType {
    use Opcode::*;
    match opcode {
        MovReg => OpcodeType::RegMov,

        MovRm |
        AddRm |
        SubRm |
        CmpRm => OpcodeType::Rm,

        MovMem2Acc |
        MovAcc2Mem |
        AddAcc |
        SubAcc |
        CmpAcc => OpcodeType::Acc,

        MovStd |
        AddStd |
        SubStd |
        CmpStd => OpcodeType::Standard,

        Jnz |
        Je |
        Jl |
        Jle |
        Jb |
        Jbe |
        Jp |
        Jo |
        Js |
        Jnl |
        Jg |
        Jnb |
        Ja |
        Jnp |
        Jno |
        Jns |
        Loop |
        Loopz |
        Loopnz |
        Jcxz => OpcodeType::Jump,

        PushRm |
        PushReg |
        PushSr |
        PopRm |
        PopReg |
        PopSr => OpcodeType::PushPop,

        XchgReg |
        XchgAcc => OpcodeType::Xchg,

        InData |
        InReg  |
        OutData |
        OutReg => OpcodeType::InOut,
    }
}

fn get_opcode(data: &[u8]) -> Opcode {
    use Opcode::*;
    const LUT: [(u8, u8, u8, u8, Opcode); NO_OPCODES] = [
        (0x88, 0xFC, 0x00, 0x00, MovStd),
        (0xB0, 0xF0, 0x00, 0x00, MovReg),
        (0xc6, 0xFE, 0x00, 0x00, MovRm),
        (0xA0, 0xFE, 0x00, 0x00, MovMem2Acc),
        (0xA2, 0xFE, 0x00, 0x00, MovAcc2Mem),
        (0x00, 0xFC, 0x00, 0x00, AddStd),
        (0x80, 0xFC, 0x00, 0x38, AddRm),
        (0x04, 0xFC, 0x00, 0x00, AddAcc),
        (0x28, 0xFC, 0x00, 0x00, SubStd),
        (0x80, 0xFC, 0x28, 0x38, SubRm),
        (0x2C, 0xFE, 0x00, 0x00, SubAcc),
        (0x38, 0xFC, 0x00, 0x00, CmpStd),
        (0x80, 0xFC, 0x38, 0x38, CmpRm),
        (0x3c, 0xFE, 0x00, 0x00, CmpAcc),
        (0x75, 0xFF, 0x00, 0x00, Jnz),
        (0x74, 0xFF, 0x00, 0x00, Je),
        (0x7C, 0xFF, 0x00, 0x00, Jl),
        (0x7E, 0xFF, 0x00, 0x00, Jle),
        (0x72, 0xFF, 0x00, 0x00, Jb),
        (0x76, 0xFF, 0x00, 0x00, Jbe),
        (0x7A, 0xFF, 0x00, 0x00, Jp),
        (0x70, 0xFF, 0x00, 0x00, Jo),
        (0x78, 0xFF, 0x00, 0x00, Js),
        (0x7D, 0xFF, 0x00, 0x00, Jnl),
        (0x7F, 0xFF, 0x00, 0x00, Jg),
        (0x73, 0xFF, 0x00, 0x00, Jnb),
        (0x77, 0xFF, 0x00, 0x00, Ja),
        (0x7B, 0xFF, 0x00, 0x00, Jnp),
        (0x71, 0xFF, 0x00, 0x00, Jno),
        (0x79, 0xFF, 0x00, 0x00, Jns),
        (0xE2, 0xFF, 0x00, 0x00, Loop),
        (0xE1, 0xFF, 0x00, 0x00, Loopz),
        (0xE0, 0xFF, 0x00, 0x00, Loopnz),
        (0xE3, 0xFF, 0x00, 0x00, Jcxz),
        (0xFF, 0xFF, 0x00, 0x00, PushRm),
        (0x50, 0xF8, 0x00, 0x00, PushReg),
        (0x06, 0xE7, 0x00, 0x00, PushSr),
        (0x8F, 0xFF, 0x00, 0x00, PopRm),
        (0x58, 0xF8, 0x00, 0x00, PopReg),
        (0x03, 0xE3, 0x00, 0x00, PopSr),
        (0x86, 0xFE, 0x00, 0x00, XchgReg),
        (0x90, 0xF8, 0x00, 0x00, XchgAcc),
        (0xE4, 0xFE, 0x00, 0x00, InData),
        (0xEC, 0xFE, 0x00, 0x00, InReg),
        (0xE6, 0xFE, 0x00, 0x00, OutData),
        (0xEE, 0xFE, 0x00, 0x00, OutReg),
    ];

    for t in LUT.iter() {
        if data[0] & t.1 == t.0 {
            if t.3 == 0 {
                return t.4;
            } else if (data[1] & t.3) == t.2 {
                return t.4;
            }
        }
    }
    panic!("Unknown opcode {}", data[0])
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
                _ => unreachable!()
            };
        ret.push_str(&suffix);
        (data_offset, ret)
    }
}

fn parse_reg_mov(opcode: Opcode, data: &[u8]) -> (usize, String) {
    let oc_mnmnc = get_opcode_mnemonic(opcode);
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

fn parse_rm_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    use TwoBitValue::*;
    use ThreeBitValue::*;
    let oc_mnmnc = get_opcode_mnemonic(opcode);
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);

    if mod_code != DBV11 {
        let (data_offset, reg_string) = get_mem_ptr_and_displacement(data, rm_code, mod_code);
        let data_idx: usize = if (mod_code == DBV01) || (mod_code == DBV10) ||
            ((mod_code == DBV00) & (rm_code == TBV110)) { 4 } else { 2 };
        if opcode == Opcode::MovRm {
            return match w_flag {
                BitValue::BV0 =>  (data_offset + 3, String::from(format!("{} {}, byte {}", oc_mnmnc, reg_string, data[data_idx]))),
                BitValue::BV1 =>  (data_offset + 4, String::from(format!("{} {}, word {}", oc_mnmnc, reg_string,
                                                                      read_u16_val(&data[data_idx..data_idx+2])))),
            }
        } else {
            return match w_flag {
                BitValue::BV0 =>  (data_offset + 3, String::from(format!("{} byte {}, {}", oc_mnmnc, reg_string, data[data_idx]))),
                BitValue::BV1 =>  {
                    let s_flag = (data[0] & S_MASK) >> S_SHFT;
                    if s_flag == 0 {
                        (data_offset + 3, String::from(format!("{} word {}, {}", oc_mnmnc, reg_string,
                                                                        read_u16_val(&data[data_idx..data_idx+2]))))
                    } else {
                        (data_offset + 3, String::from(format!("{} word {}, {}", oc_mnmnc, reg_string,
                                                                        (data[data_idx] as i8) as i16)))
                    }
                }
            }
        }
    } else {
        let rm_string = get_reg_str(w_flag, rm_code);
        let data_idx: usize = 2;
        (3, String::from(format!("{} {}, {}", oc_mnmnc, rm_string, data[data_idx])))
    }
}

fn parse_acc_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    use ThreeBitValue::*;

    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_string = get_reg_str(w_flag, TBV000);
    let oc_mnmnc = get_opcode_mnemonic(opcode);
    if ((opcode == Opcode::AddAcc) |
        (opcode == Opcode::SubAcc) |
        (opcode == Opcode::CmpAcc)) & (w_flag == BitValue::BV0) {
        (2, String::from(format!("{} {}, {}", oc_mnmnc,  reg_string, data[1] as i8)))
    } else {
        let val = read_u16_val(&data[1..2+w_flag as usize]);
        match opcode {
            Opcode::MovMem2Acc => (3, String::from(format!("{} {}, [{}]", oc_mnmnc, reg_string, val))),
            Opcode::MovAcc2Mem => (3, String::from(format!("{} [{}], {}", oc_mnmnc, val, reg_string))),
            Opcode::AddAcc |
            Opcode::SubAcc |
            Opcode::CmpAcc => (3, String::from(format!("{} {}, {}", oc_mnmnc,  reg_string, val))),
            _ => unreachable!()
        }
    }
}

fn get_reg_and_rm_strings(data: &[u8],
                          w_flag: BitValue,
                          mod_code: TwoBitValue,
                          reg_code: ThreeBitValue,
                          rm_code: ThreeBitValue,
                          d_flag: BitValue) -> (usize, String) {

    use BitValue::*;
    use TwoBitValue::*;

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

    (offset, String::from(format!("{}, {}", left, right)))
}

fn parse_std_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    let oc_mnmnc = get_opcode_mnemonic(opcode);
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_code = get_three_bit_value((data[1] & REG_MASK) >> REG_SHFT);
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let d_flag = get_bit_value((data[0] & D_MASK) >> D_SHFT);

    let (offset, reg_str) = get_reg_and_rm_strings(data,
                                                   w_flag,
                                                   mod_code,
                                                   reg_code,
                                                   rm_code,
                                                   d_flag);

    (offset, String::from(format!("{} {}", oc_mnmnc, reg_str)))
}

fn parse_jmp_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    let jump_offset: i8 = data[1] as i8;
    let oc_mnmc = get_opcode_mnemonic(opcode);
    (2, String::from(format!("{} {} {}", oc_mnmc, OFFSET_STR, jump_offset)))
}

fn parse_pushpop_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    use Opcode::*;
    use BitValue::*;

    let oc_mnmc = get_opcode_mnemonic(opcode);
    match opcode {
        PushRm | PopRm => {
            let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
            let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
            let t = get_mem_ptr_and_displacement(data, rm_code, mod_code);
            (2 + t.0, String::from(format!("{} word {}", oc_mnmc, t.1)))
        },
        PushReg | PopReg => {
            let reg_string = get_reg_str(BV1, get_three_bit_value(data[0] & 0x7));
            (1, String::from(format!{"{} {}", oc_mnmc, reg_string}))
        },
        PushSr | PopSr => {
            let reg_string = get_seg_reg_str(
                get_two_bit_value((data[0] & 0x18) >> 3));
            (1, String::from(format!{"{} {}", oc_mnmc, reg_string}))
        },
        _ => unreachable!()
    }
}

fn parse_xchg_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    use Opcode::*;
    use BitValue::*;
    let oc_mnmc = get_opcode_mnemonic(opcode);
    match opcode {
        XchgReg => {
            let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
            let reg_code = get_three_bit_value((data[1] & REG_MASK) >> REG_SHFT);
            let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
            let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);

            let (offset, operand_str) = get_reg_and_rm_strings(data,
                                                               w_flag,
                                                               mod_code,
                                                               reg_code,
                                                               rm_code,
                                                               BV1);
            (offset, String::from(format!("{} {}", oc_mnmc, operand_str)))
        },
        XchgAcc => {
            let reg_string = get_reg_str(BV1, get_three_bit_value(data[0] & 0x7));
            (1, String::from(format!("{} ax, {}", oc_mnmc, reg_string)))
        },
        _ => unreachable!()
    }
}

fn parse_inout_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    use Opcode::*;
    use BitValue::*;
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_mnmc = match w_flag { BV0 => "al", BV1 => "ax" };
    match opcode {
        InData => {
            (2, String::from(format!("in {}, {}",
                                     reg_mnmc, data[1])))
        },
        InReg => {
            (1, String::from(format!("in {}, dx",
                                     reg_mnmc)))
        },
        OutData => {
            (2, String::from(format!("out {}, {}", data[1], reg_mnmc)))
        },
        OutReg => {
            (1, String::from(format!("out dx, {}", reg_mnmc)))
        }
        _ => unreachable!()
    }
}

fn parse_instruction(data: &[u8]) -> (usize, String)
{
    let opcode = get_opcode(data);
    match get_opcode_type(opcode) {
        OpcodeType::RegMov => parse_reg_mov(opcode, data),
        OpcodeType::Rm => parse_rm_instruction(opcode, data),
        OpcodeType::Acc =>  parse_acc_instruction(opcode, data),
        OpcodeType::Standard => parse_std_instruction(opcode, data),
        OpcodeType::Jump => parse_jmp_instruction(opcode, data),
        OpcodeType::PushPop  => parse_pushpop_instruction(opcode, data),
        OpcodeType::Xchg => parse_xchg_instruction(opcode, data),
        OpcodeType::InOut => parse_inout_instruction(opcode, data),
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
        let test_data_w6: [[u8; 6]; 1] = [[0xc7, 0x85, 0x85, 0x03, 0x5b, 0x01]];

        assert_eq!(parse_instruction(&test_data_w3[0]), (3, String::from("mov [bp + di], byte 7")));
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
    fn test_xlat_instruction() {
        assert_eq!(parse_instruction(&[0xd7]),
                   (1, String::from("xlat")));
    }
}
