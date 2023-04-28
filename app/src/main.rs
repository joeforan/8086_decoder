use std::env::args;
use std::fs::read;

enum OpcodeType
{
    RegMov,
    Rm,
    Acc,
    Standard
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
    SubRm
}

#[derive(PartialEq, Clone, Copy)]
enum BitValue{
    BV0 = 0,
    BV1 = 1
}

#[derive(PartialEq, Clone, Copy)]
enum TwoBitValue{
    TBV00 = 0b00,
    TBV01 = 0b01,
    TBV10 = 0b10,
    TBV11 = 0b11
}

const NO_OPCODES: usize = 10;

const REGS_BYTE: [&str; 8] = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
const REGS_WORD: [&str; 8] = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];

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
            SubRm => "sub",

            MovStd |
            MovReg |
            MovRm |
            MovMem2Acc |
            MovAcc2Mem  => "mov"
        }
    )
}

fn get_opcode_type(opcode: Opcode) -> OpcodeType {
    use Opcode::*;
    match opcode {
        MovReg => OpcodeType::RegMov,

        MovRm |
        AddRm |
        SubRm => OpcodeType::Rm,

        MovMem2Acc |
        MovAcc2Mem |
        AddAcc => OpcodeType::Acc,

        MovStd |
        AddStd |
        SubStd => OpcodeType::Standard,
    }
}

fn get_opcode(data: &[u8]) -> Opcode {
    use Opcode::*;
    const LUT: [(u8, u8, u8, u8, Opcode); NO_OPCODES] = [
        (0x88, 0xFC, 0, 0, MovStd),
        (0xB0, 0xF0, 0, 0, MovReg),
        (0xc6, 0xFE, 0, 0, MovRm),
        (0xA0, 0xFE, 0, 0, MovMem2Acc),
        (0xA2, 0xFE, 0, 0, MovAcc2Mem),
        (0x00, 0xFC, 0, 0, AddStd),
        (0x80, 0xFC, 0, 0x38, AddRm),
        (0x04, 0xFC, 0, 0, AddAcc),
        (0x28, 0xFC, 0, 0, SubStd),
        (0x80, 0xFC, 0x28, 0x38, SubRm)];

    for t in LUT.iter() {
        if t.3 != 0 {
            if ((data[0] & t.1) == t.0) & ((data[1] & t.3) == t.2) {
                return t.4
            }
        } else if (data[0] & t.1) == t.0 {
            return t.4
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
        TwoBitValue::TBV00
    } else if tb == 1 {
        TwoBitValue::TBV01
    } else if tb == 2 {
        TwoBitValue::TBV10
    } else {
        TwoBitValue::TBV11
    }
}

fn get_reg_str(flag: BitValue,
               code: u8) -> String
{
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
                                rm_code: u8,
                                mod_code: TwoBitValue) -> (usize, String)
{
    let mut data_offset: usize = 0;
    let mut ret: String = "[".to_owned();
    if (mod_code == TwoBitValue::TBV00) && (rm_code == 0b110) {
        let disp = read_u16_val(&data[2..4]);
        ret.push_str(&format!("{}]", disp));
        (2, ret)
    } else {
        ret.push_str(
            match rm_code {
                0b000 => "bx + si",
                0b001 => "bx + di",
                0b010 => "bp + si",
                0b011 => "bp + di",
                0b100 => "si",
                0b101 => "di",
                0b110 => "bp",
                0b111 => "bx",
                _ => unreachable!()
            }
        );
        let suffix: String =
            match mod_code {
                TwoBitValue::TBV00 => String::from("]"),
                TwoBitValue::TBV01 => {
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
                TwoBitValue::TBV10 => {
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
    let immreg_code = (data[0] & IMMREG_MASK) >> IMMREG_SHFT;
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
    let oc_mnmnc = get_opcode_mnemonic(opcode);
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let r_m_code = (data[1] & RM_MASK) >> RM_SHFT;
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);

    if mod_code != TBV11 {
        let (data_offset, reg_string) = get_mem_ptr_and_displacement(data, r_m_code, mod_code);
        let data_idx: usize = if (mod_code == TBV01) || (mod_code == TBV10) { 4 } else { 2 };
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
        let rm_string = get_reg_str(w_flag, r_m_code);
        let data_idx: usize = 2;
        (3, String::from(format!("{} {}, {}", oc_mnmnc, rm_string, data[data_idx])))
    }
}

fn parse_acc_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_string = get_reg_str(w_flag, 0b000);
    let oc_mnmnc = get_opcode_mnemonic(opcode);
     if (opcode == Opcode::AddAcc) & (w_flag == BitValue::BV0) {
        (2, String::from(format!("{} {}, {}", oc_mnmnc,  reg_string, data[1] as i8)))
    } else {
        let val = read_u16_val(&data[1..2+w_flag as usize]);
        match opcode {
            Opcode::MovMem2Acc => (3, String::from(format!("{} {}, [{}]", oc_mnmnc, reg_string, val))),
            Opcode::MovAcc2Mem => (3, String::from(format!("{} [{}], {}", oc_mnmnc, val, reg_string))),
            Opcode::AddAcc => (3, String::from(format!("{} {}, {}", oc_mnmnc,  reg_string, val))),
            _ => unreachable!()
        }
    }
}

fn parse_std_instruction(opcode: Opcode, data: &[u8]) -> (usize, String) {
    use TwoBitValue::*;
    let oc_mnmnc = get_opcode_mnemonic(opcode);
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let d_flag = (data[0] & D_MASK) >> D_SHFT;
    let reg_code = (data[1] & REG_MASK) >> REG_SHFT;
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let r_m_code = (data[1] & RM_MASK) >> RM_SHFT;

    let reg_string = get_reg_str(w_flag, reg_code);
    let mut offset: usize = 2;

    let rm_string = match mod_code {
        TBV11 => get_reg_str(w_flag, r_m_code),
        _ => {
            let t = get_mem_ptr_and_displacement(data, r_m_code, mod_code);
            offset += t.0;
            t.1
        }
    };
    let (left, right) = if d_flag == 0 {
        (rm_string, reg_string)
    } else {
        (reg_string, rm_string)
    };

    (offset, String::from(format!("{} {}, {}", oc_mnmnc, left, right)))
}

fn parse_instruction(data: &[u8]) -> (usize, String)
{
    let opcode = get_opcode(data);
    match get_opcode_type(opcode) {
        OpcodeType::RegMov => parse_reg_mov(opcode, data),
        OpcodeType::Rm => parse_rm_instruction(opcode, data),
        OpcodeType::Acc =>  parse_acc_instruction(opcode, data),
        OpcodeType::Standard => parse_std_instruction(opcode, data)
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
    let mut ret: String = "".to_owned();
    ret.push_str("bits 16\n\n");
    while i < n {
        let (offset, code): (usize, String) = {
            parse_instruction(&data[i..n])
        };
        ret.push_str(&code);
        ret.push_str("\n");
        i += offset;
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

}
