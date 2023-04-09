use std::env::args;
use std::fs::read;

const REGS_WORD: [&str; 8] = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
const REGS_BYTE: [&str; 8] = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
const MOV_OPCODE: u8 = 0x22;
const IMMMOV_OPCODE: u8 = 0x0B;

//const IMMOPCODE_MASK: u8 = 0xF0;
//const IMMOPCODE_SHFT: u8 = 0x04;

const OPCODE_MASK: u8 = 0xFC;
const OPCODE_SHFT: u8 = 0x02;

const D_MASK: u8 = 0x02;
const D_SHFT: u8 = 1;

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

fn get_reg_str(flag: u8,
               code: u8) -> String
{
    String::from(
        match flag {
            0 => REGS_WORD[code as usize],
            1 => REGS_BYTE[code as usize],
            _ => {panic!()}
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
                                mod_code: u8) -> (usize, String)
{
    let mut data_offset: usize = 0;
    let mut ret: String = "[".to_owned();
    if (mod_code == 0b00) && (rm_code == 0b110) {
        let disp = read_u16_val(&data[2..4]);
        ret.push_str(&format!("{}]", disp));
        return (2, ret);
    }
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
            _ => panic!()
        }
    );
    let suffix: String =
        match mod_code {
            0b00 => String::from("]"),
            0b01 => {
                data_offset = 1;
                if data[2] == 0 {
                    String::from(format!("]"))
                } else {
                    String::from(format!(" + {}]", data[2]))
                }
            },
            0b10 => {
                data_offset = 2;
                let data_val = read_u16_val(&data[2..4]);
                if data_val == 0 {
                    String::from(format!("]"))
                }else {
                    String::from(format!(" + {}]", data_val))
                }
            }
            _ => panic!()
        };
    ret.push_str(&suffix);
    (data_offset, ret)
}

fn parse_mov(opcode: u8, data: &[u8]) -> (usize, String)
{
    let mut offset: usize = 2;
    if opcode == IMMMOV_OPCODE {
        let w_flag = (data[0] & IMMW_MASK) >> IMMW_SHFT;
        let reg_code = (data[0] & IMMREG_MASK) >> IMMREG_SHFT;
        let reg_string = get_reg_str(w_flag, reg_code);
        return match w_flag {
            0 => (offset, String::from(format!("mov {}, {}", reg_string, data[1] as i8))),
            1 => {
                let val: i16 = read_i16_val(&data[1..3]);
                (offset+1, String::from(format!("mov {}, {}", reg_string, val)))
            },
            _ => panic!()
        }
    }

    let d_flag = (data[0] & D_MASK) >> D_SHFT;
    let w_flag = (data[0] & W_MASK) >> W_SHFT;
    let mod_code = (data[1] & MOD_MASK) >> MOD_SHFT;
    let reg_code = (data[1] & REG_MASK) >> REG_SHFT;
    let r_m_code = (data[1] & RM_MASK) >> RM_SHFT;

    let reg_string = get_reg_str(w_flag, reg_code);
    let rm_string = match mod_code {
        0b11 => get_reg_str(w_flag, r_m_code),
        _ => {
            let t = get_mem_ptr_and_displacement(data, r_m_code, mod_code);
            offset += t.0;
            t.1
        }
    };
    match d_flag {
        0 => (offset, String::from(format!("mov {}, {}", rm_string, reg_string))),
        1 => (offset, String::from(format!("mov {}, {}", reg_string, rm_string))),
        _ => panic!()
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
        let byte = data[i];
        let opcode = (byte & OPCODE_MASK) >> OPCODE_SHFT;
        match opcode {
            MOV_OPCODE => {
                let (offset, code) = parse_mov(opcode, &data[i..n]);
                ret.push_str(&code);
                ret.push_str("\n");
                i += offset;
            }
            _ => {
                println!("Unknown opcode 0x{:x}", opcode);
                panic!();
            }
        };
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
    fn test_parse_mov() {
        let test_data: [u8; 2] = [0x89, 0xd9];
        assert_eq!(parse_mov(MOV_OPCODE, &test_data), (2, String::from("mov cx, bx")));
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
    fn test_parse_8bit_imm_mov() {
        {
            let test_data: [u8; 2] = [0xb1, 0x0c];
            assert_eq!(parse_mov(IMMMOV_OPCODE, &test_data), (2, String::from("mov cl, 12")));
        }
        {
            let test_data: [u8; 2] = [0xb5, 0xf4];
            assert_eq!(parse_mov(IMMMOV_OPCODE, &test_data), (2, String::from("mov ch, -12")));
        }
    }

    #[test]
    fn test_parse_16bit_imm_mov() {
        let test_data: [[u8; 3]; 4] = [[0xb9, 0x0c, 0x00],
                                       [0xb9, 0xf4, 0xff],
                                       [0xba, 0x6c, 0x0f],
                                       [0xba, 0x94, 0xf0]];

        assert_eq!(parse_mov(IMMMOV_OPCODE, &test_data[0]), (3, String::from("mov cx, 12")));
        assert_eq!(parse_mov(IMMMOV_OPCODE, &test_data[1]), (3, String::from("mov cx, -12")));
        assert_eq!(parse_mov(IMMMOV_OPCODE, &test_data[2]), (3, String::from("mov dx, 3948")));
        assert_eq!(parse_mov(IMMMOV_OPCODE, &test_data[3]), (3, String::from("mov dx, -3948")));
    }

    #[test]
    fn test_src_address_calcualtion() {
        let test_data_w2: [[u8; 2]; 2] = [[0x8a, 0x00],
                                          [0x8b, 0x1b]];
        let test_data_w3: [[u8; 3]; 2] = [[0x8b, 0x56, 0x00],
                                          [0x8a, 0x60, 0x04]];
        let test_data_w4: [[u8; 4]; 1] = [[0x8a, 0x80, 0x87, 0x13]];

        assert_eq!(parse_mov(MOV_OPCODE, &test_data_w2[0]), (2, String::from("mov al, [bx + si]")));
        assert_eq!(parse_mov(MOV_OPCODE, &test_data_w2[1]), (2, String::from("mov bx, [bp + di]")));
        assert_eq!(parse_mov(MOV_OPCODE, &test_data_w3[0]), (3, String::from("mov dx, [bp]")));
        assert_eq!(parse_mov(MOV_OPCODE, &test_data_w3[1]), (3, String::from("mov ah, [bx + si + 4]")));
        assert_eq!(parse_mov(MOV_OPCODE, &test_data_w4[0]), (4, String::from("mov al, [bx + si + 4999]")));
    }
}
