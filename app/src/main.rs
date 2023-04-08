use std::env::args;
use std::fs::read;

const REGS_WORD: [&str; 8] = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"];
const REGS_BYTE: [&str; 8] = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
const MOV_OPCODE: u8 = 0x22;

const OPCODE_MASK: u8 = 0x8C;
const OPCODE_SHFT: u8 = 0x02;

const D_MASK: u8 = 0x02;
const D_SHFT: u8 = 1;

const W_MASK: u8 = 0x01;
const W_SHFT: u8 = 0;

const MOD_MASK: u8 = 0xC0;
const MOD_SHFT: u8 = 6;

const REG_MASK: u8 = 0x38;
const REG_SHFT: u8 = 3;

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

fn parse_mov(data: &[u8]) -> (usize, String)
{
    let d_flag = (data[0] & D_MASK) >> D_SHFT;
    let w_flag = (data[0] & W_MASK) >> W_SHFT;
    let mod_code = (data[1] & MOD_MASK) >> MOD_SHFT;
    let reg_code = (data[1] & REG_MASK) >> REG_SHFT;
    let r_m_code = (data[1] & RM_MASK) >> RM_SHFT;

    let reg_string = get_reg_str(w_flag, reg_code);
    let rm_string = match mod_code {
        0b11 => get_reg_str(w_flag, r_m_code),
        _ => {panic!()}
    };

    let offset: usize = 2;
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
                let (offset, code) = parse_mov(&data[i..n]);
                ret.push_str(&code);
                ret.push_str("\n");
                i += offset;
            }
            _ => {
                println!("Unknown opcode 0x{:x}", opcode)
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
        assert_eq!(parse_mov(&test_data), (2, String::from("mov cx, bx")));
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
}
