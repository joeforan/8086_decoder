use std::env::args;
use std::fs::read;

fn get_reg_str(w: u8,
               reg_code: u8) -> String
{
    String::from(
        match w{
            0 => match reg_code {
                0b000 => "al",
                0b001 => "cl",
                0b010 => "dl",
                0b011 => "bl",
                0b100 => "ah",
                0b101 => "ch",
                0b110 => "dh",
                0b111 => "bh",
                _ => {panic!()}
            },
            1 => match reg_code {
                0b000 => "ax",
                0b001 => "cx",
                0b010 => "dx",
                0b011 => "bx",
                0b100 => "sp",
                0b101 => "bp",
                0b110 => "si",
                0b111 => "di",
                _ => {panic!()}
            },
            _ => panic!()
        }
    )
}

fn parse_mov(d: u8,
             w: u8,
             mod_code: u8,
             reg_code: u8,
             r_m: u8) -> String
{
    let reg_string = get_reg_str(w, reg_code);
    let rm_string = match mod_code {
        0b11 => get_reg_str(w, r_m),
        _ => {panic!()}
    };
    match d {
        0 => String::from(format!("mov {}, {}", rm_string, reg_string)),
        1 => String::from(format!("mov {}, {}", reg_string, rm_string)),
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
        let opcode = byte & 0xFC;
        match opcode {
            0x88 => {
                let d = (byte & 0x02) >> 1;
                let w = byte & 0x01;
                i = i + 1;
                let aux = data[i];
                let mod_code = (aux & 0xC0) >> 6;
                let reg_code = (aux & 0x38) >> 3;
                let r_m = aux & 0x07;
                ret.push_str(&parse_mov(d, w, mod_code, reg_code, r_m));
                ret.push_str("\n")
            }
            _ => {
                println!("Unknown opcode 0x{:x}", opcode)
            }
        };
        i += 1;
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
        let d = (test_data[0] & 0x02) >> 1;
        let w = test_data[0] & 0x01;
        let mc = (test_data[1] & 0xC0) >> 6;
        let rc = (test_data[1] & 0x38) >> 3;
        let rm = test_data[1] & 0x07;
        assert_eq!(parse_mov(d, w, mc, rc, rm), String::from("mov cx, bx"));
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
