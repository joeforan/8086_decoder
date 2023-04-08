use std::env::args;
use std::fs::read;

fn get_reg_str(w: u8,
               reg_code: u8) -> String
{
    String::from(
        match w{
            0 => match reg_code {
                0b000 => "AL",
                0b001 => "CL",
                0b010 => "DL",
                0b011 => "BL",
                0b100 => "AH",
                0b101 => "CH",
                0b110 => "DH",
                0b111 => "BH",
                _ => {panic!()}
            },
            1 => match reg_code {
                0b000 => "AX",
                0b001 => "CX",
                0b010 => "DX",
                0b011 => "BX",
                0b100 => "SP",
                0b101 => "BP",
                0b110 => "SI",
                0b111 => "DI",
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
        0 => String::from(format!("MOV {},{}", rm_string, reg_string)),
        1 => String::from(format!("MOV {},{}", reg_string, rm_string)),
        _ => panic!()
    }
}


fn decode_from_file(filepath: &String) {
    let data = read(filepath).expect(
        &format!("Failure reading {}", filepath));
    decode_from_data(&data)
}

fn decode_from_data(data: &[u8]) {
    let n = data.len();
    let mut i = 0;
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
                println!("{}", parse_mov(d, w, mod_code, reg_code, r_m));
            }
            _ => {
                println!("Unknown opcode 0x{:x}", opcode)
            }
        }
    }
}

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        println!("Usage: args[0] binary_file");
        return;
    }
    decode_from_file(&args[1])
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
        assert_eq!(parse_mov(d, w, mc, rc, rm), String::from("MOV CX,BX"));
    }
}
