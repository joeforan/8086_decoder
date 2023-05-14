use std::env::args;
use std::fs::read;
use std::collections::{BTreeSet, HashMap};
use std::fmt::Display;

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
    MovWithSr,
    JumpI8,
    JumpI16,
    RmWithDisp,
    ShiftRot,
    SingleByteWithReg,
    SingleByteWithSr,
    InOut,
    AsciiAdjust,
    Repeat,
    ImmU8,
//    ImmI8,
  //  ImmU16,
    ImmI16,
    Lock,
    Segment,
    Intersegment,
    Direct,
    Nop
}

#[derive (PartialEq, Clone, Copy)]
enum Command {
    Mov,
    Add,
    Adc,
    Sub,
    Sbb,
    Cmp,
    Inc,
    Dec,
    Neg,
    And,
    Or,
    Not,
    Xor,
    Test,
    Mul,
    Imul,
    Div,
    IDiv,
    Shl,
    Shr,
    Sar,
    Rol,
    Ror,
    Rcl,
    Rcr,
    Push,
    Pop,
    Call,
    Jmp,
    Ret,
    Retf,
    Je,
    Jl,
    Jle,
    Jb,
    Jbe,
    Jp,
    Jo,
    Js,
    Jnz,
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
    Int,
    Int3,
    Into,
    Iret,
    Xchg,
    Rep,
    Repnz,
    In,
    Out,
    Xlat,
    Lea,
    Lds,
    Les,
    Lahf,
    Sahf,
    Pushf,
    Popf,
    Aaa,
    Aas,
    Daa,
    Das,
    Aam,
    Aad,
    Cbw,
    Cwd,
    Clc,
    Cmc,
    Stc,
    Cld,
    Std,
    Cli,
    Sti,
    Hlt,
    Wait,
    Lock,
    Nop,
}

#[derive (Clone, Copy, Debug)]
enum Reg {
    Al,
    Ah,
    Ax,
    Bl,
    Bh,
    Bx,
    Cl,
    Ch,
    Cx,
    Dl,
    Dh,
    Dx,
    Sp,
    Bp,
    Si,
    Di
}
#[derive (Clone, Copy, Debug)]
enum SegReg {
    Cs,
    Ds,
    Es,
    Ss
}

#[derive (PartialEq, Copy, Clone, Debug)]
enum AdrReg {
    BxSi,
    BxDi,
    BpSi,
    BpDi,
    Si,
    Di,
    Bp,
    Bx,
}

#[derive(Copy, Clone, Debug)]
enum RepeatOperand {
    Movsb,
    Movsw,
    Cmpsb,
    Cmpsw,
    Scasb,
    Scasw,
    Lodsb,
    Lodsw,
    Stosb,
    Stosw,
}

#[derive (Copy, Clone, Debug)]
enum Operand{
    Reg(Reg),
    SegReg(SegReg),
    ImmU8(u8),
    ImmI8(i8),
    ImmU16(u16),
    ImmI16(i16),
    PtrDisp((AdrReg, i16)),
    PtrDir(u16),
    Offset8(i8),
    Offset16(i16),
    RepeatOperand(RepeatOperand)
}

#[derive (Copy, Clone)]
enum DataSize
{
    Byte,
    Word
}

#[derive (Copy, Clone)]
struct Instruction
{
    cmd: Command,
    op1: Option<Operand>,
    op2: Option<Operand>,
    size: Option<DataSize>,
    lock: bool,
    segment: Option<SegReg>,
    is_intersegment: bool,
    is_far: bool
}

impl Command {
    fn to_str(self) -> &'static str {
        use Command::*;
        match self {
            Mov => "mov",
            Add => "add",
            Adc => "adc",
            Sub => "sub",
            Sbb => "sbb",
            Cmp => "cmp",
            Inc => "inc",
            Dec => "dec",
            Neg => "neg",
            And => "and",
            Or => "or",
            Not => "not",
            Xor => "xor",
            Test => "test",
            Mul => "mul",
            Imul => "imul",
            Div => "div",
            IDiv => "idiv",
            Shl => "shl",
            Shr => "shr",
            Sar => "sar",
            Rol => "rol",
            Ror => "ror",
            Rcl => "rcl",
            Rcr => "rcr",
            Push => "push",
            Pop => "pop",
            Call => "call",
            Jmp => "jmp",
            Ret => "ret",
            //Retnz => "retnz",
            Je => "je",
            Jl => "jl",
            Jle => "jle",
            Jb => "jb",
            Jbe => "jbe",
            Jp => "jp",
            Jo => "jo",
            Js => "js",
            Jnz => "jnz",
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
            Int => "int",
            Int3 => "int3",
            Into => "into",
            Iret => "iret",
            Xchg => "xchg",
            Rep => "rep",
            Repnz => "repnz",
            In => "in",
            Out => "out",
            Xlat => "xlat",
            Lea => "lea",
            Lds => "lds",
            Les => "les",
            Lahf => "lahf",
            Sahf => "sahf",
            Pushf => "pushf",
            Popf => "popf",
            Aaa => "aaa",
            Aas => "aas",
            Daa => "daa",
            Das => "das",
            Aam => "aam",
            Aad => "aad",
            Cbw => "cbw",
            Cwd => "cwd",
            Clc => "clc",
            Cmc => "cmc",
            Stc => "stc",
            Cld => "cld",
            Std => "std",
            Cli => "cli",
            Sti => "sti",
            Hlt => "hlt",
            Wait => "wait",
            Lock => "lock",
            Retf => "retf",
            Nop => "nop"
        }
    }
}

impl Reg {
    fn to_str(self) -> &'static str
    {
        use Reg::*;
        match self {
            Al => "al",
            Ah => "ah",
            Ax => "ax",
            Bl => "bl",
            Bh => "bh",
            Bx => "bx",
            Cl => "cl",
            Ch => "ch",
            Cx => "cx",
            Dl => "dl",
            Dh => "dh",
            Dx => "dx",
            Sp => "sp",
            Bp => "bp",
            Si => "si",
            Di => "di",
        }
    }
}

impl AdrReg {
    fn to_str(self) -> &'static str
    {
        use AdrReg::*;
        match self {
            BxSi => "bx + si",
            BxDi => "bx + di",
            BpSi => "bp + si",
            BpDi => "bp + di",
            Si => "si",
            Di => "di",
            Bp => "bp",
            Bx => "bx",
        }
    }
}

impl SegReg {
    fn to_str(self) -> &'static str {
        use SegReg::*;
        match self {
            Cs => "cs",
            Ds => "ds",
            Es => "es",
            Ss => "ss"
        }
    }
}


impl RepeatOperand {
    fn to_str(self) -> &'static str
    {
        use RepeatOperand::*;
        match self {
            Movsb => "movsb",
            Movsw => "movsw",
            Cmpsb => "cmpsb",
            Cmpsw => "cmpsw",
            Scasb => "scasb",
            Scasw => "scasw",
            Lodsb => "lodsb",
            Lodsw => "lodsw",
            Stosb => "stosb",
            Stosw => "stosw"
        }
    }
}

impl Operand {

    fn maybe_print_label<T>(dst_label: Option<&String>, offset: T) -> String
    where T: Display
    {
        match dst_label {
            None => String::from(format!("#OFFSET# {}", offset)),
            Some(s) => s.clone()
        }
    }

    fn to_str(self, dst_label: Option<&String>) -> String
    {
        use Operand::*;
        match self {
            Reg(r) => String::from(r.to_str()),
            SegReg(sr) => String::from(sr.to_str()),
            ImmU8(v) => String::from(format!("{}", v)),
            ImmI8(v) => String::from(format!("{}", v)),
            ImmU16(v) => String::from(format!("{}", v)),
            ImmI16(v) => String::from(format!("{}", v)),
            PtrDir(v) => String::from(format!("[{}]", v)),
            PtrDisp((ar, d)) => {
                let rstr = ar.to_str();
                if d == 0 {
                    String::from(format!("[{}]", rstr))
                } else {
                    if d > 0 {
                        String::from(format!("[{} + {}]", rstr, d))
                    } else {
                        String::from(format!("[{} - {}]", rstr, -d))
                    }
                }
            },
            Offset8(offset) => {
                Self::maybe_print_label::<i8>(dst_label, offset)
            },
            Offset16(offset) => {
                Self::maybe_print_label::<i16>(dst_label, offset)
            },
            RepeatOperand(op) => {
                String::from(op.to_str())
            }

        }
    }
}

impl Instruction {
    fn no_op(cmd: Command) -> Self
    {
        Instruction {
            cmd: cmd,
            op1: None,
            op2: None,
            size: None,
            lock: false,
            segment: None,
            is_intersegment: false,
            is_far: false
        }
    }
    fn single_op(cmd: Command,
                 op: Operand) -> Self
    {
        let mut ret = Self::no_op(cmd);
        ret.op1 = Some(op);
        ret
    }

    fn src_dst(cmd: Command,
                src: Operand,
                dst: Operand) -> Self
    {
        let mut ret = Self::single_op(cmd, dst);
        ret.op2 = Some(src);
        ret
    }

    fn jumpi8(cmd: Command,
            offset: i8) -> Self
    {
        let mut ret = Self::no_op(cmd);
        ret.op1 = Some(Operand::Offset8(offset));
        ret
    }

    fn jumpi16(cmd: Command,
            offset: i16) -> Self
    {
        let mut ret = Self::no_op(cmd);
        ret.op1 = Some(Operand::Offset16(offset));
        ret
    }

    fn size(mut self, sz: DataSize) -> Self{
        self.size = Some(sz);
        self
    }

    fn lock(mut self) -> Self {
        self.lock = true;
        self
    }

    fn set_segment(mut self, sr: SegReg) -> Self {
        self.segment = Some(sr);
        self
    }

    fn set_intersegment(mut self) -> Self {
        self.is_intersegment = true;
        self
    }

    fn set_far(mut self) -> Self {
        self.is_far = true;
        self
    }

    fn offset(&self) -> Option<isize>
    {
        match self.op1
        {
            Some(o) => match o {
                Operand::Offset8(v) => Some(v as isize),
                Operand::Offset16(v) => Some(v as isize),
                _ => None
            },
            _ => None
        }
    }

    fn maybe_prepend_segment(op: Operand, dst_label: Option<&String>, segment: Option<SegReg>) -> String {
        use Operand::*;
        match segment {
            None => op.to_str(dst_label),
            Some(sr) => {
                match op {
                    PtrDir(_) |
                    PtrDisp(_) => {
                        String::from(format!("{}:{}",sr.to_str(),op.to_str(dst_label)))
                    },
                    _ => op.to_str(dst_label)
                }
            }
        }
    }

    fn to_str(&self, dst_label: Option<&String>) -> String {
        let mut ret = String::from("");

        if self.lock {
            ret.push_str("lock ")
        }

        ret.push_str(self.cmd.to_str());
        match self.size {
            None => {},
            Some(sz) => {
                match sz {
                    DataSize::Byte => {ret.push_str(" byte");},
                    DataSize::Word => {ret.push_str(" word");}
                }
            }
        };
        if self.is_far {
            ret.push_str(" far")
        }
        match self.op1 {
            Some(o1) => {
                ret.push_str(&String::from(format!(" {}", Self::maybe_prepend_segment(o1, dst_label, self.segment))));
                match self.op2 {
                    Some(o2) => {
                        if self.is_intersegment {
                            ret.push_str(&String::from(format!(":{}", o2.to_str(dst_label))));
                        } else {
                            ret.push_str(&String::from(format!(", {}", Self::maybe_prepend_segment(o2, dst_label, self.segment))));
                        }
                    },
                    None => {;}
                }
            },
            None => {;}
        }
        ret
    }
}

struct Machine {
    registers: [u16; 8]
}

impl Machine {
    fn new() -> Self {
        Machine { registers: [0; 8] }
    }

    fn get_reg_idx(r: Reg) -> usize {
        use Reg::*;
        match r {
            Al | Ah | Ax => 0,
            Bl | Bh | Bx => 1,
            Cl | Ch | Cx => 2,
            Dl | Dh | Dx => 3,
            Sp => 4,
            Bp => 5,
            Si => 6,
            Di => 7
        }
    }

    fn get_read_mask(r: Reg) -> u16 {
        use Reg::*;
        match r {
            Al | Bl | Cl | Dl => 0x00FF,
            Ah | Bh | Ch | Dh => 0xFF00,
            _ => 0xFFFF,
        }
    }

    fn get_read_shift(r: Reg) -> u8 {
        use Reg::*;
        match r {
            Ah | Bh | Ch | Dh => 8,
            _ => 0
        }
    }

    fn get_write_reg_mask(r: Reg) -> u16 {
        use Reg::*;
        match r {
            Ah | Bh | Ch | Dh => 0x00FF,
            Al | Bl | Cl | Dl => 0xFF00,
            _ => 0,
        }
    }

    fn get_write_value_mask(r: Reg) -> u16 {
        use Reg::*;
        match r {
            Ah | Al | Bh | Bl | Ch | Cl | Dh | Dl => 0x00FF,
            _ => 0xFFFF
        }
    }

    fn get_write_shift(r: Reg) -> u8 {
        use Reg::*;
        match r {
            Ah | Bh | Ch | Dh => 8,
            _ => 0
        }
    }

    fn reg_value(&self, r: Reg) -> u16 {
        (self.registers[Self::get_reg_idx(r)] & Self::get_read_mask(r)) >> Self::get_read_shift(r)
    }

    fn write_reg(&mut self, r: Reg, v: u16) {
        let idx = Self::get_reg_idx(r);
        self.registers[idx] = (self.registers[idx] & Self::get_write_reg_mask(r)) |
        ((v & Self::get_write_value_mask(r)) << Self::get_write_shift(r));
    }

    fn execute_mov_instruction(&mut self, instruction: &Instruction) {
        //assert!(instruction.cmd == Command::Mov);
        use Operand::*;
        let src_value = match instruction.op2 {
            Some(o) => {
                match o {
                    Reg(r) => { self.reg_value(r) },
                    ImmU8(v) => v as u16,
                    ImmI8(v) => v as u16,
                    ImmU16(v) => v as u16,
                    ImmI16(v) => v as u16,
                    _ => panic!("Invalid src for mov command: {:?}", o)
                }
            },
            None => panic!("No source indicated for mov command.")
        };
        match instruction.op1 {
            Some(o) => {
                match o {
                    Reg(r) => {self.write_reg(r, src_value);},
                    _ => panic!("Invalid dst for mov command: {:?}", o)
                }
            }
            None => panic!("No destination indicated for mov command.")
        }
    }

    fn execute_single_instruction(&mut self, instruction: &Instruction) {
        match instruction.cmd {
            Command::Mov => self.execute_mov_instruction(instruction),
            _ => {}
        }
    }

    fn execute(&mut self, instructions: &Vec<Instruction>) {
        for i in instructions {
            self.execute_single_instruction(&i)
        }
    }
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

const SR_MASK: u8 = 0x18;
const SR_SHFT: u8 = 3;

const IMMREG_MASK: u8 = 0x07;
const IMMREG_SHFT: u8 = 0;

const RM_MASK: u8 = 0x07;
const RM_SHFT: u8 = 0;

const SUB_CODE_MASK: u8 = REG_MASK;
const SUB_CODE_SHFT: u8 = REG_SHFT;


#[derive (Clone, Copy)]
struct OpcodeTableEntry {
    cmd: Command,
    opt: OpcodeParseType
}

const OPCODE_TABLE: [OpcodeTableEntry; 256] =
    [
        OpcodeTableEntry { cmd: Command::Add, opt: OpcodeParseType::RegRmWithDisp}, //0x00
        OpcodeTableEntry { cmd: Command::Add, opt: OpcodeParseType::RegRmWithDisp}, //0x01
        OpcodeTableEntry { cmd: Command::Add, opt: OpcodeParseType::RegRmWithDisp}, //0x02
        OpcodeTableEntry { cmd: Command::Add, opt: OpcodeParseType::RegRmWithDisp}, //0x03
        OpcodeTableEntry { cmd: Command::Add, opt: OpcodeParseType::ImmAcc}, //0x04
        OpcodeTableEntry { cmd: Command::Add, opt: OpcodeParseType::ImmAcc}, //0x05
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithSr}, //0x06
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithSr}, //0x07
        OpcodeTableEntry { cmd: Command::Or, opt: OpcodeParseType::RegRmWithDisp}, //0x08
        OpcodeTableEntry { cmd: Command::Or, opt: OpcodeParseType::RegRmWithDisp}, //0x09
        OpcodeTableEntry { cmd: Command::Or, opt: OpcodeParseType::RegRmWithDisp}, //0x0A
        OpcodeTableEntry { cmd: Command::Or, opt: OpcodeParseType::RegRmWithDisp}, //0x0B
        OpcodeTableEntry { cmd: Command::Or, opt: OpcodeParseType::ImmAcc}, //0x0C
        OpcodeTableEntry { cmd: Command::Or, opt: OpcodeParseType::ImmAcc}, //0x0D
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithSr}, //0x0E
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x0F
        OpcodeTableEntry { cmd: Command::Adc, opt: OpcodeParseType::RegRmWithDisp}, //0x10
        OpcodeTableEntry { cmd: Command::Adc, opt: OpcodeParseType::RegRmWithDisp}, //0x11
        OpcodeTableEntry { cmd: Command::Adc, opt: OpcodeParseType::RegRmWithDisp}, //0x12
        OpcodeTableEntry { cmd: Command::Adc, opt: OpcodeParseType::RegRmWithDisp}, //0x13
        OpcodeTableEntry { cmd: Command::Adc, opt: OpcodeParseType::ImmAcc}, //0x14
        OpcodeTableEntry { cmd: Command::Adc, opt: OpcodeParseType::ImmAcc}, //0x15
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithSr}, //0x16
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithSr}, //0x17
        OpcodeTableEntry { cmd: Command::Sbb, opt: OpcodeParseType::RegRmWithDisp}, //0x18
        OpcodeTableEntry { cmd: Command::Sbb, opt: OpcodeParseType::RegRmWithDisp}, //0x19
        OpcodeTableEntry { cmd: Command::Sbb, opt: OpcodeParseType::RegRmWithDisp}, //0x1A
        OpcodeTableEntry { cmd: Command::Sbb, opt: OpcodeParseType::RegRmWithDisp}, //0x1B
        OpcodeTableEntry { cmd: Command::Sbb, opt: OpcodeParseType::ImmAcc}, //0x1C
        OpcodeTableEntry { cmd: Command::Sbb, opt: OpcodeParseType::ImmAcc}, //0x1D
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithSr}, //0x1E
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithSr}, //0x1F
        OpcodeTableEntry { cmd: Command::And, opt: OpcodeParseType::RegRmWithDisp}, //0x20
        OpcodeTableEntry { cmd: Command::And, opt: OpcodeParseType::RegRmWithDisp}, //0x21
        OpcodeTableEntry { cmd: Command::And, opt: OpcodeParseType::RegRmWithDisp}, //0x22
        OpcodeTableEntry { cmd: Command::And, opt: OpcodeParseType::RegRmWithDisp}, //0x23
        OpcodeTableEntry { cmd: Command::And, opt: OpcodeParseType::ImmAcc}, //0x24
        OpcodeTableEntry { cmd: Command::And, opt: OpcodeParseType::ImmAcc}, //0x25
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Segment}, //0x26
        OpcodeTableEntry { cmd: Command::Daa, opt: OpcodeParseType::Direct}, //0x27
        OpcodeTableEntry { cmd: Command::Sub, opt: OpcodeParseType::RegRmWithDisp}, //0x28
        OpcodeTableEntry { cmd: Command::Sub, opt: OpcodeParseType::RegRmWithDisp}, //0x29
        OpcodeTableEntry { cmd: Command::Sub, opt: OpcodeParseType::RegRmWithDisp}, //0x2A
        OpcodeTableEntry { cmd: Command::Sub, opt: OpcodeParseType::RegRmWithDisp}, //0x2B
        OpcodeTableEntry { cmd: Command::Sub, opt: OpcodeParseType::ImmAcc}, //0x2C
        OpcodeTableEntry { cmd: Command::Sub, opt: OpcodeParseType::ImmAcc}, //0x2D
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Segment}, //0x2E
        OpcodeTableEntry { cmd: Command::Das, opt: OpcodeParseType::Direct}, //0x2F
        OpcodeTableEntry { cmd: Command::Xor, opt: OpcodeParseType::RegRmWithDisp}, //0x30
        OpcodeTableEntry { cmd: Command::Xor, opt: OpcodeParseType::RegRmWithDisp}, //0x31
        OpcodeTableEntry { cmd: Command::Xor, opt: OpcodeParseType::RegRmWithDisp}, //0x32
        OpcodeTableEntry { cmd: Command::Xor, opt: OpcodeParseType::RegRmWithDisp}, //0x33
        OpcodeTableEntry { cmd: Command::Xor, opt: OpcodeParseType::ImmAcc}, //0x34
        OpcodeTableEntry { cmd: Command::Xor, opt: OpcodeParseType::ImmAcc}, //0x35
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Segment}, //0x36
        OpcodeTableEntry { cmd: Command::Aaa, opt: OpcodeParseType::Direct}, //0x37
        OpcodeTableEntry { cmd: Command::Cmp, opt: OpcodeParseType::RegRmWithDisp}, //0x38
        OpcodeTableEntry { cmd: Command::Cmp, opt: OpcodeParseType::RegRmWithDisp}, //0x39
        OpcodeTableEntry { cmd: Command::Cmp, opt: OpcodeParseType::RegRmWithDisp}, //0x3A
        OpcodeTableEntry { cmd: Command::Cmp, opt: OpcodeParseType::RegRmWithDisp}, //0x3B
        OpcodeTableEntry { cmd: Command::Cmp, opt: OpcodeParseType::ImmAcc}, //0x3C
        OpcodeTableEntry { cmd: Command::Cmp, opt: OpcodeParseType::ImmAcc}, //0x3D
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Segment}, //0x3E
        OpcodeTableEntry { cmd: Command::Aas, opt: OpcodeParseType::Direct}, //0x3F
        OpcodeTableEntry { cmd: Command::Inc, opt: OpcodeParseType::SingleByteWithReg}, //0x40
        OpcodeTableEntry { cmd: Command::Inc, opt: OpcodeParseType::SingleByteWithReg}, //0x41
        OpcodeTableEntry { cmd: Command::Inc, opt: OpcodeParseType::SingleByteWithReg}, //0x42
        OpcodeTableEntry { cmd: Command::Inc, opt: OpcodeParseType::SingleByteWithReg}, //0x43
        OpcodeTableEntry { cmd: Command::Inc, opt: OpcodeParseType::SingleByteWithReg}, //0x44
        OpcodeTableEntry { cmd: Command::Inc, opt: OpcodeParseType::SingleByteWithReg}, //0x45
        OpcodeTableEntry { cmd: Command::Inc, opt: OpcodeParseType::SingleByteWithReg}, //0x46
        OpcodeTableEntry { cmd: Command::Inc, opt: OpcodeParseType::SingleByteWithReg}, //0x47
        OpcodeTableEntry { cmd: Command::Dec, opt: OpcodeParseType::SingleByteWithReg}, //0x48
        OpcodeTableEntry { cmd: Command::Dec, opt: OpcodeParseType::SingleByteWithReg}, //0x49
        OpcodeTableEntry { cmd: Command::Dec, opt: OpcodeParseType::SingleByteWithReg}, //0x4A
        OpcodeTableEntry { cmd: Command::Dec, opt: OpcodeParseType::SingleByteWithReg}, //0x4B
        OpcodeTableEntry { cmd: Command::Dec, opt: OpcodeParseType::SingleByteWithReg}, //0x4C
        OpcodeTableEntry { cmd: Command::Dec, opt: OpcodeParseType::SingleByteWithReg}, //0x4D
        OpcodeTableEntry { cmd: Command::Dec, opt: OpcodeParseType::SingleByteWithReg}, //0x4E
        OpcodeTableEntry { cmd: Command::Dec, opt: OpcodeParseType::SingleByteWithReg}, //0x4F
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithReg}, //0x50
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithReg}, //0x51
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithReg}, //0x52
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithReg}, //0x53
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithReg}, //0x54
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithReg}, //0x55
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithReg}, //0x56
        OpcodeTableEntry { cmd: Command::Push, opt: OpcodeParseType::SingleByteWithReg}, //0x57
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithReg}, //0x58
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithReg}, //0x59
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithReg}, //0x5A
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithReg}, //0x5B
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithReg}, //0x5C
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithReg}, //0x5D
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithReg}, //0x5E
        OpcodeTableEntry { cmd: Command::Pop, opt: OpcodeParseType::SingleByteWithReg}, //0x5F
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x60
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x61
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x62
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x63
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x64
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x65
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x66
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x67
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x68
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x69
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x6A
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x6B
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x6C
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x6D
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x6E
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0x6F
        OpcodeTableEntry { cmd: Command::Jo, opt: OpcodeParseType::JumpI8}, //0x70
        OpcodeTableEntry { cmd: Command::Jno, opt: OpcodeParseType::JumpI8}, //0x71
        OpcodeTableEntry { cmd: Command::Jb, opt: OpcodeParseType::JumpI8}, //0x72
        OpcodeTableEntry { cmd: Command::Jnb, opt: OpcodeParseType::JumpI8}, //0x73
        OpcodeTableEntry { cmd: Command::Je, opt: OpcodeParseType::JumpI8}, //0x74
        OpcodeTableEntry { cmd: Command::Jnz, opt: OpcodeParseType::JumpI8}, //0x75
        OpcodeTableEntry { cmd: Command::Jbe, opt: OpcodeParseType::JumpI8}, //0x76
        OpcodeTableEntry { cmd: Command::Ja, opt: OpcodeParseType::JumpI8}, //0x77
        OpcodeTableEntry { cmd: Command::Js, opt: OpcodeParseType::JumpI8}, //0x78
        OpcodeTableEntry { cmd: Command::Jns, opt: OpcodeParseType::JumpI8}, //0x79
        OpcodeTableEntry { cmd: Command::Jp, opt: OpcodeParseType::JumpI8}, //0x7A
        OpcodeTableEntry { cmd: Command::Jnp, opt: OpcodeParseType::JumpI8}, //0x7B
        OpcodeTableEntry { cmd: Command::Jl, opt: OpcodeParseType::JumpI8}, //0x7C
        OpcodeTableEntry { cmd: Command::Jnl, opt: OpcodeParseType::JumpI8}, //0x7D
        OpcodeTableEntry { cmd: Command::Jle, opt: OpcodeParseType::JumpI8}, //0x7E
        OpcodeTableEntry { cmd: Command::Jg, opt: OpcodeParseType::JumpI8}, //0x7F
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::ImmRm}, //0x80
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::ImmRm}, //0x81
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::ImmRm}, //0x82
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::ImmRm}, //0x83
        OpcodeTableEntry { cmd: Command::Test, opt: OpcodeParseType::RegRmWithDisp}, //0x84
        OpcodeTableEntry { cmd: Command::Test, opt: OpcodeParseType::RegRmWithDisp}, //0x85
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::RegRmWithDispD1}, //0x86
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::RegRmWithDispD1}, //0x87
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::RegRmWithDisp}, //0x88
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::RegRmWithDisp}, //0x89
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::RegRmWithDisp}, //0x8A
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::RegRmWithDisp}, //0x8B
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::MovWithSr}, //0x8C
        OpcodeTableEntry { cmd: Command::Lea, opt: OpcodeParseType::RegRmWithDispD1}, //0x8D
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::MovWithSr}, //0x8E
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::RmWithDisp}, //0x8F
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::SingleByteWithReg}, //0x90
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::SingleByteWithReg}, //0x91
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::SingleByteWithReg}, //0x92
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::SingleByteWithReg}, //0x93
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::SingleByteWithReg}, //0x94
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::SingleByteWithReg}, //0x95
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::SingleByteWithReg}, //0x96
        OpcodeTableEntry { cmd: Command::Xchg, opt: OpcodeParseType::SingleByteWithReg}, //0x97
        OpcodeTableEntry { cmd: Command::Cbw, opt: OpcodeParseType::Direct}, //0x98
        OpcodeTableEntry { cmd: Command::Cwd, opt: OpcodeParseType::Direct}, //0x99
        OpcodeTableEntry { cmd: Command::Call, opt: OpcodeParseType::Intersegment}, //0x9A
        OpcodeTableEntry { cmd: Command::Wait, opt: OpcodeParseType::Direct}, //0x9B
        OpcodeTableEntry { cmd: Command::Pushf, opt: OpcodeParseType::Direct}, //0x9C
        OpcodeTableEntry { cmd: Command::Popf, opt: OpcodeParseType::Direct}, //0x9D
        OpcodeTableEntry { cmd: Command::Sahf, opt: OpcodeParseType::Direct}, //0x9E
        OpcodeTableEntry { cmd: Command::Lahf, opt: OpcodeParseType::Direct}, //0x9F
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xA0
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::AccMem}, //0xA1
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::AccMem}, //0xA2
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::AccMem}, //0xA3
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::AccMem}, //0xA4
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xA5
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xA6
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xA7
        OpcodeTableEntry { cmd: Command::Test, opt: OpcodeParseType::ImmAcc}, //0xA8
        OpcodeTableEntry { cmd: Command::Test, opt: OpcodeParseType::ImmAcc}, //0xA9
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xAA
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xAB
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xAC
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xAD
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xAE
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xAF
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB0
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB1
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB2
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB3
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB4
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB5
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB6
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB7
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB8
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xB9
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xBA
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xBB
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xBC
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xBD
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xBE
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmReg}, //0xBF
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xC0
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xC1
        OpcodeTableEntry { cmd: Command::Ret, opt: OpcodeParseType::ImmI16}, //0xC2
        OpcodeTableEntry { cmd: Command::Ret, opt: OpcodeParseType::Direct}, //0xC3
        OpcodeTableEntry { cmd: Command::Les, opt: OpcodeParseType::RegRmWithDispD1W1}, //0xC4
        OpcodeTableEntry { cmd: Command::Lds, opt: OpcodeParseType::RegRmWithDispD1}, //0xC5
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmRm}, //0xC6
        OpcodeTableEntry { cmd: Command::Mov, opt: OpcodeParseType::ImmRm}, //0xC7
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xC8
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xC9
        OpcodeTableEntry { cmd: Command::Retf, opt: OpcodeParseType::ImmI16}, //0xCA
        OpcodeTableEntry { cmd: Command::Retf, opt: OpcodeParseType::Direct}, //0xCB
        OpcodeTableEntry { cmd: Command::Int3, opt: OpcodeParseType::Direct}, //0xCC
        OpcodeTableEntry { cmd: Command::Int, opt: OpcodeParseType::ImmU8}, //0xCD
        OpcodeTableEntry { cmd: Command::Into, opt: OpcodeParseType::Direct}, //0xCE
        OpcodeTableEntry { cmd: Command::Iret, opt: OpcodeParseType::Direct}, //0xCF
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::ShiftRot}, //0xD0
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::ShiftRot}, //0xD1
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::ShiftRot}, //0xD2
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::ShiftRot}, //0xD3
        OpcodeTableEntry { cmd: Command::Aam, opt: OpcodeParseType::AsciiAdjust}, //0xD4
        OpcodeTableEntry { cmd: Command::Aad, opt: OpcodeParseType::AsciiAdjust}, //0xD5
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xD6
        OpcodeTableEntry { cmd: Command::Xlat, opt: OpcodeParseType::Direct}, //0xD7
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xD8
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xD9
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xDA
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xDB
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xDC
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xDD
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xDE
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xDF
        OpcodeTableEntry { cmd: Command::Loopnz, opt: OpcodeParseType::JumpI8}, //0xE0
        OpcodeTableEntry { cmd: Command::Loopz, opt: OpcodeParseType::JumpI8}, //0xE1
        OpcodeTableEntry { cmd: Command::Loop, opt: OpcodeParseType::JumpI8}, //0xE2
        OpcodeTableEntry { cmd: Command::Jcxz, opt: OpcodeParseType::JumpI8}, //0xE3
        OpcodeTableEntry { cmd: Command::In, opt: OpcodeParseType::InOut}, //0xE4
        OpcodeTableEntry { cmd: Command::In, opt: OpcodeParseType::InOut}, //0xE5
        OpcodeTableEntry { cmd: Command::Out, opt: OpcodeParseType::InOut}, //0xE6
        OpcodeTableEntry { cmd: Command::Out, opt: OpcodeParseType::InOut}, //0xE7
        OpcodeTableEntry { cmd: Command::Call, opt: OpcodeParseType::JumpI16}, //0xE8
        OpcodeTableEntry { cmd: Command::Jmp, opt: OpcodeParseType::JumpI16}, //0xE9
        OpcodeTableEntry { cmd: Command::Jmp, opt: OpcodeParseType::Intersegment}, //0xEA
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xEB
        OpcodeTableEntry { cmd: Command::In, opt: OpcodeParseType::InOut}, //0xEC
        OpcodeTableEntry { cmd: Command::In, opt: OpcodeParseType::InOut}, //0xED
        OpcodeTableEntry { cmd: Command::Out, opt: OpcodeParseType::InOut}, //0xEE
        OpcodeTableEntry { cmd: Command::Out, opt: OpcodeParseType::InOut}, //0xEF
        OpcodeTableEntry { cmd: Command::Lock, opt: OpcodeParseType::Lock}, //0xF0
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::Nop}, //0xF1
        OpcodeTableEntry { cmd: Command::Repnz, opt: OpcodeParseType::Repeat}, //0xF2
        OpcodeTableEntry { cmd: Command::Rep, opt: OpcodeParseType::Repeat}, //0xF3
        OpcodeTableEntry { cmd: Command::Hlt, opt: OpcodeParseType::Direct}, //0xF4
        OpcodeTableEntry { cmd: Command::Cmc, opt: OpcodeParseType::Direct}, //0xF5
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::RmWithDisp}, //0xF6
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::RmWithDisp}, //0xF7
        OpcodeTableEntry { cmd: Command::Clc, opt: OpcodeParseType::Direct}, //0xF8
        OpcodeTableEntry { cmd: Command::Stc, opt: OpcodeParseType::Direct}, //0xF9
        OpcodeTableEntry { cmd: Command::Cli, opt: OpcodeParseType::Direct}, //0xFA
        OpcodeTableEntry { cmd: Command::Sti, opt: OpcodeParseType::Direct}, //0xFB
        OpcodeTableEntry { cmd: Command::Cld, opt: OpcodeParseType::Direct}, //0xFC
        OpcodeTableEntry { cmd: Command::Std, opt: OpcodeParseType::Direct}, //0xFD
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::RmWithDisp}, //0xFE
        OpcodeTableEntry { cmd: Command::Nop, opt: OpcodeParseType::RmWithDisp}, //0xFF
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

fn read_i16_val(data: &[u8]) -> i16
{
    data [0] as i16 | ((data[1] as i16) << 8)
}

fn read_u16_val(data: &[u8]) -> u16
{
    data [0] as u16 | ((data[1] as u16) << 8)
}

fn get_seg_reg_operand(code: TwoBitValue) -> Operand
{
    use TwoBitValue::*;
    use SegReg::*;
    Operand::SegReg(match code {
        DBV00 => Es,
        DBV01 => Cs,
        DBV10 => Ss,
        DBV11 => Ds
    })
}

fn get_reg_operand(flag: BitValue,
                   code: ThreeBitValue) -> Operand
{
    use Reg::*;
    const REGS_BYTE: [Reg; 8] = [Al, Cl, Dl, Bl, Ah, Ch, Dh, Bh];
    const REGS_WORD: [Reg; 8] = [Ax, Cx, Dx, Bx, Sp, Bp, Si, Di];

    Operand::Reg(
        match flag {
            BitValue::BV0 => REGS_BYTE[code as usize],
            BitValue::BV1 => REGS_WORD[code as usize]
        }
    )
}

fn repeat_operand_from_byte(byte: u8) -> Result<Operand, String>
{
    use RepeatOperand::*;
    let op = Operand::RepeatOperand(
        match byte {
            0xA4 => Movsb,
            0xA5 => Movsw,
            0xA6 => Cmpsb,
            0xA7 => Cmpsw,
            0xAA => Stosb,
            0xAB => Stosw,
            0xAC => Lodsb,
            0xAD => Lodsw,
            0xAE => Scasb,
            0xAF => Scasw,
            _ => { return Err(String::from(format!("Unknown repeat operand byte: {}", byte))); }
        });
    Ok(op)

}

fn get_mem_ptr_and_displacement(data: &[u8],
                                rm_code: ThreeBitValue,
                                mod_code: TwoBitValue) -> (usize, Operand)
{
    use TwoBitValue::*;
    use ThreeBitValue::*;
    let mut data_offset: usize = 0;

    if (mod_code == DBV00) && (rm_code == TBV110) {
        let disp = read_u16_val(&data[2..4]);
        (2, Operand::PtrDir(disp))
    } else {
        let ar = match rm_code {
            TBV000 => AdrReg::BxSi,
            TBV001 => AdrReg::BxDi,
            TBV010 => AdrReg::BpSi,
            TBV011 => AdrReg::BpDi,
            TBV100 => AdrReg::Si,
            TBV101 => AdrReg::Di,
            TBV110 => AdrReg::Bp,
            TBV111 => AdrReg::Bx
        };
        let displacement: i16  =
            match mod_code {
                DBV00 => 0,
                DBV01 => {
                    data_offset = 1;
                    (data[2] as i8) as i16
                },
                DBV10 => {
                    data_offset = 2;
                    read_i16_val(&data[2..4])
                },
                DBV11 => {
                    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
                    return (0, get_reg_operand(w_flag, rm_code));
                }
            };
        (data_offset, Operand::PtrDisp((ar, displacement)))
    }
}

fn decode_imm_reg_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    let immw_flag = get_bit_value((data[0] & IMMW_MASK) >> IMMW_SHFT);
    let immreg_code = get_three_bit_value((data[0] & IMMREG_MASK) >> IMMREG_SHFT);
    let immreg_operand = get_reg_operand(immw_flag, immreg_code);

    let (offset, src) = match immw_flag {
        BitValue::BV0 => (2, Operand::ImmI8(data[1] as i8)),
        BitValue::BV1 => (3, Operand::ImmI16(read_i16_val(&data[1..3])))
    };
    Ok((offset, Instruction::src_dst(cmd, src, immreg_operand)))
}

fn decode_imm_rm_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    use BitValue::*;
    use TwoBitValue::*;
    use ThreeBitValue::*;
    use Command::*;
    let (use_s_flag, sub_code) = if (data[0] & 0xFC) == 0x80 {
        let sub_opcode = get_three_bit_value((data[1] & SUB_CODE_MASK) >> SUB_CODE_SHFT);
        match sub_opcode {
            TBV000 => (true, Add),
            TBV001 => (false, Or),
            TBV010 => (true, Adc),
            TBV011 => (true, Sbb),
            TBV100 => (false, And),
            TBV101 => (true, Sub),
            TBV110 => (false, Xor),
            TBV111 => (true, Cmp),
        }
    } else {
        (false, cmd)
    };
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);

    if mod_code != DBV11 {
        let (data_offset, reg_operand) = get_mem_ptr_and_displacement(data, rm_code, mod_code);
        let data_idx: usize = if (mod_code == DBV10) || ((mod_code == DBV00) & (rm_code == TBV110)) {
            4
        } else if mod_code == DBV01 {
            3
        } else {
            2
        };

        if (data[0] & 0xFE) == 0xC6 {
            return match w_flag {
                BV0 =>  Ok((data_offset + 3, Instruction::src_dst(sub_code, Operand::ImmU8(data[data_idx]), reg_operand).size(DataSize::Byte))),
                BV1 =>  Ok((data_offset + 4, Instruction::src_dst(sub_code, Operand::ImmU16(read_u16_val(&data[data_idx..data_idx+2])),
                                                                reg_operand).size(DataSize::Word)))
            }
        } else {
            return match w_flag {
                BV0 =>  Ok((data_offset + 3,
                            Instruction::src_dst(sub_code, Operand::ImmU8(data[data_idx]), reg_operand).size(DataSize::Byte))),
                BV1 =>  {
                    if use_s_flag {
                        let s_flag = (data[0] & S_MASK) >> S_SHFT;
                        if s_flag == 0 {
                            Ok((data_offset + 4,
                                Instruction::src_dst(sub_code, Operand::ImmU16(read_u16_val(&data[data_idx..data_idx+2])), reg_operand).size(DataSize::Word)))
                        } else {
                            Ok((data_offset + 3,
                                Instruction::src_dst(sub_code, Operand::ImmU8(data[data_idx]), reg_operand).size(DataSize::Word)))
                        }
                    } else {
                        Ok((data_offset + 4,
                            Instruction::src_dst(sub_code, Operand::ImmU16(read_u16_val(&data[data_idx..data_idx+2])), reg_operand).size(DataSize::Word)))
                    }
                }
            }
        }
    } else {
        let rm_operand = get_reg_operand(w_flag, rm_code);
        let data_idx: usize = 2;
        if cmd == Mov {
            Ok((3, Instruction::src_dst(cmd,
                                        Operand::ImmU8(data[data_idx]),
                                        rm_operand)))
        } else {
            let sw_flag = get_two_bit_value(data[0] & 0x3);
            match sw_flag {
                DBV01 => Ok((4, Instruction::src_dst(sub_code,
                                                     Operand::ImmU16(read_u16_val(&data[data_idx..data_idx+2])),
                                                     rm_operand))),
                _ => Ok((3, Instruction::src_dst(sub_code, Operand::ImmU8(data[data_idx]), rm_operand)))
            }
        }
    }
}

fn decode_acc_mem_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    use BitValue::*;
    use ThreeBitValue::*;

    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_operand = get_reg_operand(w_flag, TBV000);
    let val = read_u16_val(&data[1..2+w_flag as usize]);
    let d_flag = get_bit_value((data[0] & D_MASK) >> D_SHFT);
    match d_flag {
        BV0 => Ok((3, Instruction::src_dst(cmd, Operand::PtrDir(val), reg_operand))),
        BV1 => Ok((3, Instruction::src_dst(cmd, reg_operand, Operand::PtrDir(val))))
    }
}

fn decode_imm_acc_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    use BitValue::*;
    use ThreeBitValue::*;

    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let reg_operand = get_reg_operand(w_flag, TBV000);
    match w_flag {
        BV0 => Ok((2, Instruction::src_dst(cmd, Operand::ImmI8(data[1] as i8), reg_operand))),
        BV1 => {
            let val = read_i16_val(&data[1..2+w_flag as usize]);
            Ok((3, Instruction::src_dst(cmd, Operand::ImmI16(val), reg_operand)))
        }
    }
}

fn decode_reg_rm_strings_with_indicated_dw(cmd: Command,
                                          data: &[u8],
                                          w_flag: BitValue,
                                          d_flag: BitValue) -> Result<(usize, Instruction), String>
{
    use BitValue::*;
    use TwoBitValue::*;

    let reg_code = get_three_bit_value((data[1] & REG_MASK) >> REG_SHFT);
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let reg_operand = get_reg_operand(w_flag, reg_code);
    let mut offset: usize = 2;
    let rm_operand = match mod_code {
        DBV11 => get_reg_operand(w_flag, rm_code),
        _ => {
            let t = get_mem_ptr_and_displacement(data, rm_code, mod_code);
            offset += t.0;
            t.1
        }
    };

    let (src, dst) = if d_flag == BV0 {
        (reg_operand, rm_operand)
    } else {
        (rm_operand, reg_operand)
    };

    Ok((offset, Instruction::src_dst(cmd, src, dst)))
}

fn decode_reg_rm_with_disp_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let d_flag = get_bit_value((data[0] & D_MASK) >> D_SHFT);
    decode_reg_rm_strings_with_indicated_dw(cmd, data, w_flag, d_flag)
}

fn decode_reg_rm_with_disp_instruction_d1(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    decode_reg_rm_strings_with_indicated_dw(cmd, data, w_flag, BitValue::BV1)
}

fn decode_reg_rm_with_disp_instruction_d1w1(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    decode_reg_rm_strings_with_indicated_dw(cmd, data, BitValue::BV1, BitValue::BV1)
}

fn decode_mov_with_sr_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    let sr_code = get_two_bit_value((data[1] & SR_MASK) >> SR_SHFT);
    let seg_reg_operand = get_seg_reg_operand(sr_code);

    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let t = get_mem_ptr_and_displacement(data, rm_code, mod_code);

    Ok((t.0 + 2, if (data[0] & 0x2) == 0x2 {
        Instruction::src_dst(cmd, t.1, seg_reg_operand)
    } else {
        Instruction::src_dst(cmd, seg_reg_operand, t.1)
    }))
}

fn decode_jmp_i8_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    let jump_offset: i8 = data[1] as i8;
    Ok((2, Instruction::jumpi8(cmd, jump_offset)))
}

fn decode_jmp_i16_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    let jump_offset: i16 = read_i16_val(&data[1..3]);
    Ok((3, Instruction::jumpi16(cmd, jump_offset)))
}

fn decode_shift_rot_instruction(_cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    use BitValue::*;
    use TwoBitValue::*;
    use ThreeBitValue::*;
    use Command::*;
    let sub_code = (data[1] & SUB_CODE_MASK) >> SUB_CODE_SHFT;
    let sub_cmd = match get_three_bit_value(sub_code) {
        TBV000 => Rol,
        TBV001 => Ror,
        TBV010 => Rcl,
        TBV011 => Rcr,
        TBV100 => Shl,
        TBV101 => Shr,
        TBV111 => Sar,
        _ => {return Err(String::from(format!("Unsupported subcode {:x}", sub_code)));}
    };
    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
    let v_flag = get_bit_value((data[0] & V_MASK) >> V_SHFT);
    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let t = get_mem_ptr_and_displacement(data, rm_code, mod_code);

    Ok(match mod_code {
        DBV11 => {
            match v_flag {
                BV0 => (2 + t.0, Instruction::src_dst(sub_cmd, Operand::ImmU8(1), t.1)),
                BV1 => (2 + t.0, Instruction::src_dst(sub_cmd, Operand::Reg(Reg::Cl), t.1))
            }
        },
        _ => {
            let size = match w_flag { BV0 => DataSize::Byte, BV1 => DataSize::Word};
            match v_flag {
                BV0 => (2 + t.0, Instruction::src_dst(sub_cmd, Operand::ImmU8(1), t.1).size(size)),
                BV1 => (2 + t.0, Instruction::src_dst(sub_cmd, Operand::Reg(Reg::Cl), t.1).size(size))
            }
        }
    })
}

fn decode_rm_with_disp_instruction(_cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    use Command::*;
    use BitValue::*;
    use TwoBitValue::*;
    use ThreeBitValue::*;
    let (use_w_flag, use_far, sub_cmd) = {
        match get_three_bit_value((data[1] & SUB_CODE_MASK) >> SUB_CODE_SHFT) {
            TBV000 => {
                if data[0] == 0x8F {
                    (true, false, Pop)
                } else if (data[0] &  0xFE) == 0xFE {
                    (true, false, Inc)
                } else if (data[0] & 0xFE) == 0xF6 {
                    return decode_imm_rm_instruction(Test, data);
                } else {
                    return Err(String::from(format!("unknown opcode/subcode {:x}", data[0])));
                }
            },
            TBV001 => (true, false, Dec),
            TBV010 => {if data[0] == 0xFF { (false, false, Call) } else { (true, false, Not)}},
            TBV011 => {if data[0] == 0xFF { (false, true, Call) } else { (true, false, Neg) }},
            TBV100 => {if data[0] == 0xFF { (false, false, Jmp) } else { (true, false, Mul) }},
            TBV101 => {if data[0] == 0xFF { (false, true, Jmp) } else { (true, false, Imul) }},
            TBV110 => {
                if data[0] == 0xFF {
                    (true, false, Push)
                } else if (data[0] & 0xFE) == 0xF6 {
                    (true, false, Div)
                } else {
                    return Err(String::from(format!("unknown opcode/subcode {:x}", data[0])));
                }
            }
            TBV111 => (true, false, IDiv),
        }
    };

    let mod_code = get_two_bit_value((data[1] & MOD_MASK) >> MOD_SHFT);
    let rm_code = get_three_bit_value((data[1] & RM_MASK) >> RM_SHFT);
    let t = get_mem_ptr_and_displacement(data, rm_code, mod_code);

    Ok(if use_w_flag && (mod_code != DBV11) {
        let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);
        let size = match w_flag {
            BV0 => DataSize::Byte,
            BV1 => DataSize::Word
        };
        (2 + t.0, Instruction::single_op(sub_cmd, t.1).size(size))
    } else {
        if use_far {
            (2 + t.0, Instruction::single_op(sub_cmd, t.1).set_far())
        } else {
            (2 + t.0, Instruction::single_op(sub_cmd, t.1))
        }
    })
}

fn decode_single_byte_instruction_with_sr(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    let reg_operand = get_seg_reg_operand(get_two_bit_value((data[0] & 0x18) >> 3));
    Ok((1, Instruction::single_op(cmd, reg_operand)))
}

fn decode_single_byte_instruction_with_reg(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    let reg_operand = get_reg_operand(BitValue::BV1, get_three_bit_value(data[0] & 0x7));
    if cmd == Command::Xchg {
        Ok((1, Instruction::src_dst(cmd, Operand::Reg(Reg::Ax), reg_operand)))
    } else {
        Ok((1, Instruction::single_op(cmd, reg_operand)))
    }
}

fn decode_inout_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    use BitValue::*;
    use Reg::*;
    use Command::*;

    let w_flag = get_bit_value((data[0] & W_MASK) >> W_SHFT);

    let operand1 = match w_flag { BV0 => Operand::Reg(Al), BV1 => Operand::Reg(Ax) };
    let (offset, operand2) = if (data[0] & 0x08) == 0x08 {
        (1, Operand::Reg(Dx))
    } else {
        (2, Operand::ImmU8(data[1]))
    };
    Ok(if cmd == In {
        (offset, Instruction::src_dst(cmd, operand2, operand1))
    } else {
        (offset, Instruction::src_dst(cmd, operand1, operand2))
    })
}

fn decode_ascii_adjust_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    if data[1] != 0x0A {
        Err(String::from("Invalid second byte for ascii adjust for mul/div instruction"))
    } else {
        Ok((2, Instruction::no_op(cmd)))
    }
}

fn decode_repeat_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    match repeat_operand_from_byte(data[1]) {
        Ok(op) => Ok((2, Instruction::single_op(cmd, op))),
        Err(e) => Err(e)
    }
}

fn decode_instruction_with_imm_u8(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    Ok((2, Instruction::single_op(cmd, Operand::ImmU8(data[1]))))
}
// fn decode_instruction_with_imm_i8(cmd: Command, data: &[u8]) -> Result<usize, Instruction), String> {
//     (2, Instruction::single_op(cmd, Operand::ImmI8(data[1] as i8)))
// }

// fn decode_instruction_with_imm_u16(cmd: Command, data: &[u8]) -> Result<usize, Instruction), String> {
//     (3, Instruction::single_op(cmd, Operand::ImmU16(read_u16_val(&data[1..3]))))
// }

fn decode_instruction_with_imm_i16(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    Ok((3, Instruction::single_op(cmd, Operand::ImmI16(read_i16_val(&data[1..3])))))
}

fn decode_direct_instruction(cmd: Command, _data: &[u8]) -> Result<(usize, Instruction), String> {
    Ok((1, Instruction::no_op(cmd)))
}

fn decode_intersegment_instruction(cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    Ok((5, Instruction::src_dst(cmd,
                                Operand::ImmU16(read_u16_val(&data[1..3])),
                                Operand::ImmU16(read_u16_val(&data[3..5])))
        .set_intersegment()))
}

fn decode_lock_instruction(_cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    match decode_instruction(&data[1..]) {
        Ok((sub_len, sub_inst)) => Ok((sub_len + 1, sub_inst.lock())),
        Err(e) => Err(e)
    }
}

fn decode_segment_instruction(_cmd: Command, data: &[u8]) -> Result<(usize, Instruction), String> {
    use SegReg::*;
    let sr = match data[0] {
        0x26 => Es,
        0x2E => Cs,
        0x36 => Ss,
        0x3E => Ds,
        _ => { return Err(String::from(format!("Invalid segment prefix: {:x}", data[0]))); }
    };
    match decode_instruction(&data[1..]) {
        Ok ((sub_len, sub_inst)) => Ok((sub_len + 1, sub_inst.set_segment(sr))),
        Err(e) => Err(e)
    }
}

fn decode_instruction(data: &[u8]) -> Result<(usize, Instruction), String>
{
    let opcode = get_opcode_info(data[0]);
    match opcode.opt {
        OpcodeParseType::RegRmWithDisp => decode_reg_rm_with_disp_instruction(opcode.cmd, data),
        OpcodeParseType::RegRmWithDispD1 => decode_reg_rm_with_disp_instruction_d1(opcode.cmd, data),
        OpcodeParseType::RegRmWithDispD1W1 => decode_reg_rm_with_disp_instruction_d1w1(opcode.cmd, data),
        OpcodeParseType::ImmReg => decode_imm_reg_instruction(opcode.cmd, data),
        OpcodeParseType::ImmRm => decode_imm_rm_instruction(opcode.cmd, data),
        OpcodeParseType::AccMem =>  decode_acc_mem_instruction(opcode.cmd, data),
        OpcodeParseType::ImmAcc =>  decode_imm_acc_instruction(opcode.cmd, data),
        OpcodeParseType::MovWithSr => decode_mov_with_sr_instruction(opcode.cmd, data),
        OpcodeParseType::RmWithDisp => decode_rm_with_disp_instruction(opcode.cmd, data),
        OpcodeParseType::ShiftRot => decode_shift_rot_instruction(opcode.cmd, data),
        OpcodeParseType::JumpI8 => decode_jmp_i8_instruction(opcode.cmd, data),
        OpcodeParseType::JumpI16 => decode_jmp_i16_instruction(opcode.cmd, data),
        OpcodeParseType::SingleByteWithSr  => decode_single_byte_instruction_with_sr(opcode.cmd, data),
        OpcodeParseType::SingleByteWithReg => decode_single_byte_instruction_with_reg(opcode.cmd, data),
        OpcodeParseType::InOut => decode_inout_instruction(opcode.cmd, data),
        OpcodeParseType::AsciiAdjust => decode_ascii_adjust_instruction(opcode.cmd, data),
        OpcodeParseType::Repeat => decode_repeat_instruction(opcode.cmd, data),
        OpcodeParseType::ImmU8 => decode_instruction_with_imm_u8(opcode.cmd, data),
        // OpcodeParseType::ImmI8 => decode_instruction_with_imm_i8(opcode.cmd, data),
        // OpcodeParseType::ImmU16 => decode_instruction_with_imm_u16(opcode.cmd, data),
        OpcodeParseType::ImmI16 => decode_instruction_with_imm_i16(opcode.cmd, data),
        OpcodeParseType::Intersegment => decode_intersegment_instruction(opcode.cmd, data),
        OpcodeParseType::Lock => decode_lock_instruction(opcode.cmd, data),
        OpcodeParseType::Segment => decode_segment_instruction(opcode.cmd, data),
        OpcodeParseType::Direct => decode_direct_instruction(opcode.cmd, data),
        OpcodeParseType::Nop => Err(String::from(format!("Invalid opcode 0x{:x}",data[0])))
    }
}

fn decode_to_text(data: &[u8]) -> Result<(usize, String), String> {
    match decode_instruction(data) {
        Ok((offset, inst)) => Ok((offset, inst.to_str(None))),
        Err(e) => Err(e)
    }
}

fn decode_from_file(filepath: &String) -> Result<(String, Vec<Instruction>), String> {
    match read(filepath) {
        Err(e) => { return Err(String::from(format!("Failure reading {}: {}", filepath, e)));}
        Ok(data) => decode_from_data(&data)
    }
}

fn decode_from_data(data: &[u8]) -> Result<(String, Vec<Instruction>), String> {
    let n = data.len();
    let mut i = 0;
    let mut instructions: Vec<(Instruction, usize, Option<usize>)> = Vec::new();
    let mut labels: BTreeSet<usize> = BTreeSet::new();
    let mut addresses: BTreeSet<usize> = BTreeSet::new();

    while i < n {
        let (offset, instruction): (usize, Instruction) = {
            decode_instruction(&data[i..n])?
        };

        let dst = match instruction.offset() {
            None => None,
            Some(jump_offset) => {
                let jump_address: usize = (i as isize + offset as isize + jump_offset) as usize;
                labels.insert(jump_address);
                Some(jump_address)
            }
        };

        addresses.insert(i);
        instructions.push((instruction, i, dst));
        i += offset;
    }

    let mut label_map = HashMap::<usize, String>::new();

    for (label_no, label_address) in labels.iter().enumerate() {
        let label_string = String::from(if addresses.contains(label_address) {
            format!("label_{}", label_no)
        } else {
            format!("{}", label_address)
        });
        label_map.insert(*label_address, label_string);
    }

    let mut ret: String = "bits 16\n\n".to_owned();
    let mut inst_vec: Vec<Instruction> = Vec::new();
    for (instruction, adr, dst_opt) in instructions.iter() {
        if label_map.contains_key(&adr) {
            ret.push_str(&format!("{}:\n", label_map[&adr]));
        }
        ret.push_str(&format!("{}\n", String::from(instruction.to_str(
            match dst_opt {
                None => None,
                Some(adr) => { Some(&label_map[&adr]) }
            }))));

        inst_vec.push(instruction.clone());

//        ret.push_str(&code)// code.push_str(

        // ret.push_str(&code);
    }
    Ok((ret, inst_vec))
}

fn run_emulation(instructions: &Vec<Instruction>) {
    println!("Running emulation!")
}

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        println!("Usage: args[0] binary_file");
        return;
    }
    let mut emulate = false;
    let mut binary_file = &String::from("");
    for arg in &args[1..] {
        if arg == "--emulate" {
            emulate = true
        } else {
            binary_file = arg
        }
    }
    let (code_str, instructions) = match decode_from_file(&binary_file) {
        Ok(r) => r,
        Err(e) => {println!("Error: {}", e); return;}
    };
    if emulate {
        run_emulation(&instructions);
    } else {
        println!("{}", code_str)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn disassemble(data: &[u8]) -> (usize, String) {
        decode_to_text(data).unwrap()
    }

    #[test]
    fn test_instruction_to_str() {
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::Reg(Reg::Bx),
                                        Operand::Reg(Reg::Cx)).to_str(None),
                   "mov cx, bx");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::ImmU8(12),
                                        Operand::Reg(Reg::Si)).to_str(None),
                   "mov si, 12");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::ImmI16(-3948),
                                        Operand::Reg(Reg::Dx)).to_str(None),
                   "mov dx, -3948");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::PtrDisp((AdrReg::BxSi, 0)),
                                        Operand::Reg(Reg::Al)).to_str(None),
                   "mov al, [bx + si]");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::PtrDisp((AdrReg::BxSi, 0)),
                                        Operand::Reg(Reg::Al)).to_str(None),
                   "mov al, [bx + si]");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::PtrDisp((AdrReg::BxDi, 4)),
                                        Operand::Reg(Reg::Ah)).to_str(None),
                   "mov ah, [bx + di + 4]");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::PtrDisp((AdrReg::BxSi, 4999)),
                                        Operand::Reg(Reg::Al)).to_str(None),
                   "mov al, [bx + si + 4999]");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::PtrDisp((AdrReg::BxDi, -37)),
                                        Operand::Reg(Reg::Ax)).to_str(None),
                   "mov ax, [bx + di - 37]");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::Reg(Reg::Cl),
                                        Operand::PtrDisp((AdrReg::BpSi, 0))).to_str(None),
                   "mov [bp + si], cl");
       assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::PtrDir(3458),
                                        Operand::Reg(Reg::Bx)).to_str(None),
                  "mov bx, [3458]");
        assert_eq!(Instruction::jumpi8(Command::Jnz, -2).to_str(Some(&String::from("label2"))),
                   format!("jnz label2"));
        assert_eq!(Instruction::src_dst(Command::Add,
                                        Operand::PtrDisp((AdrReg::BxSi, 0)),
                                        Operand::Reg(Reg::Bx)).to_str(None),
                   "add bx, [bx + si]");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::ImmU8(7),
                                        Operand::PtrDisp((AdrReg::BpDi, 0)))
                   .size(DataSize::Byte)
                   .to_str(None),
                   "mov byte [bp + di], 7");
        assert_eq!(Instruction::src_dst(Command::Mov,
                                        Operand::ImmU8(7),
                                        Operand::PtrDisp((AdrReg::BpDi, 0))).size(DataSize::Word)
                   .to_str(None),
                   "mov word [bp + di], 7");
        assert_eq!(Instruction::single_op(Command::Push,
                                          Operand::Reg(Reg::Cx)).to_str(None),
                   "push cx");
        assert_eq!(Instruction::single_op(Command::Push,
                                          Operand::PtrDisp((AdrReg::BxDi, -30)))
                   .size(DataSize::Word)
                   .to_str(None),
                   "push word [bx + di - 30]");

        assert_eq!(Instruction::no_op(Command::Xlat).to_str(None),
                   "xlat")

    }

    #[test]
    fn test_disassemble() {
        let test_data: [u8; 2] = [0x89, 0xd9];
        assert_eq!(disassemble(&test_data), (2, String::from("mov cx, bx")));
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

        assert_eq!(decode_from_data(&test_data).unwrap().0, expected);
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

        assert_eq!(decode_from_data(&test_data).unwrap().0, expected);
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
             mov byte [bp + di], 7\n\
             mov word [di + 901], 347\n\
             mov bp, [5]\n\
             mov bx, [3458]\n\
             mov ax, [2555]\n\
             mov ax, [16]\n\
             mov [2554], ax\n\
             mov [15], ax\n";

        assert_eq!(decode_from_data(&test_data).unwrap().0, expected);
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

        assert_eq!(decode_from_data(&test_data).unwrap().0, expected);
    }

    #[test]
    fn test_decode_8bit_imm_mov() {
        {
            let test_data: [u8; 2] = [0xb1, 0x0c];
            assert_eq!(disassemble(&test_data), (2, String::from("mov cl, 12")));
        }
        {
            let test_data: [u8; 2] = [0xb5, 0xf4];
            assert_eq!(disassemble(&test_data), (2, String::from("mov ch, -12")));
        }
    }

    #[test]
    fn test_parse_16bit_imm_mov() {
        let test_data: [[u8; 3]; 4] = [[0xb9, 0x0c, 0x00],
                                       [0xb9, 0xf4, 0xff],
                                       [0xba, 0x6c, 0x0f],
                                       [0xba, 0x94, 0xf0]];

        assert_eq!(disassemble(&test_data[0]), (3, String::from("mov cx, 12")));
        assert_eq!(disassemble(&test_data[1]), (3, String::from("mov cx, -12")));
        assert_eq!(disassemble(&test_data[2]), (3, String::from("mov dx, 3948")));
        assert_eq!(disassemble(&test_data[3]), (3, String::from("mov dx, -3948")));
    }

    #[test]
    fn test_src_address_calcualtion() {
        let test_data_w2: [[u8; 2]; 2] = [[0x8a, 0x00],
                                          [0x8b, 0x1b]];
        let test_data_w3: [[u8; 3]; 2] = [[0x8b, 0x56, 0x00],
                                          [0x8a, 0x60, 0x04]];
        let test_data_w4: [[u8; 4]; 1] = [[0x8a, 0x80, 0x87, 0x13]];

        assert_eq!(disassemble(&test_data_w2[0]), (2, String::from("mov al, [bx + si]")));
        assert_eq!(disassemble(&test_data_w2[1]), (2, String::from("mov bx, [bp + di]")));
        assert_eq!(disassemble(&test_data_w3[0]), (3, String::from("mov dx, [bp]")));
        assert_eq!(disassemble(&test_data_w3[1]), (3, String::from("mov ah, [bx + si + 4]")));
        assert_eq!(disassemble(&test_data_w4[0]), (4, String::from("mov al, [bx + si + 4999]")));
    }

    #[test]
    fn test_dst_address_calcualtion() {
        let test_data_w2: [[u8; 2]; 2] = [[0x89, 0x09],
                                          [0x88, 0x0a]];
        let test_data_w3: [[u8; 3]; 1] = [[0x88, 0x6e, 0x00]];

        assert_eq!(disassemble(&test_data_w2[0]), (2, String::from("mov [bx + di], cx")));
        assert_eq!(disassemble(&test_data_w2[1]), (2, String::from("mov [bp + si], cl")));
        assert_eq!(disassemble(&test_data_w3[0]), (3, String::from("mov [bp], ch")));

    }

    #[test]
    fn test_signed_displacements() {
        let test_data_w3: [[u8; 3]; 2] = [[0x8b, 0x41, 0xdb],
                                          [0x8b, 0x57, 0xe0]];
        let test_data_w4: [[u8; 4]; 1] = [[0x89, 0x8c, 0xd4, 0xfe]];

        assert_eq!(disassemble(&test_data_w3[0]), (3, String::from("mov ax, [bx + di - 37]")));
        assert_eq!(disassemble(&test_data_w3[1]), (3, String::from("mov dx, [bx - 32]")));
        assert_eq!(disassemble(&test_data_w4[0]), (4, String::from("mov [si - 300], cx")));
    }

    #[test]
    fn test_explicit_sizes() {
        let test_data_w3: [[u8; 3]; 1] = [[0xc6, 0x03, 0x07]];
        let test_data_w4: [[u8; 4]; 1] = [[0xc6, 0x46, 0xd9, 0xef]];
        let test_data_w6: [[u8; 6]; 1] = [[0xc7, 0x85, 0x85, 0x03, 0x5b, 0x01]];

        assert_eq!(disassemble(&test_data_w3[0]), (3, String::from("mov byte [bp + di], 7")));
        assert_eq!(disassemble(&test_data_w4[0]), (4, String::from("mov byte [bp - 39], 239")));
        assert_eq!(disassemble(&test_data_w6[0]), (6, String::from("mov word [di + 901], 347")));
    }

    #[test]
    fn test_direct_addresses() {
        let test_data_w4: [[u8; 4]; 2] = [[0x8b, 0x2e, 0x05, 0x00],
                                          [0x8b, 0x1e, 0x82, 0x0d]];

        assert_eq!(disassemble(&test_data_w4[0]),
                   (4, String::from("mov bp, [5]")));
        assert_eq!(disassemble(&test_data_w4[1]),
                   (4, String::from("mov bx, [3458]")));
    }

    #[test]
    fn test_mem_to_acc() {
        let test_data_w3: [[u8; 3]; 2] = [[0xa1, 0xfb, 0x09],
                                          [0xa1, 0x10, 0x00]];

        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("mov ax, [2555]")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("mov ax, [16]")));
    }

    #[test]
    fn test_acc_to_mem() {
        let test_data_w3: [[u8; 3]; 2] = [[0xa3, 0xfa, 0x09],
                                          [0xa3, 0x0f, 0x00]];

        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("mov [2554], ax")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("mov [15], ax")));
    }

    #[test]
    fn test_add_instructions_1 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x03, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x03, 0x5e, 0x00],
                                          [0x83, 0xc6, 0x02],
                                          [0x83, 0xc5, 0x02],
                                          [0x83, 0xc1, 0x08]];
        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("add bx, [bx + si]")));
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("add bx, [bp]")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("add si, 2")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("add bp, 2")));
        assert_eq!(disassemble(&test_data_w3[3]),
                   (3, String::from("add cx, 8")));
    }

    #[test]
    fn test_add_instructions_2 () {
        let test_data_w3: [[u8; 3]; 4] = [[0x03, 0x5e, 0x00],
                                          [0x03, 0x4f, 0x02],
                                          [0x02, 0x7a, 0x04],
                                          [0x03, 0x7b, 0x06]];
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("add bx, [bp]")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("add cx, [bx + 2]")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("add bh, [bp + si + 4]")));
        assert_eq!(disassemble(&test_data_w3[3]),
                   (3, String::from("add di, [bp + di + 6]")));
    }

    #[test]
    fn test_add_instructions_3 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x01, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x01, 0x5e, 0x00],
                                          [0x01, 0x4f, 0x02],
                                          [0x00, 0x7a, 0x04],
                                          [0x01, 0x7b, 0x06]];

        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("add [bx + si], bx")));
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("add [bp], bx")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("add [bx + 2], cx")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("add [bp + si + 4], bh")));
        assert_eq!(disassemble(&test_data_w3[3]),
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

        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("add al, [bx + si]")));
        assert_eq!(disassemble(&test_data_w2[1]),
                   (2, String::from("add ax, bx")));
        assert_eq!(disassemble(&test_data_w2[2]),
                   (2, String::from("add al, ah")));
        assert_eq!(disassemble(&test_data_w2[3]),
                   (2, String::from("add al, -30")));
        assert_eq!(disassemble(&test_data_w2[4]),
                   (2, String::from("add al, 9")));
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("add byte [bx], 34")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("add ax, [bp]")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("add ax, 1000")));
        assert_eq!(disassemble(&test_data_w5[0]),
                   (5, String::from("add word [bp + si + 1000], 29")));
    }

    #[test]
    fn test_sub_instructions_1 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x2b, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x2b, 0x5e, 0x00],
                                          [0x83, 0xee, 0x02],
                                          [0x83, 0xed, 0x02],
                                          [0x83, 0xe9, 0x08]];
        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("sub bx, [bx + si]")));
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("sub bx, [bp]")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("sub si, 2")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("sub bp, 2")));
        assert_eq!(disassemble(&test_data_w3[3]),
                   (3, String::from("sub cx, 8")));
    }

    #[test]
    fn test_sub_instructions_2 () {
        let test_data_w3: [[u8; 3]; 4] = [[0x2b, 0x5e, 0x00],
                                          [0x2b, 0x4f, 0x02],
                                          [0x2a, 0x7a, 0x04],
                                          [0x2b, 0x7b, 0x06]];
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("sub bx, [bp]")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("sub cx, [bx + 2]")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("sub bh, [bp + si + 4]")));
        assert_eq!(disassemble(&test_data_w3[3]),
                   (3, String::from("sub di, [bp + di + 6]")));
    }

    #[test]
    fn test_sub_instructions_3 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x29, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x29, 0x5e, 0x00],
                                          [0x29, 0x4f, 0x02],
                                          [0x28, 0x7a, 0x04],
                                          [0x29, 0x7b, 0x06]];

        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("sub [bx + si], bx")));
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("sub [bp], bx")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("sub [bx + 2], cx")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("sub [bp + si + 4], bh")));
        assert_eq!(disassemble(&test_data_w3[3]),
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

        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("sub byte [bx], 34")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("sub word [bx + di], 29")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("sub ax, [bp]")));

        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("sub al, [bx + si]")));
        assert_eq!(disassemble(&test_data_w2[1]),
                   (2, String::from("sub ax, bx")));
        assert_eq!(disassemble(&test_data_w2[2]),
                   (2, String::from("sub al, ah")));

        assert_eq!(disassemble(&test_data_w3[3]),
                   (3, String::from("sub ax, 1000")));

        assert_eq!(disassemble(&test_data_w2[3]),
                   (2, String::from("sub al, -30")));
        assert_eq!(disassemble(&test_data_w2[4]),
                   (2, String::from("sub al, 9")));
    }

    #[test]
    fn test_cmp_instructions_1 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x3b, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x3b, 0x5e, 0x00],
                                          [0x83, 0xfe, 0x02],
                                          [0x83, 0xfd, 0x02],
                                          [0x83, 0xf9, 0x08]];
        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("cmp bx, [bx + si]")));
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("cmp bx, [bp]")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("cmp si, 2")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("cmp bp, 2")));
        assert_eq!(disassemble(&test_data_w3[3]),
                   (3, String::from("cmp cx, 8")));
    }

    #[test]
    fn test_cmp_instructions_2 () {
        let test_data_w3: [[u8; 3]; 4] = [[0x3b, 0x5e, 0x00],
                                          [0x3b, 0x4f, 0x02],
                                          [0x3a, 0x7a, 0x04],
                                          [0x3b, 0x7b, 0x06]];
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("cmp bx, [bp]")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("cmp cx, [bx + 2]")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("cmp bh, [bp + si + 4]")));
        assert_eq!(disassemble(&test_data_w3[3]),
                   (3, String::from("cmp di, [bp + di + 6]")));
    }

    #[test]
    fn test_cmp_instructions_3 () {
        let test_data_w2: [[u8; 2]; 1] = [[0x39, 0x18]];
        let test_data_w3: [[u8; 3]; 4] = [[0x39, 0x5e, 0x00],
                                          [0x39, 0x4f, 0x02],
                                          [0x38, 0x7a, 0x04],
                                          [0x39, 0x7b, 0x06]];

        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("cmp [bx + si], bx")));
        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("cmp [bp], bx")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("cmp [bx + 2], cx")));
        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("cmp [bp + si + 4], bh")));
        assert_eq!(disassemble(&test_data_w3[3]),
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

        assert_eq!(disassemble(&test_data_w3[0]),
                   (3, String::from("cmp byte [bx], 34")));
        assert_eq!(disassemble(&test_data_w5[0]),
                   (5, String::from("cmp word [4834], 29")));
        assert_eq!(disassemble(&test_data_w3[1]),
                   (3, String::from("cmp ax, [bp]")));

        assert_eq!(disassemble(&test_data_w2[0]),
                   (2, String::from("cmp al, [bx + si]")));
        assert_eq!(disassemble(&test_data_w2[1]),
                   (2, String::from("cmp ax, bx")));
        assert_eq!(disassemble(&test_data_w2[2]),
                   (2, String::from("cmp al, ah")));

        assert_eq!(disassemble(&test_data_w3[2]),
                   (3, String::from("cmp ax, 1000")));

        assert_eq!(disassemble(&test_data_w2[3]),
                   (2, String::from("cmp al, -30")));
        assert_eq!(disassemble(&test_data_w2[4]),
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

        assert_eq!(disassemble(&test_data[0]),
                   (2, String::from("jnz #OFFSET# 2")));
        assert_eq!(disassemble(&test_data[1]),
                   (2, String::from("jnz #OFFSET# -4")));
        assert_eq!(disassemble(&test_data[2]),
                   (2, String::from("jnz #OFFSET# -6")));
        assert_eq!(disassemble(&test_data[3]),
                   (2, String::from("jnz #OFFSET# -4")));
        assert_eq!(disassemble(&test_data[4]),
                   (2, String::from("je #OFFSET# -2")));
        assert_eq!(disassemble(&test_data[5]),
                   (2, String::from("jl #OFFSET# -4")));
        assert_eq!(disassemble(&test_data[6]),
                   (2, String::from("jle #OFFSET# -6")));
        assert_eq!(disassemble(&test_data[7]),
                   (2, String::from("jb #OFFSET# -8")));
        assert_eq!(disassemble(&test_data[8]),
                   (2, String::from("jbe #OFFSET# -10")));
        assert_eq!(disassemble(&test_data[9]),
                   (2, String::from("jp #OFFSET# -12")));
        assert_eq!(disassemble(&test_data[10]),
                   (2, String::from("jo #OFFSET# -14")));
        assert_eq!(disassemble(&test_data[11]),
                   (2, String::from("js #OFFSET# -16")));
        assert_eq!(disassemble(&test_data[12]),
                   (2, String::from("jnz #OFFSET# -18")));
        assert_eq!(disassemble(&test_data[13]),
                   (2, String::from("jnl #OFFSET# -20")));
        assert_eq!(disassemble(&test_data[14]),
                   (2, String::from("jg #OFFSET# -22")));
        assert_eq!(disassemble(&test_data[15]),
                   (2, String::from("jnb #OFFSET# -24")));
        assert_eq!(disassemble(&test_data[16]),
                   (2, String::from("ja #OFFSET# -26")));
        assert_eq!(disassemble(&test_data[17]),
                   (2, String::from("jnp #OFFSET# -28")));
        assert_eq!(disassemble(&test_data[18]),
                   (2, String::from("jno #OFFSET# -30")));
        assert_eq!(disassemble(&test_data[19]),
                   (2, String::from("jns #OFFSET# -32")));
        assert_eq!(disassemble(&test_data[20]),
                   (2, String::from("loop #OFFSET# -34")));
        assert_eq!(disassemble(&test_data[21]),
                   (2, String::from("loopz #OFFSET# -36")));
        assert_eq!(disassemble(&test_data[22]),
                   (2, String::from("loopnz #OFFSET# -38")));
        assert_eq!(disassemble(&test_data[23]),
                   (2, String::from("jcxz #OFFSET# -40")));
    }

    #[test]
    fn test_push_pop_instructions() {
        assert_eq!(disassemble(&[0xff, 0x32]),
                   (2, String::from("push word [bp + si]")));
        assert_eq!(disassemble(&[0xff, 0x36, 0xb8, 0x0b]),
                   (4, String::from("push word [3000]")));
        assert_eq!(disassemble(&[0xff, 0x71, 0xe2]),
                   (3, String::from("push word [bx + di - 30]")));
        assert_eq!(disassemble(&[0x51]),
                   (1, String::from("push cx")));
        assert_eq!(disassemble(&[0x50]),
                   (1, String::from("push ax")));
        assert_eq!(disassemble(&[0x52]),
                   (1, String::from("push dx")));
        assert_eq!(disassemble(&[0x0e]),
                   (1, String::from("push cs")));
        assert_eq!(disassemble(&[0x8f, 0x02]),
                   (2, String::from("pop word [bp + si]")));
        assert_eq!(disassemble(&[0x8f, 0x06, 0x03, 0x00]),
                   (4, String::from("pop word [3]")));
        assert_eq!(disassemble(&[0x8f, 0x81, 0x48, 0xf4]),
                   (4, String::from("pop word [bx + di - 3000]")));
        assert_eq!(disassemble(&[0x5c]),
                   (1, String::from("pop sp")));
        assert_eq!(disassemble(&[0x5f]),
                   (1, String::from("pop di")));
        assert_eq!(disassemble(&[0x5e]),
                   (1, String::from("pop si")));
        assert_eq!(disassemble(&[0x1f]),
                   (1, String::from("pop ds")));
    }

    #[test]
    fn test_xchg_instructions() {
        assert_eq!(disassemble(&[0x87, 0x86, 0x18, 0xfc]),
                   (4, String::from("xchg ax, [bp - 1000]")));
        // assert_eq!(disassemble(&[0x87, 0x6f, 0x32]),
        //            (3, String::from("xchg [bx + 50], bp")));
        // The above fails but below passes (should be equivalent anyway)
        // I checked using nasm, and both instructions assemble to the same machine code
        assert_eq!(disassemble(&[0x87, 0x6f, 0x32]),
                   (3, String::from("xchg bp, [bx + 50]")));
        assert_eq!(disassemble(&[0x90]),
                   (1, String::from("xchg ax, ax")));
        assert_eq!(disassemble(&[0x92]),
                   (1, String::from("xchg dx, ax")));
        assert_eq!(disassemble(&[0x94]),
                   (1, String::from("xchg sp, ax")));
        assert_eq!(disassemble(&[0x96]),
                   (1, String::from("xchg si, ax")));
        assert_eq!(disassemble(&[0x97]),
                   (1, String::from("xchg di, ax")));
        assert_eq!(disassemble(&[0x87, 0xca]),
                   (2, String::from("xchg cx, dx")));
        assert_eq!(disassemble(&[0x87, 0xf1]),
                   (2, String::from("xchg si, cx")));
        assert_eq!(disassemble(&[0x86, 0xcc]),
                   (2, String::from("xchg cl, ah")));
    }

    #[test]
    fn test_in_instructions() {
        assert_eq!(disassemble(&[0xe4, 0xc8]),
                   (2, String::from("in al, 200")));
        assert_eq!(disassemble(&[0xec]),
                   (1, String::from("in al, dx")));
        assert_eq!(disassemble(&[0xed]),
                   (1, String::from("in ax, dx")));
    }

    #[test]
    fn test_out_instructions() {
        assert_eq!(disassemble(&[0xe7, 0x2c]),
                   (2, String::from("out 44, ax")));
        assert_eq!(disassemble(&[0xee]),
                   (1, String::from("out dx, al")));
        assert_eq!(disassemble(&[0xef]),
                   (1, String::from("out dx, ax")));
    }

    #[test]
    fn test_direct_instructions() {
        assert_eq!(disassemble(&[0xd7]),
                   (1, String::from("xlat")));
        assert_eq!(disassemble(&[0x9f]),
                   (1, String::from("lahf")));
        assert_eq!(disassemble(&[0x9e]),
                   (1, String::from("sahf")));
        assert_eq!(disassemble(&[0x9c]),
                   (1, String::from("pushf")));
        assert_eq!(disassemble(&[0x9d]),
                   (1, String::from("popf")));
        assert_eq!(disassemble(&[0x37]),
                   (1, String::from("aaa")));
        assert_eq!(disassemble(&[0x27]),
                   (1, String::from("daa")));
        assert_eq!(disassemble(&[0x3f]),
                   (1, String::from("aas")));
        assert_eq!(disassemble(&[0x2f]),
                   (1, String::from("das")));
        assert_eq!(disassemble(&[0x98]),
                   (1, String::from("cbw")));
        assert_eq!(disassemble(&[0x99]),
                   (1, String::from("cwd")));
        assert_eq!(disassemble(&[0xcc]),
                   (1, String::from("int3")));
        assert_eq!(disassemble(&[0xce]),
                   (1, String::from("into")));
        assert_eq!(disassemble(&[0xcf]),
                   (1, String::from("iret")));
        assert_eq!(disassemble(&[0xf8]),
                   (1, String::from("clc")));
        assert_eq!(disassemble(&[0xf5]),
                   (1, String::from("cmc")));
        assert_eq!(disassemble(&[0xf9]),
                   (1, String::from("stc")));
        assert_eq!(disassemble(&[0xfc]),
                   (1, String::from("cld")));
        assert_eq!(disassemble(&[0xfd]),
                   (1, String::from("std")));
        assert_eq!(disassemble(&[0xfa]),
                   (1, String::from("cli")));
        assert_eq!(disassemble(&[0xfb]),
                   (1, String::from("sti")));
        assert_eq!(disassemble(&[0xf4]),
                   (1, String::from("hlt")));
        assert_eq!(disassemble(&[0x9b]),
                   (1, String::from("wait")));
    }

    #[test]
    fn test_lea_instruction() {
        assert_eq!(disassemble(&[0x8d, 0x81, 0x8c, 0x05]),
                   (4, String::from("lea ax, [bx + di + 1420]")));
        assert_eq!(disassemble(&[0x8d, 0x5e, 0xce]),
                   (3, String::from("lea bx, [bp - 50]")));
        assert_eq!(disassemble(&[0x8d, 0xa6, 0x15, 0xfc]),
                   (4, String::from("lea sp, [bp - 1003]")));
        assert_eq!(disassemble(&[0x8d, 0x78, 0xf9]),
                   (3, String::from("lea di, [bx + si - 7]")));
    }

    #[test]
    fn test_lds_instruction() {
        assert_eq!(disassemble(&[0xc5, 0x81, 0x8c, 0x05]),
                   (4, String::from("lds ax, [bx + di + 1420]")));
        assert_eq!(disassemble(&[0xc5, 0x5e, 0xce]),
                   (3, String::from("lds bx, [bp - 50]")));
        assert_eq!(disassemble(&[0xc5, 0xa6, 0x15, 0xfc]),
                   (4, String::from("lds sp, [bp - 1003]")));
        assert_eq!(disassemble(&[0xc5, 0x78, 0xf9]),
                   (3, String::from("lds di, [bx + si - 7]")));
    }

    #[test]
    fn test_les_instruction() {
        assert_eq!(disassemble(&[0xc4, 0x81, 0x8c, 0x05]),
                   (4, String::from("les ax, [bx + di + 1420]")));
        assert_eq!(disassemble(&[0xc4, 0x5e, 0xce]),
                   (3, String::from("les bx, [bp - 50]")));
        assert_eq!(disassemble(&[0xc4, 0xa6, 0x15, 0xfc]),
                   (4, String::from("les sp, [bp - 1003]")));
        assert_eq!(disassemble(&[0xc4, 0x78, 0xf9]),
                   (3, String::from("les di, [bx + si - 7]")));
    }

    #[test]
    fn test_add_instructions_5() {
        assert_eq!(disassemble(&[0x81, 0xc4, 0x88, 0x01]),
                   (4, String::from("add sp, 392")));
        assert_eq!(disassemble(&[0x83, 0xc6, 0x05]),
                   (3, String::from("add si, 5")));
        assert_eq!(disassemble(&[0x05, 0xe8, 0x03]),
                   (3, String::from("add ax, 1000")));
        assert_eq!(disassemble(&[0x80, 0xc4, 0x1e]),
                   (3, String::from("add ah, 30")));
        assert_eq!(disassemble(&[0x04, 0x09]),
                   (2, String::from("add al, 9")));
        assert_eq!(disassemble(&[0x01, 0xd9]),
                   (2, String::from("add cx, bx")));
        assert_eq!(disassemble(&[0x00, 0xc5]),
                   (2, String::from("add ch, al")));
    }

    #[test]
    fn test_adc_instructions() {
        assert_eq!(disassemble(&[0x13, 0x4e, 0x00]),
                   (3, String::from("adc cx, [bp]")));
        assert_eq!(disassemble(&[0x13, 0x10]),
                   (2, String::from("adc dx, [bx + si]")));
        assert_eq!(disassemble(&[0x10, 0xa3, 0x88, 0x13]),
                   (4, String::from("adc [bp + di + 5000], ah")));
        assert_eq!(disassemble(&[0x10, 0x07]),
                   (2, String::from("adc [bx], al")));
        assert_eq!(disassemble(&[0x81, 0xd4, 0x88, 0x01]),
                   (4, String::from("adc sp, 392")));
        assert_eq!(disassemble(&[0x83, 0xd6, 0x05]),
                   (3, String::from("adc si, 5")));
        assert_eq!(disassemble(&[0x15, 0xe8, 0x03]),
                   (3, String::from("adc ax, 1000")));
        assert_eq!(disassemble(&[0x80, 0xd4, 0x1e]),
                   (3, String::from("adc ah, 30")));
        assert_eq!(disassemble(&[0x14, 0x09]),
                   (2, String::from("adc al, 9")));
        assert_eq!(disassemble(&[0x11, 0xd9]),
                   (2, String::from("adc cx, bx")));
        assert_eq!(disassemble(&[0x10, 0xc5]),
                   (2, String::from("adc ch, al")));
    }

    #[test]
    fn test_inc_instructions() {
        assert_eq!(disassemble(&[0x40]),
                   (1, String::from("inc ax")));
        assert_eq!(disassemble(&[0x41]),
                   (1, String::from("inc cx")));
        assert_eq!(disassemble(&[0xfe, 0xc6]),
                   (2, String::from("inc dh")));
        assert_eq!(disassemble(&[0xfe, 0xc0]),
                   (2, String::from("inc al")));
        assert_eq!(disassemble(&[0xfe, 0xc4]),
                   (2, String::from("inc ah")));
        assert_eq!(disassemble(&[0x44]),
                   (1, String::from("inc sp")));
        assert_eq!(disassemble(&[0x47]),
                   (1, String::from("inc di")));
        assert_eq!(disassemble(&[0xfe, 0x86, 0xea, 0x03]),
                   (4, String::from("inc byte [bp + 1002]")));
        assert_eq!(disassemble(&[0xff, 0x47,  0x27]),
                   (3, String::from("inc word [bx + 39]")));
        assert_eq!(disassemble(&[0xfe, 0x40, 0x05]),
                   (3, String::from("inc byte [bx + si + 5]")));
        assert_eq!(disassemble(&[0xff, 0x83, 0xc4, 0xd8]),
                   (4, String::from("inc word [bp + di - 10044]")));
        assert_eq!(disassemble(&[0xff, 0x06, 0x85, 0x24]),
                   (4, String::from("inc word [9349]")));
        assert_eq!(disassemble(&[0xfe, 0x46, 0x00]),
                   (3, String::from("inc byte [bp]")));
    }

    #[test]
    fn test_sbb_instructions() {
        assert_eq!(disassemble(&[0x1b, 0x4e, 0x00]),
                   (3, String::from("sbb cx, [bp]")));
        assert_eq!(disassemble(&[0x1b, 0x10]),
                   (2, String::from("sbb dx, [bx + si]")));
        assert_eq!(disassemble(&[0x18, 0xa3, 0x88, 0x13]),
                   (4, String::from("sbb [bp + di + 5000], ah")));
        assert_eq!(disassemble(&[0x18, 0x07]),
                   (2, String::from("sbb [bx], al")));
        assert_eq!(disassemble(&[0x81, 0xdc, 0x88, 0x01]),
                   (4, String::from("sbb sp, 392")));
        assert_eq!(disassemble(&[0x83, 0xde, 0x05]),
                   (3, String::from("sbb si, 5")));
        assert_eq!(disassemble(&[0x1d, 0xe8, 0x03]),
                   (3, String::from("sbb ax, 1000")));
        assert_eq!(disassemble(&[0x80, 0xdc, 0x1e]),
                   (3, String::from("sbb ah, 30")));
        assert_eq!(disassemble(&[0x1c, 0x09]),
                   (2, String::from("sbb al, 9")));
        assert_eq!(disassemble(&[0x19, 0xd9]),
                   (2, String::from("sbb cx, bx")));
        assert_eq!(disassemble(&[0x18, 0xc5]),
                   (2, String::from("sbb ch, al")));
    }

    #[test]
    fn test_dec_instructions() {
        assert_eq!(disassemble(&[0x48]),
                   (1, String::from("dec ax")));
        assert_eq!(disassemble(&[0x49]),
                   (1, String::from("dec cx")));
        assert_eq!(disassemble(&[0xfe, 0xce]),
                   (2, String::from("dec dh")));
        assert_eq!(disassemble(&[0xfe, 0xc8]),
                   (2, String::from("dec al")));
        assert_eq!(disassemble(&[0xfe, 0xcc]),
                   (2, String::from("dec ah")));
        assert_eq!(disassemble(&[0x4c]),
                   (1, String::from("dec sp")));
        assert_eq!(disassemble(&[0x4f]),
                   (1, String::from("dec di")));
        assert_eq!(disassemble(&[0xfe, 0x8e, 0xea, 0x03]),
                   (4, String::from("dec byte [bp + 1002]")));
        assert_eq!(disassemble(&[0xff, 0x4f, 0x27]),
                   (3, String::from("dec word [bx + 39]")));
        assert_eq!(disassemble(&[0xfe, 0x48, 0x05]),
                   (3, String::from("dec byte [bx + si + 5]")));
        assert_eq!(disassemble(&[0xff, 0x8b, 0xc4, 0xd8]),
                   (4, String::from("dec word [bp + di - 10044]")));
        assert_eq!(disassemble(&[0xff, 0x0e, 0x85, 0x24]),
                   (4, String::from("dec word [9349]")));
        assert_eq!(disassemble(&[0xfe, 0x4e, 0x00]),
                   (3, String::from("dec byte [bp]")));
    }

    #[test]
    fn test_neg_instructions() {
        assert_eq!(disassemble(&[0xf7, 0xd8]),
                   (2, String::from("neg ax")));
        assert_eq!(disassemble(&[0xf7, 0xd9]),
                   (2, String::from("neg cx")));
        assert_eq!(disassemble(&[0xf6, 0xde]),
                   (2, String::from("neg dh")));
        assert_eq!(disassemble(&[0xf6, 0xd8]),
                   (2, String::from("neg al")));
        assert_eq!(disassemble(&[0xf6, 0xdc]),
                   (2, String::from("neg ah")));
        assert_eq!(disassemble(&[0xf7, 0xdc]),
                   (2, String::from("neg sp")));
        assert_eq!(disassemble(&[0xf7, 0xdf]),
                   (2, String::from("neg di")));
        assert_eq!(disassemble(&[0xf6, 0x9e, 0xea, 0x03]),
                   (4, String::from("neg byte [bp + 1002]")));
        assert_eq!(disassemble(&[0xf7, 0x5f, 0x27]),
                   (3, String::from("neg word [bx + 39]")));
        assert_eq!(disassemble(&[0xf6, 0x58, 0x05]),
                   (3, String::from("neg byte [bx + si + 5]")));
        assert_eq!(disassemble(&[0xf7, 0x9b, 0xc4, 0xd8]),
                   (4, String::from("neg word [bp + di - 10044]")));
        assert_eq!(disassemble(&[0xf7, 0x1e, 0x85, 0x24,]),
                   (4, String::from("neg word [9349]")));
        assert_eq!(disassemble(&[0xf6, 0x5e, 0x00]),
                   (3, String::from("neg byte [bp]")));
    }

    #[test]
    fn test_mul_div_instructions() {
        assert_eq!(disassemble(&[0xf6, 0xe0]),
                   (2, String::from("mul al")));
        assert_eq!(disassemble(&[0xf7, 0xe1]),
                   (2, String::from("mul cx")));
        assert_eq!(disassemble(&[0xf7, 0x66, 0x00]),
                   (3, String::from("mul word [bp]")));
        assert_eq!(disassemble(&[0xf6, 0xa1, 0xf4, 0x01]),
                   (4, String::from("mul byte [bx + di + 500]")));
        assert_eq!(disassemble(&[0xf6, 0xed]),
                   (2, String::from("imul ch")));
        assert_eq!(disassemble(&[0xf7, 0xea]),
                   (2, String::from("imul dx")));
        assert_eq!(disassemble(&[0xf6, 0x2f]),
                   (2, String::from("imul byte [bx]")));
        assert_eq!(disassemble(&[0xf7, 0x2e, 0x0b, 0x25]),
                   (4, String::from("imul word [9483]")));
        assert_eq!(disassemble(&[0xf6, 0xf3,]),
                   (2, String::from("div bl")));
        assert_eq!(disassemble(&[0xf7, 0xf4,]),
                   (2, String::from("div sp")));
        assert_eq!(disassemble(&[0xf6, 0xb0, 0xae, 0x0b]),
                   (4, String::from("div byte [bx + si + 2990]")));
        assert_eq!(disassemble(&[0xf7, 0xb3, 0xe8, 0x03]),
                   (4, String::from("div word [bp + di + 1000]")));
        assert_eq!(disassemble(&[0xf7, 0xf8]),
                   (2, String::from("idiv ax")));
        assert_eq!(disassemble(&[0xf7, 0xfe]),
                   (2, String::from("idiv si")));
        assert_eq!(disassemble(&[0xf6, 0x3a]),
                   (2, String::from("idiv byte [bp + si]")));
        assert_eq!(disassemble(&[0xf7, 0xbf, 0xed, 0x01]),
                   (4, String::from("idiv word [bx + 493]")));
    }

    #[test]
    fn test_ascii_adjust_instructions() {
        assert_eq!(disassemble(&[0xd4, 0x0a]),
                   (2, String::from("aam")));
        assert_eq!(disassemble(&[0xd5, 0x0a]),
                   (2, String::from("aad")));
    }

    #[test]
    fn test_not_instructions() {
        assert_eq!(disassemble(&[0xf6, 0xd4]),
                   (2, String::from("not ah")));
        assert_eq!(disassemble(&[0xf6, 0xd3]),
                   (2, String::from("not bl")));
        assert_eq!(disassemble(&[0xf7, 0xd4]),
                   (2, String::from("not sp")));
        assert_eq!(disassemble(&[0xf7, 0xd6]),
                   (2, String::from("not si")));
        assert_eq!(disassemble(&[0xf7, 0x56, 0x00]),
                   (3, String::from("not word [bp]")));
        assert_eq!(disassemble(&[0xf6, 0x96, 0xb1, 0x26]),
                   (4, String::from("not byte [bp + 9905]")));
    }

    #[test]
    fn test_shift_rot_instructions() {
        assert_eq!(disassemble(&[0xd0, 0xe4]),
                   (2, String::from("shl ah, 1")));
        assert_eq!(disassemble(&[0xd1, 0xe8]),
                   (2, String::from("shr ax, 1")));
        assert_eq!(disassemble(&[0xd1, 0xfb]),
                   (2, String::from("sar bx, 1")));
        assert_eq!(disassemble(&[0xd1, 0xc1]),
                   (2, String::from("rol cx, 1")));
        assert_eq!(disassemble(&[0xd0, 0xce]),
                   (2, String::from("ror dh, 1")));
        assert_eq!(disassemble(&[0xd1, 0xd4]),
                   (2, String::from("rcl sp, 1")));
        assert_eq!(disassemble(&[0xd1, 0xdd]),
                   (2, String::from("rcr bp, 1")));
        assert_eq!(disassemble(&[0xd1, 0x66, 0x05]),
                   (3, String::from("shl word [bp + 5], 1")));
        assert_eq!(disassemble(&[0xd0, 0xa8, 0x39, 0xff]),
                   (4, String::from("shr byte [bx + si - 199], 1")));
        assert_eq!(disassemble(&[0xd0, 0xb9, 0xd4, 0xfe]),
                   (4, String::from("sar byte [bx + di - 300], 1")));
        assert_eq!(disassemble(&[0xd1, 0x46, 0x00]),
                   (3, String::from("rol word [bp], 1")));
        assert_eq!(disassemble(&[0xd1, 0x0e, 0x4a, 0x13]),
                   (4, String::from("ror word [4938], 1")));
        assert_eq!(disassemble(&[0xd0, 0x16, 0x03, 0x00]),
                   (4, String::from("rcl byte [3], 1")));
        assert_eq!(disassemble(&[0xd1, 0x1f]),
                   (2, String::from("rcr word [bx], 1")));
        assert_eq!(disassemble(&[0xd2, 0xe4]),
                   (2, String::from("shl ah, cl")));
        assert_eq!(disassemble(&[0xd3, 0xe8]),
                   (2, String::from("shr ax, cl")));
        assert_eq!(disassemble(&[0xd3, 0xfb]),
                   (2, String::from("sar bx, cl")));
        assert_eq!(disassemble(&[0xd3, 0xc1]),
                   (2, String::from("rol cx, cl")));
        assert_eq!(disassemble(&[0xd2, 0xce]),
                   (2, String::from("ror dh, cl")));
        assert_eq!(disassemble(&[0xd3, 0xd4]),
                   (2, String::from("rcl sp, cl")));
        assert_eq!(disassemble(&[0xd3, 0xdd]),
                   (2, String::from("rcr bp, cl")));
        assert_eq!(disassemble(&[0xd3, 0x66, 0x05]),
                   (3, String::from("shl word [bp + 5], cl")));
        assert_eq!(disassemble(&[0xd3, 0xa8, 0x39, 0xff]),
                   (4, String::from("shr word [bx + si - 199], cl")));
        assert_eq!(disassemble(&[0xd2, 0xb9, 0xd4, 0xfe]),
                   (4, String::from("sar byte [bx + di - 300], cl")));
        assert_eq!(disassemble(&[0xd2, 0x46, 0x00]),
                   (3, String::from("rol byte [bp], cl")));
        assert_eq!(disassemble(&[0xd2, 0x0e, 0x4a, 0x13]),
                   (4, String::from("ror byte [4938], cl")));
        assert_eq!(disassemble(&[0xd2, 0x16, 0x03, 0x00]),
                   (4, String::from("rcl byte [3], cl")));
        assert_eq!(disassemble(&[0xd3, 0x1f]),
                   (2, String::from("rcr word [bx], cl")));
    }

    #[test]
    fn test_and_instructions(){
        assert_eq!(disassemble(&[0x20, 0xe0]),
                   (2, String::from("and al, ah")));
        assert_eq!(disassemble(&[0x20, 0xcd]),
                   (2, String::from("and ch, cl")));
        assert_eq!(disassemble(&[0x21, 0xf5]),
                   (2, String::from("and bp, si")));
        assert_eq!(disassemble(&[0x21, 0xe7]),
                   (2, String::from("and di, sp")));
        assert_eq!(disassemble(&[0x24, 0x5d]),
                   (2, String::from("and al, 93")));
        assert_eq!(disassemble(&[0x25, 0xa8, 0x4f]),
                   (3, String::from("and ax, 20392")));
        assert_eq!(disassemble(&[0x20, 0x6a, 0x0a]),
                   (3, String::from("and [bp + si + 10], ch")));
        assert_eq!(disassemble(&[0x21, 0x91, 0xe8, 0x03]),
                   (4, String::from("and [bx + di + 1000], dx")));
        assert_eq!(disassemble(&[0x23, 0x5e, 0x00]),
                   (3, String::from("and bx, [bp]")));
        assert_eq!(disassemble(&[0x23, 0x0e, 0x20, 0x11]),
                   (4, String::from("and cx, [4384]")));
        assert_eq!(disassemble(&[0x80, 0x66, 0xd9, 0xef]),
                   (4, String::from("and byte [bp - 39], 239")));
        assert_eq!(disassemble(&[0x81, 0xa0, 0x14, 0xef, 0x58, 0x28]),
                   (6, String::from("and word [bx + si - 4332], 10328")));
    }

    #[test]
    fn test_test_instructions() {
        assert_eq!(disassemble(&[0x85, 0xcb]),
                   (2, String::from("test bx, cx")));
        assert_eq!(disassemble(&[0x84, 0xb6, 0x86, 0x01]),
                   (4, String::from("test [bp + 390], dh")));
        assert_eq!(disassemble(&[0x85, 0x76, 0x02]),
                   (3, String::from("test [bp + 2], si")));
        assert_eq!(disassemble(&[0xf6, 0xc3, 0x14]),
                  (3, String::from("test bl, 20")));
        assert_eq!(disassemble(&[0xf6, 0x07, 0x22]),
                   (3, String::from("test byte [bx], 34")));
        assert_eq!(disassemble(&[0xa9, 0x65, 0x5d]),
                   (3, String::from("test ax, 23909")));
    }

    #[test]
    fn test_or_instructions() {
        assert_eq!(disassemble(&[0x08, 0xe0]),
                   (2, String::from("or al, ah")));
        assert_eq!(disassemble(&[0x08, 0xcd]),
                   (2, String::from("or ch, cl")));
        assert_eq!(disassemble(&[0x09, 0xf5]),
                   (2, String::from("or bp, si")));
        assert_eq!(disassemble(&[0x09, 0xe7]),
                   (2, String::from("or di, sp")));
        assert_eq!(disassemble(&[0x0c, 0x5d]),
                   (2, String::from("or al, 93")));
        assert_eq!(disassemble(&[0x0d, 0xa8, 0x4f]),
                   (3, String::from("or ax, 20392")));
        assert_eq!(disassemble(&[0x08, 0x6a, 0x0a]),
                   (3, String::from("or [bp + si + 10], ch")));
        assert_eq!(disassemble(&[0x09, 0x91, 0xe8, 0x03]),
                   (4, String::from("or [bx + di + 1000], dx")));
        assert_eq!(disassemble(&[0x0b, 0x5e, 0x00]),
                   (3, String::from("or bx, [bp]")));
        assert_eq!(disassemble(&[0x0b, 0x0e, 0x20, 0x11]),
                   (4, String::from("or cx, [4384]")));
        assert_eq!(disassemble(&[0x80, 0x4e, 0xd9, 0xef]),
                   (4, String::from("or byte [bp - 39], 239")));
        assert_eq!(disassemble(&[0x81, 0x88, 0x14, 0xef, 0x58, 0x28]),
                   (6, String::from("or word [bx + si - 4332], 10328")));
    }

    #[test]
    fn test_xor_instructions() {
        assert_eq!(disassemble(&[0x30, 0xe0]),
                   (2, String::from("xor al, ah")));
        assert_eq!(disassemble(&[0x30, 0xcd]),
                   (2, String::from("xor ch, cl")));
        assert_eq!(disassemble(&[0x31, 0xf5]),
                   (2, String::from("xor bp, si")));
        assert_eq!(disassemble(&[0x31, 0xe7]),
                   (2, String::from("xor di, sp")));
        assert_eq!(disassemble(&[0x34, 0x5d]),
                   (2, String::from("xor al, 93")));
        assert_eq!(disassemble(&[0x35, 0xa8, 0x4f]),
                   (3, String::from("xor ax, 20392")));
        assert_eq!(disassemble(&[0x30, 0x6a, 0x0a]),
                   (3, String::from("xor [bp + si + 10], ch")));
        assert_eq!(disassemble(&[0x31, 0x91, 0xe8, 0x03]),
                   (4, String::from("xor [bx + di + 1000], dx")));
        assert_eq!(disassemble(&[0x33, 0x5e, 0x00]),
                   (3, String::from("xor bx, [bp]")));
        assert_eq!(disassemble(&[0x33, 0x0e, 0x20, 0x11]),
                   (4, String::from("xor cx, [4384]")));
        assert_eq!(disassemble(&[0x80, 0x76, 0xd9, 0xef]),
                   (4, String::from("xor byte [bp - 39], 239")));
        assert_eq!(disassemble(&[0x81, 0xb0, 0x14, 0xef, 0x58, 0x28]),
                   (6, String::from("xor word [bx + si - 4332], 10328")));
    }

    #[test]
    fn test_rep_instructions() {
        assert_eq!(disassemble(&[0xf3, 0xa4]),
                   (2, String::from("rep movsb")));
        assert_eq!(disassemble(&[0xf3, 0xa6]),
                   (2, String::from("rep cmpsb")));
        assert_eq!(disassemble(&[0xf3, 0xae]),
                   (2, String::from("rep scasb")));
        assert_eq!(disassemble(&[0xf3, 0xac]),
                   (2, String::from("rep lodsb")));
        assert_eq!(disassemble(&[0xf3, 0xa5]),
                   (2, String::from("rep movsw")));
        assert_eq!(disassemble(&[0xf3, 0xa7]),
                   (2, String::from("rep cmpsw")));
        assert_eq!(disassemble(&[0xf3, 0xaf]),
                   (2, String::from("rep scasw")));
        assert_eq!(disassemble(&[0xf3, 0xad]),
                   (2, String::from("rep lodsw")));
        assert_eq!(disassemble(&[0xf3, 0xaa]),
                   (2, String::from("rep stosb")));
        assert_eq!(disassemble(&[0xf3, 0xab]),
                   (2, String::from("rep stosw")));
        assert_eq!(disassemble(&[0xf2, 0xa4]),
                   (2, String::from("repnz movsb")));
        assert_eq!(disassemble(&[0xf2, 0xa6]),
                   (2, String::from("repnz cmpsb")));
        assert_eq!(disassemble(&[0xf2, 0xae]),
                   (2, String::from("repnz scasb")));
        assert_eq!(disassemble(&[0xf2, 0xac]),
                   (2, String::from("repnz lodsb")));
        assert_eq!(disassemble(&[0xf2, 0xa5]),
                   (2, String::from("repnz movsw")));
        assert_eq!(disassemble(&[0xf2, 0xa7]),
                   (2, String::from("repnz cmpsw")));
        assert_eq!(disassemble(&[0xf2, 0xaf]),
                   (2, String::from("repnz scasw")));
        assert_eq!(disassemble(&[0xf2, 0xad]),
                   (2, String::from("repnz lodsw")));
        assert_eq!(disassemble(&[0xf2, 0xaa]),
                   (2, String::from("repnz stosb")));
        assert_eq!(disassemble(&[0xf2, 0xab]),
                   (2, String::from("repnz stosw")));
    }

    #[test]
    fn test_call_ret() {
        assert_eq!(disassemble(&[0xff, 0x16, 0x21, 0x99]),
                   (4, String::from("call [39201]")));
        assert_eq!(disassemble(&[0xff, 0x56, 0x9c]),
                   (3, String::from("call [bp - 100]")));
        assert_eq!(disassemble(&[0xff, 0xd4]),
                   (2, String::from("call sp")));
        assert_eq!(disassemble(&[0xff, 0xd0]),
                   (2, String::from("call ax")));
        assert_eq!(disassemble(&[0xff, 0xe0]),
                   (2, String::from("jmp ax")));
        assert_eq!(disassemble(&[0xff, 0xe7]),
                   (2, String::from("jmp di")));
        assert_eq!(disassemble(&[0xff, 0x26, 0x0c, 0x00]),
                   (4, String::from("jmp [12]")));
        assert_eq!(disassemble(&[0xff, 0x26, 0x2b, 0x11]),
                   (4, String::from("jmp [4395]")));
        assert_eq!(disassemble(&[0xc2, 0xf9, 0xff]),
                   (3, String::from("ret -7")));
        assert_eq!(disassemble(&[0xc2, 0xf4, 0x01]),
                   (3, String::from("ret 500")));
        assert_eq!(disassemble(&[0xc3]),
                   (1, String::from("ret")));
    }

    #[test]
    fn test_int_instructions() {
        assert_eq!(disassemble(&[0xcd, 0x0d]),
                   (2, String::from("int 13")));
    }
    #[test]
    fn test_lock_instructions() {
        assert_eq!(disassemble(&[0xf0, 0xf6, 0x96, 0xb1, 0x26]),
                   (5, String::from("lock not byte [bp + 9905]")));
        assert_eq!(disassemble(&[0xf0, 0x86, 0x06, 0x64, 0x00]),
                   (5, String::from("lock xchg al, [100]")));
    }

    #[test]
    fn test_segment_instructions() {
        assert_eq!(disassemble(&[0x2e, 0x8a, 0x00]),
                   (3, String::from("mov al, cs:[bx + si]")));
        assert_eq!(disassemble(&[0x3e, 0x8b, 0x1b]),
                   (3, String::from("mov bx, ds:[bp + di]")));
        assert_eq!(disassemble(&[0x26, 0x8b, 0x56, 0x00]),
                   (4, String::from("mov dx, es:[bp]")));
        assert_eq!(disassemble(&[0x36, 0x8a, 0x60, 0x04]),
                   (4, String::from("mov ah, ss:[bx + si + 4]")));
        assert_eq!(disassemble(&[0x8c, 0x40, 0x3b]),
                   (3, String::from("mov [bx + si + 59], es")));
        assert_eq!(disassemble(&[0x8e, 0x8a, 0x40, 0xf7]),
                   (4, String::from("mov cs, [bp + si - 2240]")));
    }

    #[test]
    fn test_lock_then_seg() {
        assert_eq!(disassemble(&[0x81, 0x98, 0x14, 0xef, 0x58, 0x28]),
                   (6, String::from("sbb word [bx + si - 4332], 10328")));
        assert_eq!(disassemble(&[0x2e, 0x81, 0x98, 0x14, 0xef, 0x58, 0x28]),
                   (7, String::from("sbb word cs:[bx + si - 4332], 10328")));
        assert_eq!(disassemble(&[0xf0, 0x2e, 0xf6, 0x96, 0xb1, 0x26]),
                   (6, String::from("lock not byte cs:[bp + 9905]")));
    }

    #[test]
    fn test_intersegment_calls() {
        assert_eq!(disassemble(&[0x9a, 0xc8, 0x01, 0x7b, 0x00]),
                   (5, String::from("call 123:456")));
        assert_eq!(disassemble(&[0xea, 0x22, 0x00, 0x15, 0x03]),
                   (5, String::from("jmp 789:34")));
    }

    #[test]
    fn test_direct_within_segment_calls() {
        assert_eq!(disassemble(&[0xe9, 0xd9, 0x06]),
                   (3, String::from("jmp #OFFSET# 1753")));
        assert_eq!(disassemble(&[0xe9, 0x39, 0x0a]),
                   (3, String::from("jmp #OFFSET# 2617")));
        assert_eq!(disassemble(&[0xe8, 0x16, 0x2e]),
                   (3, String::from("call #OFFSET# 11798")));
    }

    #[test]
    fn test_ret_retf_instructions() {
        assert_eq!(disassemble(&[0xca, 0x94, 0x44]),
                   (3, String::from("retf 17556")));
        assert_eq!(disassemble(&[0xc2, 0x98, 0x44]),
                   (3, String::from("ret 17560")));
        assert_eq!(disassemble(&[0xcb]),
                   (1, String::from("retf")));
        assert_eq!(disassemble(&[0xc3]),
                   (1, String::from("ret")));
    }

    #[test]
    fn test_more_call_jump_instructions() {
        assert_eq!(disassemble(&[0xff, 0x52, 0xc6]),
                   (3, String::from("call [bp + si - 58]")));
        assert_eq!(disassemble(&[0xff, 0x5a, 0xc6]),
                   (3, String::from("call far [bp + si - 58]")));
        assert_eq!(disassemble(&[0xff, 0x25]),
                   (2, String::from("jmp [di]")));
        assert_eq!(disassemble(&[0xff, 0x2d]),
                   (2, String::from("jmp far [di]")));
        assert_eq!(disassemble(&[0xea, 0x88, 0x77, 0x66, 0x55]),
                   (5, String::from("jmp 21862:30600")));
    }

    #[test]
    fn test_machine_state() {
        let m: Machine = Machine::new();
        assert_eq!(m.reg_value(Reg::Al), 0);
        assert_eq!(m.reg_value(Reg::Ah), 0);
        assert_eq!(m.reg_value(Reg::Ax), 0);
        assert_eq!(m.reg_value(Reg::Bl), 0);
        assert_eq!(m.reg_value(Reg::Bh), 0);
        assert_eq!(m.reg_value(Reg::Bx), 0);
        assert_eq!(m.reg_value(Reg::Cl), 0);
        assert_eq!(m.reg_value(Reg::Ch), 0);
        assert_eq!(m.reg_value(Reg::Cx), 0);
        assert_eq!(m.reg_value(Reg::Dl), 0);
        assert_eq!(m.reg_value(Reg::Dh), 0);
        assert_eq!(m.reg_value(Reg::Dx), 0);
        assert_eq!(m.reg_value(Reg::Sp), 0);
        assert_eq!(m.reg_value(Reg::Bp), 0);
        assert_eq!(m.reg_value(Reg::Si), 0);
        assert_eq!(m.reg_value(Reg::Di), 0);
    }

    #[test]
    fn test_machine_state_after_mov() {
        let mut m: Machine = Machine::new();
        let instructions = vec![
            Instruction::src_dst(Command::Mov,
                                 Operand::ImmU16(0xdead),
                                 Operand::Reg(Reg::Ax)),
            Instruction::src_dst(Command::Mov,
                                 Operand::ImmU16(0xbeef),
                                 Operand::Reg(Reg::Bx)),
            Instruction::src_dst(Command::Mov,
                                 Operand::ImmU16(0xabcd),
                                 Operand::Reg(Reg::Cx)),
            Instruction::src_dst(Command::Mov,
                                 Operand::ImmU16(0xef01),
                                 Operand::Reg(Reg::Dx)),
            Instruction::src_dst(Command::Mov,
                                 Operand::ImmU16(0x1),
                                 Operand::Reg(Reg::Sp)),
            Instruction::src_dst(Command::Mov,
                                 Operand::ImmU16(0x2),
                                 Operand::Reg(Reg::Bp)),
            Instruction::src_dst(Command::Mov,
                                 Operand::ImmU16(0x3),
                                 Operand::Reg(Reg::Si)),
            Instruction::src_dst(Command::Mov,
                                 Operand::ImmU16(0x4),
                                 Operand::Reg(Reg::Di)),
        ];

        m.execute(&instructions);
        assert_eq!(m.reg_value(Reg::Al), 0xad);
        assert_eq!(m.reg_value(Reg::Ah), 0xde);
        assert_eq!(m.reg_value(Reg::Ax), 0xdead);
        assert_eq!(m.reg_value(Reg::Bl), 0xef);
        assert_eq!(m.reg_value(Reg::Bh), 0xbe);
        assert_eq!(m.reg_value(Reg::Bx), 0xbeef);
        assert_eq!(m.reg_value(Reg::Cl), 0xcd);
        assert_eq!(m.reg_value(Reg::Ch), 0xab);
        assert_eq!(m.reg_value(Reg::Cx), 0xabcd);
        assert_eq!(m.reg_value(Reg::Dl), 0x01);
        assert_eq!(m.reg_value(Reg::Dh), 0xef);
        assert_eq!(m.reg_value(Reg::Dx), 0xef01);
        assert_eq!(m.reg_value(Reg::Sp), 0x1);
        assert_eq!(m.reg_value(Reg::Bp), 0x2);
        assert_eq!(m.reg_value(Reg::Si), 0x3);
        assert_eq!(m.reg_value(Reg::Di), 0x4);
    }
}
