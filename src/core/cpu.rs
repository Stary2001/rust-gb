use core::mem::MemBlock;
use core::mem::RAMBlock;
use core::mem::ROMBlock;
use core::mem::EmptyBlock;

use core::ppu::PPU;
use core::sound::Sound;
use core::timer::Timer;
use core::serial::Serial;

struct CPURegs
{
	a: u8,
	b: u8,
	c: u8,
	d: u8,
	e: u8,
	h: u8,
	l: u8,
	f: u8,
	
	sp: u16,
	pc: u16
}

pub struct CPU
{
	regs: CPURegs,
	pub vram: RAMBlock,
	pub wram: RAMBlock,
	pub hram: RAMBlock,
	pub cart: Option<ROMBlock>,
	pub cart_ram: Option<RAMBlock>,
	pub bios: Option<ROMBlock>, // BIOS will shadow the first part of cart when enabled
	pub bios_enabled: bool,
	pub interrupts_enabled: bool,
	pub sound: Sound,
	pub ppu: PPU,
	pub timer: Timer,
	pub link_port: Serial,
	pub pending_interrupts: u8,
	pub enabled_interrupts: u8,
}

#[derive(Copy, Clone, Debug)]
pub enum Reg8
{
	A = 0,
	B,
	C,
	D,
	E,
	H,
	L,
	F,
	DerefHL
}

#[derive(Copy, Clone, Debug)]
pub enum Reg16
{
	AF = 0,
	BC,
	DE,
	HL,
	SP,
	PC
}

#[derive(Copy, Clone, Debug)]
pub enum CPUFlag
{
	Carry = 1 << 4,
	Zero = 1 << 7,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum InstrFlag
{
	None = 0,
	Dec,
	Inc
}

#[derive(Debug)]
pub enum Instr
{
	Nop(),
	LoadImm8(Reg8, u8),
	LoadImm16(Reg16, u16),
	LoadReg8(Reg8, Reg8),
	LoadReg16(Reg16, Reg16),
	LoadDeref8(Reg8, Reg16, InstrFlag),
	StoreDeref8(Reg16, Reg8, InstrFlag),
	StoreDerefSp(Reg16, u16),
	LoadDerefImm(u16, Reg8),
	StoreDerefImm(Reg8, u16),

	Inc8(Reg8),
	Dec8(Reg8),
	Inc16(Reg16),
	Dec16(Reg16),

	Add(Reg8),
	AddImm(u8),
	AddCarry(Reg8),
	AddCarryImm(u8),
	Add16(Reg16, Reg16),
	Sub(Reg8),
	SubImm(u8),
	SubCarry(Reg8),
	SubCarryImm(u8),
	And(Reg8),
	AndImm(u8),
	Xor(Reg8),
	XorImm(u8),
	Or(Reg8),
	OrImm(u8),
	Cp(Reg8),
	CpImm(u8),
	Not(Reg8),
	NotCarry(),
	SetCarry(),

	Rlc(Reg8),
	RlcA(),
	Rrc(Reg8),
	RrcA(),
	Rl(Reg8),
	RlA(),
	Rr(Reg8),
	RrA(),
	Sla(Reg8),
	Sra(Reg8),
	Swap(Reg8),
	Srl(Reg8),
	Bit(u8, Reg8),
	Res(u8, Reg8),

	Call(u16),
	CallFlag(u16, CPUFlag, bool),
	Ret(),
	RetFlag(CPUFlag, bool),
	Jr(u8),
	JrFlag(u8, CPUFlag, bool),
	Jp(u16),
	JpFlag(u16, CPUFlag, bool),
	JpReg(Reg16),

	Push(Reg16),
	Pop(Reg16),

	Halt(),
	SetInterruptFlag(bool),
	HiReadReg(),
	HiReadImm(u8),
	HiWriteImm(u8),
	HiWriteReg(),

	Daa(),

	Invalid(u16)
}

impl Instr
{
	fn size(i: Instr) -> u16
	{
		match i
		{
			Instr::LoadImm8(_, _) => 2,
			Instr::LoadImm16(_, _) => 3,
			JrFlag(_, _, _) | Jr(_) => 2,
			Jp(_) | JpFlag(_, _, _) => 3,
			Rlc(_) |
			Rrc(_) |
			Rl(_) |
			Rr(_) |
			Sla(_) |
			Sra(_) | 
			Swap(_) |
			Srl(_) |
			Bit(_, _) => 2,

			StoreDerefSp(_, _) => 3,
			Call(_) | CallFlag(_, _, _) => 3,
			AddImm(_) | AddCarryImm(_) | SubImm(_) | AndImm(_) | OrImm(_) | XorImm(_) | CpImm(_) => 2,
			LoadDerefImm(_, _) |
			StoreDerefImm(_, _) => 3,
			HiReadImm(_) | HiWriteImm(_) => 2,
			_ => 1
		}
	}
}

use core::cpu::Instr::*;

impl CPU
{
	pub fn exec(&mut self, i: Instr)
	{
		let mut jumped: bool = false;

		match i
		{
			Nop() => (),
			LoadImm8(r, i) => { self.set_reg8(r, i) },
			LoadImm16(r, i) => { self.set_reg16(r, i) },
			LoadReg8(to, from) => { let v = self.get_reg8(from); self.set_reg8(to, v) },
			LoadReg16(to, from) => { let v = self.get_reg16(from); self.set_reg16(to, v) },
			LoadDeref8(from, reg, flag) => {
											let mut loc = self.get_reg16(reg);
											

											let v = self.read8(loc);
											self.set_reg8(from, v);

											if flag == InstrFlag::Dec
											{
												loc -= 1;
											}
											if flag == InstrFlag::Inc
											{
												loc += 1;
											}

											if flag != InstrFlag::None
											{
												self.set_reg16(reg, loc);
											}
										},

			StoreDeref8(reg, to, flag) => {
											let v = self.get_reg8(to);
											let mut loc = self.get_reg16(reg);
											
											self.write8(loc, v);

											if flag == InstrFlag::Dec
											{
												loc -= 1;
											}
											
											if flag == InstrFlag::Inc
											{
												loc += 1;
											}

											if flag != InstrFlag::None
											{
												self.set_reg16(reg, loc);
											}
										},

			StoreDerefSp(reg, imm) => 
			{
				let sp = self.get_reg16(reg);
				self.write16(imm, sp);
			},

			LoadDerefImm(imm, reg) => 
			{
				let v = self.read8(imm);
				self.set_reg8(reg, v);
			},

			StoreDerefImm(reg, imm) =>
			{
				let v = self.get_reg8(reg);
				self.write8(imm, v);
			},

			CallFlag(off, flag, expected) =>
			{
				if self.test_flag(flag) == expected
				{
					self.exec(Call(off));
					jumped = true;
				}
			},

			Call(off) =>
			{
				let pc = self.regs.pc;
				self.push(pc + 3);
				self.regs.pc = off;

				jumped = true;
			},

			RetFlag(flag, expected) =>
			{
				if self.test_flag(flag) == expected
				{
					self.exec(Ret());
					jumped = true;
				}
			},

			Ret() =>
			{
				let pc = self.pop();
				self.regs.pc = pc;
				jumped = true;
			},

			JrFlag(off, flag, expected) =>
			{
				if self.test_flag(flag) == expected
				{
					self.exec(Jr(off));
					jumped = true;
				}
			}

			Jr(off) =>
			{
				let mut pc = self.get_reg16(Reg16::PC);
				pc += 2;
				let off_ = off as i8;
				if off_ > 0
				{
					pc += off as u16;
				}
				else
				{
					pc -= (-off_) as u16;
				}
				self.set_reg16(Reg16::PC, pc);

				jumped = true;
			},

			JpFlag(off, flag, expected) =>
			{
				if self.test_flag(flag) == expected
				{
					self.exec(Jp(off));
					jumped = true;
				}
			},

			Jp(imm) =>
			{
				self.set_reg16(Reg16::PC, imm);
				jumped = true;
			},

			JpReg(r) =>
			{
				let loc = self.get_reg16(r);
				self.set_reg16(Reg16::PC, loc);
				jumped = true;
			},

			Inc8(r) => { let v = self.get_reg8(r); self.set_reg8(r, v.wrapping_add(1)); self.set_flag(CPUFlag::Zero, v == 0xff); },
			Inc16(r) => { let v = self.get_reg16(r); self.set_reg16(r, v.wrapping_add(1)); self.set_flag(CPUFlag::Zero, v == 0xff); },
			Dec8(r) => { let v = self.get_reg8(r); self.set_reg8(r, v.wrapping_sub(1)); self.set_flag(CPUFlag::Zero, v == 1); },
			Dec16(r) => { let v = self.get_reg16(r); self.set_reg16(r, v.wrapping_sub(1)); self.set_flag(CPUFlag::Zero, v == 1); },

			CpImm(imm) => {
				let v = self.get_reg8(Reg8::A);
				self.set_flag(CPUFlag::Zero, v == imm);	
				self.set_flag(CPUFlag::Carry, v < imm); // overflow WOULD occur if v < imm.
			}

			AddImm(imm) =>
			{
				let v = self.get_reg8(Reg8::A);
				let res = v.wrapping_add(imm);
				self.set_reg8(Reg8::A, res);
				self.set_flag(CPUFlag::Carry, res < v);
				self.update_flags8(Reg8::A)
			},

			Add(r) =>
			{
				let v = self.get_reg8(Reg8::A);
				let vv = self.get_reg8(r);
				let res = v.wrapping_add(vv);

				self.set_reg8(Reg8::A, res);
				self.set_flag(CPUFlag::Carry, res < v);
				self.update_flags8(Reg8::A)
			},

			AddCarry(r) =>
			{
				let v = self.get_reg8(Reg8::A);
				let vv = self.get_reg8(r);
				let mut c = 0;
				if self.test_flag(CPUFlag::Carry)
				{
					c = 1;
				}
				let res = v.wrapping_add(vv).wrapping_add(c);
				self.set_flag(CPUFlag::Carry, res < v);
				self.set_reg8(Reg8::A, res);
				self.update_flags8(Reg8::A)
			},

			AddCarryImm(imm) =>
			{
				let v = self.get_reg8(Reg8::A);
				let mut c = 0;
				if self.test_flag(CPUFlag::Carry)
				{
					c = 1;
				}
				let res = v.wrapping_add(imm).wrapping_add(c);
				self.set_flag(CPUFlag::Carry, res < v);
				self.set_reg8(Reg8::A, res);
				self.update_flags8(Reg8::A)
			},

			Add16(dst, src) =>
			{
				let v = self.get_reg16(dst);
				let vv = self.get_reg16(src);
				let res = v.wrapping_add(vv);

				self.set_flag(CPUFlag::Carry, res < v);
				self.set_reg16(dst, res);
				self.update_flags16(dst)
			}

			SubImm(imm) =>
			{
				let v = self.get_reg8(Reg8::A);
				let res = v.wrapping_sub(imm);

				self.set_flag(CPUFlag::Carry, res > v);

				self.set_reg8(Reg8::A, res);
				self.update_flags8(Reg8::A)
			},

			Sub(r) =>
			{
				let v = self.get_reg8(Reg8::A);
				let vv = self.get_reg8(r);
				let res = v.wrapping_sub(vv);

				self.set_flag(CPUFlag::Carry, res > v);
				self.set_reg8(Reg8::A, res);
				self.update_flags8(Reg8::A)
			},

			SubCarry(r) =>
			{
				let v = self.get_reg8(Reg8::A);
				let vv = self.get_reg8(r);
				let mut c = 0;
				if self.test_flag(CPUFlag::Carry)
				{
					c = 1;
				}
				let res = v.wrapping_sub(vv).wrapping_sub(c);
				self.set_flag(CPUFlag::Carry, res > v);
				self.set_reg8(Reg8::A, res);
				self.update_flags8(Reg8::A)
			},

			SubCarryImm(imm) =>
			{
				let v = self.get_reg8(Reg8::A);
				let mut c = 0;
				if self.test_flag(CPUFlag::Carry)
				{
					c = 1;
				}
				let res = v.wrapping_sub(imm).wrapping_sub(c);
				self.set_flag(CPUFlag::Carry, res > v);
				self.set_reg8(Reg8::A, res);
				self.update_flags8(Reg8::A)
			},

			And(r) => { let v = self.get_reg8(Reg8::A); let vv = self.get_reg8(r); self.set_reg8(Reg8::A, v & vv); self.update_flags8(Reg8::A) },
			AndImm(imm) => { let v = self.get_reg8(Reg8::A); self.set_reg8(Reg8::A, v & imm); self.update_flags8(Reg8::A)},
			Xor(r) => { let v = self.get_reg8(Reg8::A); let vv = self.get_reg8(r); self.set_reg8(Reg8::A, v ^ vv); self.update_flags8(Reg8::A) },
			XorImm(imm) => { let v = self.get_reg8(Reg8::A); self.set_reg8(Reg8::A, v ^ imm); self.update_flags8(Reg8::A)},
			Or(r) => { let v = self.get_reg8(Reg8::A); let vv = self.get_reg8(r); self.set_reg8(Reg8::A, v | vv); self.update_flags8(Reg8::A) },
			OrImm(imm) => { let v = self.get_reg8(Reg8::A); self.set_reg8(Reg8::A, v | imm); self.update_flags8(Reg8::A) },
			Cp(r) => { let v = self.get_reg8(Reg8::A); let vv = self.get_reg8(r); self.set_flag(CPUFlag::Zero, v == vv); self.set_flag(CPUFlag::Carry, v < vv); },

			Not(r) =>
			{
				let v = self.get_reg8(r);
				self.set_reg8(r, !v); 
			},

			NotCarry() =>
			{
				let v = self.test_flag(CPUFlag::Carry);
				self.set_flag(CPUFlag::Carry, !v);
			},

			SetCarry() => { self.set_flag(CPUFlag::Carry, true); },

			RlcA() => { self.exec(Rlc(Reg8::A)); self.regs.pc -= 2 },
			RlA() => { self.exec(Rl(Reg8::A)); self.regs.pc -= 2 },

			Rlc(r) =>
			{
				let mut v = self.get_reg8(r);
				let set_carry = v & 0x80 == 0x80;
				v = v << 1;
				if set_carry
				{
					v |= 1;
				}
				self.set_reg8(r, v);
				self.set_flag(CPUFlag::Carry, set_carry);
				self.update_flags8(r)
			},

			Rl(r) => 
			{
				let mut v = self.get_reg8(r);
				let set_carry = v & 0x80 == 0x80;
				v = v << 1;
				if self.test_flag(CPUFlag::Carry)
				{
					v |= 1;
				}
				self.set_reg8(r, v);
				self.set_flag(CPUFlag::Carry, set_carry);
				self.update_flags8(r)
			},

			Sla(r) =>
			{
				let mut v = self.get_reg8(r);
				let set_carry = v & 1 == 1;
				v = v << 1;
				self.set_reg8(r, v);
				self.set_flag(CPUFlag::Carry, set_carry);
				self.update_flags8(r)
			},

			RrcA() => { self.exec(Rrc(Reg8::A)); self.regs.pc -= 2 },
			RrA() => { self.exec(Rr(Reg8::A)); self.regs.pc -= 2 },

			Rr(r) =>
			{
				let mut v = self.get_reg8(r);
				let set_carry = v & 1 == 1;
				v = v >> 1;
				if self.test_flag(CPUFlag::Carry)
				{
					v |= 0x80;
				}
				self.set_reg8(r, v);
				self.set_flag(CPUFlag::Carry, set_carry);
				self.update_flags8(r)
			},

			Rrc(r) =>
			{
				let mut v = self.get_reg8(r);
				let set_carry = v & 1 == 1;
				v = v >> 1;
				if set_carry
				{
					v |= 0x80;
				}
				self.set_reg8(r, v);
				self.set_flag(CPUFlag::Carry, set_carry);
				self.update_flags8(r)
			},

			Sra(r) =>
			{
				let mut v = self.get_reg8(r);
				let set_carry = v & 1 == 1;
				v = v >> 1;
				self.set_reg8(r, v);
				self.set_flag(CPUFlag::Carry, set_carry);
				self.update_flags8(r)
			},

			Srl(r) =>
			{
				let mut v = self.get_reg8(r);
				let set_carry = v & 1 == 1;
				v = v >> 1;
				self.set_reg8(r, v);
				self.set_flag(CPUFlag::Carry, set_carry);
				self.update_flags8(r)
			},

			Bit(n, r) =>
			{
				let v = self.get_reg8(r);
				let f = v & (1 << n) == (1 << n);
				self.set_flag(CPUFlag::Zero, !f);
			},

			Swap(r) =>
			{
				let v = self.get_reg8(r);
				self.set_reg8(r, (v & 0xf0 >> 4) | ((v & 0x0f) << 4) );
			},

			HiReadImm(off) =>
			{
				let v = self.read8(0xff00 + off as u16);
				self.set_reg8(Reg8::A, v);
			},

			HiReadReg() => 
			{
				let v = self.read8(0xff00 + self.get_reg8(Reg8::C) as u16);
				self.set_reg8(Reg8::A, v);
			},

			HiWriteImm(off) => { let v  = self.get_reg8(Reg8::A); self.write8(0xff00 + off as u16, v) },

			HiWriteReg() => { let v  = self.get_reg8(Reg8::A); let off = self.get_reg8(Reg8::C); self.write8(0xff00 + off as u16, v) },

			Push(reg) => { let v = self.get_reg16(reg); self.push(v); },
			Pop(reg) => { let v = self.pop(); self.set_reg16(reg, v); },

			Daa() => (),

			SetInterruptFlag(val) => self.interrupts_enabled = val,
			Instr::Invalid(aa) => panic!("Invalid instr 0x{:x} at pc {:x}", aa, self.regs.pc),
			_ => panic!("Unimplemented instr!! {:?} at pc {:x}", i, self.regs.pc)
		}

		if !jumped
		{
			self.regs.pc += Instr::size(i);
		}
	}

	pub fn decode(&self) -> Instr
	{
		let i: u8 = self.read8(self.regs.pc);

		let reg_lut: Vec<Reg8> = vec![Reg8::B, Reg8::C, Reg8::D, Reg8::E, Reg8::H, Reg8::L, Reg8::DerefHL, Reg8::A];
		let reg16_lut: Vec<Reg16> = vec![Reg16::BC, Reg16::DE, Reg16::HL, Reg16::SP];

		match i
		{
			0x00 => Nop(),
			0x07 => RlcA(),
			0x0f => RrcA(),
			0x08 => StoreDerefSp(Reg16::SP, self.read16(self.regs.pc + 1)),
			// load imm 16
			0x01 | 0x11 | 0x21 | 0x31 => LoadImm16(reg16_lut[((i & 0xf0) >> 4) as usize], self.read16(self.regs.pc + 1)),

			0x03 | 0x13 | 0x23 | 0x33 => Inc16(reg16_lut[((i & 0xf0) >> 4) as usize]),
			0x04 | 0x14 | 0x24 | 0x34 => Inc8(reg_lut[(((i & 0xf0) >> 4) * 2) as usize]),
			0x05 | 0x15 | 0x25 | 0x35 => Dec8(reg_lut[(((i & 0xf0) >> 4) * 2) as usize]),

			0x06 | 0x16 | 0x26 | 0x36 => LoadImm8(reg_lut[(((i & 0xf0) >> 4) * 2) as usize], self.read8(self.regs.pc + 1)),
			0x0a | 0x1a => LoadDeref8(Reg8::A, reg16_lut[((i & 0xf0) >> 4) as usize], InstrFlag::None),
			0x0b | 0x1b | 0x2b | 0x3b => Dec16(reg16_lut[((i & 0xf0) >> 4) as usize]),

			0x0c | 0x1c | 0x2c | 0x3c => Inc8(reg_lut[(((i & 0xf0) >> 4) * 2 + 1) as usize]),
			0x0d | 0x1d | 0x2d | 0x3d => Dec8(reg_lut[(((i & 0xf0) >> 4) * 2 + 1) as usize]),

			0x0e | 0x1e | 0x2e | 0x3e => LoadImm8(reg_lut[(((i & 0xf0) >> 4) * 2 + 1) as usize], self.read8(self.regs.pc + 1)),

			0x2 =>  StoreDeref8(Reg16::BC, Reg8::A, InstrFlag::None),
			0x12 => StoreDeref8(Reg16::DE, Reg8::A, InstrFlag::None),

			0x17 => RlA(),
			0x18 => Jr(self.read8(self.regs.pc + 1)),
			0x1f => RrA(),

			0x20 => JrFlag(self.read8(self.regs.pc + 1), CPUFlag::Zero, false),
			0x30 => JrFlag(self.read8(self.regs.pc + 1), CPUFlag::Carry, false),

			0x27 => Daa(),
			0x28 => JrFlag(self.read8(self.regs.pc + 1), CPUFlag::Zero, true),
			0x9 | 0x19 | 0x29 | 0x39 => Add16(Reg16::HL, reg16_lut[((i & 0xf0) >> 4) as usize]),
			0x2f => Not(Reg8::A),
			0x3f => NotCarry(),
			0x37 => SetCarry(),
			0x38 => JrFlag(self.read8(self.regs.pc + 1), CPUFlag::Carry, true),

			0xc2 => JpFlag(self.read16(self.regs.pc + 1), CPUFlag::Zero, false),
			0xc3 => Jp(self.read16(self.regs.pc + 1)),
			// \/ gameboy specific ldi (HL), A / ldi A, (HL)
			0x22 => StoreDeref8(Reg16::HL, Reg8::A, InstrFlag::Inc),
			0x2a => LoadDeref8(Reg8::A, Reg16::HL, InstrFlag::Inc),

			// \/ gameboy specific ldd (HL), A / ldd A, (HL)
			0x32 => StoreDeref8(Reg16::HL, Reg8::A, InstrFlag::Dec),
			0x3a => LoadDeref8(Reg8::A, Reg16::HL, InstrFlag::Dec),

			// b/c load
			0x40...0x47 => LoadReg8(Reg8::B, reg_lut[(i&0x0f) as usize]),
			0x48...0x4f => LoadReg8(Reg8::C, reg_lut[(i&0x0f - 8) as usize]),

			// d/e load 
			0x50...0x57 => LoadReg8(Reg8::D, reg_lut[(i&0x0f) as usize]),
			0x58...0x5f => LoadReg8(Reg8::E, reg_lut[(i&0x0f - 8) as usize]),

			// h/l load
			0x60...0x67 => LoadReg8(Reg8::H, reg_lut[(i&0x0f) as usize]),
			0x68...0x6f => LoadReg8(Reg8::L, reg_lut[(i&0x0f - 8) as usize]),
			
			// (hl) / a load
			0x76 => Halt(),
			0x70...0x75 | 0x77 => StoreDeref8(Reg16::HL, reg_lut[(i&0x0f) as usize], InstrFlag::None),
			0x78...0x7f => LoadReg8(Reg8::A, reg_lut[(i&0x0f - 8) as usize]),

			// ADDs / ADCs
			0x80...0x87 => Add(reg_lut[(i&0x0f) as usize]),
			0x88...0x8f => AddCarry(reg_lut[(i&0x0f - 8) as usize]),
				
			// SUBs / SBCs
			0x90...0x97 => Sub(reg_lut[(i&0x0f) as usize]),
			0x98...0x9f => SubCarry(reg_lut[(i&0x0f - 8) as usize]),

			// ANDs / XORs
			0xa0...0xa7 => And(reg_lut[(i&0x0f) as usize]),
			0xa8...0xaf => Xor(reg_lut[(i&0x0f - 8) as usize]),
				
			// ORs / CPs
			0xb0...0xb7 => Or(reg_lut[(i&0x0f) as usize]),
			0xb8...0xbf => Cp(reg_lut[(i&0x0f - 8) as usize]),

			0xc0 => RetFlag(CPUFlag::Zero, false),
			0xc1 | 0xd1 | 0xe1 => Pop(reg16_lut[(((i&0xf0) >> 4)- 0xc) as usize]),
			0xf1 => Pop(Reg16::AF),

			0xc5 | 0xd5 | 0xe5 => Push(reg16_lut[(((i&0xf0) >> 4)- 0xc) as usize]),
			0xf5 => Push(Reg16::AF),

			0xc4 => CallFlag(self.read16(self.regs.pc + 1), CPUFlag::Zero, false),
			0xd4 => CallFlag(self.read16(self.regs.pc + 1), CPUFlag::Carry, false),

			0xc6 => AddImm(self.read8(self.regs.pc + 1)),
			0xd6 => SubImm(self.read8(self.regs.pc + 1)),
			0xe6 => AndImm(self.read8(self.regs.pc + 1)),
			0xf6 => OrImm(self.read8(self.regs.pc + 1)),

			0xc8 => RetFlag(CPUFlag::Zero, true),
			0xc9 => Ret(),
			0xcb => 
			{
				let i = self.read8(self.regs.pc + 1);
				match i
				{
					0x00...0x07 => Rlc(reg_lut[(i&0x0f) as usize]),
					0x08...0x0f => Rrc(reg_lut[(i&0x0f) as usize - 8]),

					0x10...0x17 => Rl(reg_lut[(i&0x0f) as usize]),
					0x18...0x1f => Rr(reg_lut[(i&0x0f) as usize - 8]),

					0x20...0x27 => Sla(reg_lut[(i&0x0f) as usize]),
					0x28...0x2f => Sra(reg_lut[(i&0x0f) as usize - 8]),

					0x30...0x37 => Swap(reg_lut[(i&0x0f) as usize]),
					0x38...0x3f => Srl(reg_lut[(i&0x0f) as usize - 8]),

					0x40...0x7f =>  {
										let mut n = (((i & 0xf0) >> 4) - 4) * 2;
										n += (i & 0x0f) / 8;
										Bit(n, reg_lut[((i&0x0f) % 8) as usize])
									},

					0x80...0xbf =>  {
										let mut n = (((i & 0xf0) >> 4) - 8) * 2;
										n += (i & 0x0f) / 8;
										Bit(n, reg_lut[((i&0x0f) % 8) as usize])
									},

					0xc0...0xff =>  {
										let mut n = (((i & 0xf0) >> 4) - 0xc) * 2;
										n += (i & 0x0f) / 8;
										Bit(n, reg_lut[((i&0x0f) % 8) as usize])
									},

					_ => Instr::Invalid(0xcb00 | i as u16)
				}
			},

			0xcd => Call(self.read16(self.regs.pc + 1)),
			0xce => AddCarryImm(self.read8(self.regs.pc + 1)),

			0xd0 => RetFlag(CPUFlag::Carry, false),
			0xd8 => RetFlag(CPUFlag::Carry, true),
			0xde => SubCarryImm(self.read8(self.regs.pc + 1)),
			0xe0 => HiWriteImm(self.read8(self.regs.pc + 1)),
			0xe2 => HiWriteReg(),
			0xe9 => JpReg(Reg16::HL),
			0xea => StoreDerefImm(Reg8::A, self.read16(self.regs.pc + 1)),
			0xee => XorImm(self.read8(self.regs.pc + 1)),
			0xf0 => HiReadImm(self.read8(self.regs.pc + 1)),
			0xf3 => SetInterruptFlag(false),
			0xf9 => LoadReg16(Reg16::SP, Reg16::HL),
			0xfa => LoadDerefImm(self.read16(self.regs.pc + 1), Reg8::A),
			0xfb => SetInterruptFlag(true),
			0xfe => CpImm(self.read8(self.regs.pc + 1)),

			_ => Instr::Invalid(i as u16),
		}
	}

	fn push(&mut self, v: u16)
	{
		self.regs.sp -= 2;
		let sp = self.regs.sp;
		self.write16(sp, v)
	}

	fn pop(&mut self) -> u16
	{
		let v = self.read16(self.regs.sp);
		self.regs.sp += 2;
		v
	}

	fn map(&self, loc: u16) -> &MemBlock
	{
		match loc
		{
			0...0xff => { if self.bios_enabled { &self.bios } else { &self.cart } }
			0x100 ... 0x3fff => &self.cart,
			0x4000 ... 0x7fff => &self.cart,
			0x8000 ... 0x9fff => &self.vram,
			0xa000 ... 0xbfff => &self.cart_ram,
			0xc000 ... 0xdfff => &self.wram,
			0xff01 ... 0xff02 => &self.link_port,
			0xff04 ... 0xff07 => &self.timer,
			0xff10...0xff26 => &self.sound,
			0xff40...0xff49 => &self.ppu,
			0xff80...0xfffe => &self.hram,
			_ => panic!("unmapped mem access at 0x{:x}", loc)
		}
	}

	fn map_mut(&mut self, loc: u16) -> &mut MemBlock
	{
		match loc
		{
			0...0xff => { if self.bios_enabled { &mut self.bios } else { &mut self.cart } }
			0x100 ... 0x3fff => &mut self.cart,
			0x4000 ... 0x7fff => &mut self.cart,
			0x8000 ... 0x9fff => &mut self.vram,
			0xa000 ... 0xbfff => &mut self.cart_ram,
			0xc000 ... 0xdfff => &mut self.wram,
			0xff04 ... 0xff07 => &mut self.timer,
			0xff01 ... 0xff02 => &mut self.link_port,
			0xff10...0xff26 => &mut self.sound,
			0xff40...0xff49 => &mut self.ppu,
			0xff80...0xfffe => &mut self.hram,
			_ => panic!("unmapped mem access at 0x{:x}", loc)
		}
	}

	pub fn read8(&self, loc: u16) -> u8
	{
		match loc
		{
			0xff0f => self.pending_interrupts,
			0xffff => self.enabled_interrupts,
			// Echo RAM
			0xe000 ... 0xfdff => { let loc_ = loc - 0x2000; self.map(loc_).read8(loc_) },
			_ => self.map(loc).read8(loc)
		}
	}

	pub fn write8(&mut self, loc: u16, v: u8)
	{
		match loc
		{
			0xff50 => self.bios_enabled = false,
			0xff0f => self.pending_interrupts = v,
			0xffff => self.enabled_interrupts = v,
			// Echo RAM
			0xe000 ... 0xfdff => { let loc_ = loc - 0x2000; self.map_mut(loc_).write8(loc_, v) },
			_ => self.map_mut(loc).write8(loc, v)
		}
	}

	pub fn read16(&self, loc: u16) -> u16
	{
		let r = self.map(loc);
		let a: u16 = r.read8(loc) as u16 | (r.read8(loc + 1) as u16) << 8;
		a
	}

	pub fn write16(&mut self, loc: u16, v: u16)
	{
		let r = self.map_mut(loc);
		r.write8(loc, (v & 0xff) as u8);
		r.write8(loc + 1, ((v & 0xff00) >> 8) as u8);
	}

	pub fn new() -> CPU
	{
		let mut vram = Vec::with_capacity(0x2000);
		vram.resize(0x2000, 0);
		let mut wram = Vec::with_capacity(0x2000);
		wram.resize(0x2000, 0);
		let mut hram = Vec::with_capacity(0x7f);
		hram.resize(0x7f, 0);

		let mut cram = Vec::with_capacity(0x2000);
		cram.resize(0x2000, 0);

		let mut c = CPU
		{
			regs: CPURegs{a: 0, b: 0, c: 0, d: 0, e: 0, h: 0, l: 0, f: 0, sp: 0, pc: 0},

			vram: RAMBlock { base: 0x8000, v: vram}, // 8k vram
			wram: RAMBlock { base: 0xc000, v: wram}, // 8k wram
			hram: RAMBlock { base: 0xff80, v: hram},
			cart: None,
			cart_ram: Some(RAMBlock {base: 0xa000, v: cram}),
			bios: None,
			bios_enabled: true,
			interrupts_enabled: false,
			pending_interrupts: 0,
			enabled_interrupts: 0,
			ppu: PPU {lcdc: 0, scanline: 0, scroll_x: 0, scroll_y: 0, palette: 0},
			sound: Sound{},
			timer: Timer{div_ticks: 0, counter: 0, modulo: 0, ctrl: 0, last_time: 0},
			link_port: Serial{clock: 0}
		};

		c
	}

	fn get_reg8(&self, r: Reg8) -> u8
	{
		match r
		{
			Reg8::A => self.regs.a,
			Reg8::B => self.regs.b,
			Reg8::C => self.regs.c,
			Reg8::D => self.regs.d,
			Reg8::E => self.regs.e,
			Reg8::H => self.regs.h,
			Reg8::L => self.regs.l,
			Reg8::F => self.regs.f,
			Reg8::DerefHL => self.read8(self.get_reg16(Reg16::HL))
		}
	}

	fn get_reg16(&self, r: Reg16) -> u16
	{
		match r
		{
			Reg16::AF => self.regs.f as u16 | (self.regs.a as u16) << 8,
			Reg16::BC => self.regs.c as u16 | (self.regs.b as u16) << 8,
			Reg16::DE => self.regs.e as u16 | (self.regs.d as u16) << 8,
			Reg16::HL => self.regs.l as u16 | (self.regs.h as u16) << 8,
			Reg16::SP => self.regs.sp,
			Reg16::PC => self.regs.pc
		}
	}

	fn set_reg8(&mut self, r: Reg8, v: u8)
	{
		match r
		{
			Reg8::A => self.regs.a = v,
			Reg8::B => self.regs.b = v,
			Reg8::C => self.regs.c = v,
			Reg8::D => self.regs.d = v,
			Reg8::E => self.regs.e = v,
			Reg8::H => self.regs.h = v,
			Reg8::L => self.regs.l = v,
			Reg8::F => self.regs.f = v,
			Reg8::DerefHL => {let loc = self.get_reg16(Reg16::HL); self.write8(loc, v); }
		}
	}

	fn set_reg16(&mut self, r: Reg16, v: u16)
	{
		match r
		{
			Reg16::AF => { self.regs.a = ((v>>8) & 0xff) as u8; self.regs.f = (v&0xff) as u8; },
			Reg16::BC => { self.regs.b = ((v>>8) & 0xff) as u8; self.regs.c = (v&0xff) as u8; },
			Reg16::DE => { self.regs.d = ((v>>8) & 0xff) as u8; self.regs.e = (v&0xff) as u8; },
			Reg16::HL => { self.regs.h = ((v>>8) & 0xff) as u8; self.regs.l = (v&0xff) as u8; },
			Reg16::SP => { self.regs.sp = v },
			Reg16::PC => { self.regs.pc = v },
		}
	}

	fn update_flags8(&mut self, r: Reg8)
	{
		let v = self.get_reg8(r);
		self.set_flag(CPUFlag::Zero, v==0);
	}

	fn update_flags16(&mut self, r: Reg16)
	{
		let v = self.get_reg16(r);
		self.set_flag(CPUFlag::Zero, v==0);
	}

	pub fn test_flag(&mut self, flag: CPUFlag) -> bool
	{
		self.regs.f & (flag as u8) == (flag as u8)
	}

	pub fn set_flag(&mut self, flag: CPUFlag, state: bool)
	{
		if state == true
		{
			self.regs.f |= flag as u8;
		}
		else
		{
			self.regs.f &= !(flag as u8);
		}
	}
}