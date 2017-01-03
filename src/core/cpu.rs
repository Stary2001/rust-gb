use core::mem::MemBlock;
use core::mem::RAMBlock;
use core::mem::ROMBlock;
use core::mem::EmptyBlock;

use core::ppu::PPU;
use core::sound::Sound;

use std::ops::*;
use std::fmt::Display;
use std::fmt;

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
	pub bios: Option<ROMBlock>, // BIOS will shadow the first part of cart when enabled
	pub bios_enabled: bool,
	pub sound: Sound,
	pub ppu: PPU,
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
	LoadDerefImm(u16, Reg8),
	StoreDerefImm(Reg8, u16),

	Inc8(Reg8),
	Dec8(Reg8),
	Inc16(Reg16),
	Dec16(Reg16),

	Add(Reg8),
	AddCarry(Reg8),
	Sub(Reg8),
	SubCarry(Reg8),
	And(Reg8),
	Xor(Reg8),
	Or(Reg8),
	Cp(Reg8),
	CpImm(u8),

	Rlc(Reg8),
	Rrc(Reg8),
	Rl(Reg8),
	Rr(Reg8),
	Sla(Reg8),
	Sra(Reg8),
	Swap(Reg8),
	Srl(Reg8),
	Bit(u8, Reg8),

	Call(u16),
	Ret(),
	Jr(u8),
	JrFlag(u8, CPUFlag, bool),

	Push(Reg16),
	Pop(Reg16),

	Halt(),
	HiReadReg(),
	HiReadImm(u8),
	HiWriteImm(u8),
	HiWriteReg(),
	Invalid(u16)
}

impl Instr
{
	fn size(i: Instr) -> u16
	{
		match i
		{
			Rl(Reg8::A) => 1,
			Instr::LoadImm8(_, _) => 2,
			Instr::LoadImm16(_, _) => 3,
			JrFlag(_, _, _) | Jr(_) => 2,
			Rlc(_) |
			Rrc(_) |
			Rl(_) |
			Rr(_) |
			Sla(_) |
			Sra(_) | 
			Swap(_) |
			Srl(_) |
			Bit(_, _) => 2,

			Call(_) => 3,
			CpImm(_) => 2,
			LoadDerefImm(_, _) |
			StoreDerefImm(_, _) => 3,
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

		println!("Instr: {:?} at pc {:x}", i, self.regs.pc);
		match i
		{
			LoadImm8(r, i) => { self.regs.set8(r, i) },
			LoadImm16(r, i) => { self.regs.set16(r, i) },
			LoadReg8(to, from) => { let v = self.regs.get8(from); self.regs.set8(to, v) },
			LoadReg16(to, from) => { let v = self.regs.get16(from); self.regs.set16(to, v) },
			LoadDeref8(from, reg, flag) => {
											let mut loc = self.regs.get16(reg);
											

											let v = self.read8(loc);
											self.regs.set8(from, v);

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
												self.regs.set16(reg, loc);
											}
										},

			StoreDeref8(reg, to, flag) => {
											let v = self.regs.get8(to);
											let mut loc = self.regs.get16(reg);
											
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
												println!("inc/dec {:?} -> {:x}", reg, loc);
												self.regs.set16(reg, loc);
											}
										},
			LoadDerefImm(imm, reg) => 
			{
				let v = self.read8(imm);
				self.regs.set8(reg, v);
			},

			StoreDerefImm(reg, imm) =>
			{
				let v = self.regs.get8(reg);
				self.write8(imm, v);
			},

			Call(off) =>
			{
				let pc = self.regs.pc;
				self.push(pc + 3);
				self.regs.pc = off;
				jumped = true;
			},

			Ret() =>
			{
				let pc = self.pop();
				self.regs.pc = pc;
				jumped = true;
			}

			JrFlag(off, flag, expected) =>
			{
				if self.regs.test_flag(flag) == expected
				{
					self.exec(Jr(off));
					jumped = true;
				}
			}

			Jr(off) =>
			{
				let mut pc = self.regs.get16(Reg16::PC);
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
				self.regs.set16(Reg16::PC, pc);

				jumped = true;
			}

			Inc8(r) => { let v = self.regs.get8(r); self.regs.set8(r, v+1); self.regs.set_flag(CPUFlag::Zero, v == 0xff); },
			Inc16(r) => { let v = self.regs.get16(r); self.regs.set16(r, v+1); self.regs.set_flag(CPUFlag::Zero, v == 0xff); },
			Dec8(r) => { let v = self.regs.get8(r); self.regs.set8(r, v-1); self.regs.set_flag(CPUFlag::Zero, v == 1); },
			Dec16(r) => { let v = self.regs.get16(r); self.regs.set16(r, v-1); self.regs.set_flag(CPUFlag::Zero, v == 1); },

			CpImm(imm) => {
				let v = self.regs.get8(Reg8::A);
				self.regs.set_flag(CPUFlag::Zero, v == imm);	
			}
			// Add()
			// Adc()
			// Sub()
			// Sbc()
			// And()
			Xor(r) => { let v = self.regs.get8(Reg8::A); let vv = self.regs.get8(r); self.regs.set8(Reg8::A, v ^ vv)},

			Rl(r) => 
			{
				let mut v = self.regs.get8(r);
				let set_carry = v & 0x80 == 0x80;
				v = (v << 1);
				if self.regs.test_flag(CPUFlag::Carry)
				{
					v |= 1;
				}
				self.regs.set_flag(CPUFlag::Carry, set_carry)
			},

			Bit(n, r) =>
			{
				let v = self.regs.get8(r);
				let f = (v & (1 << n) == (1 << n));
				println!("{} {} {}", f, v, 1<<n);
				self.regs.set_flag(CPUFlag::Zero, !f);
			},

			HiReadImm(off) =>
			{
				let v = self.read8(0xff00 + off as u16);
				self.regs.set8(Reg8::A, v);
			},

			HiReadReg() => 
			{
				let v = self.read8(0xff00 + self.regs.get8(Reg8::C) as u16);
				self.regs.set8(Reg8::A, v);
			},

			HiWriteImm(off) => { let v  = self.regs.get8(Reg8::A); self.write8(0xff00 + off as u16, v) },

			HiWriteReg() => { let v  = self.regs.get8(Reg8::A); let off = self.regs.get8(Reg8::C); self.write8(0xff00 + off as u16, v) },

			Push(reg) => { let v = self.regs.get16(reg); self.push(v); },
			Pop(reg) => { let v = self.pop(); self.regs.set16(reg, v); },

			Instr::Invalid(aa) => panic!("Invalid instr 0x{:x}", aa),
			_ => panic!("Unimplemented instr!!")
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

			0x17 => Rl(Reg8::A),
			0x18 => Jr(self.read8(self.regs.pc + 1)),
			0x20 => JrFlag(self.read8(self.regs.pc + 1), CPUFlag::Zero, false),
			0x30 => JrFlag(self.read8(self.regs.pc + 1), CPUFlag::Carry, false),

			0x28 => JrFlag(self.read8(self.regs.pc + 1), CPUFlag::Zero, true),

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
			0x78...0x7f => LoadReg8(Reg8::C, reg_lut[(i&0x0f - 8) as usize]),

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

			0xc1 | 0xd1 | 0xe1 => Pop(reg16_lut[(((i&0xf0) >> 4)- 0xc) as usize]),
			0xf1 => Pop(Reg16::AF),

			0xc5 | 0xd5 | 0xe5 => Push(reg16_lut[(((i&0xf0) >> 4)- 0xc) as usize]),
			0xf5 => Push(Reg16::AF),


			0xcb => 
			{
				let i = self.read8(self.regs.pc + 1);
				match i
				{
					0x00...0x07 => Rlc(reg_lut[(i&0x0f) as usize]),
					0x08...0x0f => Rrc(reg_lut[(i&0x0f) as usize]),

					0x10...0x17 => Rl(reg_lut[(i&0x0f) as usize]),
					0x18...0x1f => Rr(reg_lut[(i&0x0f) as usize]),

					0x20...0x27 => Sla(reg_lut[(i&0x0f) as usize]),
					0x28...0x2f => Sra(reg_lut[(i&0x0f) as usize]),

					0x30...0x37 => Swap(reg_lut[(i&0x0f) as usize]),
					0x38...0x3f => Srl(reg_lut[(i&0x0f) as usize]),

					0x40...0x7f =>  {
										let mut n = (((i & 0xf0) >> 4) - 4) * 2;
										n += (i & 0x0f) / 8;
										Bit(n, reg_lut[((i&0x0f) % 8) as usize])
									},
					_ => Instr::Invalid(0xcb00 | i as u16)
				}
			},

			0xc9 => Ret(),
			0xcd => Call(self.read16(self.regs.pc + 1)),

			0xe0 => HiWriteImm(self.read8(self.regs.pc + 1)),
			0xe2 => HiWriteReg(),
			0xea => StoreDerefImm(Reg8::A, self.read16(self.regs.pc + 1)),
			0xf0 => HiReadImm(self.read8(self.regs.pc + 1)),
			0xfa => LoadDerefImm(self.read16(self.regs.pc + 1), Reg8::A),
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
			0x8000 ... 0x9fff => &self.vram,
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
			0x8000 ... 0x9fff => &mut self.vram,
			0xff10...0xff26 => &mut self.sound,
			0xff40...0xff49 => &mut self.ppu,
			0xff80...0xfffe => &mut self.hram,
			_ => panic!("unmapped mem access at 0x{:x}", loc)
		}
	}

	pub fn read8(&self, loc: u16) -> u8
	{
		println!("Read8 at 0x{:x}", loc);
		return self.map(loc).read8(loc);
	}

	pub fn write8(&mut self, loc: u16, v: u8)
	{
		println!("Write to 0x{:x}: 0x{:x}", loc, v);
		self.map_mut(loc).write8(loc, v);
	}

	pub fn read16(&self, loc: u16) -> u16
	{
		let r = self.map(loc);
		let a: u16 = r.read8(loc) as u16 | (r.read8(loc + 1) as u16) << 8;
		println!("Read16 at 0x{:x} => 0x{:x}", loc, a);
		a
	}

	pub fn write16(&mut self, loc: u16, v: u16)
	{
		println!("Write16 at 0x{:x}, 0x{}", loc, v);
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

		let mut c = CPU
		{
			regs: CPURegs{a: 0, b: 0, c: 0, d: 0, e: 0, h: 0, l: 0, f: 0, sp: 0, pc: 0},

			vram: RAMBlock { base: 0x8000, v: vram}, // 8k vram
			wram: RAMBlock { base: 0xc000, v: wram}, // 8k wram
			hram: RAMBlock { base: 0xff80, v: hram},
			cart: None,
			bios: None,
			bios_enabled: true,
			ppu: PPU{},
			sound: Sound{}
		};

		c
	}
}

impl CPURegs
{
	fn get8(&self, r: Reg8) -> u8
	{
		match r
		{
			Reg8::A => self.a,
			Reg8::B => self.b,
			Reg8::C => self.c,
			Reg8::D => self.d,
			Reg8::E => self.e,
			Reg8::H => self.h,
			Reg8::L => self.l,
			Reg8::F => self.f,
			Reg8::DerefHL => panic!("Trying to get deref!")
		}
	}

	fn get16(&self, r: Reg16) -> u16
	{
		match r
		{
			Reg16::AF => self.f as u16 | (self.a as u16) << 8,
			Reg16::BC => self.c as u16 | (self.b as u16) << 8,
			Reg16::DE => self.e as u16 | (self.d as u16) << 8,
			Reg16::HL => self.l as u16 | (self.h as u16) << 8,
			Reg16::SP => self.sp,
			Reg16::PC => self.pc
		}
	}

	fn set8(&mut self, r: Reg8, v: u8)
	{
		println!("setting {:?} to 0x{:x}", r, v);
		match r
		{
			Reg8::A => self.a = v,
			Reg8::B => self.b = v,
			Reg8::C => self.c = v,
			Reg8::D => self.d = v,
			Reg8::E => self.e = v,
			Reg8::H => self.h = v,
			Reg8::L => self.l = v,
			Reg8::F => self.f = v,
			Reg8::DerefHL => panic!("Trying to set deref!")
		}
	}

	fn set16(&mut self, r: Reg16, v: u16)
	{
		println!("setting {:?} to 0x{:x}", r, v);
		match r
		{
			Reg16::AF => { self.a = ((v>>8) & 0xff) as u8; self.f = (v&0xff) as u8; },
			Reg16::BC => { self.b = ((v>>8) & 0xff) as u8; self.c = (v&0xff) as u8; },
			Reg16::DE => { self.d = ((v>>8) & 0xff) as u8; self.e = (v&0xff) as u8; },
			Reg16::HL => { self.h = ((v>>8) & 0xff) as u8; self.l = (v&0xff) as u8; },
			Reg16::SP => { self.sp = v },
			Reg16::PC => { self.pc = v },
		}
	}

	pub fn test_flag(&mut self, flag: CPUFlag) -> bool
	{
		self.f & (flag as u8) == (flag as u8)
	}

	pub fn set_flag(&mut self, flag: CPUFlag, state: bool)
	{
		if state == true
		{
			self.f |= flag as u8;
		}
		else
		{
			self.f &= !(flag as u8);
		}
	}
}