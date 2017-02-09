extern crate time;

use core::mem::MemBlock;

pub struct Timer
{
	pub div_ticks: u8,
	pub counter: u8,
	pub modulo: u8,
	pub ctrl: u8,

	pub last_time: u64
}

impl Timer
{
	pub fn step(&mut self)
	{
		if self.last_time == 0
		{
			self.last_time = time::precise_time_ns();
			return;
		}

		let t = time::precise_time_ns();
		//self.ticks = self.ticks.wrapping_add(1)
		// do timers lol
		self.last_time = t
	}
}

impl MemBlock for Timer
{
	fn read8(&self, loc: u16) -> u8
	{
		match loc - 0xff04
		{
			0 => self.div_ticks,
			1 => self.counter,
			2 => self.modulo,
			3 => self.ctrl,
			_ => panic!("unimplemented timer read!")
		}
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		println!("Timer write at 0x{:x}!", loc);
		match loc - 0xff04
		{
			0 => self.div_ticks = 0,
			1 => self.counter = v,
			2 => self.modulo = v,
			3 => self.ctrl = v,
			_ => panic!("unimplemented timer write!")
		};
	}
}
