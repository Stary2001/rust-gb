use core::mem::MemBlock;

pub struct Serial
{
	pub clock: u8
}

impl MemBlock for Serial
{
	fn read8(&self, loc: u16) -> u8
	{
		match loc - 0xff01
		{
			0 => 0, // serial data.
			1 => self.clock,
			_ => panic!("unimplemented serial read")
		}
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		match loc - 0xff01
		{
			0 => println!("Serial data: {} ({:x})", (v as char).to_string(), v), // serial data.
			1 => self.clock = v,
			_ => panic!("unimplemented serial write")
		};
	}
}
