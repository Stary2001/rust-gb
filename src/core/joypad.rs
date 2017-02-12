use core::mem::MemBlock;

pub struct Joypad
{
}

impl MemBlock for Joypad
{
	fn read8(&self, loc: u16) -> u8
	{
		match loc - 0xff00
		{
			0 => 0, // joy
			_ => panic!("unimplemented joypad read")
		}
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		match loc - 0xff00
		{
			0 => (),
			_ => panic!("unimplemented joypad write")
		};
	}
}
