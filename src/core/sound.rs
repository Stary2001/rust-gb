use core::mem::MemBlock;

pub struct Sound
{}

impl MemBlock for Sound
{
	fn read8(&self, loc: u16) -> u8
	{
		println!("Sound read at 0x{:x}!", loc);

		0
	}

	fn write8(&mut self, loc: u16, v: u8)
	{
		println!("Sound write at 0x{:x}!", loc);
	}
}
