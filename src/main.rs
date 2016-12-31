mod core;
use core::cpu::CPU;

fn main()
{
	let mut c: CPU = CPU::new();
	c.mmu.bios = core::loader::make_region_from_file("./bios.bin").unwrap();

	for n in 1..0x7000
	{
		c.step();
	}
}
