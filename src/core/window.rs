use core::ppu::PPU;

pub trait Window
{
	fn process_events(&mut self);
	fn do_frame(&mut self, p: &PPU);
	fn should_quit(&self) -> bool;
}