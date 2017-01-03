pub trait Window
{
	fn process_events(&mut self);
	fn do_frame(&mut self);
	fn should_quit(&self) -> bool;
}