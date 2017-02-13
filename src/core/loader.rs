use core::mem::ROMBlock;
use std::fs::File;
use std::io::Read;

pub fn load_from_file(name: &str) -> Option<ROMBlock>
{
	match File::open(name)
	{
		Ok(mut f) =>
		{
			let mut v: Vec<u8> = Vec::new();
			let _ = f.read_to_end(&mut v).unwrap();
			Some(ROMBlock {base: 0, v: v})
		},
		Err(_) => None,
	}
}
