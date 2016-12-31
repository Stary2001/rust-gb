use core::mem::MemRegion;
use std::fs::File;
use std::io::Read;

pub fn make_region_from_file(name: &str) -> Option<MemRegion>
{
	match File::open(name)
	{
		Ok(mut f) =>
		{
			let mut v: Vec<u8> = Vec::new();
			let s: usize = f.read_to_end(&mut v).unwrap();
			Some(MemRegion::Read(0, v))
		},
		Err(e) => None
	}
}
