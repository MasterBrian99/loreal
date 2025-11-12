use std::alloc::{GlobalAlloc, Layout, System};
use std::os::raw::c_void;

#[repr(C)]
pub struct LorealValue {
    pub tag: u64,
    pub data: *mut c_void,
}

#[no_mangle]
pub extern "C" fn loreal_alloc(size: usize) -> *mut c_void {
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, 8);
        System.alloc(layout) as *mut c_void
    }
}

#[no_mangle]
pub extern "C" fn loreal_free(ptr: *mut c_void, size: usize) {
    unsafe {
        let layout = Layout::from_size_align_unchecked(size, 8);
        System.dealloc(ptr as *mut u8, layout);
    }
}
