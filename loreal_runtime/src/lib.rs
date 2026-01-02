use std::alloc::{GlobalAlloc, Layout, System};
use std::ffi::c_char;
use std::os::raw::c_void;
use std::slice;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
pub struct LorealValue {
    pub tag: u64,
    pub data: *mut c_void,
}

#[repr(C)]
pub struct RcHeader {
    pub ref_count: AtomicUsize,
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_string_alloc(len: usize) -> *mut c_void {
    unsafe {
        let header_size = std::mem::size_of::<RcHeader>();
        let total_size = header_size + len;
        let layout = Layout::from_size_align_unchecked(total_size, 8);
        let ptr = System.alloc(layout) as *mut u8;

        let header = ptr as *mut RcHeader;
        (*header).ref_count.store(1, Ordering::SeqCst);

        let string_ptr = ptr.add(header_size);
        slice::from_raw_parts_mut(string_ptr as *mut c_char, len).fill(0);

        string_ptr as *mut c_void
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_string_copy(dest: *mut c_char, src: *const c_char, len: usize) {
    unsafe {
        if !dest.is_null() && !src.is_null() {
            slice::from_raw_parts_mut(dest, len)
                .copy_from_slice(slice::from_raw_parts(src as *const c_char, len));
        }
    }
}

#[repr(C)]
pub struct LorealList {
    pub ptr: *mut LorealValue,
    pub len: usize,
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_list_alloc(len: usize, elem_size: usize) -> LorealList {
    unsafe {
        let header_size = std::mem::size_of::<RcHeader>();
        let total_size = header_size + len * elem_size;
        let layout = Layout::from_size_align_unchecked(total_size, 8);
        let ptr = System.alloc(layout) as *mut u8;

        let header = ptr as *mut RcHeader;
        (*header).ref_count.store(1, Ordering::SeqCst);

        LorealList {
            ptr: ptr.add(header_size) as *mut LorealValue,
            len,
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_list_get(list: LorealList, index: usize) -> *mut LorealValue {
    unsafe {
        if index < list.len {
            list.ptr.add(index)
        } else {
            std::ptr::null_mut()
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_eq(a: *mut LorealValue, b: *mut LorealValue) -> bool {
    unsafe {
        if a.is_null() || b.is_null() {
            return false;
        }

        let a_val = &*a;
        let b_val = &*b;

        if a_val.tag != b_val.tag {
            return false;
        }

        match a_val.tag {
            0 => {
                let a_int = (*a).data as *const i64;
                let b_int = (*b).data as *const i64;
                *a_int == *b_int
            }
            2 => {
                let a_bool = (*a).data as *const bool;
                let b_bool = (*b).data as *const bool;
                *a_bool == *b_bool
            }
            _ => a_val.data == b_val.data,
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_eq_int(a: i64, b: i64) -> bool {
    a == b
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_eq_bool(a: bool, b: bool) -> bool {
    a == b
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_alloc(size: usize) -> *mut c_void {
    unsafe {
        let header_size = std::mem::size_of::<RcHeader>();
        let total_size = header_size + size;
        let layout = Layout::from_size_align_unchecked(total_size, 8);
        let ptr = System.alloc(layout) as *mut u8;

        let header = ptr as *mut RcHeader;
        (*header).ref_count.store(1, Ordering::SeqCst);

        (ptr.add(header_size)) as *mut c_void
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_free(ptr: *mut c_void, size: usize) {
    unsafe {
        let header_size = std::mem::size_of::<RcHeader>();
        let total_size = header_size + size;
        let layout = Layout::from_size_align_unchecked(total_size, 8);

        let ptr_with_header = (ptr as *mut u8).sub(header_size);
        System.dealloc(ptr_with_header, layout);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_inc_rc(ptr: *mut c_void) {
    unsafe {
        let header_size = std::mem::size_of::<RcHeader>();
        let header = (ptr as *mut u8).sub(header_size) as *mut RcHeader;

        (*header).ref_count.fetch_add(1, Ordering::SeqCst);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn loreal_dec_rc(ptr: *mut c_void) -> bool {
    unsafe {
        let header_size = std::mem::size_of::<RcHeader>();
        let header = (ptr as *mut u8).sub(header_size) as *mut RcHeader;

        let old_count = (*header).ref_count.fetch_sub(1, Ordering::SeqCst);
        old_count == 1
    }
}
