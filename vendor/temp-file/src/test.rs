use crate::{TempFile, COUNTER};
use core::sync::atomic::Ordering;
use safe_lock::SafeLock;
use std::io::ErrorKind;

// The error tests require all tests to run single-threaded.
static LOCK: SafeLock = SafeLock::new();

#[test]
fn empty() {
    let _guard = LOCK.lock();
    let temp_file = crate::empty();
    let metadata = std::fs::metadata(temp_file.path()).unwrap();
    assert!(metadata.is_file());
    assert_eq!(0, metadata.len());
    std::fs::write(temp_file.path(), b"abc").unwrap();
    assert_eq!("abc", std::fs::read_to_string(temp_file.path()).unwrap());
    let temp_file2 = crate::empty();
    assert_ne!(temp_file.path(), temp_file2.path());
}

#[test]
fn empty_error() {
    let _guard = LOCK.lock();
    let previous_counter_value = COUNTER.load(Ordering::SeqCst);
    let _temp_file = crate::empty();
    COUNTER.store(previous_counter_value, Ordering::SeqCst);
    let any = std::panic::catch_unwind(|| crate::empty()).unwrap_err();
    let msg = any.downcast_ref::<String>().unwrap();
    assert!(
        msg.contains("error creating file"),
        "unexpected error {:?}",
        msg
    );
    assert!(msg.contains("AlreadyExists"), "unexpected error {:?}", msg);
}

#[test]
fn with_contents() {
    let _guard = LOCK.lock();
    let temp_file = crate::with_contents(b"abc");
    let metadata = std::fs::metadata(temp_file.path()).unwrap();
    assert!(metadata.is_file());
    assert_eq!(3, metadata.len());
    assert_eq!("abc", std::fs::read_to_string(temp_file.path()).unwrap());
    std::fs::write(temp_file.path(), b"def").unwrap();
    assert_eq!("def", std::fs::read_to_string(temp_file.path()).unwrap());
}

#[test]
fn temp_file_new() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap();
    println!("{:?}", temp_file.path());
    println!("{:?}", TempFile::new().unwrap().path());
    let metadata = std::fs::metadata(temp_file.path()).unwrap();
    assert!(metadata.is_file());
    assert_eq!(0, metadata.len());
    std::fs::write(temp_file.path(), b"abc").unwrap();
    assert_eq!("abc", std::fs::read_to_string(temp_file.path()).unwrap());
    let temp_file2 = TempFile::new().unwrap();
    assert_ne!(temp_file.path(), temp_file2.path());
}

#[test]
fn temp_file_new_error() {
    let _guard = LOCK.lock();
    let previous_counter_value = COUNTER.load(Ordering::SeqCst);
    let temp_file = TempFile::new().unwrap();
    COUNTER.store(previous_counter_value, Ordering::SeqCst);
    let e = TempFile::new().unwrap_err();
    assert_eq!(std::io::ErrorKind::AlreadyExists, e.kind());
    assert!(
        e.to_string()
            .starts_with(&format!("error creating file {:?}", temp_file.path())),
        "unexpected error {:?}",
        e
    );
}

#[test]
fn temp_file_with_prefix() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::with_prefix("prefix1").unwrap();
    let name = temp_file.path().file_name().unwrap();
    assert!(
        name.to_str().unwrap().starts_with("prefix1"),
        "{:?}",
        temp_file
    );
    let metadata = std::fs::metadata(temp_file.path()).unwrap();
    assert!(metadata.is_file());
    assert_eq!(0, metadata.len());
    std::fs::write(temp_file.path(), b"abc").unwrap();
    assert_eq!("abc", std::fs::read_to_string(temp_file.path()).unwrap());
    let temp_file2 = TempFile::new().unwrap();
    assert_ne!(temp_file.path(), temp_file2.path());
}

#[test]
fn temp_file_with_prefix_error() {
    let _guard = LOCK.lock();
    let previous_counter_value = COUNTER.load(Ordering::SeqCst);
    let temp_file = TempFile::with_prefix("prefix1").unwrap();
    COUNTER.store(previous_counter_value, Ordering::SeqCst);
    let e = TempFile::with_prefix("prefix1").unwrap_err();
    assert_eq!(std::io::ErrorKind::AlreadyExists, e.kind());
    assert!(
        e.to_string()
            .starts_with(&format!("error creating file {:?}", temp_file.path())),
        "unexpected error {:?}",
        e
    );
}

#[test]
fn temp_file_with_contents() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap().with_contents(b"abc").unwrap();
    let metadata = std::fs::metadata(temp_file.path()).unwrap();
    assert!(metadata.is_file());
    assert_eq!(3, metadata.len());
    assert_eq!("abc", std::fs::read_to_string(temp_file.path()).unwrap());
    std::fs::write(temp_file.path(), b"def").unwrap();
    assert_eq!("def", std::fs::read_to_string(temp_file.path()).unwrap());
}

#[test]
fn temp_file_with_contents_error() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap();
    std::fs::remove_file(temp_file.path()).unwrap();
    let temp_file_path = temp_file.path().to_path_buf();
    std::fs::create_dir(&temp_file_path).unwrap();
    let result = temp_file.with_contents(b"abc");
    std::fs::remove_dir(&temp_file_path).unwrap();
    let e = result.unwrap_err();
    assert!(
        e.to_string()
            .starts_with(&format!("error writing file {:?}", temp_file_path)),
        "unexpected error {:?}",
        e
    );
}

#[test]
fn cleanup() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap();
    let path = temp_file.path().to_path_buf();
    temp_file.cleanup().unwrap();
    assert_eq!(
        ErrorKind::NotFound,
        std::fs::metadata(&path).unwrap_err().kind()
    );
}

#[test]
fn leak_then_cleanup() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap();
    let path = temp_file.path().to_path_buf();
    temp_file.cleanup().unwrap();
    assert_eq!(
        ErrorKind::NotFound,
        std::fs::metadata(&path).unwrap_err().kind()
    );
}

#[test]
fn cleanup_already_deleted() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap();
    let path = temp_file.path().to_path_buf();
    std::fs::remove_file(&path).unwrap();
    temp_file.cleanup().unwrap();
    assert_eq!(
        ErrorKind::NotFound,
        std::fs::metadata(&path).unwrap_err().kind()
    );
}

#[test]
fn cleanup_error() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap();
    std::fs::remove_file(temp_file.path()).unwrap();
    let path = temp_file.path().to_path_buf();
    std::fs::create_dir(&path).unwrap();
    let result = temp_file.cleanup();
    std::fs::remove_dir(&path).unwrap();
    let e = result.unwrap_err();
    assert!(
        e.to_string()
            .starts_with(&format!("error removing file {:?}", path)),
        "unexpected error {:?}",
        e
    );
}

#[test]
fn test_drop() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap();
    let path = temp_file.path().to_path_buf();
    TempFile::new().unwrap();
    drop(temp_file);
    assert_eq!(
        ErrorKind::NotFound,
        std::fs::metadata(&path).unwrap_err().kind()
    );
}

#[test]
fn drop_already_deleted() {
    let _guard = LOCK.lock();
    let temp_file = TempFile::new().unwrap();
    std::fs::remove_file(temp_file.path()).unwrap();
}

#[cfg(unix)]
#[test]
fn drop_error_ignored() {
    // On Gitlab's shared CI runners, the cleanup always succeeds and the
    // test fails.  So we skip this test when it's running on Gitlab CI.
    if std::env::current_dir().unwrap().starts_with("/builds/") {
        println!("Running on Gitlab CI.  Skipping test.");
        return;
    }
    let _guard = LOCK.lock();
    let f = crate::empty();
    let path = f.path().to_path_buf();
    std::fs::remove_file(&path).unwrap();
    std::fs::create_dir(&path).unwrap();
    drop(f);
    std::fs::metadata(&path).unwrap();
    std::fs::remove_dir(&path).unwrap();
}

#[cfg(unix)]
#[test]
fn drop_error_panic() {
    // On Gitlab's shared CI runners, the cleanup always succeeds and the
    // test fails.  So we skip this test when it's running on Gitlab CI.
    if std::env::current_dir().unwrap().starts_with("/builds/") {
        println!("Running on Gitlab CI.  Skipping test.");
        return;
    }
    let _guard = LOCK.lock();
    let f = crate::empty().panic_on_cleanup_error();
    let path = f.path().to_path_buf();
    std::fs::remove_file(&path).unwrap();
    std::fs::create_dir(&path).unwrap();
    let result = std::panic::catch_unwind(move || drop(f));
    std::fs::metadata(&path).unwrap();
    std::fs::remove_dir(&path).unwrap();
    let msg = result.unwrap_err().downcast::<String>().unwrap();
    assert!(
        msg.contains("error removing file ",),
        "unexpected panic message {:?}",
        msg
    );
}

#[test]
fn leak() {
    let _guard = LOCK.lock();
    let f = crate::empty();
    let path = f.path().to_path_buf();
    f.leak();
    std::fs::metadata(&path).unwrap();
    std::fs::remove_file(&path).unwrap();
}
