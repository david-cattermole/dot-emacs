use std::process;
use std::thread;
use std::time::Duration;

fn main() {
    println!("Rust test attach program started with PID: {}", process::id());
    println!("Waiting for debugger to attach...");

    let mut counter = 0u32;
    loop {
        println!("Running... Counter: {}", counter);
        counter += 1;

        thread::sleep(Duration::from_secs(3));

        if counter > 100 {
            println!("Exiting after 100 iterations");
            break;
        }
    }
}