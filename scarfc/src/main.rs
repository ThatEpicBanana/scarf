use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[clap(version, about)]
pub struct Args {
    #[clap(parse(from_os_str))]
    root_file: PathBuf,

    #[clap(parse(from_os_str), short, long, value_name = "DIRECTORY", default_value = ".")]
    root_directory: PathBuf,

    #[clap(parse(from_os_str), short, long, value_name = "DIRECTORY", default_value = "/../scarfc/")]
    compile_target: PathBuf,
    #[clap(parse(from_os_str), short, long, value_name = "DIRECTORY", default_value = "/../functions/")]
    interpret_target: PathBuf,

    // config file is only for weaver
    // #[clap(short, long, value_name = "FILE", parse(from_os_str))]
    // config_file: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    compile(args);
}

pub fn compile(args: Args) {
    todo!();
}
