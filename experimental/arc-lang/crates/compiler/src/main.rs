use im_rc::Vector;
use parser::parse;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
struct Opt {
    file: Option<PathBuf>,
    #[clap(short = 'v', long = "verbose")]
    verbose: bool,
}

fn main() {
    let opt = Opt::parse();

    let mut diagnostics = Vec::new();
    let mut files = Vec::new();

    if let Some(file) = opt.file {
        files.push(("{file}".to_string(), std::fs::read_to_string(file).unwrap()));
    } else {
        let mut source = String::new();
        std::io::stdin().read_to_string(&mut source).unwrap();
        files.push(("<stdin>".to_string(), source));
    };

    files.push((
        "std".to_string(),
        include_str!("../../../../../arc-lang/stdlib/std.arc").to_string(),
    ));

    let ast = files
        .iter()
        .enumerate()
        .flat_map(|(file_id, (_, source))| parse(file_id, source, &mut diagnostics))
        .collect::<Vector<_>>();

    if opt.verbose {
        ast::write::print().write_ast(&ast);
    }

    // let _ir1 = ast_to_ir1::lower::ast_to_ir1(ast);

    // if opt.verbose {
    //     println!("IR1: {:#?}", ir1);
    // }

    // let ir2 = ir1_to_ir2::lower::ir1_to_ir2(ir1);

    // if opt.verbose {
    //     println!("IR2: {:#?}", ir2);
    // }
}
