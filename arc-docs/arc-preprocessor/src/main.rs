use mdbook::book::Book;
use mdbook::errors::Error;
use mdbook::preprocess::{CmdPreprocessor, Preprocessor, PreprocessorContext};
use std::io;
use std::process;

fn main() {
    let preprocessor = ArcLang::new();

    let mut args = std::env::args();
    let _ = args.next().unwrap();
    if args.next().is_some() {
        if preprocessor.supports_renderer(&args.next().unwrap()) {
            process::exit(0);
        } else {
            process::exit(1);
        }
    } else {
        if let Err(e) = handle_preprocessing(&preprocessor) {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}

fn handle_preprocessing(pre: &dyn Preprocessor) -> Result<(), Error> {
    let (ctx, book) = CmdPreprocessor::parse_input(io::stdin())?;

    if ctx.mdbook_version != mdbook::MDBOOK_VERSION {
        eprintln!(
            "Warning: The {} plugin was built against version {} of mdbook, \
             but we're being called from version {}",
            pre.name(),
            mdbook::MDBOOK_VERSION,
            ctx.mdbook_version
        );
    }

    let processed_book = pre.run(&ctx, book)?;
    serde_json::to_writer(io::stdout(), &processed_book)?;

    Ok(())
}

pub struct ArcLang;

impl ArcLang {
    pub fn new() -> ArcLang {
        ArcLang
    }
}

const KEYWORDS: &[&str] = &[
    "and", "or", "xor", "band", "bor", "bxor", "is", "not", "in", "class", "instance", "def",
    "task", "on", "receive", "val", "var", "fun", "mod", "extern", "enum", "type", "match", "loop",
    "for", "while", "if", "else", "return", "break", "continue", "use", "as", "from", "group",
    "window", "compute", "every", "order", "yield", "where", "desc", "of", "reduce", "after",
    "join",
];

impl Preprocessor for ArcLang {
    fn name(&self) -> &str {
        "arc-preprocessor"
    }

    fn run(&self, _: &PreprocessorContext, mut book: Book) -> Result<Book, Error> {
        preprocess_exec(&mut book);
        preprocess_grammar(&mut book);
        preprocess_snippet(&mut book);

        Ok(book)
    }

    fn supports_renderer(&self, renderer: &str) -> bool {
        renderer != "not-supported"
    }
}

/// Expand `{{exec <cmd>}}` into the output of executing `<cmd>` using bash.
fn preprocess_exec(book: &mut Book) {
    let exec_regex = regex::Regex::new(r"\{\{#exec (.*?)\}\}").unwrap();
    book.for_each_mut(|item| {
        if let mdbook::BookItem::Chapter(ch) = item {
            ch.content = exec_regex
                .replace_all(&ch.content, |caps: &regex::Captures<'_>| {
                    let s = caps.get(1).unwrap().as_str();
                    let s = std::process::Command::new("/bin/sh")
                        .arg("-c")
                        .arg(s)
                        .output()
                        .unwrap();
                    let s = s.stdout;
                    let s = std::str::from_utf8(s.as_ref()).unwrap();
                    format!("{}", s)
                })
                .into_owned();
        }
    });
}

/// Adds syntax highlighting and links to `grammar` code blocks.
fn preprocess_grammar(book: &mut Book) {
    let grammar_regex = regex::Regex::new(r"(?s)```grammar\n(.*?)```").unwrap();
    let head_regex = regex::Regex::new(r"([A-Z][A-Za-z]*)( ::=.*)").unwrap();
    let keyword_regex = regex::Regex::new(r#""([^ ]+?)""#).unwrap();
    let keyword_subst = r"<b>${1}</b>";
    let nonterm_regex = regex::Regex::new(r"\[([A-Z][A-Za-z]+)\]").unwrap();
    let nonterm_subst = r#"<a href="${1}.html#${1}">${1}</a>"#;
    let subterm_regex = regex::Regex::new(r"\[\[([A-Z][A-Za-z]+)\]\]").unwrap();
    let comment_regex = regex::Regex::new(r"( *(?:\||::=).*)#[^{](.*)").unwrap();
    let comment_subst = r#"$1<i style="color:gray">${2}</i>"#;
    book.for_each_mut(|item| {
        if let mdbook::BookItem::Chapter(ch) = item {
            let path = ch.path.as_ref().unwrap();
            let name = path.file_stem().unwrap().to_str().unwrap();
            ch.content = grammar_regex
                .replace_all(&ch.content, |caps: &regex::Captures<'_>| {
                    let subterm_subst = format!(r#"<a href="{}.html#${{1}}">${{1}}</a>"#, name);
                    let head_subst =
                        format!(r#"<a id="${{1}}" href="{}.html#${{1}}">${{1}}</a>$2"#, name);
                    let s = caps.get(1).unwrap().as_str();
                    let s = keyword_regex.replace_all(&s, keyword_subst);
                    let s = comment_regex.replace_all(&s, comment_subst);
                    let s = subterm_regex.replace_all(&s, subterm_subst);
                    let s = nonterm_regex.replace_all(&s, nonterm_subst);
                    let s = head_regex.replace_all(&s, head_subst);
                    let s = s.trim();
                    format!("<pre><code>{}</code></pre>", s)
                })
                .into_owned();
        }
    });
}

/// Adds syntax highlighting to `arc-lang` code blocks.
fn preprocess_snippet(book: &mut Book) {
    let grammar_regex = regex::Regex::new(r"(?s)```arc-lang(-todo)?\n(.*?)```").unwrap();
    let comment_regex = regex::Regex::new(r"#[^{].*").unwrap();
    let comment_subst = r#"<i style="color:gray">${0}</i>"#;
    let keyword_regex = regex::Regex::new(&format!(
        r"(^|\n|[^[:alnum:]_])({})($|\n|[^[:alnum:]_])",
        KEYWORDS.join("|"),
    ))
    .unwrap();
    let keyword_subst = r"${1}<b>${2}</b>${3}";
    let numeric_regex = regex::Regex::new(
        r"([^a-zA-Z0-9])([0-9]+((\.[0-9]+)|%|ns|us|ms|s|m|h|d|w|((-[0-9]+-[0-9]+T[0-9]+)?:[0-9]+:[0-9]+))?)",
    )
    .unwrap();
    let numeric_subst = r#"${1}<b style="color:darkorange">${2}</b>"#;
    let textual_regex = regex::Regex::new(r##"("[^"]+")|'[^']'"##).unwrap();
    let textual_subst = r#"<b style="color:green">${0}</b>"#;
    book.for_each_mut(|item| {
        if let mdbook::BookItem::Chapter(ch) = item {
            ch.content = grammar_regex
                .replace_all(&ch.content, |caps: &regex::Captures<'_>| {
                    let s = caps.get(2).unwrap().as_str();
                    let s = keyword_regex.replace_all(&s, keyword_subst);
                    let s = textual_regex.replace_all(&s, textual_subst);
                    let s = numeric_regex.replace_all(&s, numeric_subst);
                    let s = comment_regex.replace_all(&s, comment_subst);
                    let s = s.trim();
                    if caps.get(1).is_some() {
                        format!(
                            r#"<pre><code style="background-color:#FFC590">{}</code></pre>"#,
                            s
                        )
                    } else {
                        format!("<pre><code>{}</code></pre>", s)
                    }
                })
                .into_owned();
        }
    });
}
