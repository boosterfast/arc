use crate::server::Backend;
use anyhow::Result;
use lspower::LspService;
use lspower::Server;

#[cfg(not(any(
    feature = "runtime-futures",
    feature = "runtime-smol",
    feature = "runtime-tokio"
)))]
pub fn start() -> Result<()> {
    panic!("No runtime enabled");
}

#[cfg(feature = "runtime-futures")]
pub fn start() -> Result<()> {
    futures::future::block_on(async {
        let stdin = blocking::Unblock::new(std::io::stdin());
        let stdout = blocking::Unblock::new(std::io::stdout());
        let (service, messages) = LspService::new(|client| Backend::new(client));
        Server::new(stdin, stdout)
            .interleave(messages)
            .serve(service)
            .await;

        Ok(())
    })
}

#[cfg(feature = "runtime-smol")]
pub fn start() -> Result<()> {
    smol::block_on(async {
        let stdin = smol::Unblock::new(std::io::stdin());
        let stdout = smol::Unblock::new(std::io::stdout());
        let (service, messages) = LspService::new(|client| Backend::new(client));
        Server::new(stdin, stdout)
            .interleave(messages)
            .serve(service)
            .await;

        Ok(())
    })
}

#[cfg(feature = "runtime-tokio")]
pub fn start() -> Result<()> {
    tokio::runtime::Runtime::new()?.block_on(async {
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        let (service, messages) = LspService::new(|client| Backend::new(client));
        Server::new(stdin, stdout)
            .interleave(messages)
            .serve(service)
            .await;

        Ok(())
    })
}
