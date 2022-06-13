use anyhow::Context as _;
use std::env;
use std::ffi::OsStr;
use std::io::{self, Read as _, Write as _};
use std::process::{Child, ChildStdin, ChildStdout, Command, ExitStatus, Stdio};

/// Interface to UI viewer process.
#[derive(Debug)]
pub struct UiViewer {
    child: Child,
    stdin: ChildStdin,
    stdout: ChildStdout,
}

impl UiViewer {
    pub fn spawn() -> anyhow::Result<UiViewer> {
        let bin_dir = env::current_exe().context("failed to get executable path")?;
        Self::spawn_program(bin_dir.with_file_name("qmluic-uiviewer"))
    }

    pub fn spawn_program<S>(program: S) -> anyhow::Result<UiViewer>
    where
        S: AsRef<OsStr>,
    {
        let mut cmd = Command::new(program);
        cmd.arg("--pipe")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped());
        log::debug!("spawning {cmd:?}");
        let mut child = cmd
            .spawn()
            .with_context(|| format!("failed to spawn {:?}", cmd.get_program()))?;
        let stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();
        let mut viewer = UiViewer {
            child,
            stdin,
            stdout,
        };
        log::trace!("waiting for viewer start message");
        viewer
            .read_response()
            .context("failed to wait for viewer start message")?;
        Ok(viewer)
    }

    pub fn wait(mut self) -> anyhow::Result<ExitStatus> {
        drop(self.stdin);
        self.child.wait().context("failed to wait for viewer exit")
    }

    pub fn write_ui_data(&mut self, data: &[u8]) -> anyhow::Result<()> {
        let len = i32::try_from(data.len()).context("serialized XML too large")?;
        log::trace!("sending serialized data to viewer");
        self.write_data(len, data)
            .context("failed to communicate with viewer")?;
        log::trace!("waiting for viewer response");
        self.read_response()
            .context("failed to get viewer response")?;
        Ok(())
    }

    fn read_response(&mut self) -> io::Result<i32> {
        let mut buf = [0; 4];
        self.stdout.read_exact(&mut buf)?;
        Ok(i32::from_ne_bytes(buf))
    }

    fn write_data(&mut self, len: i32, data: &[u8]) -> io::Result<()> {
        self.stdin.write_all(&len.to_ne_bytes())?;
        self.stdin.write_all(data)?;
        self.stdin.flush()
    }
}
