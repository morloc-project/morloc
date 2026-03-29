//! Unix domain socket IPC for communication between nexus and language pools.
//! Replaces ipc.c.

use crate::error::MorlocError;
use crate::packet::PacketHeader;
use std::io::{Read, Write};
use std::os::unix::net::UnixStream;
use std::path::Path;

/// Send a packet (header + payload) over a Unix stream socket and receive the response.
pub fn send_and_receive(
    socket_path: &Path,
    header: &PacketHeader,
    payload: &[u8],
) -> Result<(PacketHeader, Vec<u8>), MorlocError> {
    let mut stream = UnixStream::connect(socket_path).map_err(|e| {
        MorlocError::Ipc(format!(
            "failed to connect to {}: {e}",
            socket_path.display()
        ))
    })?;

    // Send header
    let header_bytes = header.to_bytes();
    stream
        .write_all(&header_bytes)
        .map_err(|e| MorlocError::Ipc(format!("failed to send header: {e}")))?;

    // Send payload
    if !payload.is_empty() {
        stream
            .write_all(payload)
            .map_err(|e| MorlocError::Ipc(format!("failed to send payload: {e}")))?;
    }

    // Read response header
    let mut resp_header_bytes = [0u8; 32];
    stream
        .read_exact(&mut resp_header_bytes)
        .map_err(|e| MorlocError::Ipc(format!("failed to read response header: {e}")))?;

    let resp_header = PacketHeader::from_bytes(&resp_header_bytes)?;

    // Read response payload
    let payload_len = resp_header.length as usize;
    let mut resp_payload = vec![0u8; payload_len];
    if payload_len > 0 {
        stream
            .read_exact(&mut resp_payload)
            .map_err(|e| MorlocError::Ipc(format!("failed to read response payload: {e}")))?;
    }

    Ok((resp_header, resp_payload))
}

/// Read a single packet from a connected stream.
pub fn read_packet(stream: &mut UnixStream) -> Result<(PacketHeader, Vec<u8>), MorlocError> {
    let mut header_bytes = [0u8; 32];
    stream
        .read_exact(&mut header_bytes)
        .map_err(|e| MorlocError::Ipc(format!("failed to read packet header: {e}")))?;

    let header = PacketHeader::from_bytes(&header_bytes)?;

    // Skip metadata between header and payload
    let skip = header.offset as usize - 32;
    if skip > 0 {
        let mut discard = vec![0u8; skip];
        stream
            .read_exact(&mut discard)
            .map_err(|e| MorlocError::Ipc(format!("failed to skip metadata: {e}")))?;
    }

    let payload_len = header.length as usize;
    let mut payload = vec![0u8; payload_len];
    if payload_len > 0 {
        stream
            .read_exact(&mut payload)
            .map_err(|e| MorlocError::Ipc(format!("failed to read payload: {e}")))?;
    }

    Ok((header, payload))
}

/// Send a packet over a connected stream.
pub fn send_packet(
    stream: &mut UnixStream,
    header: &PacketHeader,
    payload: &[u8],
) -> Result<(), MorlocError> {
    let header_bytes = header.to_bytes();
    stream
        .write_all(&header_bytes)
        .map_err(|e| MorlocError::Ipc(format!("failed to send header: {e}")))?;
    if !payload.is_empty() {
        stream
            .write_all(payload)
            .map_err(|e| MorlocError::Ipc(format!("failed to send payload: {e}")))?;
    }
    Ok(())
}
