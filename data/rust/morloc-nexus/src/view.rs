//! `morloc-nexus view` subcommand.
//!
//! Reads a data file (morloc packet, .json, .mpk, .arrow, .parquet,
//! or .csv) and re-emits it in a chosen output format. The
//! implementation reuses the same FFI chain `run` uses for argument
//! ingress (`initialize_positional` -> `parse_cli_data_argument` ->
//! C `load_morloc_data_file`) and the same output emitter
//! (`dispatch::print_result_c`), so format support and conversion
//! semantics cannot drift away from a real morloc program run.

use std::ffi::c_char;
use std::path::Path;

use morloc_runtime_types::cschema::CSchema;
use morloc_runtime_types::packet::{PacketHeader, PACKET_FORMAT_ARROW};
use morloc_runtime_types::schema::parse_schema;

use crate::cli::ViewArgs;
use crate::dispatch::{print_result_c, NexusConfig};
use crate::file::{classify_path, Classification, DataArg, DEFAULT_PROBE_BYTES};
use crate::loader::load_with_schema;
use crate::process::{self, redirect_stdout_to, take_c_errmsg};

extern "C" {
    fn get_morloc_data_packet_value(
        data: *const u8,
        schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> *mut u8;
}

/// Entry point. Classifies the input file once (which doubles as the
/// truncation/oversize gate), resolves the schema, loads via the
/// shared loader, and hands a voidstar to [`print_result_c`].
pub fn run(args: &ViewArgs) -> ! {
    redirect_stdout_to(args.output_path.as_deref());

    let cls = classify_path(Path::new(&args.target), DEFAULT_PROBE_BYTES);
    if let Classification::Error(e) = &cls {
        eprintln!("Error: {}: {}", args.target, e);
        process::clean_exit(1);
    }

    let schema_str = match resolve_schema_str(args, &cls) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }
    };

    let loaded = match load_with_schema(&args.target, &schema_str) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error: failed to load '{}': {}", args.target, e);
            process::clean_exit(1);
        }
    };

    let config = NexusConfig {
        output_format: args.output_form.to_internal(),
        compression_level: args.compression_level,
        output_path: args.output_path.clone(),
        ..NexusConfig::default()
    };

    let mut errmsg: *mut c_char = std::ptr::null_mut();
    let voidstar = unsafe {
        get_morloc_data_packet_value(loaded.packet, loaded.c_schema, &mut errmsg)
    };
    if voidstar.is_null() {
        let msg = take_c_errmsg(errmsg).unwrap_or_else(|| "unknown error".to_string());
        eprintln!("Error: failed to extract value from '{}': {}", args.target, msg);
        process::clean_exit(1);
    }

    let is_arrow = packet_format_is_arrow(loaded.as_slice());
    print_result_c(voidstar, loaded.c_schema, loaded.as_slice(), is_arrow, &config);
    unreachable!("print_result_c always exits via process::clean_exit")
}

/// Resolve the schema string view will use to decode the input. Order:
///  1. `--schema` flag.
///  2. Schema embedded in a morloc-packet's metadata, already
///     extracted by [`classify_path`] in `run`.
///  3. Error directing the user to `--schema`.
///
/// View does not implement schema-less passthrough — every conversion
/// goes through the typed loader so behavior matches `run`. Users who
/// want a byte-identity copy already have `cp`.
fn resolve_schema_str(args: &ViewArgs, cls: &Classification) -> Result<String, String> {
    if let Some(s) = &args.schema {
        parse_schema(s).map_err(|e| format!("failed to parse --schema '{}': {}", s, e))?;
        return Ok(s.clone());
    }
    if let Classification::MorlocDataPacket {
        arg: DataArg { schema: Some(s), .. },
        ..
    } = cls
    {
        return Ok(s.clone());
    }
    Err(format!(
        "no schema available for '{}'. \
         Inputs without a morloc-packet header need an explicit --schema STRING.",
        args.target
    ))
}

/// True iff the loaded packet's DATA-command format byte is ARROW.
/// Used by `print_result_c` to route to the table-only output path.
fn packet_format_is_arrow(packet: &[u8]) -> bool {
    if packet.len() < 32 {
        return false;
    }
    let mut hdr_bytes = [0u8; 32];
    hdr_bytes.copy_from_slice(&packet[..32]);
    let hdr = match PacketHeader::from_bytes(&hdr_bytes) {
        Ok(h) => h,
        Err(_) => return false,
    };
    hdr.is_data() && unsafe { hdr.command.data.format } == PACKET_FORMAT_ARROW
}
