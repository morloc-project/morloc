# C exports

libmorloc's C header (data/morloc/morloc.h) is the ABI the language
bindings and generated pools link against. The spec has no item saying what
belongs in it.

- [read] Spec gap: 65 functions declared in morloc.h have no caller
  outside the Rust runtime that defines them: no pool template, language
  binding, compiler string or stdlib module names them (scan of data/,
  library/, executable/ and lib/stdlib, 2026-10-12). Each is surface to
  keep stable and a symbol a binding can reach past the runtime's own
  checks. Open: should morloc.h hold only what a binding or generated pool
  calls, with the rest made crate-private? The list:
  abs2blk arrow_copied_bytes calculate_voidstar_size check_cache_packet
  daemon_build_discovery daemon_parse_response del_cache_packet
  delete_directory file_exists flatten_voidstar_to_buffer free_manifest
  get_cache_packet get_data_packet_as_mpk get_ptr has_suffix
  hash_morloc_packet http_free_request http_parse_request
  http_to_daemon_request http_write_response make_call_packet_from_cli
  make_data_indirection_packet make_morloc_bound_var
  make_morloc_literal make_morloc_pattern make_morloc_pattern_end
  make_morloc_remote_call_packet make_mpk_data_packet make_ping_packet
  manifest_to_discovery_json mkdir_p mlc_handle_pack_path
  mlc_handle_unpack_path mlc_probe_packet morloc_cache_stats
  morloc_get_shm_enabled morloc_packet_size_from_header
  morloc_payload_region_error morloc_set_shm_enabled morloc_set_tmpdir
  morloc_xxh64 parse_morloc_call_arguments parse_slurm_time
  pool_dispatch_packet print_binary print_hex_dump put_cache_packet
  read_binary_fd read_manifest read_morloc_packet_header
  read_schema_from_packet_meta rel2vol relptr_offset_bits
  relptr_vol_idx router_free router_start_program shcalloc shmemcpy
  slurm_job_is_complete submit_morloc_slurm_job total_shm_size
  write_atomic write_binary_fd write_slurm_time write_voidstar_binary
