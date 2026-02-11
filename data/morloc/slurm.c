#include "morloc.h"
#include "cache.h"

#define XXH_INLINE_ALL
#include "xxhash.h"

size_t parse_slurm_time(const char* time_str, ERRMSG) {
    VAL_RETURN_SETUP(size_t, 0)

    int days = 0;
    int hours = 0;
    int minutes = 0;
    int seconds = 0;

    int n;
    errno = 0;
    n = sscanf(time_str, "%d-%02d:%02d:%02d", &days, &hours, &minutes, &seconds);
    // if not all of the 4 values where parsed, try the day-less format
    if(n != 4){
        days = 0;
        n = sscanf(time_str, "%02d:%02d:%02d", &hours, &minutes, &seconds);
        // if not all of the 3 values where parsed, die
        if(n != 3){
            RAISE("Failed to scan slurm walltime string '%s'", time_str);
        }
    }

    // Check for negative time
    if(days < 0 || hours < 0 || minutes < 0 || seconds < 0) {
        RAISE("Negative time component in '%s'", time_str);
    }

    // Check for non-canonical time specifications
    if(hours > 23 || minutes > 59 || seconds > 59) {
        RAISE("Invalid time component in '%s' (HH<=23 MM<=59 SS<=59)", time_str);
    }

    // Specifying too many days is probably a mistake and could lead to overflows
    if(days > 3650){
        RAISE("Do you really want to run this job for more than 10 years?")
    }

    return seconds + 60*minutes + 60*60*hours + 60*60*24*days;
}

char* write_slurm_time(int seconds){

    int days = seconds / (60 * 60 * 24);
    seconds -= days * 60 * 60 * 24;

    int hours = seconds / (60 * 60);
    seconds -= hours * 60 * 60;

    int minutes = seconds / 60;
    seconds -= minutes * 60;

    char* time_str = (char*)calloc(32, sizeof(char));
    snprintf(time_str, 32, "%d-%02d:%02d:%02d", days, hours, minutes, seconds);

    return time_str;
}

bool parse_morloc_call_arguments(
    uint8_t* packet, // input call packet
    uint8_t** args, // pointer to vector of arguments (MUTATED)
    size_t* nargs, // pointer to number of arguments (MUTATED)
    ERRMSG
){
    BOOL_RETURN_SETUP

    *nargs = 0;

    morloc_packet_header_t* header = (morloc_packet_header_t*)packet;
    size_t packet_size = morloc_packet_size_from_header(header);

    RAISE_IF(
        header->command.cmd_type.type != PACKET_TYPE_CALL,
        "Unexpected packet type (BUG)"
    )

    morloc_packet_header_t* arg_header;
    size_t pos = sizeof(morloc_packet_header_t) + header->offset;
    while(pos < packet_size){
        arg_header = (morloc_packet_header_t*)(packet + pos);
        pos += sizeof(morloc_packet_header_t) + arg_header->offset + arg_header->length;
        *nargs += 1;
    }

    pos = sizeof(morloc_packet_header_t) + (size_t)header->offset;
    for(size_t i = 0; i < *nargs; i++){
        uint8_t* arg = packet + pos;
        morloc_packet_header_t* arg_header = (morloc_packet_header_t*)arg;
        args[i] = packet + pos;
        pos += sizeof(morloc_packet_header_t) + arg_header->offset + arg_header->length;
    }

    return true;
}

bool slurm_job_is_complete(uint32_t job_id) {
    char cmd[256];
    snprintf(cmd, sizeof(cmd), "sacct -j %u --format=State --noheader", job_id);

    FILE *sacct = popen(cmd, "r");
    if (!sacct) return false;

    char state[16];
    bool done = false;
    while (fgets(state, sizeof(state), sacct)) {
        if (strstr(state, "COMPLETED") ||
            strstr(state, "FAILED") ||
            strstr(state, "CANCELLED")) {
            done = true;
            break;
        }
    }
    pclose(sacct);
    return done;
}

uint32_t submit_morloc_slurm_job(
    const char* nexus_path,
    const char* socket_basename,
    const char* call_packet_filename,
    const char* result_cache_filename,
    const char* output_filename,
    const char* error_filename,
    const resources_t* resources,
    ERRMSG)
{
    VAL_RETURN_SETUP(uint32_t, 0)

    RAISE_IF(nexus_path == NULL, "nexus path undefined")
    RAISE_IF(socket_basename == NULL, "socket basename undefined")
    RAISE_IF(call_packet_filename == NULL, "call packet filename undefined")
    RAISE_IF(result_cache_filename == NULL, "result cache filename undefined")
    RAISE_IF(output_filename == NULL, "slurm output filename undefined")
    RAISE_IF(error_filename == NULL, "slurm error filename undefined")

    char cmd[MAX_SLURM_COMMAND_LENGTH];
    FILE *slurm;
    uint32_t job_id = 0;

    // Build resource parameters
    char* time_str = write_slurm_time(resources->time); // DD-HH:MM:SS
    char resources_spec[256];
    snprintf(resources_spec, sizeof(resources_spec),
        "--mem=%dG --time=%s --cpus-per-task=%d --gres=gpu:%d",
        resources->memory,
        time_str,
        resources->cpus,
        resources->gpus
    );
    free(time_str);

    // Build submission command
    int written = snprintf(
        cmd,
        MAX_SLURM_COMMAND_LENGTH,
        "sbatch --parsable -o %s -e %s %s --wrap='%s --call-packet %s --socket-base %s --output-file %s --output-form packet",
        output_filename,
        error_filename,
        resources_spec,
        nexus_path,
        call_packet_filename,
        socket_basename,
        result_cache_filename
    );

    RAISE_IF(written >= MAX_SLURM_COMMAND_LENGTH, "Command too long")

    // Submit job and capture output
    slurm = popen(cmd, "r");
    RAISE_IF(!slurm, "Failed to execute sbatch")

    // Parse job ID from first line
    if (fscanf(slurm, "%u", &job_id) != 1) {
        pclose(slurm);
        RAISE("Failed to parse job ID from sbatch output");
    }

    pclose(slurm);
    return job_id;
}

uint8_t* remote_call(
    int midx,

    // The base socket name for the target pool (e.g., "pipe-r").
    // This is not the full socket path, since the parent and remote will be
    // using different socket files in different temporary directories. But the
    // basename generation will be conserved.
    const char* socket_basename,

    // path where args and results will be written
    const char* cache_path,

    // required system resources (mem, cpus, etc)
    const resources_t* resources,

    // voidstar for each argument
    const uint8_t** arg_packets,

    // number of arguments
    size_t nargs,
    ERRMSG
){
    ERROR_HANDLING_SETUP

    uint64_t seed = (uint64_t)midx;

    // Initialization of allocated data
    uint8_t* return_packet = NULL;
    uint64_t* arg_hashes = NULL;
    uint8_t** arg_voidstars = NULL;
    Schema** arg_schemas = NULL;
    char** cached_arg_filenames = NULL;
    uint8_t* call_packet = NULL;
    char* result_cache_filename = NULL;
    uint8_t** cached_arg_packets = NULL;
    char* call_packet_filename = NULL;
    char* output_filename = NULL;
    char* error_filename = NULL;

    // Initializations
    size_t call_packet_size = 0;
    uint64_t call_packet_hash_code = 0;
    char output_ext[] = ".out";
    char error_ext[] = ".err";
    char call_ext[] = "-call.dat";

    uint32_t pid = 0;
    size_t return_packet_size = 0;
    char* failure = NULL;

    // Initialize function hash
    //
    // The function hash determines the output file name on the remote node and is
    // used to determine if this computation has already been run. The function
    // hash **should** be unique to a function and its inputs.
    //
    // TODO: Actually hash the function code, not just the manifold id.
    uint64_t function_hash = mix(seed, DEFAULT_XXHASH_SEED);

    // Collect the hash of the voidstar data in every argument packet. This is
    // independent of language, does not consider the header or metadata, and is
    // unaffected by representation in the shared memory pool.
    arg_hashes = (uint64_t*)calloc(nargs, sizeof(uint64_t));

    // Collect the voidstar data for each argument
    arg_voidstars = (uint8_t**)calloc(nargs, sizeof(uint8_t*));

    // Collect the schema for each argument. This schema must be written into
    // the packet metadata (which will be done automatically by the packet
    // creators provided in this library.
    arg_schemas = (Schema**)calloc(nargs, sizeof(Schema*));

    // It would be rather better if I automatically hashed every packet at
    // creation time and stored the hash in the packet metadata. For file
    // sources, then I would need to hash the file and store the hash time.
    // Then I could avoid unpacking the data.
    for(size_t i = 0; i < nargs; i++){

        // direct pointer to the packet string (do not free)
        char* arg_schema_str = TRY_GOTO(read_schema_from_packet_meta, arg_packets[i]);

        arg_schemas[i] = TRY_GOTO(parse_schema, arg_schema_str);

        // get the raw data stored in the packet (after the header and metadata)
        // possibly heavy memory
        arg_voidstars[i] = TRY_GOTO(get_morloc_data_packet_value, arg_packets[i], arg_schemas[i]);

        arg_hashes[i] = TRY_GOTO(hash_voidstar, arg_voidstars[i], arg_schemas[i], DEFAULT_XXHASH_SEED);

        // update function hash with ith argument hash
        function_hash = mix(function_hash, arg_hashes[i]);
    }

    TRY_GOTO(mkdir_p, cache_path)

    call_packet = NULL;
    result_cache_filename = check_cache_packet(function_hash, cache_path, &CHILD_ERRMSG);
    CHILD_ERRMSG = NULL; // ignore error
                         //
    // If a cached result already exists, return it
    if(result_cache_filename != NULL){
        // return result is cached, so load the cache and go
        return_packet = TRY_GOTO(get_cache_packet, function_hash, cache_path);
        goto end;
    } else {
        result_cache_filename = TRY_GOTO(make_cache_filename, function_hash, cache_path);
    }

    // return result is not cached, so we need to run
    cached_arg_filenames = (char**)calloc(nargs, sizeof(char*));
    for(size_t i = 0; i < nargs; i++){
        cached_arg_filenames[i] = check_cache_packet(arg_hashes[i], cache_path, &CHILD_ERRMSG);
        if(cached_arg_filenames[i] == NULL){
            CHILD_ERRMSG = NULL; // ignore error, if it failed, remake the cache
            cached_arg_filenames[i] = TRY_GOTO(put_cache_packet, arg_voidstars[i], arg_schemas[i], arg_hashes[i], cache_path);
        }
    }

    // read the cached argument packets (these will be small since they contain
    // only a wrapper around the MessagePack file names)
    cached_arg_packets = (uint8_t**)calloc(nargs, sizeof(uint8_t*));
    for(size_t i = 0; i < nargs; i++){
        size_t file_size = 0;
        cached_arg_packets[i] = TRY_GOTO(read_binary_file, cached_arg_filenames[i], &file_size);
    }

    // write the call packet to cache
    call_packet = TRY_GOTO(
        make_morloc_remote_call_packet,
        (uint32_t)midx,
        (const uint8_t**)cached_arg_packets,
        nargs
    );

    call_packet_size = TRY_GOTO(morloc_packet_size, call_packet);

    // Note that this is not the same as the result hash, the remote compute
    // node will load this packet as a call to run the job and will then write
    // the results to the result hash cache.
    call_packet_hash_code = XXH64(call_packet, call_packet_size, DEFAULT_XXHASH_SEED);

    call_packet_filename = TRY_GOTO(make_cache_filename_ext, call_packet_hash_code, cache_path, call_ext);

    // Write packet the call to disk, this will be sent the worker daemon
    TRY_GOTO(write_atomic, call_packet_filename, call_packet, call_packet_size)

    output_filename = TRY_GOTO(make_cache_filename_ext, function_hash, cache_path, output_ext);
    error_filename = TRY_GOTO(make_cache_filename_ext, function_hash, cache_path, error_ext);

    // submit slurm call, save process ID for watching and killing, if needed
    pid = TRY_GOTO(
        submit_morloc_slurm_job,
        "./nexus", // TODO: need a non-hard-coded path here
        socket_basename,
        call_packet_filename,
        result_cache_filename,
        output_filename,
        error_filename,
        resources
    );

    // Wait forever. Since this is a remote job, it is assumed that it will be
    // rather heavy so the 1 second loop is probably fine.
    while(!slurm_job_is_complete(pid)) {
        sleep(1);
    }

    return_packet_size = 0;
    return_packet = TRY_GOTO(read_binary_file, result_cache_filename, &return_packet_size);

    failure = TRY_GOTO(get_morloc_data_packet_error_message, return_packet)
    if(failure != NULL){
        fprintf(stderr, "Failed, deleting result %s\n", result_cache_filename);
        unlink(result_cache_filename);
    }

end:
    for(size_t i = 0; i < nargs; i++){
        if(arg_schemas != NULL){
            FREE(arg_schemas[i])
        }
        if(cached_arg_filenames != NULL){
            FREE(cached_arg_filenames[i])
        }
        if(cached_arg_packets != NULL){
            FREE(cached_arg_packets[i])
        }
    }

    FREE(arg_hashes)
    FREE(arg_voidstars)
    FREE(arg_schemas)
    FREE(result_cache_filename)
    FREE(call_packet_filename)
    FREE(output_filename)
    FREE(error_filename)
    FREE(cached_arg_filenames)
    FREE(cached_arg_packets)

    return return_packet;
}
