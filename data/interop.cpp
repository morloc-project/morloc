// Functions used in making foreign calls

std::string generateTempFilename() {
    char template_file[] = "/tmp/morloc_cpp_XXXXXX";
    int fd = mkstemp(template_file);

    if (fd == -1) {
        perror("Error generating temporary filename");
        exit(EXIT_FAILURE);
    }

    // Close the file descriptor
    close(fd);

    return std::string(template_file);
}

void deleteTempFile(const std::string& filename) {
    if (remove(filename.c_str()) != 0) {
        perror("Error deleting temporary file");
    }
}

std::string foreign_call(
    const std::string& cmd,
    const std::vector<std::string>& args
) {
    // Vector to store temporary filenames
    std::vector<std::string> tempFiles;

    // Create temporary files for arguments
    std::string full_cmd = cmd;
    for (const auto& arg : args) {
        std::string tempFilename = generateTempFilename();
        std::ofstream tempFile(tempFilename);
        tempFile << arg;
        tempFile.close();
        tempFiles.push_back(tempFilename);
        full_cmd += " " + tempFilename;
    }

    // Execute the command and capture the output
    char buffer[1024];
    std::string result = "";
    FILE* pipe = popen(full_cmd.c_str(), "r");
    while (fgets(buffer, sizeof buffer, pipe) != NULL) {
        result += buffer;
    }
    pclose(pipe);

    // Delete temporary files
    for (const auto& tempFile : tempFiles) {
        deleteTempFile(tempFile);
    }

    return result;
}
