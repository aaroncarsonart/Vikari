package com.atonementcrystals.dnr.vikari.interpreter;

import com.atonementcrystals.dnr.vikari.util.Utils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.ThreadContext;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.file.Files;

/**
 * Handles generation of a unique program id for each instance of Vikari to ensure
 * that log entries contain a unique id to distinguish each separate program instance
 * when reading through the log file.
 */
public class ProgramId {
    private static final Logger log = LogManager.getLogger(ProgramId.class);

    public static final int UNINITIALIZED = 0;
    public static final int ERROR_OPENING_FILE = -10;
    public static final int ERROR_ACQUIRING_LOCK = -20;
    public static final int ERROR_READING_FROM_FILE = -30;
    public static final int ERROR_WRITING_TO_FILE = -40;

    /**
     * Initialize the program id to use for this instance of Vikari.
     */
    public static void initialize() {
        setProgramId(UNINITIALIZED);
        int programId = getNextProgramId();
        setProgramId(programId);
    }

    /**
     * Set the given program id in the Mapped Diagnostic Context (MDC).
     * @param programId The program id to set.
     */
    private static void setProgramId(int programId) {
        String key = "programId";
        ThreadContext.put(key, String.valueOf(programId));
    }

    /**
     * Read the last cached program id from disk, update it, and write
     * the new value back to disk.
     * @return The next program id to use.
     */
    private static int getNextProgramId() {
        String osName = System.getProperty("os.name");
        boolean isPlatformWindows = osName.contains("Windows");
        if (isPlatformWindows) {
            return getNextProgramId_DontLockFile();
        } else {
            return getNextProgramId_LockFile();
        }
    }

    /**
     * On macOS and Linux, lock the program.id file before reading it to ensure that each unique
     * Vikari always has a unique program id in the logs.
     * @return The next program id to use.
     */
    private static int getNextProgramId_LockFile() {
        String homeDirPath = System.getProperty("user.home");
        String fileSeparator = File.separator;
        String programIdFilePath = homeDirPath + fileSeparator + ".vikari" + fileSeparator + "vikari.program_id";
        try {
            File file = new File(programIdFilePath);
            boolean fileExists = file.exists();
            if (!fileExists) {
                file.createNewFile();
            }

            // Lock the file to ensure unique program ids across different executions of Vikari.
            try (RandomAccessFile randomAccessFile = new RandomAccessFile(file, "rw");
                 FileChannel channel = randomAccessFile.getChannel();
                 FileLock lock = channel.lock()) {

                // Fetch current value of number in file.
                String currentValue = null;
                try {
                    currentValue = Files.readString(file.toPath());
                } catch (IOException e) {
                    log.error("Error reading from ``{}``.", programIdFilePath);
                    log.error(e);

                    return ERROR_READING_FROM_FILE;
                }

                int programId = 0;
                if (Utils.isIntegerNumber(currentValue)) {
                    programId = Integer.parseInt(currentValue);
                }

                // Increment the number to get the next valid program id.
                if (programId < Integer.MAX_VALUE) {
                    programId++;
                } else {
                    programId = 1;
                }

                // Write the new program id back to the file.
                FileOutputStream outputStream = new FileOutputStream(file);
                OutputStreamWriter writer = new OutputStreamWriter(outputStream);

                try (BufferedWriter bufferedWriter = new BufferedWriter(writer)) {
                    String output = String.valueOf(programId);
                    bufferedWriter.write(output);
                } catch (IOException e) {
                    log.error("Error writing to ``{}``.", programIdFilePath);
                    log.error(e);

                    return ERROR_WRITING_TO_FILE;
                }

                // Return the program id.
                return programId;

            } catch (IOException e) {
                log.error("Error acquiring file lock for ``{}``.", programIdFilePath);
                log.error(e);

                return ERROR_ACQUIRING_LOCK;
            }
        } catch (IOException e) {
            log.error("Error opening file ``{}``.", programIdFilePath);
            log.error(e);

            return ERROR_OPENING_FILE;
        }
    }

    /**
     * OpenJDK has a bug on Windows where FileLock causes an IOException on Windows only. So to
     * circumvent this issue, the fix is to not use FileLocks for Windows. This means that if
     * many Vikari programs are executed in quick succession, different programs may share the
     * same program id in the logs on Windows systems.
     *
     * @see <a href="https://bugs.openjdk.org/browse/JDK-8225473">OpenJDK Bug Report</a>
     * @return The next program id to use.
     */
    private static int getNextProgramId_DontLockFile() {
        String homeDirPath = System.getProperty("user.home");
        String fileSeparator = File.separator;
        String programIdFilePath = homeDirPath + fileSeparator + ".vikari" + fileSeparator + "vikari.program_id";
        try {
            File file = new File(programIdFilePath);
            boolean fileExists = file.exists();
            if (!fileExists) {
                file.createNewFile();
            }

            // Fetch current value of number in file.
            String currentValue = null;
            try {
                currentValue = Files.readString(file.toPath());
            } catch (IOException e) {
                log.error("Error reading from ``{}``.", programIdFilePath);
                log.error(e);

                return ERROR_READING_FROM_FILE;
            }

            int programId = 0;
            if (Utils.isIntegerNumber(currentValue)) {
                programId = Integer.parseInt(currentValue);
            }

            // Increment the number to get the next valid program id.
            if (programId < Integer.MAX_VALUE) {
                programId++;
            } else {
                programId = 1;
            }

            // Write the new program id back to the file.
            FileOutputStream outputStream = new FileOutputStream(file);
            OutputStreamWriter writer = new OutputStreamWriter(outputStream);

            try (BufferedWriter bufferedWriter = new BufferedWriter(writer)) {
                String output = String.valueOf(programId);
                bufferedWriter.write(output);
            } catch (IOException e) {
                log.error("Error writing to ``{}``.", programIdFilePath);
                log.error(e);

                return ERROR_WRITING_TO_FILE;
            }

            // Return the program id.
            return programId;

        } catch (IOException e) {
            log.error("Error opening file ``{}``.", programIdFilePath);
            log.error(e);

            return ERROR_OPENING_FILE;
        }
    }
}
