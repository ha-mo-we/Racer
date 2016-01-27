package com.racersystems.protege.logging;

import com.racersystems.protege.preferences.RacerProPreferences;

import java.io.File;
import java.io.FilenameFilter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;

/*******************************************************************************
 * Copyright (c) 2011 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Logs files for Racer are located in {@link com.racersystems.protege.preferences.RacerProPreferences#getLoggingDirectory()}.
 * Every file is labeled with the current data, e.g., racer-2010-12-01.log.
 * Author: Olaf Noppens
 * Date: 13.12.2010
 */
public class LogFileManager {
    private String logFilePrefix = "racer";
    private String logFileSuffix = ".log";
    private String separator = "-";
    private int maxNumberOfFiles = 100;

    public String getLogFilePrefix() {
        return this.logFilePrefix;
    }

    protected String generateFileName() {
        DateFormat formatter = DateFormat.getDateTimeInstance();
        Calendar calendar = Calendar.getInstance();
//        int year = calendar.get(Calendar.YEAR);
//        int month = calendar.get(Calendar.MONTH) + 1;
//        int day = calendar.get(Calendar.DAY_OF_MONTH);

        String date = formatter.format(calendar.getTime());
        date = date.replace(" ", "_");
        date = date.replace(".", "-");
        date = date.replace(":", "-");
        return logFilePrefix + separator + date + logFileSuffix;
        //return logFilePrefix + separator + year + separator + month + separator + day + logFileSuffix;
    }

    public String getLogFile() {
        cleanUpLogFiles();
        String loggingDirectory = RacerProPreferences.getInstance().getLoggingDirectory();
        File loggingDirectoryFile = new File(loggingDirectory);
        if (!loggingDirectoryFile.exists())
            loggingDirectoryFile.mkdirs();
        return RacerProPreferences.getInstance().getLoggingDirectory() + File.separator + generateFileName();
    }

    public void cleanUpLogFiles() {
        File directory = new File(RacerProPreferences.getInstance().getLoggingDirectory());
        if (!directory.exists()) return;
        File[] files = directory.listFiles(new FilenameFilter() {
            public boolean accept(File dir, String name) {
                return (name.startsWith(logFilePrefix + separator));
            }
        });
        if (files == null) return;
        Arrays.sort(files);
        if (files.length > maxNumberOfFiles) {
            int i = 0;
            for (File file : files) {
                file.delete();
                if (i > files.length - maxNumberOfFiles) return;
                i++;
            }
        }

    }
}
