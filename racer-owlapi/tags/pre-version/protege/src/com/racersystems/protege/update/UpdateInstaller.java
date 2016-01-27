package com.racersystems.protege.update;

import com.racersystems.protege.RacerInstalledBinaryInfo;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/*******************************************************************************
 * Copyright (c) 2010 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 11.10.2010
 */
public class UpdateInstaller {

    /**
     * The file available at {@link RacerUpdateInfo#getDownloadURL()} will be downloaded to a temporary directory. If it
     * is a zip file it will be extract to the binary installation directory, otherwise it will only copied to that
     * directory.
     *
     * @param info RacerUpdateInfo
     * @return the file???
     * @throws IOException if an IOException occurs
     * @see com.racersystems.protege.RacerInstalledBinaryInfo#getBinaryInstallationDirectory()
     */
    public File downloadFile(RacerUpdateInfo info) throws IOException {
       // System.out.println("download file?");
        URL downloadURL = info.getDownloadURL();
       // System.out.println("downloadURL" + downloadURL);
        final String[] path = downloadURL.getFile().split("/");
        String downloadFileName = path[path.length - 1];

        String tmpPath = System.getProperty("java.io.tmpdir");

        File tmpFile = File.createTempFile("racer", ""+System.currentTimeMillis());
       // File tmpFile = new File(tmpPath, downloadFileName);
        tmpFile.deleteOnExit();
        URLConnection conn = downloadURL.openConnection();
        BufferedInputStream bis = new BufferedInputStream(conn.getInputStream());
        ProgressMonitorURLInputStream inputStream = new ProgressMonitorURLInputStream(null, "Downloading Racer", conn);
        inputStream.getProgressMonitor().setMillisToPopup(1);
        inputStream.getProgressMonitor().setMillisToDecideToPopup(0);
        inputStream.getProgressMonitor().setProgress(1);
        BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(tmpFile));
        while (true) {
            byte[] buffer = new byte[4096];
            int read = inputStream.read(buffer);
            if (read == -1) {
                break;
            }
            bos.write(buffer, 0, read);
        }
        bis.close();
        bos.flush();
        bos.close();

        // Extract if a zip file
        if (downloadURL.getFile().endsWith(".zip")) {
            extract(tmpFile, RacerInstalledBinaryInfo.getInstance().getBinaryInstallationDirectory());
        } else {
            copy(tmpFile, RacerInstalledBinaryInfo.getInstance().getBinaryInstallationDirectory());
        }
        //Create updateFile in installation directory:
        File file = new File(RacerInstalledBinaryInfo.getInstance().getBinaryInstallationDirectory(), RacerUpdateInfo.UPDATE_INFO_NAME);
        if (!file.exists())
            file.createNewFile();
        PrintWriter writer = new PrintWriter(new FileOutputStream(file));
        writer.println(RacerUpdateInfo.VERSION_PROP_KEY + "=" + info.getAvailableVersion().toString());
        writer.flush();
        writer.close();

        return tmpFile;
    }

    private void copy(File file, File destinationDirectory) throws IOException {
        if (!destinationDirectory.exists())
            destinationDirectory.mkdir();
        FileInputStream fis = new FileInputStream(file);
        FileOutputStream fos = new FileOutputStream(destinationDirectory + file.getName());
        try {
            byte[] buf = new byte[4068];
            int i = 0;
            while ((i = fis.read(buf)) != -1) {
                fos.write(buf, 0, i);
            }
        }
        catch (IOException e) {
            throw e;
        }
        finally {

            fis.close();
            fos.close();
        }
    }


    private void extract(File archive, File destinationDirectory) throws IOException {
        if (!destinationDirectory.exists())
            destinationDirectory.mkdir();
        else {
            deleteDir(destinationDirectory);
        }
        ProgressMonitorInputStream inputStream = new ProgressMonitorInputStream(null, "Extracting Racer", new BufferedInputStream(new FileInputStream(archive)));
        ZipInputStream zis = new ZipInputStream(inputStream);
        inputStream.getProgressMonitor().setMillisToPopup(1);
        inputStream.getProgressMonitor().setMillisToDecideToPopup(0);
        inputStream.getProgressMonitor().setProgress(1);
        ZipEntry entry;
        byte[] buffer = new byte[4068];
        while ((entry = zis.getNextEntry()) != null) {
            if (entry.getName().indexOf(".DS_Store") != -1) {
                continue;
            }
            if (entry.getName().indexOf("__MACOSX") != -1) {
                continue;
            }
            File curFile = new File(destinationDirectory, entry.getName());
            if (entry.isDirectory()) {
                curFile.mkdirs();
            } else {
                if (!curFile.getAbsoluteFile().getParentFile().exists())
                    curFile.getAbsoluteFile().getParentFile().mkdir();
                if (!curFile.exists())
                    curFile.createNewFile();
                OutputStream os = new BufferedOutputStream(new FileOutputStream(curFile));
                int count;
                while ((count = zis.read(buffer)) != -1) {
                    os.write(buffer, 0, count);
                }
                os.flush();
                os.close();
            }
        }
        inputStream.close();
    }

    private static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i = 0; i < children.length; i++) {
                boolean success = deleteDir(new File(dir, children[i]));
                if (!success) {
                    return false;
                }
            }
        }
        return dir.delete();
    }


    class ProgressMonitorURLInputStream extends FilterInputStream {
        private ProgressMonitor monitor;
        private int nread = 0;
        private int size = 0;


        /**
         * Constructs an object to monitor the progress of an input stream.
         *
         * @param message         Descriptive text to be placed in the dialog box
         *                        if one is popped up.
         * @param parentComponent The component triggering the operation
         *                        being monitored.
         * @param in              The input stream to be monitored.
         */
        public ProgressMonitorURLInputStream(Component parentComponent,
                                             Object message,
                                             URLConnection in) throws IOException {
            super(in.getInputStream());
            size = in.getContentLength();
            monitor = new ProgressMonitor(parentComponent, message, null, 0, size);
        }


        /**
         * Get the ProgressMonitor object being used by this stream. Normally
         * this isn't needed unless you want to do something like change the
         * descriptive text partway through reading the file.
         *
         * @return the ProgressMonitor object used by this object
         */
        public ProgressMonitor getProgressMonitor() {
            return monitor;
        }


        /**
         * Overrides <code>FilterInputStream.read</code>
         * to update the progress monitor after the read.
         */
        public int read() throws IOException {
            int c = in.read();
            if (c >= 0) monitor.setProgress(++nread);
            if (monitor.isCanceled()) {
                InterruptedIOException exc =
                        new InterruptedIOException("progress");
                exc.bytesTransferred = nread;
                throw exc;
            }
            return c;
        }


        /**
         * Overrides <code>FilterInputStream.read</code>
         * to update the progress monitor after the read.
         */
        public int read(byte b[]) throws IOException {
            int nr = in.read(b);
            if (nr > 0) monitor.setProgress(nread += nr);
            if (monitor.isCanceled()) {
                InterruptedIOException exc =
                        new InterruptedIOException("progress");
                exc.bytesTransferred = nread;
                throw exc;
            }
            return nr;
        }


        /**
         * Overrides <code>FilterInputStream.read</code>
         * to update the progress monitor after the read.
         */
        public int read(byte b[],
                        int off,
                        int len) throws IOException {
            int nr = in.read(b, off, len);
            if (nr > 0) monitor.setProgress(nread += nr);
            if (monitor.isCanceled()) {
                InterruptedIOException exc =
                        new InterruptedIOException("progress");
                exc.bytesTransferred = nread;
                throw exc;
            }
            return nr;
        }


        /**
         * Overrides <code>FilterInputStream.skip</code>
         * to update the progress monitor after the skip.
         */
        public long skip(long n) throws IOException {
            long nr = in.skip(n);
            if (nr > 0) monitor.setProgress(nread += nr);
            return nr;
        }


        /**
         * Overrides <code>FilterInputStream.close</code>
         * to close the progress monitor as well as the stream.
         */
        public void close() throws IOException {
            in.close();
            monitor.close();
        }


        /**
         * Overrides <code>FilterInputStream.reset</code>
         * to reset the progress monitor as well as the stream.
         */
        public synchronized void reset() throws IOException {
            in.reset();
            nread = size - in.available();
            monitor.setProgress(nread);
        }
    }


}
