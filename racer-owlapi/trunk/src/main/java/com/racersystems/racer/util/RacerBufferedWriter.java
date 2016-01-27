package com.racersystems.racer.util;

import java.io.IOException;
import java.io.Writer;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * Author: Olaf Noppens
 * Date: 07.10.2011
 */
public class RacerBufferedWriter extends Writer {

    private Writer out;

    private StringBuilder buffer;
    private static int defaultCharBufferSize = 8192;

    /**
     * Line separator string.  This is the value of the line.separator
     * property at the moment that the stream was created.
     */
    private final String lineSeparator;

    /**
     * Create a buffered character-output stream that uses a default-sized
     * output buffer.
     *
     * @param out A Writer
     */
    public RacerBufferedWriter(Writer out) {
        this.out = out;
        this.buffer = new StringBuilder(defaultCharBufferSize);
        lineSeparator = (String) java.security.AccessController.doPrivileged(
                new sun.security.action.GetPropertyAction("line.separator"));
    }

    /**
     * Check to make sure that the stream has not been closed
     *
     * @throws java.io.IOException
     */
    private void ensureOpen() throws IOException {
        if (out == null)
            throw new IOException("Stream closed");
    }

    /**
     * Flush the output buffer to the underlying character stream, without
     * flushing the stream itself.  This method is non-private only so that it
     * may be invoked by PrintStream.
     *
     * @throws java.io.IOException
     */
    void flushBuffer() throws IOException {
        synchronized (lock) {
            ensureOpen();
            out.write(buffer.toString());
            //System.out.println(buffer.toString());
            buffer = new StringBuilder();
        }
    }

    /**
     * Write a single character.
     *
     * @throws IOException If an I/O error occurs
     */
    public void write(int c) throws IOException {
        synchronized (lock) {
            ensureOpen();
            buffer.append(c);
        }
    }

    /**
     * Write a portion of an array of characters.
     * <p/>
     * <p> Ordinarily this method stores characters from the given array into
     * this stream's buffer, flushing the buffer to the underlying stream as
     * needed.  If the requested length is at least as large as the buffer,
     * however, then this method will flush the buffer and write the characters
     * directly to the underlying stream.  Thus redundant
     * <code>BufferedWriter</code>s will not copy data unnecessarily.
     *
     * @param cbuf A character array
     * @param off  Offset from which to start reading characters
     * @param len  Number of characters to write
     * @throws IOException If an I/O error occurs
     */
    public void write(char cbuf[], int off, int len) throws IOException {
        synchronized (lock) {
            ensureOpen();
            buffer.append(cbuf, off, len);
        }
    }

    /**
     * Write a portion of a String.
     * <p/>
     * <p> If the value of the <tt>len</tt> parameter is negative then no
     * characters are written.  This is contrary to the specification of this
     * method in the {@linkplain java.io.Writer#write(java.lang.String,int,int)
     * superclass}, which requires that an {@link IndexOutOfBoundsException} be
     * thrown.
     *
     * @param s   String to be written
     * @param off Offset from which to start reading characters
     * @param len Number of characters to be written
     * @throws IOException If an I/O error occurs
     */
    public void write(String s, int off, int len) throws IOException {
        synchronized (lock) {
            ensureOpen();
            buffer.append(s, off, len);
        }
    }

    /**
     * Write a line separator.  The line separator string is defined by the
     * system property <tt>line.separator</tt>, and is not necessarily a single
     * newline ('\n') character.
     *
     * @throws IOException If an I/O error occurs
     */
    public void newLine() throws IOException {
        write(lineSeparator);
    }

    /**
     * Flush the stream.
     *
     * @throws IOException If an I/O error occurs
     */
    public void flush() throws IOException {
        synchronized (lock) {
            flushBuffer();
            out.flush();
        }
    }

    /**
     * Close the stream.
     *
     * @throws IOException If an I/O error occurs
     */
    public void close() throws IOException {
        synchronized (lock) {
            if (out == null)
                return;
            flushBuffer();
            //out.close();
            out = null;
        }
    }

}
