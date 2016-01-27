package com.racersystems.racer.util;

import java.util.StringTokenizer;

/*******************************************************************************
 * Copyright (c) 2012 by Olaf Noppens. All rights reserved.<br/>
 * derivo GmbH, Germany.
 ******************************************************************************/

/**
 * @author Olaf Noppens
 */
public class RacerUtils {

    public static int compareRacerVersions(String racerVersion, String racerVersion2) {
        final StringTokenizer st1 = new StringTokenizer(racerVersion, ".");
        final StringTokenizer st2 = new StringTokenizer(racerVersion2, ".");
        while (st1.hasMoreTokens() && st2.hasMoreTokens()) {
            String token1 = st1.nextToken();
            String token2 = st2.nextToken();
            Integer i1 = Integer.parseInt(token1);
            Integer i2 = Integer.parseInt(token2);
            if (i1 < i2) return -1;
            else if (i1 > i2) return 1;
        }
        if (st1.hasMoreTokens()) return 1;
        else if (st2.hasMoreTokens()) return -1;
        return 0;

    }

}
