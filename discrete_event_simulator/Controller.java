package hw4;

import java.util.*;

public class Controller {
	
	static double totalTq = 0;
	static int completedOut = 0;

    /**
     * initialize the schedule with a birth event and a monitor event
     * @return a schedule with two events
     */
    public static PriorityQueue<Event> initSchedule(double lambda) {
        PriorityQueue<Event> schedule = new PriorityQueue<Event>();

        double t1 = Event.getTimeOfNextBirth(lambda);
        schedule.add(new Event(t1, EventType.BIRTH));

        double t2 = Event.getTimeOfNextMonitor(lambda);
        schedule.add(new Event(t2, EventType.MONITOR));

        return schedule;
    }

    public static void runSimulation(double simulationTime, double lambda, double Ts0_s0, double Ts1_s1, double Ts2_s2, double Ts1_s3, double p1_s3, double Ts2_s3, double p2_s3, double Ts3_s3, double p3_s3, int K2, double p0_1, double p0_2, double p3_out, double p3_1, double p3_2) {
        State state0 = new State(1);
        State state1 = new State(2);
        State state2 = new State(1);
        State state3 = new State(1);


        PriorityQueue<Event> schedule = initSchedule(lambda);

	// System.out.println("Ts = " + Ts + " Ts2 = " + Ts2);
	
        double time = 0, maxTime = simulationTime;
        while(time < maxTime) {
            Event event = schedule.remove();
            time = event.getTime();
            event.function(schedule, state0, state1, state2, state3, time, lambda, Ts0_s0, Ts1_s1, Ts2_s2, Ts1_s3, p1_s3, Ts2_s3, p2_s3, Ts3_s3, p3_s3, K2, p0_1, p0_2, p3_out, p3_1, p3_2);
        }

        /**
         * output the statistics over the simulated system
         */
        System.out.println();

        System.out.println("S0 UTIL: " + state0.busyTime[0] / simulationTime);
        System.out.println("S0 QLEN: " + state0.totalQueueLen / state0.numMonitorEvents);
        System.out.println("S0 TRESP: " + state0.totalRequestTime / state0.numCompletedRequests);
        System.out.println();

        System.out.println("S1,1 UTIL: " + state1.busyTime[0] / simulationTime);
        System.out.println("S1,2 UTIL: " + state1.busyTime[1] / simulationTime);
        System.out.println("S1 QLEN: " + state1.totalQueueLen / state1.numMonitorEvents);
        System.out.println("S1 TRESP: " + state1.totalRequestTime / state1.numCompletedRequests);
        System.out.println();
        
        System.out.println("S2 UTIL: " + state2.busyTime[0] / simulationTime);
        System.out.println("S2 QLEN: " + state2.totalQueueLen / state2.numMonitorEvents);
        System.out.println("S2 TRESP: " + state2.totalRequestTime / state2.numCompletedRequests);
        System.out.println("S2 DROPPED: " + state2.totalDropped);
        System.out.println();
        
        System.out.println("S3 UTIL: " + state3.busyTime[0] / simulationTime);
        System.out.println("S3 QLEN: " + state3.totalQueueLen / state3.numMonitorEvents);
        System.out.println("S3 TRESP: " + state3.totalRequestTime / state3.numCompletedRequests);
        System.out.println();
        
        System.out.println("QTOT: " + ((state0.totalQueueLen + state1.totalQueueLen+ state2.totalQueueLen + state3.totalQueueLen)/state3.numMonitorEvents));
        System.out.println("TRESP: " + (totalTq/completedOut));
    }
}
