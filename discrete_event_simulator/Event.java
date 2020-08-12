package hw4;

import java.util.*;

public class Event implements Comparable<Event> {
    /**
     * Time that the event happens
     */
    private double time;

    private EventType eventType;

    /* Requests being served. */
    private Request inServer[];

    public double getTime() {
        return time;
    }

    /**
     * @param time when the event starts
     * @param eventType Monitor or Birth or Death
     */
    public Event(double time, EventType eventType) {
        this.time = time;
        this.eventType = eventType;

    }

    /**
     *
     * @param schedule contains all the scheduled future events
     * @param state of the simulation (request queue, logging info, etc.)
     * @param timestamp current time in the discrete simulation
     * @param K maximum length of the queue expressed in number of requests
     */
    public void function(PriorityQueue<Event> schedule, State state0,  State state1, State state2, State state3, double timestamp, double lambda, double Ts0_s0, double Ts1_s1, double Ts2_s2, double Ts1_s3, double p1_s3, double Ts2_s3, double p2_s3, double Ts3_s3, double p3_s3, int K2, double p0_1, double p0_2, double p3_out, double p3_1, double p3_2) {

        switch (eventType) {
            case DEATH_S0:
                Request req = state0.queue.remove();
                req.setFinishServiceTime(timestamp);
                int serverid = req.getServer();

                state0.totalRequestTime += req.getTq();
                state0.busyTime[serverid] += req.getTs();
                state0.numCompletedRequests += 1;
                state0.numBusyServer--;
                state0.freeBusyServer(serverid);

                System.out.println("R" + req.getId() + " DONE S0: " + timestamp);

                double prob = Math.random();
               
               //go to S1
                if (prob < p0_1) {    
                    req.setArrivalTime(timestamp);
                	state1.queue.add(req);

                    System.out.println("R" + req.getId() + " FROM S0 to S1: " + timestamp);
                    if (state1.queue.size() + state1.numBusyServer <= state1.N) {
                    	int serveridb1 = state1.getServerId();
                        req.setServerId(serveridb1);
                        req.setStartServiceTime(timestamp);
                        
                        state1.currents[serveridb1] = state1.queue.remove();  //item now exist in array of request; acts like servers
                        
                        state1.numBusyServer++;
                        state1.setBusyServer(serveridb1);
                        
                        System.out.println("R" + req.getId() + " START S1," + (req.getServer()+1) + ": " + timestamp);
                   
                        EventType type;
                        if(req.getServer() == 0) {type = EventType.DEATH_S1a;}
                        else {type = EventType.DEATH_S1b;} //getServer() ==2
                        
                        Event event = new Event(timestamp + getTimeOfNextDeath(Ts1_s1), type);
                        schedule.add(event);
                    }

                }
                
              //since p0_1 + p0_2 = 1, if it's not in S1, it must go to S2
                else {
                    System.out.println("R" + req.getId() + " FROM S0 to S2: " + timestamp);

                	if(state2.queue.size() == K2) {
                	   state2.totalDropped += 1;
                       System.out.println("R" + req.getId() + " DROP S2: " + timestamp);
                	}
                	else {
	                    req.setArrivalTime(timestamp);
	                	state2.queue.add(req);
	                    if (state2.queue.size() <= 1) {
	                    	int serveridb2 = state2.getServerId();
	                        req.setServerId(serveridb2);
	                        req.setStartServiceTime(timestamp);
	                        
	                        state2.numBusyServer++;
	                        state2.setBusyServer(serveridb2);
	                        
	                        System.out.println("R" + req.getId() + " START S2: " + timestamp);
	                        Event event = new Event(timestamp + getTimeOfNextDeath(Ts2_s2), EventType.DEATH_S2);
	                        schedule.add(event);
	                    }
                    }
                	
                }
                
                /**
                 * look for another blocked event in the queue that
                 * wants to execute and schedule it's death.  at this
                 * time the waiting request enters processing time.
                 */
                if (state0.queue.size() > 0) {
                    Request nextReq = state0.queue.peek();

                    nextReq.setStartServiceTime(timestamp);
                    nextReq.setServerId(serverid);

                    state0.numBusyServer++;
                    state0.setBusyServer(serverid);

                    System.out.println("R" + nextReq.getId() + " START S0: " + timestamp);

                    /* Schedule the next death event. */
                    Event nextDeath = new Event(timestamp + getTimeOfNextDeath(Ts0_s0), EventType.DEATH_S0);
                    schedule.add(nextDeath);
                }

                break;

            case DEATH_S1a:
            	  Request req1 = state1.currents[0];
                  req1.setFinishServiceTime(timestamp);
                  int serverid1 = req1.getServer();

                  state1.totalRequestTime += req1.getTq();
                  state1.busyTime[serverid1] += req1.getTs();
                  state1.numCompletedRequests += 1;
                  state1.numBusyServer--;
                  state1.freeBusyServer(serverid1);

                  System.out.println("R" + req1.getId() + " DONE S1," + (req1.getServer()+1) + ": " + timestamp);
		
                  
                  req1.setArrivalTime(timestamp);
                  state3.queue.add(req1);
                  System.out.println("R" + req1.getId() + " FROM S1 to S3: " + timestamp);
                  if (state3.queue.size() <= 1) {
                	  int serveridb3 = state3.getServerId();
                      req1.setServerId(serveridb3);
                      req1.setStartServiceTime(timestamp);
                      
                      state3.numBusyServer++;
                      state3.setBusyServer(serveridb3);
                      
                      System.out.println("R" + req1.getId() + " START S3: " + timestamp);
                      
                      double timeP=randC(Ts1_s3,p1_s3,Ts2_s3,p2_s3,Ts3_s3,p3_s3);
                    
                      Event event = new Event(timestamp + getTimeOfNextDeath(timeP), EventType.DEATH_S3);
                      schedule.add(event);
                  }
                  
                  
		
                /**
                 * look for another blocked event in the queue that
                 * wants to execute and schedule it's death.  at this
                 * time the waiting request enters processing time.
                 */
                  if (state1.queue.size() > 0) {
                    Request nextReq = state1.queue.remove();
                    state1.currents[0] = nextReq;
                    
                    nextReq.setStartServiceTime(timestamp);
                    nextReq.setServerId(serverid1);
                    
                    state1.numBusyServer++;
                    state1.setBusyServer(serverid1);

                    System.out.println("R" + nextReq.getId() + " START S1," + nextReq.getServer() + ": " + timestamp);

                    /* Schedule the next death event. */
                    Event nextDeath = new Event(timestamp + getTimeOfNextDeath(Ts1_s1),EventType.DEATH_S1a);
                    schedule.add(nextDeath);
                }

                break;
                
            case DEATH_S1b:
          	  	Request req11 = state1.currents[1];
                req11.setFinishServiceTime(timestamp);
                int serverid11 = req11.getServer();

                state1.totalRequestTime += req11.getTq();
                state1.busyTime[serverid11] += req11.getTs();
                state1.numCompletedRequests += 1;
                state1.numBusyServer--;
                state1.freeBusyServer(serverid11);

                System.out.println("R" + req11.getId() + " DONE S1," + (req11.getServer()+1) + ": " + timestamp);
		
                
                req11.setArrivalTime(timestamp);
                state3.queue.add(req11);
                System.out.println("R" + req11.getId() + " FROM S1 to S3: " + timestamp);
                if (state3.queue.size() <= 1) {
              	  int serveridb3 = state3.getServerId();
                    req11.setServerId(serveridb3);
                    req11.setStartServiceTime(timestamp);
                    
                    state3.numBusyServer++;
                    state3.setBusyServer(serveridb3);
                    
                    System.out.println("R" + req11.getId() + " START S3: " + timestamp);
                    
                    double timeP=randC(Ts1_s3,p1_s3,Ts2_s3,p2_s3,Ts3_s3,p3_s3);
                  
                    Event event = new Event(timestamp + getTimeOfNextDeath(timeP), EventType.DEATH_S3);
                    schedule.add(event);
                }
                
                
		
              /**
               * look for another blocked event in the queue that
               * wants to execute and schedule it's death.  at this
               * time the waiting request enters processing time.
               */
                if (state1.queue.size() > 0) {
                  Request nextReq = state1.queue.remove();
                  state1.currents[1] = nextReq;
                  nextReq.setStartServiceTime(timestamp);
                  nextReq.setServerId(serverid11);
                  
                  state1.numBusyServer++;
                  state1.setBusyServer(serverid11);

                  System.out.println("R" + nextReq.getId() + " START S1," + nextReq.getServer() + ": " + timestamp);

                  /* Schedule the next death event. */
                  Event nextDeath = new Event(timestamp + getTimeOfNextDeath(Ts1_s1),EventType.DEATH_S1b);
                  schedule.add(nextDeath);
              }

              break;
            
            case DEATH_S2:
            	Request req2=state2.queue.remove();
            	req2.setFinishServiceTime(timestamp);
                int serverid2 = req2.getServer();

                state2.totalRequestTime += req2.getTq();
                state2.busyTime[serverid2] += req2.getTs();
                state2.numCompletedRequests += 1;
                state2.numBusyServer--;
                state2.freeBusyServer(serverid2);

                System.out.println("R" + req2.getId() + " DONE S2: " + timestamp);
                
                req2.setArrivalTime(timestamp);
                state3.queue.add(req2);
                System.out.println("R" + req2.getId() + " FROM S2 to S3: " + timestamp);
                if (state3.queue.size() <= 1) {
              	  int serveridb3 = state3.getServerId();
                    req2.setServerId(serveridb3);
                    req2.setStartServiceTime(timestamp);
                    
                    state3.numBusyServer++;
                    state3.setBusyServer(serveridb3);
                    
                    System.out.println("R" + req2.getId() + " START S3: " + timestamp);
                    
                    double timeP=randC(Ts1_s3,p1_s3,Ts2_s3,p2_s3,Ts3_s3,p3_s3);
                  
                    Event event = new Event(timestamp + getTimeOfNextDeath(timeP), EventType.DEATH_S3);
                    schedule.add(event);
                }
                
                if(state2.queue.size() > 0) {
                	Request nextReq=state2.queue.peek();
                	nextReq.setStartServiceTime(timestamp);
                    nextReq.setServerId(serverid2);
                    
                    state2.numBusyServer++;
                    state2.setBusyServer(serverid2);

                    System.out.println("R" + nextReq.getId() + " START S2: " + timestamp);

                    /* Schedule the next death event. */
                    Event nextDeath = new Event(timestamp + getTimeOfNextDeath(Ts2_s2), EventType.DEATH_S2);
                    schedule.add(nextDeath);
                }
                
                
            	break;
            	
            case DEATH_S3:
            	Request req3=state3.queue.remove();
            	req3.setFinishServiceTime(timestamp);
                int serverid3 = req3.getServer();

                state3.totalRequestTime += req3.getTq();
                state3.busyTime[serverid3] += req3.getTs();
                state3.numCompletedRequests += 1;
                state3.numBusyServer--;
                state3.freeBusyServer(serverid3);

                System.out.println("R" + req3.getId() + " DONE S3: " + timestamp);
            	
                double rando =randC(Ts1_s3,p3_1,Ts2_s3,p3_2,Ts3_s3,p3_out);
                if(rando == Ts3_s3) {
                	Controller.totalTq+=req3.getTotalTq();
                	Controller.completedOut+=1;
                    System.out.println("R" + req3.getId() + " FROM S3 to OUT: " + timestamp);

                }
                if(rando == Ts2_s3) {
                    System.out.println("R" + req3.getId() + " FROM S3 to S2: " + timestamp);
                	if(state2.queue.size() == K2) {
                 	   state2.totalDropped += 1;
                        System.out.println("R" + req3.getId() + " DROP S2: " + timestamp);
                 	}
                 	else {
 	                    req3.setArrivalTime(timestamp);
 	                	state2.queue.add(req3);
 	                    if (state2.queue.size() <= 1) {
 	                    	int serveridb2 = state2.getServerId();
 	                        req3.setServerId(serveridb2);
 	                        req3.setStartServiceTime(timestamp);
 	                        
 	                        state2.numBusyServer++;
 	                        state2.setBusyServer(serveridb2);
 	                        
 	                        System.out.println("R" + req3.getId() + " START S2: " + timestamp);
 	                        Event event = new Event(timestamp + getTimeOfNextDeath(Ts2_s2), EventType.DEATH_S2);
 	                        schedule.add(event);
 	                    }
                     }
                }
                if(rando == Ts1_s3) {
                    System.out.println("R" + req3.getId() + " FROM S3 to S1: " + timestamp);
                    req3.setArrivalTime(timestamp);
                	state1.queue.add(req3);

                    if (state1.queue.size() + state1.numBusyServer <= state1.N) {
//                    	System.out.println(state1.busyServer[0]+", "+ state1.busyServer[1]);
//                    	System.out.println(state1.numBusyServer);
                    	int serveridb1 = state1.getServerId();
                        req3.setServerId(serveridb1);
                        req3.setStartServiceTime(timestamp);
                        
                        state1.currents[serveridb1] = state1.queue.remove();  //item now exist in array of request; acts like servers
                        
                        state1.numBusyServer++;
                        state1.setBusyServer(serveridb1);
                        
                        System.out.println("R" + req3.getId() + " START S1," + (req3.getServer()+1) + ": " + timestamp);
                   
                        EventType type;
                        if(req3.getServer() == 0) {type = EventType.DEATH_S1a;}
                        else {type = EventType.DEATH_S1b;} //getServer() ==2
                        
                        Event event = new Event(timestamp + getTimeOfNextDeath(Ts1_s1), type);
                        schedule.add(event);
                    }
                }
                
                
                
                if(state3.queue.size() > 0) {
                	Request nextReq = state3.queue.peek();
                	nextReq.setStartServiceTime(timestamp);
                    nextReq.setServerId(serverid3);
                    
                    state3.numBusyServer++;
                    state3.setBusyServer(serverid3);

                    System.out.println("R" + nextReq.getId() + " START S3: " + timestamp);

                    /* Schedule the next death event. */
                    Event nextDeath = new Event(timestamp + getTimeOfNextDeath(Ts3_s3), EventType.DEATH_S3);
                    schedule.add(nextDeath);
                }
            	break;
            	
            case BIRTH:
                /**
                 * add the newly born request to the data structure of requests in the system.
                 */
                Request request = new Request(state0.getNextId());

                request.setArrivalTime(timestamp);
                
                request.setFirstTime(timestamp);

		        System.out.println("R" + request.getId() + " ARR: "+timestamp);
		
                state0.queue.add(request);

                /**
                 * if the queue is empty then start executing directly there is no waiting time.
                 */
                if (state0.queue.size() <= 1) {
                    int serveridb = state0.getServerId();
                    request.setServerId(serveridb);
                    request.setStartServiceTime(timestamp);
                    Event event = new Event(timestamp + getTimeOfNextDeath(Ts0_s0), EventType.DEATH_S0);
                    schedule.add(event);

                    state0.numBusyServer++;
                    state0.setBusyServer(serveridb);

                    System.out.println("R" + request.getId() + " START S0" + ": " + timestamp);
                }
                

                /**
                 * schedule the next arrival
                 */
                Event nextArrival = new Event(timestamp + getTimeOfNextBirth(lambda), EventType.BIRTH);
                schedule.add(nextArrival);

                break;


            case MONITOR:
                /**
                 * inspect the data structures describing the simulated system and log them
                 */
                state0.numMonitorEvents += 1;
                state0.totalQueueLen += state0.queue.size();

                state1.numMonitorEvents += 1;
                state1.totalQueueLen += (state1.queue.size()+state1.numBusyServer);
                
                state2.numMonitorEvents += 1;
                state2.totalQueueLen += state2.queue.size();
                
                state3.numMonitorEvents += 1;
                state3.totalQueueLen += state3.queue.size();
                /**
                 * Schedule another monitor event following PASTA principle
                 */
                Event nextMonitor = new Event(timestamp + getTimeOfNextMonitor(lambda), EventType.MONITOR);
                schedule.add(nextMonitor);

                break;
        }
    }

    /* Make sure the events are sorted according to their happening time. */
    public int compareTo(Event e) {
        double diff = time - e.getTime();
        if (diff < 0) {
            return -1;
        } else if (diff > 0) {
            return 1;
        } else {
            return 0;
        }
    }

    /**
     * exponential distribution
     * used by {@link #getTimeOfNextBirth()}, {@link #getTimeOfNextDeath()} and {@link #getTimeOfNextMonitor()}
     * @param rate
     * @return
     */
    private static double exp(double rate) {
        return (- Math.log(1.0 - Math.random()) / rate);
    }

    /**
     *
     * @return time for the next birth event
     */
    public static double getTimeOfNextBirth(double l) {

        return exp(l);
    }

    /**
     *
     * @return time for the next death event
     */
    public static double getTimeOfNextDeath(double timeservice) {
        return exp(1.0/timeservice);
    }

    /**
     *
     * @return time for the next monitor event
     */
    public static double getTimeOfNextMonitor(double l) {
        return exp(l);
    }
    
    public static double randC(double Ts1_s3, double p1_s3, double Ts2_s3, double p2_s3, double Ts3_s3, double p3_s3) {
    	double rando = Math.random();
    	double r=p1_s3;
    	
    	if(rando<r) {return Ts1_s3;}
    	
    	r+=p2_s3;
    	if(rando<r) {return Ts2_s3;}
    	
    	r+=p3_s3;
    	if(rando<r) {return Ts3_s3;}

    	return -1;
    }
    
    
    
    
//    if (state0.queue.size() == K) {
//        state.totalDropped += 1;
//        System.out.println("R" + request.getId() + " REDIR: " + timestamp);
//
//	    state.queue_sec.add(request);
//	    if (state.queue_sec.size() <= 1) {
//		request.setServerId(N);
//		request.setStartServiceTime(timestamp);
//		Event nextDeath = new Event(timestamp + getTimeOfNextDeath(Ts2),EventType.DEATH_SEC);
//        schedule.add(nextDeath);
//        System.out.println("R" + request.getId() + " START " + request.getServer() + ": " + timestamp);	
//}
//    } else {
//        state.queue.add(request);
//
//        /**
//         * if the queue is empty then start executing directly there is no waiting time.
//         */
//        if (state.queue.size() <= N) {
//            int serveridb = state.getServerId();
//            request.setServerId(serveridb);
//            request.setStartServiceTime(timestamp);
//            Event event = new Event(timestamp + getTimeOfNextDeath(Ts),
//			EventType.DEATH);
//            schedule.add(event);
//
//            state.numBusyServer++;
//            state.setBusyServer(serveridb);
//
//            System.out.println("R" + request.getId() + " START "
//		   + request.getServer() + ": " + timestamp);
//        }
//    }
}
