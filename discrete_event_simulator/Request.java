package hw4;


public class Request {
    /**
     * implement the Request class, which should include the necessary information to do statistics
     * over all the requests. e.g., to calculate Tq, one needs to record the arrival time, and finish time
     * of the request
     */
    private int requestId;
    private double arrivalTime;
    private double startServiceTime;
    private double finishServiceTime;
    private int serverId;
    private double firstTime;
    
    public Request(int id) {
        requestId = id;
        serverId = -1;
    }

    public int getId() {
        return requestId;
    }

    public void setServerId(int id) {
    	serverId = id;
    }

    public int getServer() {
    	return serverId;
    }

    public void setArrivalTime(double time) {
        arrivalTime = time;
    }

    public double getArrivalTime() {
        return arrivalTime;
    }

    public void setStartServiceTime(double time) {
        startServiceTime = time;
    }

    public double getStartServiceTime() {
        return startServiceTime;
    }

    public void setFinishServiceTime(double time) {
        finishServiceTime = time;
    }

    public double getFinishServiceTime() {
        return finishServiceTime;
    }

    /**
     * Get total response time for this request
     */
    public double getTq() {
        return finishServiceTime - arrivalTime;
    }

    /**
     * Get service time for this request
     */
    public double getTs() {
        return finishServiceTime - startServiceTime;
    }
    
    public void setFirstTime(double e) {
    	firstTime=e;
    }
    public double getTotalTq() {
    	return finishServiceTime-firstTime;
    
    }
}