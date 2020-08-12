package hw4;

public class Simulator {
    static double lambda;

    static double Ts0_s0;
    static double Ts1_s1;
    static double Ts2_s2;

    static double Ts1_s3;
    static double Ts2_s3;
    static double Ts3_s3;
    
    static double p1_s3;
    static double p2_s3;
    static double p3_s3;

    static int K2;

    static double p0_1;
    static double p0_2;
    static double p3_out;
    static double p3_1;
    static double p3_2;
    
    

    public static void simulate(double time) {
        Controller.runSimulation(time, lambda, Ts0_s0, Ts1_s1, Ts2_s2, Ts1_s3, p1_s3, Ts2_s3, p2_s3, Ts3_s3, p3_s3, K2, p0_1, p0_2, p3_out, p3_1, p3_2);
    }

    public static void main(String args[]) {
        double time = Double.parseDouble(args[0]);

        lambda = Double.parseDouble(args[1]);

        Ts0_s0 = Double.parseDouble(args[2]);
        Ts1_s1 = Double.parseDouble(args[3]);
        Ts2_s2 = Double.parseDouble(args[4]);
        
        Ts1_s3= Double.parseDouble(args[5]);
        p1_s3= Double.parseDouble(args[6]);

        Ts2_s3= Double.parseDouble(args[7]);
        p2_s3= Double.parseDouble(args[8]);
        
        Ts3_s3= Double.parseDouble(args[9]);
        p3_s3= Double.parseDouble(args[10]);

        K2 = Integer.parseInt(args[11]);
	
        p0_1= Double.parseDouble(args[12]);
        p0_2= Double.parseDouble(args[13]);
        p3_out= Double.parseDouble(args[14]);
        p3_1= Double.parseDouble(args[15]);
        p3_2= Double.parseDouble(args[16]);




        simulate(time);
    }
}

