import java.util.Random;

public class Main {

    public static void main(String[] args) {

        long start = System.currentTimeMillis();
        int X = 100000, Y = 50, Z = 50, cnt = 0;
        int N = Integer.parseInt(args[0]);

        Thread[] threads = new Thread[N];
        Flag no = new Flag();
        Matriz A = new Matriz (X,Y);
        Matriz B = new Matriz (Y,Z);
        Matriz C = new Matriz(X,Z);

        A.init();
        B.init();

        no.job = new int[2][X*Z];

        for(int i = 0; i < X; i++) {
            for(int j = 0; j < Z; j++) {
                no.job[0][cnt] = i;
                no.job[1][cnt] = j;
                cnt++;
            }
        }

        for(int i = 0 ; i< N;i++) {
            threads[i] = new Thread(new MultMatriz(A, B, C, no, X*Z));
            threads[i].start();
            try {
                threads[i].join();
            } catch (InterruptedException e) {}
        }

        System.out.println(System.currentTimeMillis() - start);
    }
}

class Matriz {

    double[][] arr;

    public Matriz(int row, int col) {
        arr = new double[row][col];
    }

    public void set(int i, int j,double val) {
        arr[i][j] = val;
    }

    public double get(int i, int j) {
        return arr[i][j];
    }

    public void init() {

        Random r = new Random();

        for(int i = 0 ; i < arr.length; i++){
            for(int j = 0; j< arr[0].length; j++){
                arr[i][j] = r.nextInt(10);
            }
        }
    }
}

class MultMatriz implements Runnable {

    Matriz A, B, C;
    int l, c, id;
    Flag no;

    public MultMatriz(Matriz A, Matriz B, Matriz C, Flag no, int sum) {
        this.A = A;
        this.B = B;
        this.C = C;
        this.no = no;
        this.id = sum;
    }

    public void run() {

        int tmp = 0;

        synchronized (no) {
            tmp = no.cnt;
            if(no.cnt >= id) {
                return;
            }

            c = no.job[1][tmp];
            l = no.job[0][tmp];
            no.cnt++;
        }

        while(tmp < id) {
            double val = 0;
            int len = B.arr.length;

            for(int k = 0; k < len; k++) {
                val += A.get(l, k) * B.get(k, c);
            }

            C.set(l, c, val);

            synchronized (no) {
                tmp = no.cnt;
                if(no.cnt >= id) {
                    return;
                }

                c = no.job[1][tmp];
                l = no.job[0][tmp];
                no.cnt++;
            }
        }
    }
}

class Flag {

    int id;
    int cnt;
    int[][] job;

    public Flag() {
        cnt = 0;
    }
}
